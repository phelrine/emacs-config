;;; mcp-config.el --- MCP server configuration helpers -*- lexical-binding: t; -*-
;;; Commentary:
;; Resolve `mcp-hub-servers' from optional per-machine override files.
;; Place configuration under `local/mcp/` (ignored by Git):
;;   servers.el              ; overrides for this machine
;; Each file should evaluate to a Lisp list of (NAME . SPEC) pairs.
;; Use `:remove' as the SPEC to drop a server from earlier layers.
;; Environment values can reference secrets using
;;   (:auth-source :host "api.github.com" :user "example^mcp")
;; which is resolved via `auth-source-search' when composing the list.
;; The defaults include the `fetch' and GitHub servers previously hard-coded
;; in `init.el'; local overrides only need to tweak or remove entries.
;;; Code:

(require 'auth-source)
(require 'subr-x)

(defconst mcp-config-local-root
  (expand-file-name "local/mcp" user-emacs-directory)
  "Directory that holds optional MCP configuration overrides.")

(defun mcp-config-default-servers ()
  "Return the default set of MCP servers shipped with the configuration."
  '(("fetch" . (:command "uvx"
                         :args ("mcp-server-fetch")))
    ("github" . (:command "docker"
                           :args ("run" "-i" "--rm"
                                   "-e" "GITHUB_PERSONAL_ACCESS_TOKEN"
                                   "ghcr.io/github/github-mcp-server")
                           :env (:GITHUB_PERSONAL_ACCESS_TOKEN
                                 (:auth-source :host "api.github.com"
                                               :user "phelrine^mcp"))))))

(defun mcp-config--normalize-secret (secret)
  "Return the underlying string value for SECRET."
  (cond
   ((functionp secret) (funcall secret))
   (secret secret)
   (t nil)))

(defun mcp-config--read-file (file)
  "Read server overrides from FILE."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((form (condition-case nil
                      (read (current-buffer))
                    (end-of-file nil)
                    (error nil))))
        (when (and form (eq (car-safe form) 'quote))
          (setq form (cadr form)))
        (when (listp form)
          form)))))

(defun mcp-config--read-env-file ()
  "Read overrides from the file pointed to by `MCP_SERVERS_FILE'."
  (let ((path (getenv "MCP_SERVERS_FILE")))
    (when (and path (not (string-empty-p path)))
      (mcp-config--read-file path))))

(defun mcp-config--merge-servers (base overrides)
  "Return BASE updated with OVERRIDES.
If a server in OVERRIDES uses `:remove' as its SPEC, drop it from the result."
  (if (null overrides)
      base
    (let ((result (copy-sequence base)))
      (dolist (entry overrides result)
        (let ((name (car entry))
              (spec (cdr entry)))
          (if (eq spec :remove)
              (setq result (assoc-delete-all name result))
            (let ((existing (assoc name result)))
              (if existing
                  (setcdr existing spec)
                (setq result (append result (list (cons name spec))))))))))))

(defun mcp-config--resolve-env-value (value)
  "Resolve VALUE for environment entries, handling auth-source placeholders."
  (if (and (listp value)
           (keywordp (car value))
           (eq (car value) :auth-source))
      (let* ((plist (cdr value))
             (host (plist-get plist :host))
             (user (plist-get plist :user))
             (max (or (plist-get plist :max) 1))
             (entry (car (auth-source-search :host host :user user :max max)))
             (secret (plist-get plist :secret)))
        (mcp-config--normalize-secret
         (or secret (plist-get entry :secret))))
    value))

(defun mcp-config--resolve-env (env-plist)
  "Resolve ENV-PLIST and expand placeholder values."
  (let ((result nil)
        (plist env-plist))
    (while plist
      (let ((key (pop plist))
            (val (pop plist)))
        (setq result (plist-put result key (mcp-config--resolve-env-value val)))))
    result))

(defun mcp-config--resolve-spec (spec)
  "Resolve SPEC, post-processing environment placeholders."
  (let ((result nil)
        (plist spec))
    (while plist
      (let ((key (pop plist))
            (val (pop plist)))
        (setq result (plist-put result key
                                (if (and (eq key :env) (listp val))
                                    (mcp-config--resolve-env val)
                                  val)))))
    result))

(defun mcp-config-resolve-servers ()
  "Compose the final list of MCP servers."
  (let* ((defaults (mcp-config-default-servers))
         (shared-file (expand-file-name "servers.el" mcp-config-local-root))
         (overrides (list (mcp-config--read-file shared-file)
                          (mcp-config--read-env-file)))
         (servers defaults))
    (dolist (override overrides)
      (setq servers (mcp-config--merge-servers servers override)))
    (mapcar (lambda (entry)
              (cons (car entry)
                    (mcp-config--resolve-spec (cdr entry))))
            servers)))

(provide 'mcp-config)

;;; mcp-config.el ends here
