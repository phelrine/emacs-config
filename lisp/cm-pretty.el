;;; cm-pretty.el --- Prettier overlay display for CriticMarkup -*- lexical-binding: t; -*-

;;; Commentary:

;; A display layer on top of cm-mode that renders CriticMarkup like a
;; GitHub review, using overlays only -- the buffer text never changes.
;;
;; Display model (ghost rendering):
;; - Comments {>>...<<}: the 💬 anchor stays at the original position
;;   and the body is always rendered on the next visual line as
;;   " └ body", even mid-line.  TAB (on the anchor) folds it to 💬…
;; - Substitutions {~~old~>new~~}: the old text is shown struck
;;   through with a ✎ indicator.  TAB renders " └ → new" below.
;;
;; Editing model (popup buffer):
;; Overlay strings cannot be edited in place, so editing happens in a
;; dedicated buffer.
;; - RET (or click) on 💬 / ✎ opens the editor at the bottom;
;;   C-c C-c applies, C-c C-k cancels.
;; - `cm-pretty-comment' inserts new markup and opens the editor;
;;   cancelling removes the freshly inserted markup entirely.
;;
;; Markup samples inside code spans, code blocks, and HTML comments are
;; left alone.  Toggle `cm-pretty-mode' (C-c * p) to see raw markup.

;;; Code:

(require 'seq)

(defvar cm-author)                      ; defined by cm-mode

(defgroup cm-pretty nil
  "Prettier overlay display for CriticMarkup."
  :group 'criticmarkup)

;;; Faces
;; cm-mode's own font-lock (strike-through etc.) sits underneath, so
;; attributes we want to override must be set explicitly even to nil,
;; or the underlying value bleeds through.

(defface cm-pretty-comment-face
  '((((background light)) :background "#eaeef2" :foreground "#57606a" :strike-through nil)
    (t :background "#30363d" :foreground "#a0a8b0" :strike-through nil))
  "Face for CriticMarkup comment bodies."
  :group 'cm-pretty)

(defface cm-pretty-author-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the author tag at the start of a comment."
  :group 'cm-pretty)

(defface cm-pretty-highlight-face
  '((((background light)) :background "#fff3c4" :strike-through nil)
    (t :background "#4a4000" :strike-through nil))
  "Face for CriticMarkup highlighted (commented-on) text."
  :group 'cm-pretty)

(defface cm-pretty-added-face
  '((((background light)) :foreground "#116329" :background "#dafbe1" :strike-through nil)
    (t :foreground "#7ee787" :background "#12261e" :strike-through nil))
  "Face for text proposed to be added."
  :group 'cm-pretty)

(defface cm-pretty-deleted-face
  '((((background light)) :foreground "#82071e" :background "#ffebe9" :strike-through t)
    (t :foreground "#ffa198" :background "#3c1618" :strike-through t))
  "Face for text proposed to be deleted."
  :group 'cm-pretty)

;;; Markup regexps

(defconst cm-pretty--comment-re "{>>\\(\\(?:.\\|\n\\)*?\\)<<}"
  "Regexp matching a CriticMarkup comment; group 1 is the body.")

(defconst cm-pretty--author-re "\\`\\s-*\\(@[[:alnum:]_-]+\\)"
  "Regexp matching an author tag at the start of a comment body.")

(defconst cm-pretty--highlight-re "{==\\(\\(?:.\\|\n\\)*?\\)==}"
  "Regexp matching a CriticMarkup highlight; group 1 is the body.")

(defconst cm-pretty--substitution-re
  "{~~\\(\\(?:.\\|\n\\)*?\\)\\(~>\\)\\(\\(?:.\\|\n\\)*?\\)~~}"
  "Regexp matching a CriticMarkup substitution.
Group 1 is the old text, group 2 the arrow, group 3 the new text.")

(defconst cm-pretty--addition-re "{\\+\\+\\(\\(?:.\\|\n\\)*?\\)\\+\\+}"
  "Regexp matching a CriticMarkup addition; group 1 is the body.")

(defconst cm-pretty--deletion-re "{--\\(\\(?:.\\|\n\\)*?\\)--}"
  "Regexp matching a CriticMarkup deletion; group 1 is the body.")

(defconst cm-pretty--delimiter-re
  (regexp-opt '("{>>" "<<}" "{==" "==}" "{~~" "~~}" "{++" "++}" "{--" "--}"))
  "Regexp matching any CriticMarkup delimiter token.")

;;; Region helpers

(defun cm-pretty-region-has-markup-p (beg end)
  "Return non-nil if any CriticMarkup delimiter overlaps BEG..END."
  (save-excursion
    (goto-char (max (point-min) (- beg 2)))
    (let ((limit (min (point-max) (+ end 2)))
          (found nil))
      (while (and (not found)
                  (re-search-forward cm-pretty--delimiter-re limit t))
        (when (and (< (match-beginning 0) end)
                   (> (match-end 0) beg))
          (setq found t)))
      found)))

(defun cm-pretty--skip-match-p (pos)
  "Return non-nil when markup at POS should be left alone.
Markup inside code spans, code blocks, and HTML comments is treated
as sample notation and not prettified.  `syntax-ppss' moves point, so
position and match data are preserved to keep this safe to call from
the middle of a search loop."
  (save-match-data
    (save-excursion
      (or (nth 4 (syntax-ppss pos))
          (and (derived-mode-p 'markdown-mode)
               (or (and (fboundp 'markdown-inline-code-at-pos-p)
                        (markdown-inline-code-at-pos-p pos))
                   (and (fboundp 'markdown-code-block-at-pos)
                        (markdown-code-block-at-pos pos))))))))

;;; Fold state
;; Recorded as markers so positions survive edits earlier in the
;; buffer.  Only deviations from the default state are stored: folded
;; comments, and expanded substitutions.

(defvar-local cm-pretty--folded-comments nil
  "Markers at the start of comments the user has folded.")

(defvar-local cm-pretty--expanded-substitutions nil
  "Markers at the start of substitutions the user has expanded.")

(defun cm-pretty--marker-at (pos markers)
  "Return the marker in MARKERS positioned at POS, if any."
  (seq-find (lambda (m) (and (marker-position m) (= (marker-position m) pos)))
            markers))

(defun cm-pretty--free-markers (markers)
  "Detach every marker in MARKERS from its buffer."
  (dolist (m markers)
    (when (markerp m)
      (set-marker m nil))))

(defun cm-pretty--toggle-membership (pos markers)
  "Return MARKERS with POS toggled: removed if present, added otherwise."
  (if-let* ((m (cm-pretty--marker-at pos markers)))
      (progn (set-marker m nil)
             (delq m markers))
    (cons (copy-marker pos) markers)))

(defun cm-pretty--prune-markers (markers valid-positions)
  "Drop markers in MARKERS that point at none of VALID-POSITIONS."
  (seq-filter (lambda (m)
                (or (and (marker-position m)
                         (memql (marker-position m) valid-positions))
                    (ignore (set-marker m nil))))
              markers))

;;; Commands: toggle and target selection

(defun cm-pretty--markup-at-point ()
  "Return the most relevant markup overlay at point.
Where overlays of several markups overlap (e.g. the boundary between
adjacent comments), pick the one with the latest start position --
that is the markup the cursor is visually on."
  (let (best)
    (dolist (o (overlays-at (point)) best)
      (when-let* ((g (overlay-get o 'cm-pretty-group)))
        (and (overlay-get o 'cm-pretty-kind)
             (when (or (null best) (> g (overlay-get best 'cm-pretty-group)))
               (setq best o)))))))

(defun cm-pretty-toggle ()
  "Fold/unfold the comment, or collapse/expand the substitution, at point."
  (interactive)
  (when-let* ((ov (cm-pretty--markup-at-point))
              (group (overlay-get ov 'cm-pretty-group)))
    (pcase (overlay-get ov 'cm-pretty-kind)
      ('comment
       (setq cm-pretty--folded-comments
             (cm-pretty--toggle-membership group cm-pretty--folded-comments)))
      ('substitution
       (setq cm-pretty--expanded-substitutions
             (cm-pretty--toggle-membership group cm-pretty--expanded-substitutions))))
    (cm-pretty--refresh)))

(defvar cm-pretty-toggle-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'cm-pretty-toggle)
    map)
  "Keymap active while point is on a toggleable cm-pretty overlay.")

(defvar cm-pretty-anchor-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'cm-pretty-toggle)
    (define-key map (kbd "RET") #'cm-pretty-edit)
    (define-key map (kbd "<mouse-1>") #'cm-pretty-edit)
    map)
  "Keymap for the 💬 / ✎ anchors: TAB toggles, RET or click edits.")

;;; Overlay construction

(defun cm-pretty--make-overlay (beg end &rest props)
  "Create a cm-pretty overlay from BEG to END with PROPS (plist)."
  (let ((ov (make-overlay beg end nil t nil)))
    (overlay-put ov 'cm-pretty t)
    (overlay-put ov 'evaporate t)
    (while props
      (overlay-put ov (pop props) (pop props)))
    ov))

(defun cm-pretty--markup-overlay (beg end kind group &rest props)
  "Create an overlay for a markup part, tagged with KIND and GROUP.
BEG..END and PROPS are as in `cm-pretty--make-overlay'."
  (apply #'cm-pretty--make-overlay beg end
         'cm-pretty-kind kind
         'cm-pretty-group group
         props))

(defun cm-pretty--clear ()
  "Remove all cm-pretty overlays in the current buffer."
  (dolist (o (overlays-in (point-min) (point-max)))
    (when (overlay-get o 'cm-pretty)
      (delete-overlay o))))

(defvar cm-pretty--block-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mouse-1>") #'cm-pretty-block-click)
    (define-key map (kbd "TAB") #'cm-pretty-toggle)
    map)
  "Keymap on ghost-block carrier overlays: click edits, TAB folds.")

(defun cm-pretty--attach-block (pos block group kind)
  "Display BLOCK (a string) below the line containing POS.
GROUP/KIND identify the markup the block belongs to.  The block is
prefixed with a cursor-carrying space so that when point is at the
end of the line, the cursor is drawn at the real insertion point
instead of at the end of the ghost line.  (A newline glyph cannot
carry the `cursor' property.)"
  (let ((block (concat (propertize " " 'cursor t) block))
        (eol (save-excursion (goto-char pos) (line-end-position))))
    (cond
     ;; Render via before-string just before the newline character.
     ((< eol (point-max))
      (cm-pretty--markup-overlay eol (1+ eol) kind group
                                 'before-string block
                                 'keymap cm-pretty--block-map))
     ;; End of buffer without a trailing newline: attach an
     ;; after-string to the last character instead (a zero-length
     ;; overlay would not be found by `overlays-in').
     ((> eol (point-min))
      (cm-pretty--markup-overlay (1- eol) eol kind group
                                 'after-string block
                                 'keymap cm-pretty--block-map)))))

(defun cm-pretty--prettify-simple (re body-face)
  "Prettify every match of RE: hide 3-char delimiters, give body BODY-FACE."
  (goto-char (point-min))
  (while (re-search-forward re nil t)
    (unless (cm-pretty--skip-match-p (match-beginning 0))
      (cm-pretty--make-overlay (match-beginning 0) (+ (match-beginning 0) 3)
                               'invisible 'cm-pretty)
      (cm-pretty--make-overlay (- (match-end 0) 3) (match-end 0)
                               'invisible 'cm-pretty)
      (cm-pretty--make-overlay (match-beginning 1) (match-end 1)
                               'face body-face))))

;;; Comments

(defun cm-pretty--comment-block (body)
  "Build the ghost block string for a comment BODY.
BODY comes from `match-string' and carries the buffer's text
properties (including cm-mode's fontification), so strip them
before applying our own faces."
  (let ((text (substring-no-properties body)))
    (add-face-text-property 0 (length text) 'cm-pretty-comment-face t text)
    (when (string-match cm-pretty--author-re body)
      (add-face-text-property (match-beginning 1) (match-end 1)
                              'cm-pretty-author-face nil text))
    (concat "\n └ " text)))

(defun cm-pretty--adjacent-highlight-start (beg)
  "Return the start of the highlight ending right before BEG, if any."
  (when (and (>= (- beg 3) (point-min))
             (equal (buffer-substring-no-properties (- beg 3) beg) "==}"))
    (save-excursion
      (goto-char beg)
      (when-let* ((hl-beg (search-backward "{==" nil t)))
        ;; Make sure what we found is actually the highlight that ends
        ;; at BEG, not some unrelated earlier "{==".
        (and (looking-at cm-pretty--highlight-re)
             (= (match-end 0) beg)
             hl-beg)))))

(defun cm-pretty--comment-reach-overlays (beg end folded)
  "Add TAB-reach overlays around the comment markup at BEG..END.
At the boundary of invisible delimiters the cursor is drawn on the
next visible glyph (the icon, the highlight, ...), so TAB must work
at every position that visually belongs to the comment.  FOLDED is
the comment's fold state."
  ;; Leading indentation: when the comment is the first thing on its
  ;; line, make TAB work there too -- otherwise the first TAB gets
  ;; consumed as indentation and only moves point onto the icon.
  (let ((bol (save-excursion (goto-char beg) (line-beginning-position))))
    (when (and (< bol beg)
               (save-excursion
                 (goto-char bol)
                 (skip-chars-forward " \t")
                 (= (point) beg)))
      (cm-pretty--markup-overlay bol beg 'comment beg
                                 'keymap cm-pretty-toggle-map)))
  ;; Right after the markup (where point snapping lands).  At end of
  ;; line the ghost carrier covers this, so only add it mid-line, or
  ;; when folded and the carrier is absent.
  (when (and (< end (point-max))
             (or folded (not (eq (char-after end) ?\n))))
    (cm-pretty--markup-overlay end (1+ end) 'comment beg
                               'keymap cm-pretty-toggle-map))
  ;; A ranged comment ({==target==}{>>...<<}): make TAB work on the
  ;; highlighted target and its hidden delimiters as well.
  (when-let* ((hl-beg (cm-pretty--adjacent-highlight-start beg)))
    (cm-pretty--markup-overlay hl-beg (- beg 3) 'comment beg
                               'keymap cm-pretty-toggle-map)
    (cm-pretty--markup-overlay (- beg 3) beg 'comment beg
                               'keymap cm-pretty-toggle-map)))

(defun cm-pretty--prettify-comments ()
  "Prettify comments; return the list of comment start positions."
  (let (starts)
    (goto-char (point-min))
    (while (re-search-forward cm-pretty--comment-re nil t)
      (let* ((beg (match-beginning 0))
             (end (match-end 0))
             (body (match-string 1))
             (folded (cm-pretty--marker-at beg cm-pretty--folded-comments)))
        (unless (cm-pretty--skip-match-p beg)
          (push beg starts)
          (cm-pretty--markup-overlay beg (+ beg 3) 'comment beg
                                     'display (if folded "💬… " "💬 ")
                                     'keymap cm-pretty-anchor-map)
          (cm-pretty--markup-overlay (match-beginning 1) (match-end 1)
                                     'comment beg
                                     'invisible 'cm-pretty
                                     'cm-pretty-hidden (cons beg end))
          (cm-pretty--markup-overlay (- end 3) end 'comment beg
                                     'invisible 'cm-pretty
                                     'cm-pretty-hidden (cons beg end)
                                     'keymap cm-pretty-toggle-map)
          (cm-pretty--comment-reach-overlays beg end folded)
          (unless folded
            (cm-pretty--attach-block end (cm-pretty--comment-block body)
                                     beg 'comment)))))
    starts))

;;; Substitutions

(defun cm-pretty--substitution-block (new)
  "Build the ghost diff block string for a substitution's NEW text.
NEW comes from `match-string'; strip its copied text properties so
cm-mode's strike-through cannot bleed into the diff block."
  (let ((text (concat "→ " (substring-no-properties new))))
    (add-face-text-property 0 (length text) 'cm-pretty-added-face t text)
    (concat "\n └ " text)))

(defun cm-pretty--prettify-substitutions ()
  "Prettify substitutions; return the list of their start positions."
  (let (starts)
    (goto-char (point-min))
    (while (re-search-forward cm-pretty--substitution-re nil t)
      (let* ((beg (match-beginning 0))
             (end (match-end 0))
             (new (match-string 3))
             (arrow-beg (match-beginning 2))
             (expanded (cm-pretty--marker-at beg cm-pretty--expanded-substitutions)))
        (unless (cm-pretty--skip-match-p beg)
          (push beg starts)
          (cm-pretty--markup-overlay beg (+ beg 3) 'substitution beg
                                     'invisible 'cm-pretty
                                     'cm-pretty-hidden (cons beg (+ beg 3))
                                     'keymap cm-pretty-toggle-map)
          (cm-pretty--markup-overlay (- end 3) end 'substitution beg
                                     'invisible 'cm-pretty
                                     'cm-pretty-hidden (cons arrow-beg end)
                                     'keymap cm-pretty-toggle-map)
          (cm-pretty--markup-overlay (match-beginning 1) (match-end 1)
                                     'substitution beg
                                     'face 'cm-pretty-deleted-face
                                     'keymap cm-pretty-toggle-map)
          (cm-pretty--markup-overlay arrow-beg (match-end 2) 'substitution beg
                                     'display "✎"
                                     'cm-pretty-hidden (cons arrow-beg end)
                                     'keymap cm-pretty-anchor-map)
          (cm-pretty--markup-overlay (match-beginning 3) (match-end 3)
                                     'substitution beg
                                     'invisible 'cm-pretty
                                     'cm-pretty-hidden (cons arrow-beg end)
                                     'keymap cm-pretty-toggle-map)
          (when expanded
            (cm-pretty--attach-block end (cm-pretty--substitution-block new)
                                     beg 'substitution)))))
    starts))

;;; Point adjustment
;; If the cursor rests inside hidden markup (a comment body, the new
;; text of a substitution, ...), the displayed cursor position and the
;; insertion position diverge -- e.g. typing right after the icon
;; would put the first character inside the comment.  Snap point out,
;; in the direction it was moving.

(defvar-local cm-pretty--last-point 1
  "Point position after the previous command, used for snap direction.")

(defun cm-pretty--point-adjust ()
  "Keep point out of hidden markup regions."
  (with-demoted-errors "cm-pretty: %S"
    (when-let* ((span (seq-some (lambda (o) (overlay-get o 'cm-pretty-hidden))
                                (overlays-at (point)))))
      (when (and (> (point) (car span)) (< (point) (cdr span)))
        (goto-char (if (< (point) cm-pretty--last-point)
                       (car span)
                     (cdr span)))))
    (setq cm-pretty--last-point (point))))

;;; Refresh

(defun cm-pretty--refresh ()
  "Rescan the buffer and rebuild all cm-pretty overlays."
  (cm-pretty--clear)
  (save-excursion
    (cm-pretty--prettify-simple cm-pretty--highlight-re 'cm-pretty-highlight-face)
    (cm-pretty--prettify-simple cm-pretty--addition-re 'cm-pretty-added-face)
    (cm-pretty--prettify-simple cm-pretty--deletion-re 'cm-pretty-deleted-face)
    (let ((subst-starts (cm-pretty--prettify-substitutions))
          (comment-starts (cm-pretty--prettify-comments)))
      (setq cm-pretty--expanded-substitutions
            (cm-pretty--prune-markers cm-pretty--expanded-substitutions subst-starts))
      (setq cm-pretty--folded-comments
            (cm-pretty--prune-markers cm-pretty--folded-comments comment-starts)))))

;;; Popup editor
;; Overlay-rendered strings cannot be edited in place, so editing
;; happens in a dedicated buffer: C-c C-c applies, C-c C-k cancels
;; (and removes freshly inserted markup for new comments).

(defvar-local cm-pretty-edit--source-buffer nil
  "Source buffer the edit buffer writes back to.")

(defvar-local cm-pretty-edit--beg nil
  "Marker at the start of the edited region in the source buffer.")

(defvar-local cm-pretty-edit--end nil
  "Marker at the end of the edited region in the source buffer.")

(defvar-local cm-pretty-edit--cleanup nil
  "List of (BEG-MARKER . END-MARKER) to delete when the edit is cancelled.
Used to undo the insertion of a brand-new comment; nil when editing
existing markup.")

(defvar cm-pretty-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'cm-pretty-edit-apply)
    (define-key map (kbd "C-c C-k") #'cm-pretty-edit-cancel)
    map)
  "Keymap for `cm-pretty-edit-mode'.")

(define-minor-mode cm-pretty-edit-mode
  "Minor mode for the cm-pretty popup edit buffer.
\\{cm-pretty-edit-mode-map}"
  :lighter " CMP-edit")

(defun cm-pretty--edit-free-markers ()
  "Detach all source markers held by the current edit buffer."
  (cm-pretty--free-markers (list cm-pretty-edit--beg cm-pretty-edit--end))
  (dolist (pair cm-pretty-edit--cleanup)
    (set-marker (car pair) nil)
    (set-marker (cdr pair) nil))
  (setq cm-pretty-edit--beg nil
        cm-pretty-edit--end nil
        cm-pretty-edit--cleanup nil))

(defun cm-pretty--edit-open (src beg end &optional cleanup-pairs)
  "Open the popup editor for SRC's region BEG..END.
CLEANUP-PAIRS is a list of (BEG . END) regions in SRC to delete on
cancel.  Return the edit buffer."
  (let ((buf (get-buffer-create "*cm-pretty-edit*"))
        (text (with-current-buffer src
                (buffer-substring-no-properties beg end)))
        (mbeg (with-current-buffer src (copy-marker beg)))
        (mend (with-current-buffer src (copy-marker end t)))
        (mpairs (with-current-buffer src
                  (mapcar (lambda (p)
                            (cons (copy-marker (car p))
                                  (copy-marker (cdr p) t)))
                          cleanup-pairs))))
    (with-current-buffer buf
      (cm-pretty--edit-free-markers)    ; in case a session was left open
      (erase-buffer)
      (insert text)
      (goto-char (point-max))
      (visual-line-mode 1)
      (cm-pretty-edit-mode 1)
      (setq cm-pretty-edit--source-buffer src
            cm-pretty-edit--beg mbeg
            cm-pretty-edit--end mend
            cm-pretty-edit--cleanup mpairs)
      (setq header-line-format
            "Edit comment: C-c C-c to apply, C-c C-k to cancel"))
    (unless noninteractive
      (select-window
       (display-buffer buf '((display-buffer-at-bottom)
                             (window-height . 8)))))
    buf))

(defun cm-pretty--edit-close ()
  "Tear down the edit buffer and its markers."
  (cm-pretty--edit-free-markers)
  (let* ((buf (current-buffer))
         (win (get-buffer-window buf)))
    (if (and win (not noninteractive))
        (quit-window t win)
      (kill-buffer buf))))

(defun cm-pretty-edit-apply ()
  "Write the edit buffer's content back into the source markup."
  (interactive)
  ;; Capture buffer-local values before switching buffers.
  (let ((text (replace-regexp-in-string "\n+" " " (string-trim (buffer-string))))
        (src cm-pretty-edit--source-buffer)
        (beg cm-pretty-edit--beg)
        (end cm-pretty-edit--end))
    (unless (buffer-live-p src)
      (user-error "Source buffer no longer exists"))
    (with-current-buffer src
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char beg)
          (delete-region beg end)
          (insert text)))))
  (cm-pretty--edit-close))

(defun cm-pretty-edit-cancel ()
  "Discard the edit; for a new comment, remove the inserted markup too."
  (interactive)
  ;; Capture buffer-local values before switching buffers.
  (let ((src cm-pretty-edit--source-buffer)
        (cleanup cm-pretty-edit--cleanup))
    (when (and cleanup (buffer-live-p src))
      (with-current-buffer src
        (let ((inhibit-read-only t))
          (dolist (pair cleanup)
            (delete-region (car pair) (cdr pair)))))))
  (cm-pretty--edit-close))

(defun cm-pretty--edit-group (beg)
  "Open the popup editor for the markup starting at BEG."
  (save-excursion
    (goto-char beg)
    (cond
     ((looking-at cm-pretty--comment-re)
      (cm-pretty--edit-open (current-buffer)
                            (match-beginning 1) (match-end 1)))
     ((looking-at cm-pretty--substitution-re)
      (cm-pretty--edit-open (current-buffer)
                            (match-beginning 3) (match-end 3))))))

(defun cm-pretty-edit ()
  "Edit the comment body or replacement text of the markup at point."
  (interactive)
  (when-let* ((ov (cm-pretty--markup-at-point))
              (beg (overlay-get ov 'cm-pretty-group)))
    (cm-pretty--edit-group beg)))

(defun cm-pretty-block-click (event)
  "Open the editor when EVENT clicks a ghost block; else move point.
Whether the click landed on the overlay string is detectable, so
ordinary end-of-line clicks keep their normal behavior."
  (interactive "e")
  (let* ((posn (event-start event))
         (on-string (posn-string posn)))
    (if-let* ((_ on-string)
              (pos (posn-point posn))
              (group (seq-some (lambda (o) (overlay-get o 'cm-pretty-group))
                               (overlays-at pos))))
        (cm-pretty--edit-group group)
      (mouse-set-point event))))

;;; Insertion commands
;; Both insert markup and immediately open the popup editor; typing
;; into the raw markup would be invisible under the ghost rendering.

(defun cm-pretty--barf-if-region-has-markup (beg end)
  "Signal a `user-error' when BEG..END overlaps existing markup."
  (when (cm-pretty-region-has-markup-p beg end)
    (user-error "Region overlaps existing CriticMarkup (TAB on 💬 or C-c * p to inspect)")))

(defun cm-pretty-comment (beg end)
  "Comment on the region BEG..END (or at point when empty).
Wraps the region in {==...==}, inserts {>>@author <<} right after it,
and opens the popup editor.  Cancelling removes the inserted markup."
  (interactive "r")
  (when (/= beg end)
    (cm-pretty--barf-if-region-has-markup beg end))
  (let* ((author (if (bound-and-true-p cm-author)
                     (concat "@" cm-author " ")
                   ""))
         (markup (concat "{>>" author "<<}"))
         (has-region (/= beg end))
         (ins-pos (if has-region (+ end 6) end))
         cleanup)
    (when has-region
      (save-excursion
        (goto-char end) (insert "==}")
        (goto-char beg) (insert "{==")))
    (save-excursion
      (goto-char ins-pos)
      (insert markup))
    (setq cleanup
          (if has-region
              (list (cons ins-pos (+ ins-pos (length markup)))
                    (cons (+ end 3) (+ end 6))
                    (cons beg (+ beg 3)))
            (list (cons ins-pos (+ ins-pos (length markup))))))
    (cm-pretty--edit-open (current-buffer)
                          (+ ins-pos 3)
                          (+ ins-pos 3 (length author))
                          cleanup)))

(defun cm-pretty-substitution (beg end)
  "Propose replacing the region BEG..END.
Wraps the region as the old text of {~~old~>new~~} and opens the
popup editor for the new text.  Cancelling removes the inserted
markup, restoring the original text."
  (interactive "r")
  (when (= beg end)
    (user-error "Select the text to replace"))
  (cm-pretty--barf-if-region-has-markup beg end)
  (save-excursion
    (goto-char end) (insert "~>~~}")
    (goto-char beg) (insert "{~~"))
  ;; After insertion: {~~ at BEG..BEG+3, old text shifted by 3,
  ;; ~> at END+3..END+5, empty new text at END+5, ~~} at END+5..END+8.
  (let ((new-pos (+ end 5)))
    (cm-pretty--edit-open (current-buffer) new-pos new-pos
                          (list (cons (+ end 3) (+ end 8))
                                (cons beg (+ beg 3))))))

;;; Minor mode

(defun cm-pretty--after-change (&rest _)
  "Rebuild overlays after a buffer change."
  (cm-pretty--refresh))

(defvar cm-pretty-mode)                 ; defined below by the minor mode

(defun cm-pretty--refresh-after (&rest _)
  "Refresh overlays after a command that suppresses modification hooks."
  (when cm-pretty-mode
    (cm-pretty--refresh)))

;; cm-mode wraps its edits in `cm-without-following-changes', which
;; binds `inhibit-modification-hooks', so the after-change refresh
;; never fires for accept/reject and friends.  Refresh explicitly.
(with-eval-after-load 'cm-mode
  (dolist (cmd '(cm-accept/reject-change-at-point
                 cm-accept/reject-all-changes
                 cm-comment cm-substitution cm-deletion cm-addition))
    (advice-add cmd :after #'cm-pretty--refresh-after)))

(define-minor-mode cm-pretty-mode
  "Prettify CriticMarkup markup using overlays."
  :lighter " CMP"
  (if cm-pretty-mode
      (progn
        (add-to-invisibility-spec 'cm-pretty)
        (add-hook 'after-change-functions #'cm-pretty--after-change nil t)
        (add-hook 'post-command-hook #'cm-pretty--point-adjust nil t)
        (setq cm-pretty--last-point (point))
        (cm-pretty--refresh))
    (remove-from-invisibility-spec 'cm-pretty)
    (remove-hook 'after-change-functions #'cm-pretty--after-change t)
    (remove-hook 'post-command-hook #'cm-pretty--point-adjust t)
    ;; Reset fold state so that toggling the mode off and on
    ;; (C-c * p twice) acts as a display reset.
    (cm-pretty--free-markers cm-pretty--folded-comments)
    (cm-pretty--free-markers cm-pretty--expanded-substitutions)
    (setq cm-pretty--folded-comments nil
          cm-pretty--expanded-substitutions nil)
    (cm-pretty--clear)))

(provide 'cm-pretty)

;;; cm-pretty.el ends here
