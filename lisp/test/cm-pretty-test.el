;;; cm-pretty-test.el --- Tests for cm-pretty -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   emacs --batch -L lisp -L straight/build/cm-mode -L straight/build/markdown-mode \
;;     -l ert -l cm-pretty -l lisp/test/cm-pretty-test.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'cm-pretty)

(defvar cm-author)                      ; dynamic binding for the tests

;;; ============================================================
;;; Helpers
;;; ============================================================

(defmacro cm-pretty-test--with-buffer (text &rest body)
  "Run BODY in a temp buffer containing TEXT with `cm-pretty-mode' enabled."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (goto-char (point-min))
     (cm-pretty-mode 1)
     ,@body))

(defmacro cm-pretty-test--with-markdown-buffer (text &rest body)
  "Like `cm-pretty-test--with-buffer' but in `markdown-mode'."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (markdown-mode)
     (syntax-propertize (point-max))
     (goto-char (point-min))
     (cm-pretty-mode 1)
     ,@body))

(defun cm-pretty-test--goto (string &optional occurrence)
  "Move point to the start of the OCCURRENCE-th STRING (default 1st)."
  (goto-char (point-min))
  (search-forward string nil nil (or occurrence 1))
  (goto-char (match-beginning 0)))

(defun cm-pretty-test--overlay-at (string &optional prop)
  "Return the cm-pretty overlay starting where STRING starts.
If PROP is non-nil, return the first non-nil value of that
property among cm-pretty overlays at that position."
  (save-excursion
    (cm-pretty-test--goto string)
    (let ((ovs (seq-filter (lambda (o) (overlay-get o 'cm-pretty))
                           (overlays-at (point)))))
      (if prop
          (seq-some (lambda (o) (overlay-get o prop)) ovs)
        (car ovs)))))

(defun cm-pretty-test--faces-at (string)
  "Return the list of cm-pretty overlay faces where STRING starts."
  (save-excursion
    (cm-pretty-test--goto string)
    (mapcar (lambda (o) (overlay-get o 'face))
            (seq-filter (lambda (o) (overlay-get o 'cm-pretty))
                        (overlays-at (point))))))

(defun cm-pretty-test--blocks ()
  "Concatenate every ghost block string cm-pretty attached to the buffer."
  (let ((result ""))
    (dolist (o (overlays-in (point-min) (point-max)))
      (when (overlay-get o 'cm-pretty)
        (dolist (p '(before-string after-string))
          (when-let* ((s (overlay-get o p)))
            (setq result (concat result s))))))
    result))

(defun cm-pretty-test--text ()
  "Return the buffer text without properties."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun cm-pretty-test--tab-command-at-point ()
  "Return the command TAB would run via cm-pretty overlays at point."
  (when-let* ((map (seq-some (lambda (o) (and (overlay-get o 'cm-pretty)
                                              (overlay-get o 'keymap)))
                             (overlays-at (point)))))
    (lookup-key map (kbd "TAB"))))

(defun cm-pretty-test--icon-display (occurrence)
  "Return the display of the OCCURRENCE-th comment icon."
  (save-excursion
    (cm-pretty-test--goto "{>>" occurrence)
    (seq-some (lambda (o) (overlay-get o 'display))
              (overlays-at (point)))))

(defun cm-pretty-test--edit-buffer ()
  "Return the live cm-pretty edit buffer, if any."
  (get-buffer "*cm-pretty-edit*"))

(defmacro cm-pretty-test--cleanup-edit (&rest body)
  "Run BODY, killing any leftover edit buffer afterwards."
  `(unwind-protect (progn ,@body)
     (when-let* ((b (cm-pretty-test--edit-buffer)))
       (kill-buffer b))))

;;; ============================================================
;;; Display: comments (ghost rendering)
;;; ============================================================

(ert-deftest cm-pretty-test-comment-icon-inline-and-body-ghosted ()
  "The icon stays at the anchor; body and closing are hidden."
  (cm-pretty-test--with-buffer "AAA {>>@phelrine note<<} BBB"
    (should (equal (cm-pretty-test--overlay-at "{>>" 'display) "💬 "))
    (should (eq (cm-pretty-test--overlay-at "@phelrine note" 'invisible) 'cm-pretty))
    (should (eq (cm-pretty-test--overlay-at "<<}" 'invisible) 'cm-pretty))))

(ert-deftest cm-pretty-test-comment-block-below-even-mid-line ()
  "The body is rendered as a block on the next line, even mid-line."
  (cm-pretty-test--with-buffer "AAA {>>@phelrine note<<} BBB\nnext"
    (let ((blocks (cm-pretty-test--blocks)))
      (should (string-prefix-p " \n └ " blocks))
      (should (string-match-p "@phelrine note" blocks)))))

(ert-deftest cm-pretty-test-comment-block-at-buffer-end ()
  "The ghost block also renders when the markup line is the last line."
  (cm-pretty-test--with-buffer "AAA {>>note<<}"
    (should (string-match-p "note" (cm-pretty-test--blocks)))))

(ert-deftest cm-pretty-test-block-cursor-glyph ()
  "The block starts with a cursor-carrying space, so the cursor is
drawn at the real insertion point instead of the ghost line end.
A newline glyph cannot carry the cursor property, hence the space."
  (cm-pretty-test--with-buffer "AAA {>>note<<} BBB\nnext"
    (let* ((carrier (seq-find (lambda (o) (and (overlay-get o 'cm-pretty)
                                               (overlay-get o 'before-string)))
                              (overlays-in (point-min) (point-max))))
           (s (overlay-get carrier 'before-string)))
      (should (eq (aref s 0) ?\s))
      (should (get-text-property 0 'cursor s)))))

;;; ============================================================
;;; Display: substitutions and other markup
;;; ============================================================

(ert-deftest cm-pretty-test-substitution-collapsed-by-default ()
  "Substitution shows only the old text struck through, plus an indicator."
  (cm-pretty-test--with-buffer "a {~~古い~>新しい~~} b"
    (should (eq (cm-pretty-test--overlay-at "{~~" 'invisible) 'cm-pretty))
    (should (eq (cm-pretty-test--overlay-at "~~}" 'invisible) 'cm-pretty))
    (should (eq (cm-pretty-test--overlay-at "古い" 'face) 'cm-pretty-deleted-face))
    (should (equal (cm-pretty-test--overlay-at "~>" 'display) "✎"))
    (should (eq (cm-pretty-test--overlay-at "新しい" 'invisible) 'cm-pretty))
    (should (equal (cm-pretty-test--blocks) ""))))

(ert-deftest cm-pretty-test-substitution-toggle-shows-diff-block ()
  "Toggling a substitution shows the replacement below the line."
  (cm-pretty-test--with-buffer "a {~~古い~>新しい~~} b\nnext"
    (cm-pretty-test--goto "~>")
    (cm-pretty-toggle)
    (let ((blocks (cm-pretty-test--blocks)))
      (should (string-prefix-p " \n └ " blocks))
      (should (string-match-p "→ 新しい" blocks)))
    (cm-pretty-toggle)
    (should (equal (cm-pretty-test--blocks) ""))))

(ert-deftest cm-pretty-test-highlight-prettified ()
  "Highlight delimiters are hidden and the body gets the highlight face."
  (cm-pretty-test--with-buffer "a {==対象テキスト==}{>>note<<} b"
    (should (eq (cm-pretty-test--overlay-at "{==" 'invisible) 'cm-pretty))
    (should (eq (cm-pretty-test--overlay-at "==}" 'invisible) 'cm-pretty))
    (should (memq 'cm-pretty-highlight-face
                  (cm-pretty-test--faces-at "対象テキスト")))))

(ert-deftest cm-pretty-test-addition-and-deletion-prettified ()
  "Addition/deletion delimiters are hidden and bodies get their faces."
  (cm-pretty-test--with-buffer "a {++追加分++} と {--削除分--} b"
    (should (eq (cm-pretty-test--overlay-at "{++" 'invisible) 'cm-pretty))
    (should (memq 'cm-pretty-added-face (cm-pretty-test--faces-at "追加分")))
    (should (eq (cm-pretty-test--overlay-at "{--" 'invisible) 'cm-pretty))
    (should (memq 'cm-pretty-deleted-face (cm-pretty-test--faces-at "削除分")))))

(ert-deftest cm-pretty-test-comment-body-and-author-faces ()
  "The ghost block styles the body and emphasizes the author tag.
The in-place body is invisible, so faces live on the block string."
  (cm-pretty-test--with-buffer "x {>>@phelrine: これは?<<} y\nnext"
    (let ((blocks (cm-pretty-test--blocks)))
      (cl-flet ((faces-at (str)
                  (let ((f (get-text-property (string-match str blocks)
                                              'face blocks)))
                    (if (listp f) f (list f)))))
        (should (memq 'cm-pretty-author-face (faces-at "@phelrine")))
        (should (memq 'cm-pretty-comment-face (faces-at "これは")))))))

(ert-deftest cm-pretty-test-blocks-strip-source-faces ()
  "Ghost blocks must not inherit faces from the buffer text.
cm-mode fontifies the whole {~~...~~} construct with strike-through;
`match-string' copies those properties, so the block builders must
strip them or the replacement text shows struck through."
  (cm-pretty-test--with-buffer "a {~~古い~>新しい~~} b\nnext"
    ;; Simulate cm-mode's fontification of the whole construct.
    (put-text-property (point-min) (point-max) 'face 'cm-pretty-deleted-face)
    (cm-pretty-test--goto "~>")
    (cm-pretty-toggle)
    (let* ((blocks (cm-pretty-test--blocks))
           (f (get-text-property (string-match "新しい" blocks) 'face blocks))
           (faces (if (listp f) f (list f))))
      (should-not (memq 'cm-pretty-deleted-face faces))
      (should (memq 'cm-pretty-added-face faces)))))

(ert-deftest cm-pretty-test-added-face-never-struck-through ()
  "The added face explicitly disables strike-through so cm-mode's
substitution fontification cannot bleed through."
  (dolist (spec (get 'cm-pretty-added-face 'face-defface-spec))
    (let ((attrs (cdr spec)))
      (should (plist-member attrs :strike-through))
      (should-not (plist-get attrs :strike-through)))))

;;; ============================================================
;;; Display: markdown exclusions (code spans, code blocks, HTML comments)
;;; ============================================================

(ert-deftest cm-pretty-test-code-span-not-prettified ()
  "Markup examples inside inline code spans are left alone."
  (skip-unless (require 'markdown-mode nil t))
  (cm-pretty-test--with-markdown-buffer "| 例 | `{>>コメント<<}` |\n"
    (should-not (cm-pretty-test--overlay-at "{>>"))
    (should (equal (cm-pretty-test--blocks) ""))))

(ert-deftest cm-pretty-test-html-comment-not-prettified ()
  "Markup examples inside HTML comments (cheat sheets) are left alone."
  (skip-unless (require 'markdown-mode nil t))
  (cm-pretty-test--with-markdown-buffer "<!--\n{>>コメント<<} の例\n-->\n本文\n"
    (should-not (cm-pretty-test--overlay-at "{>>"))))

(ert-deftest cm-pretty-test-code-block-not-prettified ()
  "Markup examples inside fenced code blocks are left alone."
  (skip-unless (require 'markdown-mode nil t))
  (cm-pretty-test--with-markdown-buffer "```\n{~~古い~>新しい~~}\n```\n"
    (should-not (cm-pretty-test--overlay-at "{~~"))))

;;; ============================================================
;;; Folding and TAB reachability
;;; ============================================================
;; At invisible-delimiter boundaries the cursor is drawn on the next
;; visible glyph, so TAB must work at every position that visually
;; looks like being on the icon.

(ert-deftest cm-pretty-test-toggle-folds-and-unfolds ()
  "TAB on the icon folds the comment to 💬… and unfolds it back."
  (cm-pretty-test--with-buffer "x {>>@phelrine: 長いコメント<<} y"
    (cm-pretty-test--goto "{>>")
    (cm-pretty-toggle)
    (should (equal (cm-pretty-test--icon-display 1) "💬… "))
    (should (equal (cm-pretty-test--blocks) ""))
    (cm-pretty-toggle)
    (should (equal (cm-pretty-test--icon-display 1) "💬 "))
    (should (string-match-p "長いコメント" (cm-pretty-test--blocks)))))

(ert-deftest cm-pretty-test-tab-reachable-around-comment ()
  "TAB toggles at every position that visually belongs to the comment:
icon, hidden closing delimiter, and right after the markup."
  (cm-pretty-test--with-buffer "AAA {>>note<<} BBB"
    (dolist (target '("{>>" "<<}" " BBB"))
      (cm-pretty-test--goto target)
      (should (eq (cm-pretty-test--tab-command-at-point) 'cm-pretty-toggle)))))

(ert-deftest cm-pretty-test-tab-reachable-from-indentation ()
  "TAB toggles from the leading whitespace of an own-line comment.
If TAB only worked on the 3-char icon, the first press would be
consumed as indentation."
  (cm-pretty-test--with-buffer "x\n  {>>note<<}\ny"
    (goto-char (point-min))
    (forward-line 1)
    (should (eq (cm-pretty-test--tab-command-at-point) 'cm-pretty-toggle))
    (cm-pretty-toggle)
    (should (equal (cm-pretty-test--icon-display 1) "💬… "))))

(ert-deftest cm-pretty-test-tab-reachable-at-eol-even-when-folded ()
  "TAB right after an end-of-line comment folds and unfolds it.
Folding removes the ghost carrier, but TAB must keep working."
  (cm-pretty-test--with-buffer "AAA {>>note<<}\nnext"
    (goto-char (point-min))
    (search-forward "<<}")
    (should (eq (cm-pretty-test--tab-command-at-point) 'cm-pretty-toggle))
    (cm-pretty-toggle)
    (should (equal (cm-pretty-test--icon-display 1) "💬… "))
    (cm-pretty-toggle)
    (should (equal (cm-pretty-test--icon-display 1) "💬 "))))

(ert-deftest cm-pretty-test-tab-reachable-on-highlight-and-delimiters ()
  "TAB toggles on the highlighted target and its hidden delimiters."
  (cm-pretty-test--with-buffer "a {==対象==}{>>note<<} b"
    (dolist (target '("対象" "{==" "==}"))
      (cm-pretty-test--goto target)
      (should (eq (cm-pretty-test--tab-command-at-point) 'cm-pretty-toggle)))
    (cm-pretty-test--goto "対象")
    (cm-pretty-toggle)
    (should (equal (cm-pretty-test--icon-display 1) "💬… "))))

(ert-deftest cm-pretty-test-tab-reachable-at-substitution-delimiters ()
  "TAB toggles at the hidden delimiters of a substitution."
  (cm-pretty-test--with-buffer "a {~~古い~>新しい~~} b"
    (dolist (target '("{~~" "~~}"))
      (cm-pretty-test--goto target)
      (should (eq (cm-pretty-test--tab-command-at-point) 'cm-pretty-toggle)))))

(ert-deftest cm-pretty-test-toggle-targets-nearest-markup ()
  "With adjacent markups, toggling at the second icon affects the second.
Overlays of several markups overlap at adjacency boundaries; the one
with the latest start -- the one the cursor is visually on -- wins."
  (cm-pretty-test--with-buffer "x {>>one<<}{>>two<<} y"
    (cm-pretty-test--goto "{>>" 2)
    (cm-pretty-toggle)
    (should (equal (cm-pretty-test--icon-display 1) "💬 "))
    (should (equal (cm-pretty-test--icon-display 2) "💬… "))))

(ert-deftest cm-pretty-test-fold-state-survives-edits ()
  "A folded comment stays folded after the buffer is edited."
  (cm-pretty-test--with-buffer "x {>>@phelrine: note<<} y"
    (cm-pretty-test--goto "{>>")
    (cm-pretty-toggle)
    (goto-char (point-min))
    (insert "PREFIX ")
    (should (equal (cm-pretty-test--blocks) ""))))

(ert-deftest cm-pretty-test-disable-resets-fold-state ()
  "Toggling the mode off clears fold state."
  (cm-pretty-test--with-buffer "x {>>@phelrine: note<<} y"
    (cm-pretty-test--goto "{>>")
    (cm-pretty-toggle)
    (cm-pretty-mode -1)
    (cm-pretty-mode 1)
    (should (string-match-p "note" (cm-pretty-test--blocks)))))

;;; ============================================================
;;; Point adjustment (keep the cursor out of hidden markup)
;;; ============================================================

(ert-deftest cm-pretty-test-point-snaps-forward-past-comment ()
  "Moving forward into a comment snaps point past the whole markup."
  (cm-pretty-test--with-buffer "AAA {>>note<<} BBB"
    (setq cm-pretty--last-point 1)
    (goto-char 8)                       ; right after {>>, inside the body
    (cm-pretty--point-adjust)
    (should (= (point) 15))             ; right after <<}
    (insert "X")
    (should (equal (cm-pretty-test--text) "AAA {>>note<<}X BBB"))))

(ert-deftest cm-pretty-test-point-snaps-backward-before-comment ()
  "Moving backward into a comment snaps point before the markup."
  (cm-pretty-test--with-buffer "AAA {>>note<<} BBB"
    (setq cm-pretty--last-point 15)
    (goto-char 12)                      ; inside the body, moving left
    (cm-pretty--point-adjust)
    (should (= (point) 5))))            ; before {>>

(ert-deftest cm-pretty-test-point-free-in-substitution-old-text ()
  "Point may rest inside the visible old text of a substitution."
  (cm-pretty-test--with-buffer "a {~~古い~>新しい~~} b"
    (setq cm-pretty--last-point 1)
    (cm-pretty-test--goto "古い")
    (forward-char 1)
    (let ((p (point)))
      (cm-pretty--point-adjust)
      (should (= (point) p)))))

(ert-deftest cm-pretty-test-point-snaps-past-substitution-tail ()
  "Moving into the hidden arrow/new part snaps past the substitution."
  (cm-pretty-test--with-buffer "a {~~古い~>新しい~~} b"
    (setq cm-pretty--last-point 1)
    (cm-pretty-test--goto "新しい")
    (cm-pretty--point-adjust)
    (should (looking-back "~~}" 3))))

;;; ============================================================
;;; Popup editing (existing markup)
;;; ============================================================

(ert-deftest cm-pretty-test-anchors-bind-ret-to-edit ()
  "The 💬 and ✎ anchors bind RET (and TAB) to their commands."
  (cm-pretty-test--with-buffer "a {>>note<<} {~~古い~>新しい~~} b"
    (dolist (target '("{>>" "~>"))
      (cm-pretty-test--goto target)
      (let ((map (seq-some (lambda (o) (and (overlay-get o 'cm-pretty)
                                            (overlay-get o 'keymap)))
                           (overlays-at (point)))))
        (should (eq (lookup-key map (kbd "RET")) 'cm-pretty-edit))
        (should (eq (lookup-key map (kbd "TAB")) 'cm-pretty-toggle))))))

(ert-deftest cm-pretty-test-block-click-binding-and-carrier-group ()
  "The ghost block binds mouse-1 to the click handler and knows its markup."
  (cm-pretty-test--cleanup-edit
   (cm-pretty-test--with-buffer "AAA {>>@phelrine note<<} BBB\nnext"
     (let ((carrier (seq-find (lambda (o) (and (overlay-get o 'cm-pretty)
                                               (overlay-get o 'before-string)))
                              (overlays-in (point-min) (point-max)))))
       (should (eq (lookup-key (overlay-get carrier 'keymap) (kbd "<mouse-1>"))
                   'cm-pretty-block-click))
       (cm-pretty--edit-group (overlay-get carrier 'cm-pretty-group))
       (with-current-buffer (cm-pretty-test--edit-buffer)
         (should (equal (buffer-string) "@phelrine note")))))))

(ert-deftest cm-pretty-test-edit-opens-buffer-with-body ()
  "`cm-pretty-edit' on the icon opens an edit buffer holding the body."
  (cm-pretty-test--cleanup-edit
   (cm-pretty-test--with-buffer "x {>>@phelrine note<<} y"
     (cm-pretty-test--goto "{>>")
     (cm-pretty-edit)
     (should (cm-pretty-test--edit-buffer))
     (with-current-buffer (cm-pretty-test--edit-buffer)
       (should (equal (buffer-string) "@phelrine note"))))))

(ert-deftest cm-pretty-test-edit-apply-updates-body ()
  "Applying the edit buffer rewrites the comment body in the source."
  (cm-pretty-test--cleanup-edit
   (cm-pretty-test--with-buffer "x {>>@phelrine note<<} y"
     (cm-pretty-test--goto "{>>")
     (cm-pretty-edit)
     (with-current-buffer (cm-pretty-test--edit-buffer)
       (erase-buffer)
       (insert "@phelrine 修正済み")
       (cm-pretty-edit-apply))
     (should (equal (cm-pretty-test--text) "x {>>@phelrine 修正済み<<} y")))))

(ert-deftest cm-pretty-test-edit-apply-collapses-newlines ()
  "Newlines typed in the edit buffer are collapsed on apply.
CriticMarkup discourages newlines inside tags."
  (cm-pretty-test--cleanup-edit
   (cm-pretty-test--with-buffer "x {>>note<<} y"
     (cm-pretty-test--goto "{>>")
     (cm-pretty-edit)
     (with-current-buffer (cm-pretty-test--edit-buffer)
       (erase-buffer)
       (insert "one\ntwo\n")
       (cm-pretty-edit-apply))
     (should (equal (cm-pretty-test--text) "x {>>one two<<} y")))))

(ert-deftest cm-pretty-test-edit-cancel-keeps-source ()
  "Cancelling the edit buffer leaves the source untouched."
  (cm-pretty-test--cleanup-edit
   (cm-pretty-test--with-buffer "x {>>note<<} y"
     (cm-pretty-test--goto "{>>")
     (cm-pretty-edit)
     (with-current-buffer (cm-pretty-test--edit-buffer)
       (erase-buffer)
       (insert "discarded")
       (cm-pretty-edit-cancel))
     (should (equal (cm-pretty-test--text) "x {>>note<<} y")))))

(ert-deftest cm-pretty-test-edit-substitution-new-text ()
  "`cm-pretty-edit' on the ✎ anchor edits the replacement text."
  (cm-pretty-test--cleanup-edit
   (cm-pretty-test--with-buffer "a {~~古い~>新しい~~} b"
     (cm-pretty-test--goto "~>")
     (cm-pretty-edit)
     (with-current-buffer (cm-pretty-test--edit-buffer)
       (erase-buffer)
       (insert "さらに新しい")
       (cm-pretty-edit-apply))
     (should (equal (cm-pretty-test--text) "a {~~古い~>さらに新しい~~} b")))))

;;; ============================================================
;;; New markup insertion (comments and substitutions + popup editing)
;;; ============================================================

(ert-deftest cm-pretty-test-comment-region-inserts-markup-and-edits ()
  "Commenting a region wraps it and opens the editor prefilled with the author."
  (cm-pretty-test--cleanup-edit
   (cm-pretty-test--with-buffer "hello world foo"
     (let ((cm-author "phelrine"))
       (cm-pretty-comment 7 12))
     (should (equal (cm-pretty-test--text)
                    "hello {==world==}{>>@phelrine <<} foo"))
     (with-current-buffer (cm-pretty-test--edit-buffer)
       (insert "コメント")
       (cm-pretty-edit-apply))
     (should (equal (cm-pretty-test--text)
                    "hello {==world==}{>>@phelrine コメント<<} foo")))))

(ert-deftest cm-pretty-test-comment-cancel-removes-markup ()
  "Cancelling a new comment removes the freshly inserted markup entirely."
  (cm-pretty-test--cleanup-edit
   (cm-pretty-test--with-buffer "hello world foo"
     (let ((cm-author "phelrine"))
       (cm-pretty-comment 7 12))
     (with-current-buffer (cm-pretty-test--edit-buffer)
       (cm-pretty-edit-cancel))
     (should (equal (cm-pretty-test--text) "hello world foo")))))

(ert-deftest cm-pretty-test-comment-empty-region-makes-lone-comment ()
  "Commenting with an empty region inserts a lone comment at point."
  (cm-pretty-test--cleanup-edit
   (cm-pretty-test--with-buffer "hello world"
     (let ((cm-author "phelrine"))
       (cm-pretty-comment 6 6))
     (should (equal (cm-pretty-test--text) "hello{>>@phelrine <<} world")))))

(ert-deftest cm-pretty-test-substitution-region-inserts-markup-and-edits ()
  "Proposing a replacement wraps the region and opens the editor."
  (cm-pretty-test--cleanup-edit
   (cm-pretty-test--with-buffer "hello world foo"
     (cm-pretty-substitution 7 12)
     (should (equal (cm-pretty-test--text) "hello {~~world~>~~} foo"))
     (with-current-buffer (cm-pretty-test--edit-buffer)
       (insert "earth")
       (cm-pretty-edit-apply))
     (should (equal (cm-pretty-test--text) "hello {~~world~>earth~~} foo")))))

(ert-deftest cm-pretty-test-substitution-cancel-removes-markup ()
  "Cancelling a new substitution restores the original text."
  (cm-pretty-test--cleanup-edit
   (cm-pretty-test--with-buffer "hello world foo"
     (cm-pretty-substitution 7 12)
     (with-current-buffer (cm-pretty-test--edit-buffer)
       (cm-pretty-edit-cancel))
     (should (equal (cm-pretty-test--text) "hello world foo")))))

(ert-deftest cm-pretty-test-substitution-requires-region ()
  "A substitution needs old text to replace."
  (cm-pretty-test--with-buffer "hello"
    (should-error (cm-pretty-substitution 3 3) :type 'user-error)))

(ert-deftest cm-pretty-test-comment-rejects-markup-overlap ()
  "Commenting a region that overlaps existing markup signals a clear error."
  (cm-pretty-test--with-buffer "a {>>x<<} b"
    (should-error (cm-pretty-comment 1 8) :type 'user-error)))

(ert-deftest cm-pretty-test-refresh-after-hook-suppressed-edits ()
  "Stale ghosts left by hook-suppressed edits are cleaned up.
cm-mode wraps accept/reject edits in `cm-without-following-changes'
(`inhibit-modification-hooks'), so the after-change refresh never
fires for them; the :after advice must refresh instead."
  (cm-pretty-test--with-buffer "a {>>note<<} b\nnext"
    (let ((inhibit-modification-hooks t))
      (cm-pretty-test--goto "{>>")
      (delete-region (point) (+ (point) (length "{>>note<<}"))))
    ;; The block carrier sits on the newline, so the ghost survives
    ;; the deletion -- this is the bug being guarded against.
    (should-not (equal (cm-pretty-test--blocks) ""))
    (cm-pretty--refresh-after)
    (should (equal (cm-pretty-test--blocks) ""))))

(ert-deftest cm-pretty-test-refresh-advice-installed-on-cm-commands ()
  "The refresh advice is installed on cm-mode's modifying commands."
  (skip-unless (require 'cm-mode nil t))
  (dolist (cmd '(cm-accept/reject-change-at-point
                 cm-accept/reject-all-changes))
    (should (advice-member-p #'cm-pretty--refresh-after cmd))))

;;; ============================================================
;;; Mode lifecycle and utilities
;;; ============================================================

(ert-deftest cm-pretty-test-disable-removes-overlays ()
  "Turning the mode off removes every cm-pretty overlay."
  (cm-pretty-test--with-buffer "a {>>x<<} {==y==} {~~p~>q~~} b"
    (cm-pretty-mode -1)
    (should-not (seq-filter (lambda (o) (overlay-get o 'cm-pretty))
                            (overlays-in (point-min) (point-max))))))

(ert-deftest cm-pretty-test-edit-updates-overlays ()
  "Markup typed after the mode is enabled gets prettified."
  (cm-pretty-test--with-buffer "plain text "
    (goto-char (point-max))
    (insert "{>>new comment<<}")
    (should (equal (cm-pretty-test--overlay-at "{>>" 'display) "💬 "))))

(ert-deftest cm-pretty-test-region-has-markup-p ()
  "Markup delimiters inside a region are detected."
  (with-temp-buffer
    (insert "plain {>>note<<} plain")
    (should (cm-pretty-region-has-markup-p 1 12))
    (should (cm-pretty-region-has-markup-p 14 20))
    (should-not (cm-pretty-region-has-markup-p 1 6))
    (should-not (cm-pretty-region-has-markup-p 18 23))))

(provide 'cm-pretty-test)

;;; cm-pretty-test.el ends here
