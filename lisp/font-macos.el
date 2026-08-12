;;; font-macos.el --- Fontset surgery for macOS -*- lexical-binding: t -*-

;;; Commentary:

;; Menlo carries no Japanese glyphs and is missing a good share of the symbol
;; blocks, so Emacs fills those gaps from elsewhere.  Left alone the result is
;; a mess of mismatched sizes, and it is worst inside a terminal emulator like
;; ghostel, which lays characters out on a fixed grid:
;;
;;   - `unicode-fonts' maps by Unicode block and picks purely for coverage.  It
;;     hands kanji and CJK punctuation to a Simplified Chinese font while kana
;;     go to Osaka, and it fills the symbol gaps with proportional fonts whose
;;     advances are nothing like the cell width.
;;   - ghostel keeps its grid exact by scaling any glyph whose metrics miss the
;;     cell down until it fits, so every one of those mismatches shows up as
;;     text that is visibly too small.
;;
;; The fix is to decide the fontset ourselves: pin Japanese to one family sized
;; to exactly two cells, and for symbols keep the default font wherever it has
;; the glyph, filling only the gaps with whichever fallback lands closest to
;; the cell width.
;;
;; `unicode-fonts-setup' runs later and would undo all of it, so
;; `font-macos-setup' is also attached to it as :after advice.

;;; Code:

(require 'cl-lib)

(defvar font-macos-height 160
  "Height of the default face, in 1/10 pt.")

(defvar font-macos-family "Menlo"
  "Family for the default face.")

(defvar font-macos-japanese-family "Hiragino Kaku Gothic ProN"
  "Family used for all Japanese text.")

(defvar font-macos-japanese-targets
  '(kana han cjk-misc
    (#x3000 . #x303F)                   ; CJK Symbols and Punctuation
    (#xFF00 . #xFFEF))                  ; Halfwidth and Fullwidth Forms
  "Fontset targets routed to `font-macos-japanese-family'.")

(defvar font-macos-symbol-ranges
  '((#x2000 . #x206F) (#x2100 . #x214F) (#x2150 . #x218F) (#x2190 . #x21FF)
    (#x2200 . #x22FF) (#x2300 . #x23FF) (#x2460 . #x24FF) (#x25A0 . #x25FF)
    (#x2600 . #x26FF) (#x2700 . #x27BF) (#x2900 . #x29FF) (#x2B00 . #x2BFF))
  "Symbol blocks to keep on the terminal grid.
Box drawing and block elements are deliberately absent - Menlo covers
them completely and they must tile seamlessly.")

(defvar font-macos-symbol-fallbacks '("Monaco" "STIX Two Math" "Apple Symbols")
  "Families tried for symbols the default font lacks.")

(defun font-macos-setup-japanese ()
  "Route all Japanese text through `font-macos-japanese-family'.

The size is pinned to two ASCII cells.  Hiragino's em is narrower than
that, so at its natural size every fullwidth glyph sits in its cell with
a gap beside it - very visible on a terminal grid."
  (let ((size (* 2 (frame-char-width))))
    (dolist (target font-macos-japanese-targets)
      (set-fontset-font t target
                        (font-spec :family font-macos-japanese-family
                                   :size size)))))

(defun font-macos--metrics (font char)
  "Return (ADVANCE . INK-HEIGHT) for CHAR in FONT, or nil if absent."
  (ignore-errors
    (let* ((glyphs (font-get-glyphs font 0 1 (string char)))
           (g (and glyphs (aref glyphs 0))))
      (when (and g (aref g 3) (not (eq (aref g 3) 0)))
        (cons (aref g 4) (+ (aref g 7) (aref g 8)))))))

(defun font-macos--better-p (a b)
  "Non-nil if score A beats score B: nearer the cell, then larger."
  (or (< (car a) (car b))
      (and (= (car a) (car b)) (< (cadr a) (cadr b)))))

(defun font-macos-setup-symbols ()
  "Route symbols the default font lacks to one that fits the terminal cell.

The default font stays authoritative wherever it has the glyph.  Prefer a
fallback whose advance is exactly one cell; failing that take the closest,
breaking ties toward the larger glyph.  Return the number of characters
reassigned."
  (let* ((cell (frame-char-width))
         (base (font-at 0 nil "a"))
         (size (font-get base :size))
         (fonts (delq nil
                      (mapcar (lambda (family)
                                (when-let* ((entity (find-font
                                                     (font-spec :family family
                                                                :size size))))
                                  (cons family (open-font entity size))))
                              font-macos-symbol-fallbacks)))
         (fixed 0))
    (dolist (range font-macos-symbol-ranges fixed)
      (cl-loop for c from (car range) to (cdr range) do
        (unless (or (memq (get-char-code-property c 'general-category)
                          '(Cc Cf Cn Co Cs))
                    (equal (car (font-macos--metrics base c)) cell))
          (let (best)
            (pcase-dolist (`(,family . ,font) fonts)
              (when-let* ((m (font-macos--metrics font c))
                          (score (list (abs (- (car m) cell)) (- (cdr m)))))
                (when (or (null best) (font-macos--better-p score (cdr best)))
                  (setq best (cons family score)))))
            (when best
              (set-fontset-font t (cons c c) (font-spec :family (car best)))
              (cl-incf fixed))))))))

;;;###autoload
(defun font-macos-setup ()
  "Apply the Japanese and symbol fontset rules."
  (interactive)
  (font-macos-setup-japanese)
  (font-macos-setup-symbols))

(set-face-attribute 'default nil
                    :family font-macos-family :height font-macos-height)
(font-macos-setup)

;; `unicode-fonts-setup' remaps whole Unicode blocks and would take the
;; Japanese and symbol ranges straight back, so re-apply once it has run.
(advice-add 'unicode-fonts-setup :after #'font-macos-setup)

(provide 'font-macos)
;;; font-macos.el ends here
