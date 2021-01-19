;;; mum-style-theme.el --- Mum style theme

;; Copyright (C) 2019  Wu Junyu

;; Author: Wu Junyu <vistar_w@hotmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'cl-lib)

(deftheme mum-style "Mum style theme.")

(defgroup mum-style nil
  "Theme options."
  :group 'faces)

(defvar mum-style--type
  'dark
  "Type is `dark' `ligth' and `care'.")

(defvar mum-style--dark
  '(:colors
	((mum--color-bg              "#282a36" "unspecified-bg" "unspecified-bg")
	 (mum--color-fg              "#f8f8f2" "#ffffff" "brightwhite")
	 (mum--color-current         "#44475a" "#303030" "brightblack")
	 (mum--color-comment         "#6272a4" "#5f5faf" "blue")
	 (mum--color-cyan            "#8be9fd" "#87d7ff" "brightcyan")
	 (mum--color-green           "#50fa7b" "#5fff87" "green")
	 (mum--color-orange          "#ffb86c" "#ffaf5f" "brightred")
	 (mum--color-pink            "#ff79c6" "#ff87d7" "magenta")
	 (mum--color-purple          "#bd93f9" "#af87ff" "brightmagenta")
	 (mum--color-red             "#ff5555" "#ff8787" "red")
	 (mum--color-yellow          "#f1fa8c" "#ffff87" "yellow")

	 (mum--color-bg2             "#373844" "#121212" "brightblack")
	 (mum--color-bg3             "#464752" "#262626" "brightblack")
	 (mum--color-bg4             "#565761" "#444444" "brightblack")
	 (mum--color-fg2             "#e2e2dc" "#e4e4e4" "brightwhite")
	 (mum--color-fg3             "#ccccc7" "#c6c6c6" "white")
	 (mum--color-fg4             "#b6b6b2" "#b2b2b2" "white")
	 (mum--color-other-blue      "#0189cc" "#0087ff" "brightblue"))
	:faces
	(;; default
	 (cursor :background ,mum--color-fg3)
	 (completions-first-difference :foreground ,mum--color-pink :weight bold)
	 (default :background ,mum--color-bg :foreground ,mum--color-fg)
	 (default-italic :slant italic)
	 (ffap :foreground ,mum--color-fg4)
	 (fringe :background ,mum--color-bg :foreground ,mum--color-fg4)
	 (highlight :foreground ,mum--color-fg3 :background ,mum--color-bg3)
	 (hl-line :background ,mum--color-current :extend t)
	 (info-quoted-name :foreground ,mum--color-orange)
	 (info-string :foreground ,mum--color-yellow)
	 (lazy-highlight :foreground ,mum--color-fg2 :background ,mum--color-bg2)
	 (link :foreground ,mum--color-cyan :underline t)
	 (linum :slant italic :foreground ,mum--color-bg4 :background ,mum--color-bg)
	 (line-number :slant italic :foreground ,mum--color-bg4 :background ,mum--color-bg)
	 (match :background ,mum--color-yellow :foreground ,mum--color-bg)
	 (minibuffer-prompt :weight bold :foreground ,mum--color-fg)
	 (read-multiple-choice-face :inherit completions-first-difference)
	 (region :inherit match :extend t)
	 (trailing-whitespace :foreground "unspecified-fg" :background ,mum--color-orange)
	 (vertical-border :foreground ,mum--color-bg2)
	 (success :foreground ,mum--color-green)
	 (warning :foreground ,mum--color-orange)
	 (error :foreground ,mum--color-red)
	 (header-line :background ,mum--color-bg)
	 ;; syntax
	 (font-lock-builtin-face :foreground ,mum--color-orange)
	 (font-lock-comment-face :foreground ,mum--color-comment)
	 (font-lock-comment-delimiter-face :foreground ,mum--color-comment)
	 (font-lock-constant-face :foreground ,mum--color-cyan)
	 (font-lock-doc-face :foreground ,mum--color-comment)
	 (font-lock-function-name-face :foreground ,mum--color-green :weight bold)
	 (font-lock-keyword-face :weight bold :foreground ,mum--color-pink)
	 (font-lock-negation-char-face :foreground ,mum--color-cyan)
	 (font-lock-preprocessor-face :foreground ,mum--color-orange)
	 (font-lock-reference-face :foreground ,mum--color-cyan)
	 (font-lock-regexp-grouping-backslash :foreground ,mum--color-cyan)
	 (font-lock-regexp-grouping-construct :foreground ,mum--color-purple)
	 (font-lock-string-face :foreground ,mum--color-yellow)
	 (font-lock-type-face :foreground ,mum--color-purple)
	 (font-lock-variable-name-face :foreground ,mum--color-fg :weight bold)
	 (font-lock-warning-face :foreground ,mum--color-orange :background ,mum--color-bg2)
	 ;; diff-hl
	 (diff-hl-change :foreground ,mum--color-orange :background ,mum--color-orange)
	 (diff-hl-delete :foreground ,mum--color-red :background ,mum--color-red)
	 (diff-hl-insert :foreground ,mum--color-green :background ,mum--color-green)
	 ;; dired
	 (dired-directory :foreground ,mum--color-green :weight bold)
	 (dired-flagged :foreground ,mum--color-pink)
	 (dired-header :foreground ,mum--color-fg3 :background ,mum--color-bg)
	 (dired-ignored :inherit shadow)
	 (dired-mark :foreground ,mum--color-fg :weight bold)
	 (dired-marked :foreground ,mum--color-orange :weight bold)
	 (dired-perm-write :foreground ,mum--color-fg3 :underline t)
	 (dired-symlink :foreground ,mum--color-yellow :weight bold :slant italic)
	 (dired-warning :foreground ,mum--color-orange :underline t)
	 (diredp-compressed-file-name :foreground ,mum--color-fg3)
	 (diredp-compressed-file-suffix :foreground ,mum--color-fg4)
	 (diredp-date-time :foreground ,mum--color-fg)
	 (diredp-deletion-file-name :foreground ,mum--color-pink :background ,mum--color-current)
	 (diredp-deletion :foreground ,mum--color-pink :weight bold)
	 (diredp-dir-heading :foreground ,mum--color-fg2 :background ,mum--color-bg4)
	 (diredp-dir-name :inherit dired-directory)
	 (diredp-dir-priv :inherit dired-directory)
	 (diredp-executable-tag :foreground ,mum--color-orange)
	 (diredp-file-name :foreground ,mum--color-fg)
	 (diredp-file-suffix :foreground ,mum--color-fg4)
	 (diredp-flag-mark-line :foreground ,mum--color-fg2 :slant italic :background ,mum--color-current)
	 (diredp-flag-mark :foreground ,mum--color-fg2 :weight bold :background ,mum--color-current)
	 (diredp-ignored-file-name :foreground ,mum--color-fg)
	 (diredp-mode-line-flagged :foreground ,mum--color-orange)
	 (diredp-mode-line-marked :foreground ,mum--color-orange)
	 (diredp-no-priv :foreground ,mum--color-fg)
	 (diredp-number :foreground ,mum--color-cyan)
	 (diredp-other-priv :foreground ,mum--color-orange)
	 (diredp-rare-priv :foreground ,mum--color-orange)
	 (diredp-read-priv :foreground ,mum--color-purple)
	 (diredp-write-priv :foreground ,mum--color-pink)
	 (diredp-exec-priv :foreground ,mum--color-yellow)
	 (diredp-symlink :foreground ,mum--color-orange)
	 (diredp-link-priv :foreground ,mum--color-orange)
	 (diredp-autofile-name :foreground ,mum--color-yellow)
	 (diredp-tagged-autofile-name :foreground ,mum--color-yellow)
	 ;; elpher
	 (elpher-gemini-heading1 :inherit bold :foreground ,mum--color-pink)
	 (elpher-gemini-heading2 :inherit bold :foreground ,mum--color-purple)
	 (elpher-gemini-heading3 :weight bold :foreground ,mum--color-green)
	 (elpher-gemini-preformatted :inherit fixed-pitch :foreground ,mum--color-orange)
	 ;; flyspell
	 (flyspell-duplicate :underline (:style wave :color ,mum--color-orange))
	 (flyspell-incorrect :underline (:style wave :color ,mum--color-red))
	 ;; font-latex
	 (font-latex-bold-face :foreground ,mum--color-purple)
	 (font-latex-italic-face :foreground ,mum--color-pink :slant italic)
	 (font-latex-match-reference-keywords :foreground ,mum--color-cyan)
	 (font-latex-match-variable-keywords :foreground ,mum--color-fg)
	 (font-latex-string-face :foreground ,mum--color-yellow)
	 ;; isearch
	 (isearch :inherit match :weight bold)
	 (isearch-fail :foreground ,mum--color-bg :background ,mum--color-orange)
	 ;; markdown
	 (markdown-blockquote-face :foreground ,mum--color-purple)
	 (markdown-code-face :foreground ,mum--color-orange)
	 (markdown-footnote-face :foreground ,mum--color-other-blue)
	 (markdown-header-face :weight bold)
	 (markdown-header-face-1 :inherit bold :foreground ,mum--color-pink)
	 (markdown-header-face-2 :inherit bold :foreground ,mum--color-purple)
	 (markdown-header-face-3 :foreground ,mum--color-green)
	 (markdown-header-face-4 :foreground ,mum--color-yellow)
	 (markdown-header-face-5 :foreground ,mum--color-cyan)
	 (markdown-header-face-6 :foreground ,mum--color-orange)
	 (markdown-header-face-7 :foreground ,mum--color-other-blue)
	 (markdown-header-face-8 :foreground ,mum--color-fg)
	 (markdown-inline-code-face :foreground ,mum--color-yellow)
	 (markdown-plain-url-face :inherit link)
	 (markdown-pre-face :foreground ,mum--color-orange)
	 (markdown-table-face :foreground ,mum--color-purple)
	 ;; message
	 (message-header-to :foreground ,mum--color-fg :weight bold)
	 (message-header-cc :foreground ,mum--color-fg :bold bold)
	 (message-header-subject :foreground ,mum--color-orange)
	 (message-header-newsgroups :foreground ,mum--color-purple)
	 (message-header-other :foreground ,mum--color-purple)
	 (message-header-name :foreground ,mum--color-green)
	 (message-header-xheader :foreground ,mum--color-cyan)
	 (message-separator :foreground ,mum--color-cyan :slant italic)
	 (message-cited-text :foreground ,mum--color-purple)
	 (message-cited-text-1 :foreground ,mum--color-purple)
	 (message-cited-text-2 :foreground ,mum--color-orange)
	 (message-cited-text-3 :foreground ,mum--color-comment)
	 (message-cited-text-4 :foreground ,mum--color-fg2)
	 (message-mml :foreground ,mum--color-green :weight bold)
	 ;; mode-line
	 (mode-line :background ,mum--color-current :box ,mum--color-current :inverse-video nil :foreground ,mum--color-fg3)
	 (mode-line-inactive :inverse-video nil :foreground ,mum--color-comment :background ,mum--color-bg :box ,mum--color-bg)
	 ;; org
	 (org-agenda-date :foreground ,mum--color-cyan :underline nil)
	 (org-agenda-dimmed-todo-face :foreground ,mum--color-comment)
	 (org-agenda-done :foreground ,mum--color-green)
	 (org-agenda-structure :foreground ,mum--color-purple)
	 (org-block :foreground ,mum--color-orange)
	 (org-code :foreground ,mum--color-yellow)
	 (org-column :background ,mum--color-bg4)
	 (org-column-title :inherit org-column :weight bold :underline t)
	 (org-date :foreground ,mum--color-cyan :underline t)
	 (org-document-info :foreground ,mum--color-other-blue)
	 (org-document-info-keyword :foreground ,mum--color-comment)
	 (org-document-title :weight bold :foreground ,mum--color-orange)
	 (org-done :foreground ,mum--color-green)
	 (org-ellipsis :foreground ,mum--color-comment)
	 (org-footnote :foreground ,mum--color-other-blue)
	 (org-formula :foreground ,mum--color-pink)
	 (org-headline-done :foreground ,mum--color-comment
						:weight bold :strike-through t)
	 (org-hide :foreground ,mum--color-bg :background ,mum--color-bg)
	 (org-level-1 :inherit bold :foreground ,mum--color-pink)
	 (org-level-2 :inherit bold :foreground ,mum--color-purple)
	 (org-level-3 :weight bold :foreground ,mum--color-green)
	 (org-level-4 :weight bold :foreground ,mum--color-yellow)
	 (org-level-5 :weight bold :foreground ,mum--color-cyan)
	 (org-level-6 :weight bold :foreground ,mum--color-orange)
	 (org-level-7 :weight bold :foreground ,mum--color-other-blue)
	 (org-level-8 :weight bold :foreground ,mum--color-fg)
	 (org-link :foreground ,mum--color-cyan :underline t)
	 (org-priority :foreground ,mum--color-cyan)
	 (org-scheduled :foreground ,mum--color-green)
	 (org-scheduled-previously :foreground ,mum--color-yellow)
	 (org-scheduled-today :foreground ,mum--color-green)
	 (org-sexp-date :foreground ,mum--color-fg4)
	 (org-special-keyword :foreground ,mum--color-yellow)
	 (org-table :foreground ,mum--color-purple)
	 (org-tag :foreground ,mum--color-pink :weight bold :background ,mum--color-bg2)
	 (org-todo :foreground ,mum--color-orange :weight bold :background ,mum--color-bg2)
	 (org-upcoming-deadline :foreground ,mum--color-yellow)
	 (org-warning :weight bold :foreground ,mum--color-pink)
	 ;; outline
	 (outline-1 :foreground ,mum--color-pink)
	 (outline-2 :foreground ,mum--color-purple)
	 (outline-3 :foreground ,mum--color-green)
	 (outline-4 :foreground ,mum--color-yellow)
	 (outline-5 :foreground ,mum--color-cyan)
	 (outline-6 :foreground ,mum--color-orange)
	 ;; rainbow-delimiters
	 (rainbow-delimiters-depth-1-face :foreground ,mum--color-fg)
	 (rainbow-delimiters-depth-2-face :foreground ,mum--color-cyan)
	 (rainbow-delimiters-depth-3-face :foreground ,mum--color-purple)
	 (rainbow-delimiters-depth-4-face :foreground ,mum--color-pink)
	 (rainbow-delimiters-depth-5-face :foreground ,mum--color-orange)
	 (rainbow-delimiters-depth-6-face :foreground ,mum--color-green)
	 (rainbow-delimiters-depth-7-face :foreground ,mum--color-yellow)
	 (rainbow-delimiters-depth-8-face :foreground ,mum--color-other-blue)
	 (rainbow-delimiters-unmatched-face :foreground ,mum--color-orange)
	 ;; show-paren
	 (show-paren-match-face :background unspecified :foreground ,mum--color-cyan :weight bold)
	 (show-paren-match :background unspecified :foreground ,mum--color-cyan :weight bold)
	 (show-paren-match-expression :background ,mum--color-bg2 :foreground nil)
	 (show-paren-mismatch :inherit font-lock-warning-face)
	 ;; term
	 (term :foreground ,mum--color-fg :background ,mum--color-bg)
	 (term-color-black :foreground ,mum--color-bg :background ,mum--color-bg)
	 (term-color-blue :foreground ,mum--color-purple :background ,mum--color-purple)
	 (term-color-cyan :foreground ,mum--color-cyan :background ,mum--color-cyan)
	 (term-color-green :foreground ,mum--color-green :background ,mum--color-green)
	 (term-color-magenta :foreground ,mum--color-pink :background ,mum--color-pink)
	 (term-color-red :foreground ,mum--color-red :background ,mum--color-red)
	 (term-color-white :foreground ,mum--color-fg :background ,mum--color-fg)
	 (term-color-yellow :foreground ,mum--color-yellow :background ,mum--color-yellow)
	 ;; undo-tree
	 (undo-tree-visualizer-current-face :foreground ,mum--color-orange)
	 (undo-tree-visualizer-default-face :foreground ,mum--color-fg2)
	 (undo-tree-visualizer-register-face :foreground ,mum--color-purple)
	 (undo-tree-visualizer-unmodified-face :foreground ,mum--color-fg)
	 ;; which-func
	 (which-func :inherit ,font-lock-function-name-face)
	 ;; whitespace
	 (whitespace-big-indent :background ,mum--color-red :foreground ,mum--color-red)
	 (whitespace-empty :background ,mum--color-orange :foreground ,mum--color-red)
	 (whitespace-hspace :background ,mum--color-bg3 :foreground ,mum--color-comment)
	 (whitespace-indentation :background ,mum--color-orange :foreground ,mum--color-red)
	 (whitespace-line :background ,mum--color-bg :foreground ,mum--color-pink)
	 (whitespace-newline :foreground ,mum--color-comment)
	 (whitespace-space :background ,mum--color-bg :foreground ,mum--color-comment)
	 (whitespace-space-after-tab :background ,mum--color-orange :foreground ,mum--color-red)
	 (whitespace-space-before-tab :background ,mum--color-orange :foreground ,mum--color-red)
	 (whitespace-tab :background ,mum--color-bg2 :foreground ,mum--color-comment)
	 (whitespace-trailing :inherit trailing-whitespace))
	)
  "Dark colors and faces.")

(defvar mum-style--light
  '(:colors
	'()
	:faces
	'())
  "Light colors and faces.")

(defvar mum-style--eye-care
  '(:colors
	'()
	:faces
	'())
  "Eye care colors and faces.")

(defun mum-style--find-colors-and-faces ()
  "Find colors with TYPE."
  (cond
   ((equal 'dark mum-style--type) mum-style--dark)
   ((equal 'light mum-style--type) mum-style--light)
   ((equal 'care mum-style--type) mum-style--eye-care)))

(defun mum-style--load ()
  "Load theme."
  (let* ((colors (plist-get (mum-style--find-colors-and-faces) :colors))
		 (faces (plist-get (mum-style--find-colors-and-faces) :faces)))
	(apply #'custom-theme-set-faces
		   'mum-style
		   (let* ((color-names (mapcar #'car colors))
				  (graphic-colors (mapcar #'cadr colors))
				  (term-colors (mapcar #'car (mapcar #'cddr colors)))
				  (tty-colors (mapcar #'car (mapcar #'last colors)))
				  (expand-for-kind (lambda (kind spec)
									 (when (string= (symbol-name kind) "term-colors")
									   (setq kind 'graphic-colors))
									 (cl-progv color-names (symbol-value kind)
									   (eval `(backquote ,spec))))))
			 (cl-loop for (face . spec) in faces
					  collect `(,face
								((((min-colors 16777216)) ,(funcall expand-for-kind 'graphic-colors spec))
								 (((min-colors 256)) ,(funcall expand-for-kind 'term-colors spec))
								 (t ,(funcall expand-for-kind 'tty-colors spec)))))))))

(mum-style--load)

(defun mum-style--toggle-theme (&optional style)
  "Toggle theme STYLE."
  (interactive (list (completing-read (format "style (%s)" mum-style--type)
									  '("dark" "ligth" "care"))))
  (let* ((styles '(("dark" . dark)
				   ("ligth" . ligth)
				   ("care" . care))))
	(when style
      (setq mum-style--type (cdr (assoc style styles))))
	(mapc #'disable-theme custom-enabled-themes)
	(load-theme 'mum-style)))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'mum-style)
;;; mum-style-theme.el ends here
