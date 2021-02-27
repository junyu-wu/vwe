;;; vwe-style-theme.el --- Vwe style theme

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

(deftheme vwe-style "Vwe style theme.")

(defgroup vwe-style nil
  "Theme options."
  :group 'faces)

(defvar vwe-style--type
  'dark
  "Type is `dark' `ligth' and `care'.")

(defvar vwe-style--dark
  '(:colors
	((vwe--color-bg              "#282a36" "unspecified-bg" "unspecified-bg")
	 (vwe--color-fg              "#f8f8f2" "#ffffff" "brightwhite")
	 (vwe--color-current         "#44475a" "#303030" "brightblack")
	 (vwe--color-comment         "#6272a4" "#5f5faf" "blue")
	 (vwe--color-cyan            "#8be9fd" "#87d7ff" "brightcyan")
	 (vwe--color-green           "#50fa7b" "#5fff87" "green")
	 (vwe--color-orange          "#ffb86c" "#ffaf5f" "brightred")
	 (vwe--color-pink            "#ff79c6" "#ff87d7" "magenta")
	 (vwe--color-purple          "#bd93f9" "#af87ff" "brightmagenta")
	 (vwe--color-red             "#ff5555" "#ff8787" "red")
	 (vwe--color-yellow          "#f1fa8c" "#ffff87" "yellow")

	 (vwe--color-bg2             "#373844" "#121212" "brightblack")
	 (vwe--color-bg3             "#464752" "#262626" "brightblack")
	 (vwe--color-bg4             "#565761" "#444444" "brightblack")
	 (vwe--color-fg2             "#e2e2dc" "#e4e4e4" "brightwhite")
	 (vwe--color-fg3             "#ccccc7" "#c6c6c6" "white")
	 (vwe--color-fg4             "#b6b6b2" "#b2b2b2" "white")
	 (vwe--color-other-blue      "#0189cc" "#0087ff" "brightblue"))
	:faces
	(;; default
	 (cursor :background ,vwe--color-fg3)
	 (completions-first-difference :foreground ,vwe--color-pink :weight bold)
	 (default :background ,vwe--color-bg :foreground ,vwe--color-fg)
	 (default-italic :slant italic)
	 (ffap :foreground ,vwe--color-fg4)
	 (fringe :background ,vwe--color-bg :foreground ,vwe--color-fg4)
	 (highlight :foreground ,vwe--color-fg3 :background ,vwe--color-bg3)
	 (hl-line :background ,vwe--color-current :extend t)
	 (info-quoted-name :foreground ,vwe--color-orange)
	 (info-string :foreground ,vwe--color-yellow)
	 (lazy-highlight :foreground ,vwe--color-fg2 :background ,vwe--color-bg2)
	 (link :foreground ,vwe--color-cyan :underline t)
	 (linum :slant italic :foreground ,vwe--color-bg4 :background ,vwe--color-bg)
	 (line-number :slant italic :foreground ,vwe--color-bg4 :background ,vwe--color-bg)
	 (match :background ,vwe--color-yellow :foreground ,vwe--color-bg)
	 (minibuffer-prompt :weight bold :foreground ,vwe--color-fg)
	 (read-multiple-choice-face :inherit completions-first-difference)
	 (region :inherit match :extend t)
	 (trailing-whitespace :foreground "unspecified-fg" :background ,vwe--color-orange)
	 (vertical-border :foreground ,vwe--color-bg2)
	 (success :foreground ,vwe--color-green)
	 (warning :foreground ,vwe--color-orange)
	 (error :foreground ,vwe--color-red)
	 (header-line :background ,vwe--color-bg)
	 ;; syntax
	 (font-lock-builtin-face :foreground ,vwe--color-orange)
	 (font-lock-comment-face :foreground ,vwe--color-comment)
	 (font-lock-comment-delimiter-face :foreground ,vwe--color-comment)
	 (font-lock-constant-face :foreground ,vwe--color-cyan)
	 (font-lock-doc-face :foreground ,vwe--color-comment)
	 (font-lock-function-name-face :foreground ,vwe--color-green :weight bold)
	 (font-lock-keyword-face :weight bold :foreground ,vwe--color-pink)
	 (font-lock-negation-char-face :foreground ,vwe--color-cyan)
	 (font-lock-preprocessor-face :foreground ,vwe--color-orange)
	 (font-lock-reference-face :foreground ,vwe--color-cyan)
	 (font-lock-regexp-grouping-backslash :foreground ,vwe--color-cyan)
	 (font-lock-regexp-grouping-construct :foreground ,vwe--color-purple)
	 (font-lock-string-face :foreground ,vwe--color-yellow)
	 (font-lock-type-face :foreground ,vwe--color-purple)
	 (font-lock-variable-name-face :foreground ,vwe--color-fg :weight bold)
	 (font-lock-warning-face :foreground ,vwe--color-orange :background ,vwe--color-bg2)
	 ;; diff-hl
	 (diff-hl-change :foreground ,vwe--color-orange :background ,vwe--color-orange)
	 (diff-hl-delete :foreground ,vwe--color-red :background ,vwe--color-red)
	 (diff-hl-insert :foreground ,vwe--color-green :background ,vwe--color-green)
	 ;; dired
	 (dired-directory :foreground ,vwe--color-green :weight bold)
	 (dired-flagged :foreground ,vwe--color-pink)
	 (dired-header :foreground ,vwe--color-fg3 :background ,vwe--color-bg)
	 (dired-ignored :inherit shadow)
	 (dired-mark :foreground ,vwe--color-fg :weight bold)
	 (dired-marked :foreground ,vwe--color-orange :weight bold)
	 (dired-perm-write :foreground ,vwe--color-fg3 :underline t)
	 (dired-symlink :foreground ,vwe--color-yellow :weight bold :slant italic)
	 (dired-warning :foreground ,vwe--color-orange :underline t)
	 (diredp-compressed-file-name :foreground ,vwe--color-fg3)
	 (diredp-compressed-file-suffix :foreground ,vwe--color-fg4)
	 (diredp-date-time :foreground ,vwe--color-fg)
	 (diredp-deletion-file-name :foreground ,vwe--color-pink :background ,vwe--color-current)
	 (diredp-deletion :foreground ,vwe--color-pink :weight bold)
	 (diredp-dir-heading :foreground ,vwe--color-fg2 :background ,vwe--color-bg4)
	 (diredp-dir-name :inherit dired-directory)
	 (diredp-dir-priv :inherit dired-directory)
	 (diredp-executable-tag :foreground ,vwe--color-orange)
	 (diredp-file-name :foreground ,vwe--color-fg)
	 (diredp-file-suffix :foreground ,vwe--color-fg4)
	 (diredp-flag-mark-line :foreground ,vwe--color-fg2 :slant italic :background ,vwe--color-current)
	 (diredp-flag-mark :foreground ,vwe--color-fg2 :weight bold :background ,vwe--color-current)
	 (diredp-ignored-file-name :foreground ,vwe--color-fg)
	 (diredp-mode-line-flagged :foreground ,vwe--color-orange)
	 (diredp-mode-line-marked :foreground ,vwe--color-orange)
	 (diredp-no-priv :foreground ,vwe--color-fg)
	 (diredp-number :foreground ,vwe--color-cyan)
	 (diredp-other-priv :foreground ,vwe--color-orange)
	 (diredp-rare-priv :foreground ,vwe--color-orange)
	 (diredp-read-priv :foreground ,vwe--color-purple)
	 (diredp-write-priv :foreground ,vwe--color-pink)
	 (diredp-exec-priv :foreground ,vwe--color-yellow)
	 (diredp-symlink :foreground ,vwe--color-orange)
	 (diredp-link-priv :foreground ,vwe--color-orange)
	 (diredp-autofile-name :foreground ,vwe--color-yellow)
	 (diredp-tagged-autofile-name :foreground ,vwe--color-yellow)
	 ;; elpher
	 (elpher-gemini-heading1 :inherit bold :foreground ,vwe--color-pink)
	 (elpher-gemini-heading2 :inherit bold :foreground ,vwe--color-purple)
	 (elpher-gemini-heading3 :weight bold :foreground ,vwe--color-green)
	 (elpher-gemini-preformatted :inherit fixed-pitch :foreground ,vwe--color-orange)
	 ;; flyspell
	 (flyspell-duplicate :underline (:style wave :color ,vwe--color-orange))
	 (flyspell-incorrect :underline (:style wave :color ,vwe--color-red))
	 ;; font-latex
	 (font-latex-bold-face :foreground ,vwe--color-purple)
	 (font-latex-italic-face :foreground ,vwe--color-pink :slant italic)
	 (font-latex-match-reference-keywords :foreground ,vwe--color-cyan)
	 (font-latex-match-variable-keywords :foreground ,vwe--color-fg)
	 (font-latex-string-face :foreground ,vwe--color-yellow)
	 ;; isearch
	 (isearch :inherit match :weight bold)
	 (isearch-fail :foreground ,vwe--color-bg :background ,vwe--color-orange)
	 ;; markdown
	 (markdown-blockquote-face :foreground ,vwe--color-purple)
	 (markdown-code-face :foreground ,vwe--color-orange)
	 (markdown-footnote-face :foreground ,vwe--color-other-blue)
	 (markdown-header-face :weight bold)
	 (markdown-header-face-1 :inherit bold :foreground ,vwe--color-pink)
	 (markdown-header-face-2 :inherit bold :foreground ,vwe--color-purple)
	 (markdown-header-face-3 :foreground ,vwe--color-green)
	 (markdown-header-face-4 :foreground ,vwe--color-yellow)
	 (markdown-header-face-5 :foreground ,vwe--color-cyan)
	 (markdown-header-face-6 :foreground ,vwe--color-orange)
	 (markdown-header-face-7 :foreground ,vwe--color-other-blue)
	 (markdown-header-face-8 :foreground ,vwe--color-fg)
	 (markdown-inline-code-face :foreground ,vwe--color-yellow)
	 (markdown-plain-url-face :inherit link)
	 (markdown-pre-face :foreground ,vwe--color-orange)
	 (markdown-table-face :foreground ,vwe--color-purple)
	 ;; message
	 (message-header-to :foreground ,vwe--color-fg :weight bold)
	 (message-header-cc :foreground ,vwe--color-fg :bold bold)
	 (message-header-subject :foreground ,vwe--color-orange)
	 (message-header-newsgroups :foreground ,vwe--color-purple)
	 (message-header-other :foreground ,vwe--color-purple)
	 (message-header-name :foreground ,vwe--color-green)
	 (message-header-xheader :foreground ,vwe--color-cyan)
	 (message-separator :foreground ,vwe--color-cyan :slant italic)
	 (message-cited-text :foreground ,vwe--color-purple)
	 (message-cited-text-1 :foreground ,vwe--color-purple)
	 (message-cited-text-2 :foreground ,vwe--color-orange)
	 (message-cited-text-3 :foreground ,vwe--color-comment)
	 (message-cited-text-4 :foreground ,vwe--color-fg2)
	 (message-mml :foreground ,vwe--color-green :weight bold)
	 ;; mode-line
	 (mode-line :background ,vwe--color-current :box ,vwe--color-current :inverse-video nil :foreground ,vwe--color-fg3)
	 (mode-line-inactive :inverse-video nil :foreground ,vwe--color-comment :background ,vwe--color-bg :box ,vwe--color-bg)
	 ;; org
	 (org-agenda-date :foreground ,vwe--color-cyan :underline nil)
	 (org-agenda-dimmed-todo-face :foreground ,vwe--color-comment)
	 (org-agenda-done :foreground ,vwe--color-green)
	 (org-agenda-structure :foreground ,vwe--color-purple)
	 (org-block :foreground ,vwe--color-orange)
	 (org-code :foreground ,vwe--color-yellow)
	 (org-column :background ,vwe--color-bg4)
	 (org-column-title :inherit org-column :weight bold :underline t)
	 (org-date :foreground ,vwe--color-cyan :underline t)
	 (org-document-info :foreground ,vwe--color-other-blue)
	 (org-document-info-keyword :foreground ,vwe--color-comment)
	 (org-document-title :weight bold :foreground ,vwe--color-orange)
	 (org-done :foreground ,vwe--color-green)
	 (org-ellipsis :foreground ,vwe--color-comment)
	 (org-footnote :foreground ,vwe--color-other-blue)
	 (org-formula :foreground ,vwe--color-pink)
	 (org-headline-done :foreground ,vwe--color-comment
						:weight bold :strike-through t)
	 (org-hide :foreground ,vwe--color-bg :background ,vwe--color-bg)
	 (org-level-1 :inherit bold :foreground ,vwe--color-pink)
	 (org-level-2 :inherit bold :foreground ,vwe--color-purple)
	 (org-level-3 :weight bold :foreground ,vwe--color-green)
	 (org-level-4 :weight bold :foreground ,vwe--color-yellow)
	 (org-level-5 :weight bold :foreground ,vwe--color-cyan)
	 (org-level-6 :weight bold :foreground ,vwe--color-orange)
	 (org-level-7 :weight bold :foreground ,vwe--color-other-blue)
	 (org-level-8 :weight bold :foreground ,vwe--color-fg)
	 (org-link :foreground ,vwe--color-cyan :underline t)
	 (org-priority :foreground ,vwe--color-cyan)
	 (org-scheduled :foreground ,vwe--color-green)
	 (org-scheduled-previously :foreground ,vwe--color-yellow)
	 (org-scheduled-today :foreground ,vwe--color-green)
	 (org-sexp-date :foreground ,vwe--color-fg4)
	 (org-special-keyword :foreground ,vwe--color-yellow)
	 (org-table :foreground ,vwe--color-purple)
	 (org-tag :foreground ,vwe--color-pink :weight bold :background ,vwe--color-bg2)
	 (org-todo :foreground ,vwe--color-orange :weight bold :background ,vwe--color-bg2)
	 (org-upcoming-deadline :foreground ,vwe--color-yellow)
	 (org-warning :weight bold :foreground ,vwe--color-pink)
	 ;; outline
	 (outline-1 :foreground ,vwe--color-pink)
	 (outline-2 :foreground ,vwe--color-purple)
	 (outline-3 :foreground ,vwe--color-green)
	 (outline-4 :foreground ,vwe--color-yellow)
	 (outline-5 :foreground ,vwe--color-cyan)
	 (outline-6 :foreground ,vwe--color-orange)
	 ;; rainbow-delimiters
	 (rainbow-delimiters-depth-1-face :foreground ,vwe--color-fg)
	 (rainbow-delimiters-depth-2-face :foreground ,vwe--color-cyan)
	 (rainbow-delimiters-depth-3-face :foreground ,vwe--color-purple)
	 (rainbow-delimiters-depth-4-face :foreground ,vwe--color-pink)
	 (rainbow-delimiters-depth-5-face :foreground ,vwe--color-orange)
	 (rainbow-delimiters-depth-6-face :foreground ,vwe--color-green)
	 (rainbow-delimiters-depth-7-face :foreground ,vwe--color-yellow)
	 (rainbow-delimiters-depth-8-face :foreground ,vwe--color-other-blue)
	 (rainbow-delimiters-unmatched-face :foreground ,vwe--color-orange)
	 ;; show-paren
	 (show-paren-match-face :background unspecified :foreground ,vwe--color-cyan :weight bold)
	 (show-paren-match :background unspecified :foreground ,vwe--color-cyan :weight bold)
	 (show-paren-match-expression :background ,vwe--color-bg2 :foreground nil)
	 (show-paren-mismatch :inherit font-lock-warning-face)
	 ;; term
	 (term :foreground ,vwe--color-fg :background ,vwe--color-bg)
	 (term-color-black :foreground ,vwe--color-bg :background ,vwe--color-bg)
	 (term-color-blue :foreground ,vwe--color-purple :background ,vwe--color-purple)
	 (term-color-cyan :foreground ,vwe--color-cyan :background ,vwe--color-cyan)
	 (term-color-green :foreground ,vwe--color-green :background ,vwe--color-green)
	 (term-color-magenta :foreground ,vwe--color-pink :background ,vwe--color-pink)
	 (term-color-red :foreground ,vwe--color-red :background ,vwe--color-red)
	 (term-color-white :foreground ,vwe--color-fg :background ,vwe--color-fg)
	 (term-color-yellow :foreground ,vwe--color-yellow :background ,vwe--color-yellow)
	 ;; undo-tree
	 (undo-tree-visualizer-current-face :foreground ,vwe--color-orange)
	 (undo-tree-visualizer-default-face :foreground ,vwe--color-fg2)
	 (undo-tree-visualizer-register-face :foreground ,vwe--color-purple)
	 (undo-tree-visualizer-unmodified-face :foreground ,vwe--color-fg)
	 ;; which-func
	 (which-func :inherit ,font-lock-function-name-face)
	 ;; whitespace
	 (whitespace-big-indent :background ,vwe--color-red :foreground ,vwe--color-red)
	 (whitespace-empty :background ,vwe--color-orange :foreground ,vwe--color-red)
	 (whitespace-hspace :background ,vwe--color-bg3 :foreground ,vwe--color-comment)
	 (whitespace-indentation :background ,vwe--color-orange :foreground ,vwe--color-red)
	 (whitespace-line :background ,vwe--color-bg :foreground ,vwe--color-pink)
	 (whitespace-newline :foreground ,vwe--color-comment)
	 (whitespace-space :background ,vwe--color-bg :foreground ,vwe--color-comment)
	 (whitespace-space-after-tab :background ,vwe--color-orange :foreground ,vwe--color-red)
	 (whitespace-space-before-tab :background ,vwe--color-orange :foreground ,vwe--color-red)
	 (whitespace-tab :background ,vwe--color-bg2 :foreground ,vwe--color-comment)
	 (whitespace-trailing :inherit trailing-whitespace))
	)
  "Dark colors and faces.")

(defvar vwe-style--light
  '(:colors
	'()
	:faces
	'())
  "Light colors and faces.")

(defvar vwe-style--eye-care
  '(:colors
	'()
	:faces
	'())
  "Eye care colors and faces.")

(defun vwe-style--find-colors-and-faces ()
  "Find colors with TYPE."
  (cond
   ((equal 'dark vwe-style--type) vwe-style--dark)
   ((equal 'light vwe-style--type) vwe-style--light)
   ((equal 'care vwe-style--type) vwe-style--eye-care)))

(defun vwe-style--load ()
  "Load theme."
  (let* ((colors (plist-get (vwe-style--find-colors-and-faces) :colors))
		 (faces (plist-get (vwe-style--find-colors-and-faces) :faces)))
	(apply #'custom-theme-set-faces
		   'vwe-style
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

(vwe-style--load)

(defun vwe-style--toggle-theme (&optional style)
  "Toggle theme STYLE."
  (interactive (list (completing-read (format "style (%s)" vwe-style--type)
									  '("dark" "ligth" "care"))))
  (let* ((styles '(("dark" . dark)
				   ("ligth" . ligth)
				   ("care" . care))))
	(when style
      (setq vwe-style--type (cdr (assoc style styles))))
	(mapc #'disable-theme custom-enabled-themes)
	(load-theme 'vwe-style t)))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'vwe-style)
;;; vwe-style-theme.el ends here
