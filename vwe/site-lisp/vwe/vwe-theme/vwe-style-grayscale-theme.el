;;; vwe-style-grayscale-theme.el --- Dark theme         -binding: t; -*-

;; Copyright (C) 2021

;; Author:  <>
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
(require 'vwe-style)

(deftheme vwe-style-grayscale
  "Vwe style theme.")

(defvar vwe-style-grayscale--colors
  '((none nil nil nil)
	(bg "#eeeeee" "#eeeeee" "white")
	(fg "#131313" "#131313" "black")
	(hl "#dddddd" "#dddddd" "#dddddd")
	(func "black" "black" "black")
	(var "#616161" "#616161" "#616161")
	(str "#474747" "#474747" "#474747")
	(part "#e6e6e6" "#e6e6e6" "#e6e6e6")
	(ragn "#d6d6d6" "#d6d6d6" "#d6d6d6")

    (current "#ffeaaa" "#ffeaaa" "brightblack")
    (comt "#6272a4" "#5f5faf" "blue")
    (cyan    "#8be9fd" "#87d7ff" "brightcyan")
    (green   "#50fa7b" "#5fff87" "green")
    (orange  "#ffb86c" "#ffaf5f" "brightred")
    (pink    "#ff79c6" "#ff87d7" "magenta")
    (purple  "#bd93f9" "#af87ff" "brightmagenta")
    (red     "#ff5555" "#ff8787" "red")
    (yellow  "#f1fa8c" "#ffff87" "yellow")
	(moccasin "#ffe4b5" "#ffe4b5" "yellow")
	(deepskyblue "00688B" "00688B" "00688B")
	(forestgreen "#228b22" "#228b22" "green")
	(darkgoldenrod  "#b8860b" "#b8860b" "yellow")
	(chocolate "#d2691e" "#d2691e" "#d2691e")
	(dodgerblue "#1e90ff" "#1e90ff" "blue")
	(darkorange  "#ff8c00" "#ff8c00" "orange")
	(orangered "#ff4500" "#ff4500" "#ff4500")
	(mediumspringgreen  "#00fa9a" "#00fa9a" "green")
	(snow3  "#cdc9c9" "#cdc9c9" "gray")
	(slategray4  "#6c7b8b" "#6c7b8b" nil)
	(parent "#fff1cc" "#fff1cc" "#fff1cc")
	(ecyan "#00ffff" "#00ffff" "#00ffff")
	(whitesmoke "#f5f5f5" "#f5f5f5" "white")
	(regionc "#3f3f5f" "#555555" "#555555")
	(grey9  "#171717" "#171717" "black")
	(gold "#ffd700" "#ffd700" "yellow")
	(darkgoldenrod "#b8860b" "#b8860b" "#b8860b")
	(trailing "#ffee99" "#ffee99" "#ffee99")
	(palegreen  "#98fb98" "#98fb98" "#98fb98")
	(darkturquoise "#00ced1" "#00ced1" "#00ced1")
	(light-string "#527884" "#527884" "#527884")
	(lightgrey "#d3d3d3" "#d3d3d3" "#d3d3d3")
	(darkgrey "#a9a9a9" "#a9a9a9" "#a9a9a9")
	(dimgrey "#696969" "#696969" "#696969")
	(darkolivegreen "#556b2f" "#556b2f" "#556b2f")
	(mistyrose4 "#8b7d7b" "#8b7d7b" "#8b7d7b")
	(darkturquoise "#00ced1" "#00ced1" "#00ced1")
    (other-blue      "#0189cc" "#0087ff" "brightblue"))
  "Theme colors.")

(defvar vwe-style-grayscale--faces
  '(;; default
    (cursor :background ,fg :weight bold)
    (default :background ,bg :foreground ,grey9)
	(error :foreground ,red)
	(fringe :background ,bg :foreground ,fg)
    (header-line :background ,fg)
    (highlight :foreground ,none :background ,hl)
	(line-number :slant italic :foreground ,darkgrey :background ,bg)
	(line-number-current-line :slant italic :foreground ,fg :background ,hl)
	(link :foreground ,var :underline t)
	(link-visited :foreground ,var :underline t)
    (match :background ,ragn :foreground ,func)
	(minibuffer-prompt :weight bold :foreground ,pink)
	(mode-line :background ,hl :inverse-video nil :foreground ,fg)
	(mode-line-inactive :foreground ,comt :background ,bg :box ,bg)
	(region :background ,ragn :foreground ,none :box (:color ,ragn :line-width 1) ;; :inverse-video t
			)
	(shadow :foreground ,part)
    (success :foreground ,green)
	(tab-bar :foreground ,purple :background ,current :inherit variable-pitch)
    (tab-bar-tab :foreground ,pink :background ,bg :box (:line-width 2 :color ,bg :style nil))
    (tab-bar-tab-inactive :foreground ,purple :background ,bg :box (:line-width 2 :color ,bg :style nil))
    (tab-line :foreground ,purple :background ,current :height 0.9 :inherit variable-pitch)
    (tab-line-tab :foreground ,pink :background ,bg :box (:line-width 2 :color ,bg :style nil))
    (tab-line-tab-inactive :foreground ,purple :background ,bg :box (:line-width 2 :color ,bg :style nil))
    (tab-line-tab-current :inherit tab-line-tab)
    (tab-line-close-highlight :foreground ,red)
	(trailing-whitespace :background ,trailing)
	(warning :foreground ,orange)

	;; font lock
	(font-lock-builtin-face :foreground ,var :slant italic)
    (font-lock-comment-delimiter-face :foreground ,darkgrey)
    (font-lock-comment-face :foreground ,darkgrey)
    (font-lock-constant-face :foreground ,var :weight bold)
    (font-lock-doc-face :foreground ,mistyrose4)
    (font-lock-function-name-face :foreground ,func :weight bold :underline t)
    (font-lock-keyword-face :foreground ,func :weight bold)
    (font-lock-negation-char-face :foreground ,str)
    (font-lock-preprocessor-face :foreground ,var :weight bold)
    (font-lock-regexp-grouping-backslash :foreground ,str)
    (font-lock-regexp-grouping-construct :foreground ,darkgrey)
    (font-lock-string-face :foreground ,str)
    (font-lock-type-face :foreground ,darkgrey)
    (font-lock-variable-name-face :foreground ,var :slant italic :underline t)
    (font-lock-warning-face :foreground ,var :background ,bg :weight bold :slant italic)

    (font-lock-reference-face :foreground ,cyan)

	;; company
	(company-echo-common :foreground ,bg :background ,fg)
    (company-preview :background ,current :foreground ,other-blue)
    (company-preview-common :background ,ragn :foreground ,bg)
    (company-preview-search :inherit company-preview :foreground ,green)
    (company-scrollbar-bg :background ,comt)
    (company-scrollbar-fg :foreground ,fg)
    (company-tooltip :foreground ,none :background ,bg)
    (company-tooltip-annotation :foreground ,cyan)
    ;;(company-tooltip-annotation-selection :inherit company-tooltip-annotation)
    (company-tooltip-common :foreground ,func :weight bold)
    ;;(company-tooltip-common-selection :inherit company-tooltip-common)
    (company-tooltip-mouse :background ,bg)
    (company-tooltip-search :foreground ,green :underline t)
    (company-tooltip-search-selection :background ,green :foreground ,bg)
    (company-tooltip-selection :background ,hl)

	;; completions (minibuffer.el)
    (completions-annotations :inherit font-lock-comment-face)
    (completions-common-part :foreground ,green)
    (completions-first-difference :foreground ,var :weight bold)

	;; diff-hl
    (diff-hl-change :foreground ,orange :background ,orange)
    (diff-hl-delete :foreground ,red :background ,red)
    (diff-hl-insert :foreground ,green :background ,green)

    ;; dired
    (dired-directory :foreground ,green :weight normal)
    (dired-flagged :foreground ,pink)
    (dired-header :foreground ,fg :background ,bg)
    (dired-ignored :inherit shadow)
    (dired-mark :foreground ,fg :weight bold)
    (dired-marked :foreground ,orange :weight bold)
    (dired-perm-write :foreground ,fg :underline t)
    (dired-symlink :foreground ,yellow :weight normal :slant italic)
    (dired-warning :foreground ,orange :underline t)
    (diredp-compressed-file-name :foreground ,fg)
    (diredp-compressed-file-suffix :foreground ,fg)
    (diredp-date-time :foreground ,fg)
    (diredp-deletion-file-name :foreground ,pink :background ,current)
    (diredp-deletion :foreground ,pink :weight bold)
    (diredp-dir-heading :foreground ,fg :background ,bg)
    (diredp-dir-name :inherit dired-directory)
    (diredp-dir-priv :inherit dired-directory)
    (diredp-executable-tag :foreground ,orange)
    (diredp-file-name :foreground ,fg)
    (diredp-file-suffix :foreground ,fg)
    (diredp-flag-mark-line :foreground ,fg :slant italic :background ,current)
    (diredp-flag-mark :foreground ,fg :weight bold :background ,current)
    (diredp-ignored-file-name :foreground ,fg)
    (diredp-mode-line-flagged :foreground ,orange)
    (diredp-mode-line-marked :foreground ,orange)
    (diredp-no-priv :foreground ,fg)
    (diredp-number :foreground ,cyan)
    (diredp-other-priv :foreground ,orange)
    (diredp-rare-priv :foreground ,orange)
    (diredp-read-priv :foreground ,purple)
    (diredp-write-priv :foreground ,pink)
    (diredp-exec-priv :foreground ,yellow)
    (diredp-symlink :foreground ,orange)
    (diredp-link-priv :foreground ,orange)
    (diredp-autofile-name :foreground ,yellow)
    (diredp-tagged-autofile-name :foreground ,yellow)

	;; elfeed
    (elfeed-search-date-face :foreground ,comt)
    (elfeed-search-title-face :foreground ,fg)
    (elfeed-search-unread-title-face :foreground ,pink :weight bold)
    (elfeed-search-feed-face :foreground ,fg :weight bold)
    (elfeed-search-tag-face :foreground ,green)
    (elfeed-search-last-update-face :weight bold)
    (elfeed-search-unread-count-face :foreground ,pink)
    (elfeed-search-filter-face :foreground ,green :weight bold)
    ;;(elfeed-log-date-face :inherit font-lock-type-face)
    (elfeed-log-error-level-face :foreground ,red)
    (elfeed-log-warn-level-face :foreground ,orange)
    (elfeed-log-info-level-face :foreground ,cyan)
    (elfeed-log-debug-level-face :foreground ,comt)

    ;; elpher
    (elpher-gemini-heading1 :inherit bold :foreground ,pink)
    (elpher-gemini-heading2 :inherit bold :foreground ,purple)
    (elpher-gemini-heading3 :weight normal :foreground ,green)
    (elpher-gemini-preformatted :inherit fixed-pitch :foreground ,orange)

	;; flyspell
    (flyspell-duplicate :underline (:style wave :color ,orange))
    (flyspell-incorrect :underline (:style wave :color ,red))

    ;; font-latex
    (font-latex-bold-face :foreground ,purple)
    (font-latex-italic-face :foreground ,pink :slant italic)
    (font-latex-match-reference-keywords :foreground ,cyan)
    (font-latex-match-variable-keywords :foreground ,fg)
    (font-latex-string-face :foreground ,yellow)

	;; gemini
    (gemini-heading-face-1 :inherit bold :foreground ,pink)
    (gemini-heading-face-2 :inherit bold :foreground ,purple)
    (gemini-heading-face-3 :weight normal :foreground ,green)
    (gemini-heading-face-rest :weight normal :foreground ,yellow)
    (gemini-quote-face :foreground ,purple)

    ;; gnus-group
    (gnus-group-mail-1 :foreground ,pink :weight bold)
    (gnus-group-mail-1-empty :inherit gnus-group-mail-1 :weight normal)
    (gnus-group-mail-2 :foreground ,cyan :weight bold)
    (gnus-group-mail-2-empty :inherit gnus-group-mail-2 :weight normal)
    (gnus-group-mail-3 :foreground ,comt :weight bold)
    (gnus-group-mail-3-empty :inherit gnus-group-mail-3 :weight normal)
    (gnus-group-mail-low :foreground ,current :weight bold)
    (gnus-group-mail-low-empty :inherit gnus-group-mail-low :weight normal)
    (gnus-group-news-1 :foreground ,pink :weight bold)
    (gnus-group-news-1-empty :inherit gnus-group-news-1 :weight normal)
    (gnus-group-news-2 :foreground ,cyan :weight bold)
    (gnus-group-news-2-empty :inherit gnus-group-news-2 :weight normal)
    (gnus-group-news-3 :foreground ,comt :weight bold)
    (gnus-group-news-3-empty :inherit gnus-group-news-3 :weight normal)
    (gnus-group-news-4 :inherit gnus-group-news-low)
    (gnus-group-news-4-empty :inherit gnus-group-news-low-empty)
    (gnus-group-news-5 :inherit gnus-group-news-low)
    (gnus-group-news-5-empty :inherit gnus-group-news-low-empty)
    (gnus-group-news-6 :inherit gnus-group-news-low)
    (gnus-group-news-6-empty :inherit gnus-group-news-low-empty)
    (gnus-group-news-low :foreground ,other-blue :weight bold)
    (gnus-group-news-low-empty :inherit gnus-group-news-low :weight normal)
    (gnus-header-content :foreground ,purple)
    (gnus-header-from :foreground ,fg)
    (gnus-header-name :foreground ,green)
    (gnus-header-subject :foreground ,pink :weight bold)
    (gnus-summary-markup-face :foreground ,cyan)
    (gnus-summary-high-unread :foreground ,pink :weight bold)
    (gnus-summary-high-read :inherit gnus-summary-high-unread :weight normal)
    (gnus-summary-high-ancient :inherit gnus-summary-high-read)
    (gnus-summary-high-ticked :inherit gnus-summary-high-read :underline t)
    (gnus-summary-normal-unread :foreground ,other-blue :weight bold)
    (gnus-summary-normal-read :foreground ,comt :weight normal)
    (gnus-summary-normal-ancient :inherit gnus-summary-normal-read :weight light)
    (gnus-summary-normal-ticked :foreground ,pink :weight bold)
    (gnus-summary-low-unread :foreground ,comt :weight bold)
    (gnus-summary-low-read :inherit gnus-summary-low-unread :weight normal)
    (gnus-summary-low-ancient :inherit gnus-summary-low-read)
    (gnus-summary-low-ticked :inherit gnus-summary-low-read :underline t)
    (gnus-summary-selected :inverse-video t)

	;; highlight-indentation minor mode
    (highlight-indentation-face :background ,bg)

	;; ivy
    (ivy-current-match :weight bold :foreground ,fg :background ,hl)
	;; Highlights the background of the match.
    (ivy-minibuffer-match-face-1 :background ,current)
    ;; Highlights the first matched group.
    (ivy-minibuffer-match-face-2 :background ,ragn :foreground ,fg)
    ;; Highlights the second matched group.
    (ivy-minibuffer-match-face-3 :background ,orange :foreground ,bg)
    ;; Highlights the third matched group.
    (ivy-minibuffer-match-face-4 :background ,var :foreground ,bg)
	(ivy-minibuffer-match-highlight :background ,green :foreground ,none)
    (ivy-confirm-face :foreground ,orange)
    (ivy-match-required-face :foreground ,red)
    (ivy-subdir :foreground ,fg :background ,ragn)
    (ivy-remote :foreground ,pink)
    (ivy-virtual :foreground ,cyan)

	;; isearch
    (isearch :inherit match :weight bold)
    (isearch-fail :foreground ,bg :background ,orange)

	;; markdown
    (markdown-blockquote-face :foreground ,purple)
    (markdown-code-face :foreground ,orange)
    (markdown-footnote-face :foreground ,other-blue)
    (markdown-header-face :weight normal)
    (markdown-header-face-1 :inherit bold :foreground ,pink)
    (markdown-header-face-2 :inherit bold :foreground ,purple)
    (markdown-header-face-3 :foreground ,green)
    (markdown-header-face-4 :foreground ,yellow)
    (markdown-header-face-5 :foreground ,cyan)
    (markdown-header-face-6 :foreground ,orange)
    (markdown-header-face-7 :foreground ,other-blue)
    (markdown-header-face-8 :foreground ,fg)
    (markdown-inline-code-face :foreground ,yellow)
    (markdown-plain-url-face :inherit link)
    (markdown-pre-face :foreground ,orange)
    (markdown-table-face :foreground ,purple)

    ;; message
    (message-header-to :foreground ,fg :weight bold)
    (message-header-cc :foreground ,fg :bold bold)
    (message-header-subject :foreground ,orange)
    (message-header-newsgroups :foreground ,purple)
    (message-header-other :foreground ,purple)
    (message-header-name :foreground ,green)
    (message-header-xheader :foreground ,cyan)
    (message-separator :foreground ,cyan :slant italic)
    (message-cited-text :foreground ,purple)
    (message-cited-text-1 :foreground ,purple)
    (message-cited-text-2 :foreground ,orange)
    (message-cited-text-3 :foreground ,comt)
    (message-cited-text-4 :foreground ,fg)
    (message-mml :foreground ,green :weight normal)

    ;; org
    (org-agenda-date :foreground ,cyan :underline nil)
    (org-agenda-dimmed-todo-face :foreground ,comt)
    (org-agenda-done :foreground ,green)
    (org-agenda-structure :foreground ,purple)
    (org-block :foreground ,orange)
    (org-code :foreground ,yellow)
    (org-column :background ,bg)
    (org-column-title :inherit org-column :weight bold :underline t)
    (org-date :foreground ,cyan :underline t)
    (org-document-info :foreground ,var)
    (org-document-info-keyword :foreground ,var)
    (org-document-title :weight bold :foreground ,var)
    (org-done :foreground ,green)
    (org-ellipsis :foreground ,comt)
    (org-footnote :foreground ,other-blue)
    (org-formula :foreground ,pink)
    (org-headline-done :foreground ,comt :weight normal :strike-through t)
    (org-hide :foreground ,bg :background ,bg)
    (org-level-1 :inherit bold :foreground ,func)
    (org-level-2 :inherit bold :foreground ,func)
    (org-level-3 :weight normal :foreground ,func)
	(org-level-4 :weight normal :foreground ,func)
	(org-level-5 :weight normal :foreground ,func)
	(org-level-6 :weight normal :foreground ,func)
	(org-level-7 :weight normal :foreground ,func)
	(org-level-8 :weight normal :foreground ,fg)
	(org-link :foreground ,cyan :underline t)
	(org-priority :foreground ,cyan)
	(org-scheduled :foreground ,green)
	(org-scheduled-previously :foreground ,yellow)
	(org-scheduled-today :foreground ,green)
	(org-sexp-date :foreground ,fg)
	(org-special-keyword :foreground ,yellow)
	(org-table :foreground ,purple)
	(org-tag :foreground ,pink :weight bold :background ,bg)
	(org-todo :foreground ,orange :weight bold :background ,bg)
	(org-upcoming-deadline :foreground ,yellow)
	(org-warning :weight bold :foreground ,pink)
	;; outline
	(outline-1 :foreground ,pink)
	(outline-2 :foreground ,purple)
	(outline-3 :foreground ,green)
	(outline-4 :foreground ,yellow)
	(outline-5 :foreground ,cyan)
	(outline-6 :foreground ,orange)

	;; show-paren
	(show-paren-match-face :background unspecified :foreground ,cyan :weight bold)
	(show-paren-match :background unspecified :foreground nil :weight bold)
	(show-paren-match-expression :foreground ,none :background ,part)
	(show-paren-mismatch :inherit font-lock-warning-face)

	;; speedbar (and sr-speedbar)
	(speedbar-button-face :foreground ,green)
	(speedbar-file-face :foreground ,cyan)
	(speedbar-directory-face :foreground ,purple)
	(speedbar-tag-face :foreground ,yellow)
	(speedbar-selected-face :foreground ,pink)
	(speedbar-highlight-face :inherit match)
	(speedbar-separator-face :background ,bg :foreground ,fg :weight bold)

	;; term
	(term :foreground ,fg :background ,bg)
	(term-color-black :foreground ,bg :background ,comt)
	(term-color-blue :foreground ,purple :background ,purple)
	(term-color-cyan :foreground ,cyan :background ,cyan)
	(term-color-green :foreground ,green :background ,green)
	(term-color-magenta :foreground ,pink :background ,pink)
	(term-color-red :foreground ,red :background ,red)
	(term-color-white :foreground ,fg :background ,fg)
	(term-color-yellow :foreground ,yellow :background ,yellow)
	;; undo-tree
	(undo-tree-visualizer-current-face :foreground ,orange)
	(undo-tree-visualizer-default-face :foreground ,fg)
	(undo-tree-visualizer-register-face :foreground ,purple)
	(undo-tree-visualizer-unmodified-face :foreground ,fg)

	;; which-func
	(which-func :inherit font-lock-function-name-face)

	;; which-key
	(which-key-key-face :inherit font-lock-builtin-face)
	(which-key-command-description-face :inherit default)
	(which-key-separator-face :inherit font-lock-comment-delimiter-face)
	(which-key-local-map-description-face :foreground ,green)

	;; whitespace
	(whitespace-big-indent :background ,grey9 :foreground ,none)
	(whitespace-empty :background ,orange :foreground ,red)
	(whitespace-hspace :background ,bg :foreground ,comt)
	(whitespace-indentation :background ,orange :foreground ,red)
	(whitespace-line :background ,bg :foreground ,pink)
	(whitespace-newline :foreground ,comt)
	(whitespace-space :background ,bg :foreground ,comt)
	(whitespace-space-after-tab :background ,orange :foreground ,red)
	(whitespace-space-before-tab :background ,orange :foreground ,red)
	(whitespace-tab :background ,bg :foreground ,comt)
	(whitespace-trailing :background ,grey9 :foreground ,none)

	;; yasnippet
	(yas-field-highlight-face :background ,ragn)

	;; symbol overlay gourp
	(symbol-overlay-default-face :background ,ragn)

	;; web-mode
    (web-mode-builtin-face :inherit font-lock-builtin-face)
    (web-mode-comment-face :inherit font-lock-comment-face)
    (web-mode-constant-face :inherit font-lock-constant-face)
    (web-mode-css-property-name-face :inherit font-lock-constant-face)
    (web-mode-doctype-face :inherit font-lock-comment-face)
    (web-mode-function-name-face :inherit font-lock-function-name-face)
    (web-mode-html-attr-name-face :foreground ,purple)
    (web-mode-html-attr-value-face :foreground ,green)
    (web-mode-html-tag-face :foreground ,darkgrey :weight bold)
    ;; (web-mode-keyword-face :foreground ,pink)
    ;; (web-mode-string-face :foreground ,yellow)
    (web-mode-type-face :inherit font-lock-type-face)
    (web-mode-warning-face :inherit font-lock-warning-face)

	;; js2-mode
    ;; (js2-external-variable :foreground ,purple)
    ;; (js2-function-param :foreground ,cyan)
    ;; (js2-jsdoc-html-tag-delimiter :foreground ,yellow)
    ;; (js2-jsdoc-html-tag-name :foreground ,yellow)
    ;; (js2-jsdoc-value :foreground ,yellow)
    ;; (js2-private-function-call :foreground ,cyan)
    ;; (js2-private-member :foreground ,fg3)
    ;; js3-mode
    ;; (js3-error-face :underline ,orange)
    ;; (js3-external-variable-face :foreground ,fg)
    ;; (js3-function-param-face :foreground ,pink)
    ;; (js3-instance-member-face :foreground ,cyan)
    ;; (js3-jsdoc-tag-face :foreground ,pink)
    ;; (js3-warning-face :underline ,pink)
	)
  "Theme faces.")

(vwe-style--load 'vwe-style-grayscale vwe-style-grayscale--colors vwe-style-grayscale--faces)
(vwe-style--load-theme-file)

(provide-theme 'vwe-style-grayscale)
;;; vwe-style-grayscale-theme.el ends here
