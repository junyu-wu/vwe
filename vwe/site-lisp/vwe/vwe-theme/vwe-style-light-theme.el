;;; vwe-style-light-theme.el --- Dark theme         -binding: t; -*-

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

(deftheme vwe-style-light
  "Vwe style theme.")

(defvar vwe-style-light--colors
  '((none nil nil nil)
	(bg "#fdf6e3" "#fdf6e3" "white")
	(fg "#556b72" "#556b72" "black")
    (current "#ffeaaa" "#ffeaaa" "brightblack")
    (comt "#6272a4" "#5f5faf" "blue")
	(comt2 "#595959" "#595959" "595959")
    (cyan    "#8be9fd" "#87d7ff" "brightcyan")
    (green   "#50fa7b" "#5fff87" "green")
    (orange  "#ffb86c" "#ffaf5f" "brightred")
    (pink    "#ff79c6" "#ff87d7" "magenta")
    (purple  "#bd93f9" "#af87ff" "brightmagenta")
    (red     "#ff5555" "#ff8787" "red")
    (yellow  "#f1fa8c" "#ffff87" "yellow")
	(moccasin "#ffe4b5" "#ffe4b5" "yellow")
	(deepskyblue "#00688B" "#00688B" "#00688B")
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
	(paleturquoise "#afeeee" "#afeeee" "#afeeee")

    (bg2             "#FCF8ED" "#FCF8ED" "brightblack")
    (bg3             "#FCF7E8" "#FCF7E8" "brightblack")
    (bg4             "#F2E6CE" "#F2E6CE" "brightblack")
    (fg2             "#E1DBCD" "#E1DBCD" "brightblack")
    (fg3             "#D6D6D6" "#D6D6D6" "brightblack")
    (fg4             "#96A7A9" "#96A7A9" "brightblack")
    (other-blue      "#0189cc" "#0087ff" "brightblue"))
  "Theme colors.")

(defvar vwe-style-light--faces
  '(;; default
    (cursor :background ,red :weight bold)
    (default :background ,bg :foreground ,grey9)
	(error :foreground ,red)
	(fringe :background ,bg :foreground ,fg4)
    (header-line :background ,bg)
    (highlight :foreground ,none :background ,paleturquoise)
    (line-number :slant italic :foreground ,darkgrey :background ,bg)
	(line-number-current-line :slant italic :foreground ,orangered :background ,darkturquoise)
	(link :foreground ,deepskyblue :underline t)
	(link-visited :foreground ,deepskyblue :underline t)
    (match :background ,ecyan :foreground ,red)
	(minibuffer-prompt :weight bold :foreground ,pink)
	(mode-line :background ,current :box ,current :inverse-video nil :foreground ,fg3)
	(mode-line-inactive :foreground ,comt :background ,bg :box ,bg)
	(region :background ,orangered :foreground ,none :box (:color ,orangered :line-width 1) ;; :inverse-video t
			)
	;; (secondary-selection :background ,bg3)
    (shadow :foreground ,comt)
    (success :foreground ,green)
	(tab-bar :foreground ,purple :background ,current :inherit variable-pitch)
    (tab-bar-tab :foreground ,pink :background ,bg :box (:line-width 2 :color ,bg :style nil))
    (tab-bar-tab-inactive :foreground ,purple :background ,bg2 :box (:line-width 2 :color ,bg2 :style nil))
    (tab-line :foreground ,purple :background ,current :height 0.9 :inherit variable-pitch)
    (tab-line-tab :foreground ,pink :background ,bg :box (:line-width 2 :color ,bg :style nil))
    (tab-line-tab-inactive :foreground ,purple :background ,bg2 :box (:line-width 2 :color ,bg2 :style nil))
    (tab-line-tab-current :inherit tab-line-tab)
    (tab-line-close-highlight :foreground ,red)
	(trailing-whitespace :background ,trailing)
	;; (vertical-border :foreground ,bg3 :background ,bg3)
    (warning :foreground ,orangered)

	;; font lock
	(font-lock-builtin-face :foreground ,pink)
    (font-lock-comment-delimiter-face :foreground ,comt2)
    (font-lock-comment-face :foreground ,comt2)
    (font-lock-constant-face :foreground ,orangered)
    (font-lock-doc-face :foreground ,darkgoldenrod)
    (font-lock-function-name-face :foreground ,darkturquoise :weight bold)
    (font-lock-keyword-face :foreground ,orangered :weight bold)
    (font-lock-negation-char-face :foreground ,other-blue)
    (font-lock-preprocessor-face :foreground ,orange)
    (font-lock-regexp-grouping-backslash :foreground ,other-blue)
    (font-lock-regexp-grouping-construct :foreground ,purple)
    (font-lock-string-face :foreground ,forestgreen)
    (font-lock-type-face :foreground ,purple)
    (font-lock-variable-name-face :foreground ,purple :weight bold)
    (font-lock-warning-face :foreground ,orangered :background ,bg2)

    (font-lock-reference-face :foreground ,cyan)

	;; company
	(company-echo-common :foreground ,bg :background ,fg)
    (company-preview :background ,current :foreground ,other-blue)
    (company-preview-common :inherit company-preview :foreground ,pink)
    (company-preview-search :inherit company-preview :foreground ,green)
    (company-scrollbar-bg :background ,comt)
    (company-scrollbar-fg :foreground ,other-blue)
    (company-tooltip :foreground ,fg :background ,current)
    (company-tooltip-annotation :foreground ,cyan)
    ;;(company-tooltip-annotation-selection :inherit company-tooltip-annotation)
    (company-tooltip-common :foreground ,pink :weight bold)
    ;;(company-tooltip-common-selection :inherit company-tooltip-common)
    (company-tooltip-mouse :background ,bg)
    (company-tooltip-search :foreground ,green :underline t)
    (company-tooltip-search-selection :background ,green :foreground ,bg)
    (company-tooltip-selection :background ,orange)

	;; completions (minibuffer.el)
    (completions-annotations :inherit font-lock-comment-face)
    (completions-common-part :foreground ,green)
    (completions-first-difference :foreground ,pink :weight bold)

	;; diff-hl
    (diff-hl-change :foreground ,orange :background ,orange)
    (diff-hl-delete :foreground ,red :background ,red)
    (diff-hl-insert :foreground ,green :background ,green)

    ;; dired
    (dired-directory :foreground ,green :weight normal)
    (dired-flagged :foreground ,pink)
    (dired-header :foreground ,fg3 :background ,bg)
    (dired-ignored :inherit shadow)
    (dired-mark :foreground ,fg :weight bold)
    (dired-marked :foreground ,orange :weight bold)
    (dired-perm-write :foreground ,fg3 :underline t)
    (dired-symlink :foreground ,yellow :weight normal :slant italic)
    (dired-warning :foreground ,orange :underline t)
    (diredp-compressed-file-name :foreground ,fg3)
    (diredp-compressed-file-suffix :foreground ,fg4)
    (diredp-date-time :foreground ,fg)
    (diredp-deletion-file-name :foreground ,pink :background ,current)
    (diredp-deletion :foreground ,pink :weight bold)
    (diredp-dir-heading :foreground ,fg2 :background ,bg4)
    (diredp-dir-name :inherit dired-directory)
    (diredp-dir-priv :inherit dired-directory)
    (diredp-executable-tag :foreground ,orange)
    (diredp-file-name :foreground ,fg)
    (diredp-file-suffix :foreground ,fg4)
    (diredp-flag-mark-line :foreground ,fg2 :slant italic :background ,current)
    (diredp-flag-mark :foreground ,fg2 :weight bold :background ,current)
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
    (gnus-group-mail-low :foreground ,orangered :weight bold)
    (gnus-group-mail-low-empty :foreground ,orangered :weight normal)
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
    (highlight-indentation-face :background ,bg2)

	;; ivy
    (ivy-current-match :weight bold :foreground ,orange :background ,forestgreen)
	;; Highlights the background of the match.
    (ivy-minibuffer-match-face-1 :background ,current)
    ;; Highlights the first matched group.
    (ivy-minibuffer-match-face-2 :background ,ecyan :foreground ,orangered)
    ;; Highlights the second matched group.
    (ivy-minibuffer-match-face-3 :background ,orange :foreground ,bg)
    ;; Highlights the third matched group.
    (ivy-minibuffer-match-face-4 :background ,pink :foreground ,bg)
	(ivy-minibuffer-match-highlight :background ,green :foreground ,none)
    (ivy-confirm-face :foreground ,orange)
    (ivy-match-required-face :foreground ,red)
    (ivy-subdir :foreground ,orangered)
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
    (message-header-subject :foreground ,orangered)
    (message-header-newsgroups :foreground ,purple)
    (message-header-other :foreground ,purple)
    (message-header-name :foreground ,green)
    (message-header-xheader :foreground ,cyan)
    (message-separator :foreground ,cyan :slant italic)
    (message-cited-text :foreground ,purple)
    (message-cited-text-1 :foreground ,purple)
    (message-cited-text-2 :foreground ,orangered)
    (message-cited-text-3 :foreground ,comt)
    (message-cited-text-4 :foreground ,fg2)
    (message-mml :foreground ,green :weight normal)

    ;; org
    (org-agenda-date :foreground ,cyan :underline nil)
    (org-agenda-dimmed-todo-face :foreground ,comt)
    (org-agenda-done :foreground ,green)
    (org-agenda-structure :foreground ,purple)
    (org-block :foreground ,orange)
    (org-code :foreground ,yellow)
    (org-column :background ,bg4)
    (org-column-title :inherit org-column :weight bold :underline t)
    (org-date :foreground ,cyan :underline t)
    (org-document-info :foreground ,other-blue)
    (org-document-info-keyword :foreground ,comt)
    (org-document-title :weight bold :foreground ,orange)
    (org-done :foreground ,green)
    (org-ellipsis :foreground ,comt)
    (org-footnote :foreground ,other-blue)
    (org-formula :foreground ,pink)
    (org-headline-done :foreground ,comt :weight normal :strike-through t)
    (org-hide :foreground ,bg :background ,bg)
    (org-level-1 :inherit bold :foreground ,pink)
    (org-level-2 :inherit bold :foreground ,purple)
    (org-level-3 :weight normal :foreground ,green)
	(org-level-4 :weight normal :foreground ,darkorange )
	(org-level-5 :weight normal :foreground ,ecyan)
	(org-level-6 :weight normal :foreground ,orange)
	(org-level-7 :weight normal :foreground ,other-blue)
	(org-level-8 :weight normal :foreground ,fg)
	(org-link :foreground ,cyan :underline t)
	(org-priority :foreground ,cyan)
	(org-scheduled :foreground ,green)
	(org-scheduled-previously :foreground ,orangered)
	(org-scheduled-today :foreground ,green)
	(org-sexp-date :foreground ,fg4)
	(org-special-keyword :foreground ,orangered)
	(org-table :foreground ,purple)
	(org-tag :foreground ,pink :weight bold :background ,bg2)
	(org-todo :foreground ,orange :weight bold :background ,bg2)
	(org-upcoming-deadline :foreground ,orangered)
	(org-warning :weight bold :foreground ,pink)
	;; outline
	(outline-1 :foreground ,pink)
	(outline-2 :foreground ,purple)
	(outline-3 :foreground ,green)
	(outline-4 :foreground ,orangered)
	(outline-5 :foreground ,cyan)
	(outline-6 :foreground ,orange)

	;; show-paren
	(show-paren-match-face :background unspecified :foreground ,cyan :weight bold)
	(show-paren-match :background unspecified :foreground nil :weight bold)
	(show-paren-match-expression :foreground ,none :background ,parent)
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
	(undo-tree-visualizer-default-face :foreground ,fg2)
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
	(whitespace-hspace :background ,bg3 :foreground ,comt)
	(whitespace-indentation :background ,orange :foreground ,red)
	(whitespace-line :background ,bg :foreground ,pink)
	(whitespace-newline :foreground ,comt)
	(whitespace-space :background ,bg :foreground ,comt)
	(whitespace-space-after-tab :background ,orange :foreground ,red)
	(whitespace-space-before-tab :background ,orange :foreground ,red)
	(whitespace-tab :background ,bg2 :foreground ,comt)
	(whitespace-trailing :background ,grey9 :foreground ,none)

	;; yasnippet
	(yas-field-highlight-face :background ,trailing)

	;; symbol overlay gourp
	(symbol-overlay-default-face :background ,trailing)

	;; web-mode
    (web-mode-builtin-face :inherit font-lock-builtin-face)
    (web-mode-comment-face :inherit font-lock-comment-face)
    (web-mode-constant-face :inherit font-lock-constant-face)
    (web-mode-css-property-name-face :inherit font-lock-constant-face)
    (web-mode-doctype-face :inherit font-lock-comment-face)
    (web-mode-function-name-face :inherit font-lock-function-name-face)
    (web-mode-html-attr-name-face :foreground ,purple)
    (web-mode-html-attr-value-face :foreground ,green)
    (web-mode-html-tag-face :foreground ,pink :weight bold)
    (web-mode-keyword-face :foreground ,pink)
    (web-mode-string-face :foreground ,yellow)
    (web-mode-type-face :inherit font-lock-type-face)
    (web-mode-warning-face :inherit font-lock-warning-face)

	;; js2-mode
    (js2-external-variable :foreground ,purple)
    (js2-function-param :foreground ,cyan)
    (js2-jsdoc-html-tag-delimiter :foreground ,yellow)
    (js2-jsdoc-html-tag-name :foreground ,yellow)
    (js2-jsdoc-value :foreground ,yellow)
    (js2-private-function-call :foreground ,cyan)
    (js2-private-member :foreground ,fg3)
    ;; js3-mode
    (js3-error-face :underline ,orange)
    (js3-external-variable-face :foreground ,fg)
    (js3-function-param-face :foreground ,pink)
    (js3-instance-member-face :foreground ,cyan)
    (js3-jsdoc-tag-face :foreground ,pink)
    (js3-warning-face :underline ,pink)
	)
  "Theme faces.")

(vwe-style--load 'vwe-style-light vwe-style-light--colors vwe-style-light--faces)
(vwe-style--load-theme-file)

(provide-theme 'vwe-style-light)
;;; vwe-style-light-theme.el ends here
