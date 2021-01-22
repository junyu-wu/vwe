;;; vwe-markdown.el --- Markdown          -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Wu Junyu

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

;; npm i -g markdownlint-cli
;; apt install pandoc ; 预览

;;; Code:
;; ***************************************************************************
;; lib
;; ***************************************************************************
(defun vwe@md--make-html-header ()
  "Make html header."
  (format "%s"
		  "<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>
		     <style>
		       body {
		         box-sizing: border-box;
		         max-width: 740px;
		         width: 100%;
		         margin: 40px auto;
		         padding: 0 10px;
		       }
		     </style>
		   <script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>
		   <script>
		     document.addEventListener('DOMContentLoaded', () => {
		       document.body.classList.add('markdown-body');
		       document.querySelectorAll('pre[lang] > code').forEach((code) => {
		         code.classList.add(code.parentElement.lang);
		         hljs.highlightBlock(code);
		       });
		    });
		   </script>"))

;; ***************************************************************************
;; config
;; ***************************************************************************

;;
;; `markdown-mode'
;;
(vwe@lib--package 'markdown-mode
				  (progn
					(push '("\\.md\\'" . gfm-mode) auto-mode-alist)
					(add-hook 'markdown-mode-hook #'auto-fill-mode))
				  (progn
					;;
					;; `markdown-toc'
					;;
					(vwe@lib--package 'markdown-toc)
					(define-key markdown-mode-map (kbd "M-C-k") #'vwe@lib--buffer-kill-current))
				  (setq markdown-enable-wiki-links t
						markdown-italic-underscore t
						markdown-asymmetric-header t
						markdown-make-gfm-checkboxes-buttons t
						markdown-gfm-uppercase-checkbox t
						markdown-fontify-code-blocks-natively t
						markdown-enable-math t
						markdown-command "pandoc"
						markdown-gfm-additional-languages "Mermaid" ;; 画图 https://mermaid-js.github.io/mermaid/
						markdown-content-type "application/xhtml+xml"
						markdown-css-paths
						'("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
						  "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css")
						markdown-xhtml-header-content "vwe@md--make-html-header"))

(provide 'vwe-markdown)
;;; vwe-markdown.el ends here
