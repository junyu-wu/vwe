;;; vwe-web.el --- Web Programming      -*- lexical-binding: t; -*-

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

;; npm install stylelint -g ;; css flycheck
;; npm install eslint -g    ;; js flycheck

;; npm install -g vscode-html-languageserver-bin ;; html lsp : server `html-ls'
;; npm install -g vscode-css-languageserver-bin  ;; css lsp : install server `css-ls'
;; npm i -g javascript-typescript-langserver     ;; js lsp : install server `js-ts'
;; npm i -g vscode-json-languageserver           ;; json lsp : install sev `json-ls'

;;; Code:
;; ***************************************************************************
;; lib
;; ***************************************************************************
(defun vwe@web--open-to-brower (url &rest _)
  "Open URL to brower."
  (interactive
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))

  (unless url (setq url (buffer-file-name)))
  (funcall browse-url-secondary-browser-function url))

;; ***************************************************************************
;; config
;; ***************************************************************************

;; ==============================
;; css
;; ==============================

;;
;; `css-mode'
;;
(vwe@lib--package 'css-mode
				  (add-hook 'css-mode-hook
							(lambda()
							  (set (make-local-variable 'company-backends)
										   '(company-css company-files company-yasnippet company-capf))))
				  nil
				  (setq css-indent-offset 2
						flycheck-stylelintrc (vwe@lib--path-vwe-etc "web/.stylelintrc" t)
						flycheck-css-stylelint-executable "stylelint"))

;; ==============================
;; javascript
;; ==============================

;;
;; `js2-mode'
;;
(vwe@lib--package 'js2-mode
				  nil nil
				  (setq indent-tabs-mode nil
						js2-basic-offset 2
						js-indent-level 2
						js2-global-externs '("module" "require" "assert"
											 "setInterval" "console" "__dirname__")
						flycheck-javascript-eslint-executable "eslint"))

;; ==============================
;; json
;; ==============================

;;
;; `json-mode'
;;
(vwe@lib--package 'json-mode)

;; ==============================
;; web misc
;; ==============================

;;
;; `web-mode'
;;
(vwe@lib--package 'web-mode
				  (progn
					(push '("\\.html\\'" . web-mode) auto-mode-alist))
				  ;;
				  ;; `company-web'
				  ;;
				  (vwe@lib--package 'company-web
									(add-hook 'web-mode-hook
											  (lambda ()
												(set (make-local-variable 'company-backends) '(company-web-html))
												(company-mode t))))
				  (setq web-mode-markup-indent-offset 2
						web-mode-css-indent-offset 2
						web-mode-code-indent-offset 2

						web-mode-style-padding 1
						web-mode-script-padding 1
						web-mode-block-padding 0

						web-mode-enable-auto-pairing nil

						web-mode-enable-css-colorization t
						web-mode-enable-block-face t
						web-mode-enable-part-face t
						web-mode-enable-comment-interpolation t
						web-mode-enable-heredoc-fontification t

						web-mode-enable-current-element-highlight t
						web-mode-enable-current-column-highlight t))

;;
;; `skewer-mode'
;;
(vwe@lib--package 'skewer-mode
				  (progn
					(add-hook 'js2-mode-hook #'skewer-mode)
					(add-hook 'css-mode-hook #'skewer-css-mode)
					(add-hook 'html-mode-hook #'skewer-html-mode)
					(add-hook 'web-mode-hook #'skewer-html-mode)))

(provide 'vwe-web)
;;; vwe-web.el ends here
