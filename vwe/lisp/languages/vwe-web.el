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

;; ***************************************************************************
;; config
;; ***************************************************************************

;; ==============================
;; css
;; ==============================
(use-package css-mode
  :ensure nil
  :mode "\\.css\\'"
  :init
  (setq css-indent-offset 2
		flycheck-stylelintrc (vwe@lib--path-vwe-etc "web/.stylelintrc" t)
		flycheck-css-stylelint-executable "stylelint")
  :config
  (add-hook 'css-mode-hook
			(lambda()
			  (add-to-list (make-local-variable 'company-backends)
						   '(company-css company-files company-yasnippet company-capf)))))

;; ==============================
;; javascript
;; ==============================
(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :defines
  (flycheck-javascript-eslint-executable)
  :init
  (setq indent-tabs-mode nil
		js2-basic-offset 2
		js-indent-level 2
		js2-global-externs '("module" "require" "assert"
							 "setInterval" "console" "__dirname__")
		flycheck-javascript-eslint-executable "eslint"))

;; ==============================
;; json
;; ==============================
(use-package json-mode
  :mode ("\\.json\\'" . json-mode))

;; ==============================
;; web misc
;; ==============================
(use-package web-mode
  :mode
  ("\\.html\\'" "\\.html?\\'" "\\.vue\\'" "\\.as[cp]x\\'")
  :init
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
		web-mode-enable-current-column-highlight t)
  :config
  (use-package company-web
	:after
	(web-mode)
	:init
	(add-hook 'web-mode-hook
			  (lambda ()
				(add-to-list (make-local-variable 'company-backends) '(company-web-html))
				(company-mode t)))))

(use-package skewer-mode
  :hook
  ((js2-mode-hook  . skewer-mode)
   (css-mode-hook  . skewer-css-mode)
   (html-mode-hook . skewer-html-mode)
   (web-mode-hook  . skewer-html-mode)))

(provide 'vwe-web)
;;; vwe-web.el ends here
