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
;; apt install npm
;; npm install nrm  ;; 更换源
;; npm install stylelint -g
;; npm install --save-dev stylelint stylelint-config-standard
;; npm install stylelint-config-recommended -g
;; npm install prettier -g
;; npm install live-server -g  ;; 目录下文件修改，自动加载
;; npm install typescript -g
;; npm install eslint -g
;; npm install tsun -g
;; npm i -g javascript-typescript-langserver
;; npm i -g vscode-json-languageserver
;; apt install tidy ;; html format
;;; Code:
;; ***************************************************************************
;; lib
;; ***************************************************************************

;; ***************************************************************************
;; config
;; ***************************************************************************
(use-package web-mode
  :mode
  ("\\.html\\'"
   "\\.html?\\'"
   "\\.vue\\'")
  :hook
  (web-mode . emmet-mode)
  :init
  (setq web-mode-enable-current-element-highlight t
		web-mode-enable-current-column-highlight t
		web-mode-markup-indent-offset 2
		web-mode-code-indent-offset 2
		web-mode-css-indent-offset 2
		web-mode-auto-close-style 1
		web-mode-enable-auto-closing t
		web-mode-enable-css-colorization t
		web-mode-content-types-alist '(("vue" . "\\.vue\\'"))
		web-mode-style-padding 1
		web-mode-script-padding 1
		web-mode-block-padding 0
		web-mode-comment-style 2
		web-mode-enable-auto-pairing t
		web-mode-enable-block-face t
		web-mode-enable-part-face t
		web-mode-enable-comment-interpolation t
		web-mode-enable-heredoc-fontification t)
  :config
  (use-package company-web
	:after
	(web-mode)
	:config
	(setq company-tooltip-limit 20
		  company-tooltip-align-annotations 't
		  company-begin-commands '(self-insert-command))))

;; 根据简单命令快速生成代码
(use-package emmet-mode
  :diminish
  (emmet-mode . nil)
  :init
  (setq emmet-move-cursor-between-quotes t
		emmet-expand-jsx-className? t
		emmet-self-closing-tag-style " /")
  :config
  (add-hook 'emmet-mode-hook
			(lambda()
			  (setq emmet-indent-after-insert t
					emmet-indentation 2))))

;;; CSS
(use-package css-mode
  :mode "\\.css\\'"
  :hook
  (css-mode . emmet-mode)
  :init
  (setq css-indent-offset 2
		flycheck-stylelintrc (vwe@lib--path-vwe-etc "web/.stylelintrc" t)
		flycheck-css-stylelint-executable "stylelint")
  :config
  (add-hook 'css-mode-hook
			(lambda()
			  (add-to-list (make-local-variable 'company-backends)
						   '(company-css company-files
										 company-yasnippet
										 company-capf)))))

;;; JavaScript
(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)
		 ("\\.json\\'" . javascript-mode))
  :hook
  (js2-mode . emmet-mode)
  :defines
  (flycheck-javascript-eslint-executable)
  :init
  (setq indent-tabs-mode nil
		js2-basic-offset 2
		js-indent-level 2
		js2-global-externs '("module" "require" "assert"
							 "setInterval" "console" "__dirname__")
		flycheck-javascript-eslint-executable "eslint"))

;; 格式化js
(use-package prettier-js
  :diminish
  (prettier-js-mode)
  :hook
  ((js2-mode . prettier-js-mode)
   (typescript-mode . prettier-js-mode)
   (css-mode . prettier-js-mode)
   (web-mode . prettier-js-mode))
  :config
  (setq prettier-js-command "prettier"
		prettier-js-args '("--trailing-comma" "all";;"es5"
						   "--bracket-spacing" "false")))

;;; Typescript
(use-package typescript-mode
  :config
  ;; TypeScript REPL
  (use-package ts-comint))

;; TypeScript交互式开发环境
(use-package tide
  :diminish
  (tide-mode . nil)
  :hook
  ((typescript-mode . tide-setup)
   (typescript-mode . tide-hl-identifier-mode)
   (before-save-hook . tide-format-before-save))
  :init
  (setq tide-completion-enable-autoimport-suggestions t))

;;; JSON
(use-package json-mode)

(provide 'vwe-web)
;;; vwe-web.el ends here
