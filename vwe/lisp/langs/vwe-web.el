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
;; npm install --save-dev --save-exact prettier

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

(defun vwe@web--wrap-html-tag (tag)
  "Add a TAG to beginning and ending of current word or text selection."
  (interactive "sEnter tag name: ")
  (let (p1 p2)
    (if (use-region-p)
        (progn
          (setq p1 (region-beginning) )
          (setq p2 (region-end) )
          )
      (let ((bds (bounds-of-thing-at-point 'symbol)))
        (setq p1 (car bds) )
        (setq p2 (cdr bds) ) ) )

    (goto-char p2)
    (insert "</" tag ">")
    (goto-char p1)
    (insert "<" tag ">")))

;; ***************************************************************************
;; config
;; ***************************************************************************

;; ==============================
;; css
;; ==============================

;;
;; `css-mode'
;;
(vwe@lib--pkg css-mode
  :config ((with-eval-after-load 'company
			 (add-hook 'css-mode-hook
					   (lambda ()
						 (vwe@pkg--company-make-mode-local-backends
						  'company-css)))))
  :variable ((setq css-indent-offset 2
				   flycheck-stylelintrc (vwe@lib--path-vwe-etc "web/.stylelintrc" t)
				   flycheck-css-stylelint-executable "stylelint")))

;; ==============================
;; javascript
;; ==============================

;;
;; `js2-mode'
;;
(vwe@lib--pkg js2-mode
  :init ((push '("\\.js\\'" . js2-mode) auto-mode-alist))
  :variable ((setq indent-tabs-mode nil
				   js2-basic-offset 2
				   js-indent-level 2
				   js2-global-externs '("module" "require" "assert"
										"setInterval" "console" "__dirname__")
				   flycheck-javascript-eslint-executable "eslint")))

;; ==============================
;; json
;; ==============================

;;
;; `json-mode'
;;
(vwe@lib--pkg json-mode)

;; ==============================
;; web misc
;; ==============================

;;
;; `web-mode'
;;
(vwe@lib--pkg web-mode
  :init ((push '("\\.html\\'" . web-mode) auto-mode-alist)
		 (push '("\\.vue\\'" . web-mode) auto-mode-alist))
  :config ((with-eval-after-load 'autoinsert
			 (define-auto-insert
			   "\\.html$"
			   ["default-html.html"
				(lambda ()
				  (when (fboundp 'yas-expand-snippet)
					(yas-expand-snippet (buffer-string)
										(point-min)
										(point-max))))]))

		   ;;
		   ;; `company-web'
		   ;;
		   (vwe@lib--pkg company-web
			 :config ((with-eval-after-load 'company
						(add-hook 'web-mode-hook
								  (lambda ()
									(vwe@pkg--company-make-mode-local-backends
									 'company-web-html)))))))
  :variable ((setq web-mode-markup-indent-offset 2
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
				   web-mode-enable-current-column-highlight t)))

;;
;; `skewer-mode'
;;
(vwe@lib--pkg skewer-mode
  :init ((add-hook 'js2-mode-hook #'skewer-mode)
		 (add-hook 'css-mode-hook #'skewer-css-mode)
		 (add-hook 'html-mode-hook #'skewer-html-mode)
		 (add-hook 'web-mode-hook #'skewer-html-mode)))

(provide 'vwe-web)
;;; vwe-web.el ends here
