;;; vwe-lsp.el --- Language Server Protocol -*- lexical-binding: t; -*-

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

;; pip3 install json-rpc

;;; Code:
;; ***************************************************************************
;; lib
;; ***************************************************************************
(defun vwe@lsp--run ()
  "Run lsp deferred."
  (interactive)
  (lsp-deferred))

;; ***************************************************************************
;; config
;; ***************************************************************************
(use-package lsp-mode
  :hook
  (;; (prog-mode . (lambda ()
   ;; 				  (unless (derived-mode-p 'emacs-lisp-mode
   ;; 										  'lisp-mode
   ;; 										  'asm-mode
   ;; 										  'nasm-mode
   ;; 										  'sh-mode)
   ;; 					(lsp-deferred))))
   (lsp-mode . lsp-lens-mode)
   (lsp-mode . (lambda ()
				 (lsp-enable-which-key-integration)
				 (add-hook 'before-save-hook
						   #'lsp-format-buffer t t)
				 (add-hook 'before-save-hook
						   #'lsp-organize-imports t t))))
  :bind
  (:map lsp-mode-map
		("C-M-i" . lsp-describe-thing-at-point)
		([remap xref-find-definitions] . lsp-find-definition)
		([remap xref-find-references] . lsp-find-references))
  :init
  (setq read-process-output-max (* 1024 1024)
		lsp-keymap-prefix "C-c l"
		lsp-keep-workspace-alive nil
		lsp-prefer-capf t
		lsp-signature-auto-activate nil
		lsp-modeline-code-actions-enable nil
		lsp-modeline-diagnostics-enable nil

		lsp-enable-file-watchers nil
		lsp-enable-file-watchers nil
		lsp-enable-folding nil
		lsp-enable-semantic-highlighting nil
		lsp-enable-symbol-highlighting nil
		lsp-enable-text-document-color nil

		lsp-enable-indentation nil
		lsp-enable-on-type-formatting nil
		lsp-session-file (vwe@lib--path-cache "lsp/.lspsession-v1" t)))

(use-package lsp-ui
  :after
  lsp-mode
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init
  (setq lsp-ui-doc-enable t
		lsp-ui-doc-use-webkit nil
		lsp-ui-doc-delay 0.5
		lsp-ui-doc-include-signature t
		lsp-ui-doc-position 'at-point
		lsp-ui-doc-border (face-foreground 'default)
		lsp-eldoc-enable-hover nil ; Disable eldoc displays in minibuffer

		lsp-ui-sideline-enable t
		lsp-ui-sideline-show-hover nil
		lsp-ui-sideline-show-diagnostics t
		lsp-ui-sideline-ignore-duplicate t

		lsp-ui-peek-enable t
		lsp-ui-peek-show-directory t
		lsp-ui-peek-always-show t

		lsp-ui-imenu-enable t
		lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
							  ,(face-foreground 'font-lock-string-face)
							  ,(face-foreground 'font-lock-constant-face)
							  ,(face-foreground 'font-lock-variable-name-face)))

  :config
  (lsp-ui-peek-find-workspace-symbol "pattern 0")
  ;; If the server supports custom cross references
  (lsp-ui-peek-find-custom 'base "$cquery/base")

  (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

  (add-hook 'after-load-theme-hook
			(lambda ()
			  (setq lsp-ui-doc-border (face-foreground 'default))
			  (set-face-background 'lsp-ui-doc-background
								   (face-background 'tooltip)))))


(provide 'vwe-lsp)
;;; vwe-lsp.el ends here
