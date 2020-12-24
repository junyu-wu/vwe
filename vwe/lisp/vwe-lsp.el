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
  (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'asm-mode 'nasm-mode 'sh-mode)
	(lsp-deferred)))

;; ***************************************************************************
;; config
;; ***************************************************************************
(use-package lsp-mode
  :bind
  (:map lsp-mode-map
		("C-M-i" . lsp-describe-thing-at-point)
		([remap xref-find-definitions] . lsp-find-definition)
		([remap xref-find-references] . lsp-find-references))
  :init
  (setq lsp-keymap-prefix "C-c l"
		lsp-completion-provider :capf
		lsp-idle-delay 0.500
		lsp-enable-file-watchers nil
		lsp-log-io nil
		lsp-keep-workspace-alive nil
		lsp-eldoc-enable-hover nil
		lsp-session-file (vwe@lib--path-cache "lsp/.lspsession-v1" t))
  :config
  (use-package lsp-ui
	:after
	(lsp-mode)
	:init
	(setq lsp-ui-doc-enable nil
		  lsp-ui-doc-use-webkit nil
		  lsp-ui-doc-delay 0.5
		  lsp-ui-doc-include-signature t
		  lsp-ui-doc-position 'at-point


		  lsp-ui-sideline-enable t
		  lsp-ui-sideline-show-hover nil
		  lsp-ui-sideline-show-diagnostics t
		  lsp-ui-sideline-ignore-duplicate t

		  lsp-ui-peek-enable t
		  lsp-ui-peek-show-directory t
		  lsp-ui-peek-always-show t

		  lsp-ui-imenu-enable t))
  (use-package lsp-ivy
	:after
	(ivy-mode lsp-mode))
  (use-package lsp-treemacs
	:after
	(treemacs lsp-mode)
	:config
	(lsp-treemacs-sync-mode 1)))

(provide 'vwe-lsp)
;;; vwe-lsp.el ends here
