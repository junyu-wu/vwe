;;; vwe-java.el --- Java Develop      -*- lexical-binding: t; -*-

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

;;; Code:
;; ***************************************************************************
;; config
;; ***************************************************************************
(use-package java-mode
  :ensure nil
  :hook
  ((java-mode . lsp)
   (java-mode . lsp-lens-mode)
   (java-mode . lsp-java-boot-lens-mode)))

(use-package lsp-java-boot
  :ensure nil
  :diminish
  (lsp-java-boot-lens-mode . nil)
  :after
  java-mode
  :config
  (lsp-lens-mode t)
  (lsp-java-lens-mode t))

(use-package lsp-java
  :after
  lsp-mode
  :init
  (setq lsp-java-server-install-dir (vwe@lib--path-cache "lsp/eclipse.jdt.ls")
		lsp-java-workspace-dir (vwe@lib--path-cache "java/workspce")
		lsp-java-java-path "java"
		lsp-java-import-gradle-enabled t
		lsp-java-import-maven-enabled t
		lsp-java-maven-download-sources t
		lsp-java-references-code-lens-enabled t
		lsp-java-signature-help-enabled t
		lsp-java-implementations-code-lens-enabled t
		lsp-java-format-enabled t
		lsp-java-save-actions-organize-imports t
		lsp-java-autobuild-enabled t
		lsp-java-completion-enabled t
		lsp-java-completion-overwrite nil
		lsp-java-completion-guess-method-arguments t
		lsp-java-format-comments-enabled t
		lsp-java-code-generation-use-blocks t
		lsp-java-code-generation-generate-comments t
		lsp-java-code-generation-to-string-limit-elements 0
		lsp-java-inhibit-message t)
  :config
  (use-package dap-java
	:ensure nil
	:after lsp-java))

(provide 'vwe-java)
;;; vwe-java.el ends here
