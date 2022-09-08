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
;; lib
;; ***************************************************************************
(defun vwe@java--init ()
  "Java init."
  (interactive)
  (lsp-deferred)
  (lsp-lens-mode 1)
  (lsp-java-lens-mode 1)
  (lsp-java-boot-lens-mode 1))

;; ***************************************************************************
;; config
;; ***************************************************************************

;;
;; `java-mode'
;;
(vwe@lib--pkg java-mode
  :init ((add-hook 'java-mode-hook
				   (lambda ()
					 (vwe@lib--server-lsp
					  vwe@custom--lsp
					  :lsp (progn
							 (add-hook 'before-save-hook
									   #'lsp-format-buffer nil t)))
					 (lsp-deferred)
					 (lsp-lens-mode 1)))

		 ;;
		 ;; `lsp-java'
		 ;;
		 (vwe@lib--pkg lsp-java
		   :init ((add-hook 'java-mode-hook (lambda ()
											  (lsp-java-lens-mode 1)
											  (lsp-java-boot-lens-mode 1)))

				  (setq lsp-java-server-install-dir (vwe@lib--path-cache "lsp/eclipse.jdt.ls")
						lsp-java-workspace-dir (vwe@lib--path-cache "java/workspace")
						lsp-java-workspace-cache-dir (vwe@lib--path-cache "java/workspace/.cache")
						lsp-java-java-path "java"))
		   :config (;;
					;; `dap-java'
					;;
					(vwe@lib--pkg dap-java
					  :variable ((setq dap-java-test-runner
									   (concat (vwe@lib--path-cache "lsp/eclipse.jdt.ls")
											   "/test-runner/junit-platform-console-standalone.jar")))
					  :undefer t
					  :buildin t))
		   :variable ((setq lsp-java-import-gradle-enabled t
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
							lsp-java-inhibit-message t))))
  :buildin t)

(provide 'vwe-java)
;;; vwe-java.el ends here
