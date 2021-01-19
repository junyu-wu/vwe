;;; vwe-prog.el --- Program              -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Wu Junyu

;; Author: Wu Junyu <vistar_w@hotmail.com>
;; Keywords: languages

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
(defvar vwe@prog--language-and-mode-alist
  '(("emacs lisp"       . emacs-lisp-mode)
	("typescript"       . typescript-mode)
	("javascript"       . js-mode)
	("javascript2"      . js2-mode)
	("commint"          . comint-mode)
	("eww"              . eww-mode)
	("web"              . web-mode)
	("fundamental"      . fundamental-mode)
	("special"          . special-mode)
	("text"             . text-mode)
	("ruby"             . ruby-mode)
	("shell script"     . sh-mode)
	("shell"            . shell-mode)
	("makefile"         . makefile-mode)
	("json"             . json-mode)
	("yaml"             . yaml-mode)
	("elisp byte code"  . elisp-byte-code-mode)
	("archive"          . archive-mode)
	("java"             . java-mode)
	("golang"           . go-mode)
	("perl"             . perl-mode)
	("php"              . php-mode)
	("python"           . python-mode)
	("rust"             . rust-mode)
	("scala"            . scala-mode)
	("scheme"           . scheme-mode)
	("c"                . c-mode)
	("c++"              . c++-mode)
	("c#"               . csharp-mode)
	("clojure"          . clojure-mode)
	("cider repl"       . cider-repl-mode)
	("clojure script"   . clojurescript-mode)
	("lisp"             . lisp-mode)
	("css"              . css-mode)
	("scss"             . scss-mode)
	("haskell"          . haskell-mode)
	("html"             . html-mode)
	("image"            . image-mode)
	("texinfo"          . texinfo-mode)
	("markdown"         . markdown-mode)
	("org"              . org-mode)
	("compilation"      . compilation-mode)
	("sql"              . sql-mode)
	("powershell"       . powershell-mode)
	("tex"              . tex-mode)
	("latex"            . latex-mode)
	("asm"              . asm-mode)
	("nasm"             . nasm-mode)
	("hexl"             . hexl-mode)))

(defun vwe@prog--switch-mode (&optional mode)
  "Switch language mode.
MODE."
  (interactive
   (list (completing-read "switch to:"
						  (mapcar (lambda(item)
									(car item))
								  vwe@prog--language-and-mode-alist))))
  (funcall (cdr (assoc mode vwe@prog--language-and-mode-alist))))

(defun vwe@prog--gud-or-gud-go ()
  "If gdb isn't running; run gdb, else call gud-go."
  (interactive)
  (if (and gud-comint-buffer
           (buffer-name gud-comint-buffer)
           (get-buffer-process gud-comint-buffer)
           (with-current-buffer gud-comint-buffer
			 (eq gud-minor-mode 'gdba)))
      (gud-call (if gdb-active-process "continue" "run") "")
    (gdb (gud-query-cmdline 'gdb)))
  (tool-bar-mode t))

(defun vwe@prog--gud-breakpoint-set-or-remove ()
  "Set/clear breakpoint."
  (interactive)
  (save-excursion
    (if (eq (car (fringe-bitmaps-at-pos (point))) 'breakpoint)
        (gud-remove nil)
      (gud-break nil))))

(defun vwe@prog--gud-proc-kill ()
  "Kill gdb process."
  (interactive)
  (with-current-buffer gud-comint-buffer (comint-skip-input))
  (kill-process (get-buffer-process gud-comint-buffer)))

(defun vwe@prog--gdb-disable ()
  "DBD disable."
  (interactive)
  (tool-bar-mode -1)
  (delete-other-windows)
  (vwe@prog--gud-proc-kill)
  (cl-loop for buffername in (buffer-list)
		   collect
		   (progn
			 (when (string-match "\\*gud-[a-z A-Z 0-9].*\\'"
								 (buffer-name buffername))
			   (kill-buffer buffername)))))

(defun vwe@lsp--run ()
  "Run lsp deferred."
  (interactive)
  (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'asm-mode 'nasm-mode 'sh-mode)
	(lsp-deferred)))

;; ***************************************************************************
;; config
;; ***************************************************************************

;; 根据已有代码快速填充当前代码
(use-package eacl
  :config
  (with-eval-after-load 'grep
	(dolist (v '("node_modules"
				 "bower_components"
				 ".sass_cache"
				 ".cache"
				 ".npm"))
	  (add-to-list 'grep-find-ignored-directories v))
	(dolist (v '("*.min.js"
				 "*.bundle.js"
				 "*.min.css"
				 "*.json"
				 "*.log"))
	  (add-to-list 'grep-find-ignored-files v))))

(use-package flycheck
  :hook
  (prog-mode . global-flycheck-mode)
  :init
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  (if (display-graphic-p)
	  (progn
		(use-package flycheck-posframe
		  :hook
		  (flycheck-mode . flycheck-posframe-mode)
		  :config
		  (add-to-list 'flycheck-posframe-inhibit-functions
					   #'(lambda () (bound-and-true-p company-backend))))
		(use-package flycheck-popup-tip
		  :hook
		  (flycheck-mode . flycheck-popup-tip-mode)))
	(use-package flycheck-pos-tip
	  :defines
	  (flycheck-pos-tip-timeout)
	  :hook
	  (flycheck-mode . flycheck-pos-tip-mode)
	  :config
	  (setq flycheck-pos-tip-timeout 30))))

(use-package projectile
  :init
  (setq projectile-known-projects-file (vwe@lib--path-cache "projectile/projectile-bookmarks.eld" t)
		projectile-cache-file (vwe@lib--path-cache "projectile/projectile.cache" t)
		projectile-completion-system 'ivy
		projectile-sort-order 'recently-active
		projectile-indexing-method 'alien
		projectile-enable-caching t
		projectile-require-project-root nil
		projectile-mode-line-function '(lambda () (format "P:[%s]" (projectile-project-name))))
  :bind
  ((:map projectile-mode-map
		 ("C-c p" . projectile-command-map)))
  :config
  (use-package counsel-projectile
	:after
	(projectile)
	:config
	(counsel-projectile-mode t))

  (use-package treemacs-projectile
	:after
	(treemacs projectile)
	:ensure t)

  (use-package find-file-in-project))

(use-package smart-compile
  :config
  (add-to-list 'smart-compile-alist
			   '("\\.[Cc]+[Pp]*\\'" . "g++ -O2 -g %f -lm -o %n"))
  (add-to-list 'smart-compile-alist
			   '("\\.asm\\'" . "nasm -g -f elf -o %n.o %f"))
  (add-to-list 'smart-compile-alist
			   '("\\.s\\'" . "nasm -g -f elf -o %n.o %f")))

(use-package quickrun
  :config
  (push '("asm" . ((:command . "nasm")
				   (:exec . "%c -f elf -o %n %s %e %a")
				   (:compile-only . "%c -f elf -o %n %s")
				   (:description . "assembly file with nasm and execute")))
		quickrun--language-alist)
  (push '("\\.asm\\'" . "asm") quickrun-file-alist)
  (push '(asm-mode . "asm") quickrun--major-mode-alist)
  (push "asm" quickrun--support-languages))

(use-package counsel-etags
  :hook
  (prog-mode . counsel-mode)
  :init
  (setq tags-revert-without-query t
		large-file-warning-threshold nil
		counsel-etags-sort-grep-result-p t
		imenu-create-index-function 'counsel-etags-imenu-default-create-index-function
		counsel-etags-update-interval 60
		counsel-etags-update-tags-backend (lambda ()
											(shell-command
											 "ctags-universal -e -R")))
  (add-hook 'prog-mode-hook (lambda ()
							  (add-hook 'after-save-hook
										'counsel-etags-virtual-update-tags
										'append
										'local)))
  :config
  (push "build" counsel-etags-ignore-directories)
  (push "build_clang" counsel-etags-ignore-directories)
  (push "TAGS" counsel-etags-ignore-filenames)
  (push ".vscode" counsel-etags-ignore-directories)
  (push ".clang-format" counsel-etags-ignore-filenames)
  (push "*.json" counsel-etags-ignore-filenames))

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
		lsp-log-io t
		lsp-modeline-diagnostics-enable nil
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

(use-package dap-mode)

(use-package magit
  :hook
  (after-save-hook . magit-after-save-refresh-status))

(use-package ejc-sql
  :config
  (use-package ejc-company
	:ensure nil
	:after
	ejc-sql-mode
	:config
	(add-to-list (make-local-variable 'company-backends)
				 '(ejc-company-backend))))

(provide 'vwe-prog)
;;; vwe-prog.el ends here
