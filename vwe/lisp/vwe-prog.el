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

;; apt install editorconfig

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
	("latex"            . latex-mode)))

(defun vwe@prog--switch-mode (&optional mode)
  "Switch language mode.
MODE."
  (interactive
   (list (completing-read "switch to:"
						  (mapcar (lambda(item)
									(car item))
								  vwe@prog--language-and-mode-alist))))
  (funcall (cdr (assoc mode vwe@prog--language-and-mode-alist))))

;; ***************************************************************************
;; config
;; ***************************************************************************
;; 显示当前函数等参数列表等
(use-package eldoc
  :ensure nil
  :diminish
  (eldoc-mode . nil))

;; 定义编码样式的文件格式和一个文本编辑器插件集合
(use-package editorconfig
  :diminish
  (editorconfig-mode)
  :hook
  (after-init . editorconfig-mode))

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
  :diminish
  (flycheck-mode)
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
  :diminish
  (projectile-mode . nil)
  :init
  (setq projectile-known-projects-file (vwe@lib--path-cache
								   "projectile/projectile-bookmarks.eld" t)
		projectile-cache-file (vwe@lib--path-cache "projectile/projectile.cache" t)
		projectile-completion-system 'ivy
		projectile-sort-order 'recently-active
		projectile-indexing-method 'alien
		projectile-enable-caching t
		projectile-require-project-root nil
		projectile-mode-line-function '(lambda ()
										 (format "P:[%s]"
												 (projectile-project-name))))
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
				   (:description . "Assb file with nasm and execute")))
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

;; ***************************************************************************
;; load
;; ***************************************************************************
(require 'vwe-lsp)

(require 'vwe-org)
(require 'vwe-markdown)

(require 'vwe-lisp)
(require 'vwe-assembly)
(require 'vwe-clang)
(require 'vwe-golang)
(require 'vwe-python)
(require 'vwe-ruby)
(require 'vwe-java)
(require 'vwe-web)
(require 'vwe-clojure)
(require 'vwe-csharp)
(require 'vwe-rust)
(require 'vwe-bat)
(require 'vwe-scheme)
(require 'vwe-ahk)
;;;(require 'vwe-yaml)

(provide 'vwe-prog)
;;; vwe-prog.el ends here
