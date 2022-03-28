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
	("conf"             . conf-mode)
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

(defun vwe@prog--gud-gdb-exec ()
  "If gdb isn't running; run gdb, else call gud-go."
  (interactive)
  (if (and (bound-and-true-p gud-comint-buffer)
		   gud-comint-buffer
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
	(when (bound-and-true-p gud-comint-buffer)
	  (if (eq (car (fringe-bitmaps-at-pos (point))) 'breakpoint)
		  (gud-remove nil)
		(gud-break nil)))))

(defun vwe@prog--gud-proc-kill ()
  "Kill gdb process."
  (interactive)
  (with-current-buffer gud-comint-buffer (comint-skip-input))
  (kill-process (get-buffer-process gud-comint-buffer)))

(defun vwe@prog--gdb-disable ()
  "GBD disable."
  (interactive)
  (tool-bar-mode -1)
  (delete-other-windows)
  (vwe@prog--gud-proc-kill)
  (dolist (buf (vwe@lib--buffer-asterisk-list t))
	(when (and (bufferp buf) (string-match "\\*gud-[a-z A-Z 0-9].*\\'" (buffer-name buf)))
	  (kill-buffer buf))))

(defun vwe@prog--gdb-get-project-name ()
  "Get current gdb project name."
  (when (and (bound-and-true-p gud-comint-buffer) (bufferp gud-comint-buffer))
	(let* ((name (string-remove-prefix "*gud-" (buffer-name gud-comint-buffer))))
	  (setq name (substring name  0 (- (length name) 1)))
	  name)))

(defun vwe@prog--gdb-switch-buffers (&optional buf)
  "Switch BUF.
If buf in not exist, display this buf."
  (interactive
   (list
	(completing-read (format "select buf(%s):" (buffer-name (current-buffer)))
					 (list "gud/gdb" "source" "breakpoints" "threads" "stack" "locals" "memory" "disassembly" "registers" "io" "frames"))))
  (let* ((name (vwe@prog--gdb-get-project-name))
		 (to))
	(when name
	  (cond
	   ((string-match-p "gub" buf) (setq to (format "*%s-%s*" buf name)))
	   ((string-match-p "gdb" buf) (setq to (format "*%s-%s*" buf name)))
	   ((string-match-p "stack" buf) (setq to (format "*%s frames of %s*" buf name)))
	   ((string-match-p "io" buf) (setq to (format "*input/output of %s*" name)))
	   (t (setq to (format "*%s of %s*" buf name))))

	  (if (get-buffer to)
		  (if (get-buffer-window to)
			  (select-window (get-buffer-window to))
			(vwe@prog--gdb-display-buffers to))
		(vwe@prog--gdb-display-buffers to))
	  (message (format "select %s buffer." to)))))

(defun vwe@prog--gdb-display-buffers (buf)
  "Display BUF."
  (cond
   ((or (string-match-p "gub" buf) (string-match-p "gdb" buf)) (gdb-display-gdb-buffer))
   ((string-match-p "source" buf) (gdb-display-source-buffer))
   ((string-match-p "breakpoints" buf) (gdb-display-breakpoints-buffer))
   ((string-match-p "threads" buf) (gdb-display-threads-buffer))
   ((string-match-p "locals" buf) (gdb-display-locals-buffer))
   ((string-match-p "memory" buf) (gdb-display-memory-buffer))
   ((string-match-p "disassembly" buf) (gdb-display-disassembly-buffer))
   ((string-match-p "registers" buf) (gdb-display-registers-buffer))
   ((string-match-p "io" buf) (gdb-display-io-buffer))
   ((string-match-p "stack" buf) (gdb-display-stack-buffer))))

(defun vwe@prog--gud-init ()
  "GUD init."
  (setq gdb-gud-control-all-threads t))

(defun vwe@prog--gdb-init ()
  "GDB init."
  (add-hook 'gud-mode-hook #'gud-tooltip-mode)
  (setq gdb-many-windows t
		gdb-show-main t
		gdb-show-changed-values t
		gdb-use-colon-colon-notation t
		gdb-speedbar-auto-raise t
		gdb-non-stop-setting t
		gdb-thread-buffer-verbose-names t
		gdb-thread-buffer-arguments t
		gdb-thread-buffer-locations t
		gdb-thread-buffer-addresses t
		gdb-stack-buffer-addresses t
		;; gdb-mi-decode-strings 'utf-8
		))

(defun vwe@lsp--run ()
  "Run lsp deferred."
  (interactive)
  (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'asm-mode 'nasm-mode 'sh-mode)
	(lsp-deferred)))

(defun vwe@base--prog-init ()
  "Prog init."
  (add-hook 'prog-mode-hook #'hs-minor-mode)

  (vwe@prog--gdb-init)
  (vwe@prog--gud-init))

;; ***************************************************************************
;; config
;; ***************************************************************************
;;
;; `hideshow'
;;
(vwe@lib--package 'hideshow
				  nil nil
				  (setq hs-set-up-overlay (lambda (ov)
											(when (eq 'code (overlay-get ov 'hs))
											  (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
													 (info (format " ... #%d " nlines)))
												(overlay-put ov 'display (propertize info 'face '((t (:inherit 'font-lock-comment-face :box t)))))))))
				  nil nil t)
;;
;; `eacl' 根据已有代码快速填充当前代码
;;
(vwe@lib--package 'eacl
				  nil
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

;;
;; `flycheck'
;;
(vwe@lib--package 'flycheck
				  (add-hook 'prog-mode-hook #'global-flycheck-mode)
				  (if (display-graphic-p)
					  (progn
						;;
						;; `flycheck-posframe'
						;;
						(vwe@lib--package 'flycheck-posframe
						  				  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
						  				  (add-to-list 'flycheck-posframe-inhibit-functions #'(lambda () (bound-and-true-p company-backend))))
						;;
						;; `flycheck-popup-tip'
						;;
						(vwe@lib--package 'flycheck-popup-tip
						  				  (add-hook 'flycheck-mode-hook #'flycheck-popup-tip-mode)))
					;;
					;; `flycheck-pos-tip'
					;;
					(vwe@lib--package 'flycheck-pos-tip
					  				  (add-hook 'flycheck-mode-hook #'flycheck-pos-tip-mode)
					  				  (setq flycheck-pos-tip-timeout 30)))
				  (setq flycheck-check-syntax-automatically '(save mode-enabled)
						flycheck-checker-error-threshold 1000))

;;
;; `smart-compile'
;;
(vwe@lib--package 'smart-compile
				  nil
				  (progn
					(add-to-list 'smart-compile-alist
								 '("\\.[Cc]+[Pp]*\\'" . "g++ -O2 -g %f -lm -o %n"))
					(add-to-list 'smart-compile-alist
								 '("\\.asm\\'" . "nasm -g -f elf -o %n.o %f"))
					(add-to-list 'smart-compile-alist
								 '("\\.s\\'" . "nasm -g -f elf -o %n.o %f"))))

;;
;; `quickrun'
;;
(vwe@lib--package 'quickrun
				  nil
				  (progn
					(push '("asm" . ((:command . "nasm")
									 (:exec . "%c -f elf -o %n %s %e %a")
									 (:compile-only . "%c -f elf -o %n %s")
									 (:description . "assembly file with nasm and execute")))
						  quickrun--language-alist)
					(push '("\\.asm\\'" . "asm") quickrun-file-alist)
					(push '(asm-mode . "asm") quickrun--major-mode-alist)
					(push "asm" quickrun--support-languages)))

;;
;; `lsp-mode'
;;
(vwe@lib--package 'lsp-mode
				  (progn
					(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
					(add-hook 'lsp-mode-hook #'dap-auto-configure-mode))
				  (progn
					(vwe@lib--keymap-set lsp-mode-map '(("C-M-i" lsp-describe-thing-at-point)
														([remap xref-find-definitions] lsp-find-definition)
														([remap xref-find-references] lsp-find-references)))

					;;
					;; `dap-mode'
					;;
					(vwe@lib--package 'dap-mode
									  nil nil
									  (setq dap-breakpoints-file (concat (vwe@lib--path-cache "dap/") ".dap-breakpoints")))

					;;
					;; `lsp-ui'
					;;
					(vwe@lib--package 'lsp-ui
									  nil
									  (lsp-ui-mode)
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

											lsp-ui-imenu-enable t)
									  t)

					;;
					;; `lsp-ivy'
					;;
					;; (vwe@lib--package 'lsp-ivy)
					)
				  (setq lsp-keymap-prefix "C-c l"
						lsp-completion-provider :capf
						lsp-idle-delay 0.500
						lsp-enable-file-watchers nil
						lsp-completion-enable-additional-text-edit nil
						lsp-log-io t
						lsp-modeline-diagnostics-enable nil
						lsp-keep-workspace-alive nil
						lsp-eldoc-enable-hover nil
						lsp-headerline-arrow (propertize ">" 'face '((t :inherit shadow :height 1.0)))
						lsp-headerline-breadcrumb-icons-enable nil
						lsp-session-file (concat (vwe@lib--path-cache "lsp/") ".lspsession-v1")))

;;
;; `eglot'
;;
(vwe@lib--package 'eglot)

;;
;; `ejc-sql'
;;
(vwe@lib--package 'ejc-sql
				  nil
				  ;;
				  ;; `ejc-company'
				  ;;
				  (vwe@lib--package 'ejc-company nil
									(add-to-list (make-local-variable 'company-backends) '(ejc-company-backend))))


(vwe@base--prog-init)

(push (expand-file-name "vwe/lisp/langs" user-emacs-directory) load-path)

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
(require 'vwe-yaml)

(provide 'vwe-prog)
;;; vwe-prog.el ends here
