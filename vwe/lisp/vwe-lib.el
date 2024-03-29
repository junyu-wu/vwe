;;; vwe-lib.el ---                                   -*- lexical-binding: t; -*-

;; Copyright (C) 2015  WuJunyu

;; Author: WuJunyu <vistar_w@hotmail.com>
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

;;

;;; Code:

;; ************************************************************************
;; sys
;; ************************************************************************
(defconst vwe@lib--sys-linux-p
  (eq system-type 'gnu/linux)
  "Is Unix/Linux?")

(defconst vw@lib--sys-mac-p
  (eq system-type 'darwin)
  "Is Mac?")

(defconst vwe@lib--sys-win-p
  (eq system-type 'windows-nt)
  "Is Windosws?")

(defconst vwe@lib--sys-cygwin-p
  (eq system-type 'cygwin)
  "Is Cygwin?")

(defconst vwe@lib--sys-32-bit-p
  (not (null (string-match "^xi386*" system-configuration)))
  "Determine system bits.")

(defconst vwe@lib--sys-64-bit-p
  (not (null (string-match "^x86_64*" system-configuration)))
  "Determine system bits.")

(defun vwe@lib--sys-separator ()
  "Get system separator."
  (interactive)
  (let* ((split))
	;; (if (or vwe@lib--sys-win-p vwe@lib--sys-cygwin-p)
	;; (setq split "\\")
	(setq split "/");;)
	split))

(defun vwe@lib--sys-startup-info ()
  "Show startup ready time and garbage collections."
  (interactive)

  (let* ((ver '((:background "DarkViolet" :foreground "white" :weight bold)))
		 (spring '((:foreground "SpringGreen" :weight bold)))
		 (orange '((:foreground "DarkOrange" :weight bold)))
		 (red '((:foreground "OrangeRed" :weight bold))))
	(format
	 ;; "%s startup in %s seconds. loaded %s packages and %s garbage collections."
	 "%s init %ss. load %s packages and %s garbage collections."
	 (propertize (format "GNU Emacs (%s)" emacs-version)
				 'face ver)
	 (propertize (format "%.2f" (float-time (time-subtract after-init-time before-init-time))) ;; (emacs-init-time)
				 'face spring)
	 (propertize (format "%s" (length package-selected-packages) ;; (length package-activated-list)
						 )
				 'face orange)
	 (propertize (format "%s" gcs-done)
				 'face red))))

(defun vwe@lib--found-custom-arg (arg)
  "Found custom ARG."
  (let* ((arg-switch (member arg command-line-args)))
    (setq command-line-args (delete arg command-line-args))
    arg-switch))

;; ************************************************************************
;; frame
;; ************************************************************************
(defun vwe@lib--frame-reset (&optional width hight x y)
  "Reset frame WIDTH HIGHT X and Y point."
  (interactive
   (let*
	   ((width (read-number
				(format "width is %s:"
						(frame-width (selected-frame)))
				(frame-width (selected-frame))))
		(hight (read-number
				(format "hight is %s:"
						(frame-height (selected-frame)))
				(frame-height (selected-frame))))
		(x (read-number
			(format "X is %s:"
					(car (frame-position (selected-frame))))
			(car (frame-position (selected-frame)))))
		(y (read-number
			(format "Y is %s:"
					(cdr (frame-position (selected-frame))))
			(cdr (frame-position (selected-frame))))))
	 (list width hight x y)))

  (let* ((cur-x (car (frame-position (selected-frame))))
		 (cur-y (cdr (frame-position (selected-frame))))
		 (old-list '(width hight x y))
		 (cur-list '((frame-width (selected-frame))
					 (frame-height (selected-frame))
					 cur-x
					 cur-y)))

	(unless (equal old-list cur-list)
	  (when (frame-parameter nil 'fullscreen)
		(set-frame-parameter nil 'fullscreen nil))
	  (when (and width (numberp width))
		(set-frame-width (selected-frame) width))
	  (when (and hight (numberp hight))
		(set-frame-height (selected-frame) hight))
	  (unless (and x (numberp x))
		(setq x cur-x))
	  (unless (and y (numberp y))
		(setq y cur-y))
	  (set-frame-position (selected-frame) x y))))

;; ************************************************************************
;; window
;; ************************************************************************
(defun vwe@lib--window-kill-current ()
  "Kill Current Window."
  (interactive)
  (delete-window (selected-window)))

(defun vwe@lib--window-get-top (&optional win)
  "Get Top Window.
WIN is Window."
  (interactive)
  (unless win
	(setq win (selected-window)))
  (if (window-parent win)
	  (vwe@lib--window-get-top (window-parent win))
	(window-parent (window-child win))))

(defun vwe@lib--window-maximize ()
  "Maximize current buffer."
  (interactive)
  (save-excursion
	(if (and (= 1 (length (cl-remove-if (lambda (window)
										  (window-parameter window 'no-delete-other-windows))
										(window-list))))
			 (assoc ?_ register-alist))
		(jump-to-register ?_)
	  (progn
		(window-configuration-to-register ?_)
		(delete-other-windows)))))

;; ************************************************************************
;; buffer
;; ************************************************************************
(defun vwe@lib--minibuffer-switch ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
	  (select-window (active-minibuffer-window))
	(error "Minibuffer is not active")))

(defun vwe@lib--buffer-major-mode (&optional buffer)
  "Get BUFFER major mode."
  (interactive)
  (unless buffer (setq buffer (current-buffer)))
  (cdr (assoc 'major-mode (buffer-local-variables buffer))))

(defun vwe@lib--buffer-current-name ()
  "Current buffer name."
  (interactive)
  (buffer-name (current-buffer)))

(defun vwe@lib--buffer-kill-current ()
  "Kill Current Buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun vwe@lib--buffer-kill-other (&optional list filter)
  "Kill other all buffer, but do not kill current buffer.
LIST is kill buffer list.
FILTER not kill buffer filter regexp."
  (interactive)
  (if (null list)
      (setq list (buffer-list)))
  (while list
    (let* ((buffer (car list))
		   (name (buffer-name buffer)))
      (and name
		   (not (string-equal name ""))
		   (/= (aref name 0) ?\s)
		   (unless (or (eq (buffer-name (current-buffer)) name)
					   (string-match filter name))
			 (kill-buffer buffer))))
    (setq list (cdr list))))

(defun vwe@lib--buffer-name-list ()
  "Get all buffer name."
  (interactive)
  (mapcar (lambda(buf) (buffer-name buf)) (buffer-list)))

(defun vwe@lib--buffer-list (regexp &optional not-i)
  "Find buffer list of REGEXP.
NOT-I is include curretn buffer."
  (if regexp
	  (progn
		(seq-filter 'stringp
					(mapcar
					 (lambda (item)
					   (if (string-match regexp (buffer-name item))

						   (if not-i
							   (format "%s" (buffer-name item))
							 (unless (equal item (current-buffer))
							   (format "%s" (buffer-name item))))))
					 (buffer-list))))
	(buffer-list)))

(defun vwe@lib--buffer-asterisk-list (&optional not-i)
  "Find asterisk buffer list.
NOT-I is include curretn buffer."
  (vwe@lib--buffer-list "^*" not-i))

(defun vwe@lib--buffer-non-asterisk-list (&optional not-i)
  "Find non asterisk buffer list.
NOT-I is include curretn buffer."
  (vwe@lib--buffer-list "^[^\s*]" not-i))

(defun vwe@lib--buffer-match-p (regs &optional buffer)
  "Is BUFFER name match in REGS?"
  (unless buffer (setq buffer (buffer-name)))
  (when regs
	(cond
	 ((stringp regs) (string-match regs buffer))
	 ((listp regs) (progn
					 (catch 'break
					   (dotimes (i (length regs))
						 (when (string-match (nth i regs) buffer)
						   (throw 'break t)))))))))

(defun vwe@lib--buffer-status ()
  "Buffer status."
  (let* ((read-only buffer-read-only)
		 (modified  (and buffer-file-name (buffer-modified-p))))
	(cond
	 (modified '(1 "R|W")) (read-only '(-1 "R|O")) (t '(0 "R|~")))))

(defun vwe@lib--buffer-vcs-branch ()
  "Header vcs branch."
  (interactive)
  (if vc-mode
      (let* ((backend (vc-backend buffer-file-name)))
        (substring-no-properties vc-mode
								 (+ (if (eq backend 'Hg) 2 3) 2)))
	"*non-vcs*"))

(defun vwe@lib--buffer-save-all ()
  "Save all buffers."
  (interactive)
  (save-excursion
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (if (and (buffer-file-name) (buffer-modified-p))
          (basic-save-buffer)))))

;; ************************************************************************
;; font
;; ************************************************************************
(defun vwe@lib--font-name ()
  "Get currrent name."
  (interactive)
  (font-get (face-attribute 'default :font) :family))

(defun vwe@lib--font-size ()
  "Get currrent name."
  (interactive)
  (font-get (face-attribute 'default :font) :size))

(defun vwe@lib--font-set-ascii (&optional font size)
  "Set ascii FONT and SIZE."
  (condition-case nil
	  (progn
		(unless font
		  (setq font (vwe@lib--font-name)))
		(unless size
		  (setq size (vwe@lib--font-size)))
		(when (display-graphic-p)
		  (set-face-attribute 'default
							  nil
							  :font (format "%s:pixelsize=%d" font size))))
	(error nil)))

(defun vwe@lib--font-set-non-ascii (&optional font size)
  "Set non ascii FONT and SIZE."
  (condition-case nil
	  (progn
		(unless font
		  (setq font (vwe@lib--font-name)))
		(unless size
		  (setq size (vwe@lib--font-size)))
		(when (display-graphic-p)
		  (dolist (charset '(han cjk-misc bopomofo symbol kana))
			(set-fontset-font (frame-parameter nil 'font) charset
							  (font-spec :family font :size size)))))
	(error nil)))

(defun vwe@lib--font-reset (&optional size)
  "Reset font SIZE."
  (interactive
   (let* ((size (read-number (format "size %s:" (vwe@lib--font-size)))))
	 (list size)))
  (vwe@lib--font-set-ascii nil size)
  (vwe@lib--font-set-non-ascii nil size))

(defun vwe@lib--font-reset-ascii (&optional size)
  "Reset font SIZE."
  (interactive
   (let* ((size (read-number (format "size %s:" (vwe@lib--font-size)))))
	 (list size)))
  (vwe@lib--font-set-ascii nil size))

(defun vwe@lib--font-reset-non-ascii (&optional size)
  "Reset font SIZE."
  (interactive
   (let* ((size (read-number (format "size %s:" (vwe@lib--font-size)))))
	 (list size)))
  (vwe@lib--font-set-non-ascii nil size))

;; ************************************************************************
;; file
;; ************************************************************************
(defun vwe@lib--path-trim (path)
  "Trim PATH."
  (when (equal (substring path 0 1) (vwe@lib--sys-separator))
	(setq path (substring path 1)))
  (when (equal (substring path (1- (length path))) (vwe@lib--sys-separator))
	(setq path (substring path 0 (1- (length path)))))
  path)

(defun vwe@lib--dir-make (dir)
  "Make DIR."
  (interactive (list (read-file-name "directory:")))
  (let* ((split (vwe@lib--sys-separator))
		 (dir-list (split-string (vwe@lib--path-trim dir) split))
		 (dir-first (car dir-list))
		 (path ""))
	(cond
	 ((string-prefix-p "~" dir-first) (setq dir-list (cdr dir-list)
											path dir-first))
	 ((string-prefix-p ".." dir-first) (setq dir-list (cdr dir-list)
											 path dir-first)))
	(dotimes (i (length dir-list))
	  (setq path (concat path split (nth i dir-list)))
	  (unless (file-directory-p path)
		(make-directory path)))
	dir))

(defun vwe@lib--file-make (file &optional dir)
  "Make FILE to DIR."
  (interactive (let ((file (read-string "file:"))
					 (path (read-file-name "dir:")))
				 (list file path)))
  (when file
	(let* ((split (vwe@lib--sys-separator))
		   (path ""))
	  (when (string-prefix-p split file)
		(setq file (substring file 1)))
	  (when (string-suffix-p split dir)
		(setq dir (substring dir 0 (1- (length dir)))))
	  (setq path (if dir (concat dir split file) file)
			dir (file-name-directory path))
	  (cond
	   ((file-exists-p path) (message "%s is existed." path))
	   ((file-directory-p path) (message "%s is not file." path))
	   (t (progn
			(vwe@lib--dir-make dir)
			(with-temp-buffer (write-file path)))))
	  path)))

(defun vwe@lib--eol-hidden-dos ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (unless buffer-display-table
	(setq buffer-display-table (make-display-table)))
  (aset buffer-display-table ?\^M []))

(defun vwe@lib--eol-remove-dos ()
  "Replace DOS eolns CR LF with Unix eolns CR."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun vwe@lib--replace (from to &optional all-p)
  "Replace FROM to TO.
ALL-P replace all buffer?"
  (interactive "sform:\nsto:")
  (let* ((rg-p (region-active-p))
		 (start-point)
		 (end-point (point-max)))
	(cond
	 (rg-p (setq start-point (region-beginning)
				 end-point (region-end)))
	 (all-p (setq start-point (point-min)))
	 (t (setq start-point (point))))
	(save-excursion
	  (goto-char start-point)
	  (catch 'break
		(while (search-forward from nil t)
		  (when (> (point) end-point)
			(throw 'break nil))
		  (replace-match to))))))

(defun vwe@lib--replace-all (from to)
  "Replace FROM to TO."
  (interactive "sform:\nsto:")
  (vwe@lib--replace from to t))

(defun vwe@lib--path-emacs.d (&optional path)
  "Get emacs.d sub directory PATH.
CONFIG is etc/lisp/cache/site-list."
  (unless path (setq path ""))
  (when (string-prefix-p (vwe@lib--sys-separator) path)
    (setq path (substring path 1)))
  (concat user-emacs-directory path))

(defun vwe@lib--path-vwe (subpath &optional file-p)
  "Get emacs.d/vwe/.
SUBPATH config sub path.
FILE-P is file?"
  (let* ((vwe (vwe@lib--path-emacs.d "vwe")))
	(when (string-prefix-p (vwe@lib--sys-separator) subpath)
      (setq subpath (substring subpath 1)))
	(setq subpath (concat vwe (vwe@lib--sys-separator) subpath))
	(vwe@lib--path-make-config-path subpath file-p)))

(defun vwe@lib--path-vwe-lisp (subpath &optional file-p)
  "Get emacs.d/vwe/lisp/.
SUBPATH config sub path.
FILE-P is file?"
  (when (string-prefix-p (vwe@lib--sys-separator) subpath)
	(setq subpath (substring subpath 1)))
  (vwe@lib--path-vwe (concat "/lisp/" subpath)
					 file-p))

(defun vwe@lib--path-vwe-etc (subpath &optional file-p)
  "Get emacs.d/vwe/ect/.
SUBPATH config sub path.
FILE-P is file?"
  (when (string-prefix-p (vwe@lib--sys-separator) subpath)
	(setq subpath (substring subpath 1)))
  (vwe@lib--path-vwe (concat "/etc/" subpath)
					 file-p))

(defun vwe@lib--path-vwe-site-lisp (subpath &optional file-p)
  "Get emacs.d/vwe/site-lisp/.
SUBPATH config sub path.
FILE-P is file?"
  (when (string-prefix-p (vwe@lib--sys-separator) subpath)
	(setq subpath (substring subpath 1)))
  (vwe@lib--path-vwe (concat "/site-lisp/" subpath)
					 file-p))

(defun vwe@lib--path-cache (subpath &optional file-p)
  "Get emacs.d/.cache/.
SUBPATH config sub path.
FILE-P is file?"
  ;; (when file-p t)
  (when (string-prefix-p (vwe@lib--sys-separator) subpath)
	(setq subpath (substring subpath 1)))
  (let* ((path (concat (vwe@lib--path-emacs.d ".cache/") subpath)))
	(vwe@lib--path-make-config-path path file-p)
	path))

(defun vwe@lib--path-make-config-path (subpath &optional file-p)
  "Make config SUBPATH.
FILE-P if t make path and file."
  (if file-p
      (unless (file-exists-p subpath) (vwe@lib--file-make subpath))
    (unless (file-directory-p subpath) (vwe@lib--dir-make subpath)))
  subpath)

(defun vwe@lib--file-delete (path &optional ignores)
  "Delete PATH file or directory.
IGNORES is a ignore file of directory list."
  (cond
   ((file-regular-p path) (delete-file path))
   ((file-directory-p path) (cond
							 ((null ignores) (delete-directory path t t))
							 (t (let* ((files (directory-files path)))
								  (dotimes (i (length files))
									(let* ((file (nth i files))
										   (full (concat path "/" file)))
									  (unless (member file ignores)
										(message "%s" full)
										(cond
										 ((or (equal file ".") (equal file "..")) (message "file is . or .."))
										 ((file-regular-p full) (delete-file full))
										 ((file-directory-p full) (delete-directory full t t))))))))))))

;; ************************************************************************
;; modeline
;; ************************************************************************
(defun vwe@lib--modeline-hide (&optional filter buffer)
  "Hide current window BUFFER(buffer name) modeline of FILTER(buffer name list)."
  (interactive)
  (unless filter (setq filter (vwe@lib--buffer-name-list)))
  (unless buffer (setq buffer (buffer-name (current-buffer))))
  (dolist (window (window-list))
	(with-selected-window window
	  (dotimes (i (length filter))
		(when (equal (nth i filter) buffer)
		  (setq mode-line-format nil))))))

;; ************************************************************************
;; point
;; ************************************************************************
(defun vwe@lib--point-find-line-bol ()
  "Find current point line begin of line position."
  (save-excursion
	(back-to-indentation)
	(point)))

(defun vwe@lib--point-find-line-eol ()
  "Find current point end of line position."
  (save-excursion
	(end-of-line)
	(skip-chars-backward " \t" (vwe@lib--point-find-line-bol))
	(point)))

(defun vwe@lib--point-unescaped-p (pos)
  "Determine whether the paren after POS is unescaped."
  (save-excursion
    (goto-char pos)
    (= (logand (skip-syntax-backward "/\\") 1) 0)))

(defun vwe@lib--point-paren-info (&optional pos)
  "POS is paren info, like (1 10) (-1 10).
1 is open and -1 is close."
  (interactive)
  (unless pos (setq pos (point)))
  (cond
   ((and (eq (syntax-class (syntax-after pos)) 4)
		 (vwe@lib--point-unescaped-p pos))
    (cons 1 pos))
   ((and (eq (syntax-class (syntax-after pos)) 5)
		 (vwe@lib--point-unescaped-p pos))
    (cons -1 (1+ pos)))))

;; ************************************************************************
;; keymap
;; ************************************************************************
(defun vwe@lib--keymap-global-set (key-alist)
  "Set global keymap of KEY-ALIST."
  (vwe@lib--keymap-set global-map key-alist))

(defun vwe@lib--keymap-set (keymap key-alist)
  "Set KEYMAP of KEY-ALIST."
  (when (listp key-alist)
	(dotimes (i (length key-alist))
	  (let* ((key (car (nth i key-alist)))
			 (func (cadr (nth i key-alist))))
		(cond
		 ((stringp key) (define-key keymap (kbd key) func))
		 ((mapp key) (define-key keymap key func)))))))

;; ************************************************************************
;; server
;; ************************************************************************
(defun vwe@lib--server-running-p ()
  "If return true `server-start' has been called."
  (interactive)
  (condition-case nil
	  (and (boundp 'server-process)
		   (memq (process-status server-process) '(connect listen open run)))
    (error nil)))

(defun vwe@lib--server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defmacro vwe@lib--server-lsp (mode &rest body)
  "Run LSP MODE and BODY."
  `(cond
	((eq ,mode 'lsp) (lsp-deferred) ,(plist-get body :lsp))
	((eq ,mode 'eglot) (eglot-ensure) ,(plist-get body :eglot))))

;; ************************************************************************
;; debug
;; ************************************************************************
(defun vwe@lib--debug-close ()
  "Debug init."
  (setq debug-on-error nil
		max-lisp-eval-depth 800))

;; ************************************************************************
;; face
;; ************************************************************************
(defmacro vwe@lib--face-of-string (str &rest properties)
  "Used to set the face of STR with PROPERTIES."
  `(propertize ,str 'face (list ,@properties)))

;; ************************************************************************
;; func call
;; ************************************************************************
(defun vwe@lib--kill-ring-save ()
  "Copy."
  (interactive)
  (if (region-active-p)
	  (kill-ring-save (region-beginning) (region-end) t)
	(kill-ring-save (point-at-bol) (point-at-eol) nil)))

(defun vwe@lib--kill-region ()
  "Cut."
  (interactive)
  (if (region-active-p)
	  (kill-region (region-beginning) (region-end) t)
	(kill-region (point-at-bol) (point-at-eol) nil)))

(defun vwe@lib--copy-to-clipboard ()
  "Copy to clipboard."
  (interactive)
  (if (display-graphic-p)
	  (progn
		(call-interactively 'clipboard-kill-ring-save))
	(if (region-active-p)
		(progn
		  (call-interactively 'clipboard-kill-ring-save)
		  (shell-command-on-region (region-beginning) (region-end) "xclip -in -selection clipboard &> /dev/null")
		  (message "Yanked region to clipboard!")
		  (deactivate-mark))
	  (message "No region active; can't yank to clipboard!"))))

(defun vwe@lib--paste-from-clipboard ()
  "Paste from clipboard."
  (interactive)
  (if (display-graphic-p)
	  (progn
		(clipboard-yank))
	(progn
	  (let* ((clipboard (shell-command-to-string "xclip -o -selection clipboard")))
		(kill-new clipboard)
		(insert clipboard)))))

(defun vwe@lib--func-call-by-name (name)
  "Call functuion by NAME."
  (funcall (nth 0 (read-from-string name))))

(defmacro vwe@lib--eval-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06fs" (float-time (time-since time)))))

;; ************************************************************************
;; package
;; ************************************************************************
(defmacro vwe@lib--package (pkg &optional pre post final requirep dir ignore)
  "After load DIR or download PKG package eval PRE POST and FINAL.
REQUIREP non-nil `require' PKG.
IGNORE non-nil ignore package."
  `(condition-case nil
	   (progn
		 (if ,pkg
			 (progn
			   (unless ,ignore
				 (if ,dir
					 (progn
					   (when (file-directory-p (format "%s" ,dir))
						 (push ,dir load-path)))
				   (unless (package-installed-p ,pkg) (package-install ,pkg))))
			   ,pre
			   (when ,requirep (require ,pkg))
			   (with-eval-after-load (format "%s" ,pkg) ,post)
			   ,final)
		   (progn ,pre ,post ,final)))
	 (error
	  (message "pkg %S not found or package inner error" ,pkg))))

(defmacro vwe@lib--pkg (pkg &rest args)
  "Package install and configure.
PKG is package name.
ARGS is configure.
  Usage:
      (vwe@lib--package package [:keyword [(option)]]...)

    :init     code to run before package has been loaded.
    :config   code to run after package has been loaded.
    :path     package local path.
    :undefer  defer loading of a package.
    :variable set variable.
    :preload  preload variable or config.
    :buildin  do not download package, use buildin package.
    :always    do not load package, just config."
  (declare (indent defun))

  (let* ((path (plist-get args :path))
		 (undefer (plist-get args :undefer))
		 (buildin (plist-get args :buildin))
		 (always (plist-get args :always)))
	(if always
		(progn
		  (eval (macroexp-progn (plist-get args :init)))
		  (eval (macroexp-progn (plist-get args :config)))
		  (eval (macroexp-progn (append (plist-get args :variable) '(nil)))))
	  (condition-case nil
		  (when pkg ;; 加载安装包并进行配置
			(progn
			  (unless buildin ;; 不安装,按照系统自带包处理
				(if path ;; 安装包
					(macroexp-progn ;; 从本地路径中加载安装
					 (when (file-directory-p (format "%s" (eval path)))
					   (push (eval path) load-path)))
				  (eval ;; 从源中下载安装
				   `(unless (package-installed-p ',pkg)
					  (package-install ',pkg)))))

			  (eval ;; 包加载前初始化
			   (macroexp-progn (plist-get args :init)))

			  (eval ;; 包需要的预加载或预定义
			   (macroexp-progn (append (plist-get args :preload) '(nil))))

			  (when undefer ;; 延迟加载
				(eval `(require ',pkg)))

			  (with-eval-after-load (format "%s" pkg) ;; 包加载后配置
				(eval (macroexp-progn (plist-get args :config)))
				(eval (macroexp-progn (append (plist-get args :variable) '(nil)))))))
		(error
		 `(message "pkg %S not found or package inner error" ,pkg))))))

(defmacro vwe@lib--load-theme (theme-path)
  "Load THEME-PATH."
  `(condition-case nil
	   (progn
		 (when (file-directory-p ,theme-path)
		   (add-to-list 'custom-theme-load-path ,theme-path)))
	 (error
	  (message "theme path %S not found." ,theme-path))))

(defmacro vwe@lib--package-install (package &optional refreshp)
  "Install PACKAGE.
REFRESHP non-nil refresh package contents."
  `(unless (package-installed-p ,package)
	 (when refreshp (package-refresh-contents))
	 (package-install ,package)))

(defun vwe@lib--package-load (pkg &optional path)
  "Load PKG of PATH."
  (interactive)
  (condition-case nil
	  (progn
		(when (and path (file-directory-p (format "%s" path)))
		  (add-to-list 'load-path path))
		(require pkg) t)
	(error
	 (condition-case nil
		 (progn (vwe@lib--package-install pkg) (require pkg) t)
	   (error
		(message "pkg %S not found" pkg))))))

;; ************************************************************************
;; log
;; ************************************************************************
(defun vwe@lib--log (msg &optional type)
  "Log MSG TYPE.
TYPE is `info' `warning' `error'."
  (let* ((log (if (stringp msg) msg (format "%S" msg))))
	(cond
	 ((equal type 'warning) (message "[vwe-warning] %s" log))
	 ((equal type 'error) (message "[vwe-error] %s" log))
	 (t (message "[vwe-info] %s" log)))))

(vwe@lib--log "vwe lib loaded.")

(provide 'vwe-lib)
;;; vwe-lib.el ends here
