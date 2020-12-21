;;; mum-key.el ---     Mum key           -*- lexical-binding: t; -*-

;; Copyright (C) 2020  WuJunyu

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

;; func: mum-key:elisp-keymap

;; Emacs Lisp keymap
;; ----------------------------------------------
;; a: eval        b: eval buffer        c: eval region        d: eval defun
;; e: ediff       f: ibuffer            g: dired
;;
;; q: quit    ?: find define    TAB: toggle func/hint    RET: common keymap

;; 1.  plist define keymap func
;; 1.1 create keymap-obj
;; ("name"
;; 	 ("header"
;; 	 (("key" 'func "discreption"))
;; 	)
;; 1.2 define show keymap func
;; 2.  call func show keymap buffer
;; 2.1 init keymap buffer
;; 2.2 generate keymap content and set keymap command
;; 2.3 show buffer
;; 3.  close keymap buffer and execute command

;;; Code:

(require 'cl-lib)

(defgroup mum-key nil
  "Mum key."
  :prefix "mum-key--"
  :group 'mum)

(defcustom mum-key--buffer-hook
  '()
  "Hook run when mum-key buffer is initialized."
  :group 'mum-key
  :type 'hook)

(defface mum-key--title-face
  '((t (:background "blue" :foreground "whtie" :weight bold)))
  "Title face.")

(defface mum-key--key-face
  '((t (:background "DarkOrange" :foreground "whtie" :weight bold)))
  "Title face.")

(defface mum-key--hint-face
  '((t (:background "DarkOrange" :foreground "whtie" :weight bold)))
  "Title face.")

(defface mum-key--keymapping-hint-face
  '((t (:background "DarkOrange" :foreground "whtie" :weight bold)))
  "Title face.")

(defvar mum-key--buffer-name
  "*mum-key*"
  "Keymap buffer name.")

(defvar mum-key--buffer-handle
  nil
  "Hold keymap buffer.")

(defvar mum-key--keymap-mapping
  (let* ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd "q") 'mum-key--close-buffer)
	keymap)
  "Key mapping.")

(defun mum-key--make-content-title (title)
  "Make content TITLE.
TITLE: [TITLE]"
  (propertize (format "%s  Keymap" (upcase title))
			  'face 'mum-key--title-face))

(defun mum-key--make-content-hint-and-keymapping (body)
  "Make content BODY."
  (when (listp body)
	(let* ((keymap mum-key--keymap-mapping);;(make-sparse-keymap))
		   (body-length (length body))
		   (func-str-list)
		   (hint-str-list)
		   (content '(:mapping nil :hint nil :func nil)))
	  (dotimes (i body-length)
		(let* ((body-item (nth i body))
			   (key (car body-item))
			   (func (cadr body-item))
			   (func-str (format "%s" func))
			   (hint-str (format "%s" (car (cddr body-item)))))

		  (when (symbolp (quote func))
			(define-key keymap (cond ((stringp key) (kbd key)) ((mapp key) key)) func)
			;; (define-key keymap (kbd key) func)
			(setq func-str (concat (propertize key 'face 'mum-key--key-face)
								   ": " func-str)
				  func-str-list (append func-str-list (list func-str))
				  hint-str (concat (propertize key 'face 'mum-key--key-face)
								   ": " hint-str)
				  hint-str-list (append hint-str-list (list hint-str))))))
	  (plist-put content :mapping keymap)
	  (plist-put content :func func-str-list)
	  (plist-put content :hint hint-str-list)
	  content)))

(defun mum-key--count-column-max (column)
  "Count COLUMN max length."
  (when (listp column)
	(let* ((max-length 0))
	  (dotimes (i (length column))
		(when (> (length (nth i column)) max-length)
		  (setq max-length (length (nth i column)))))
	  max-length)))

(defun mum-key--loop-str (str num)
  "Loop STR NUM."
  (let* ((tostr))
	(dotimes (_ num)
	  (setq tostr (concat tostr str)))
	tostr))

(defun mum-key--make-contentt-to-string (doc-list)
  "Make and format buffer show content with DOC-LIST."
  (when (listp doc-list)
	(let* ((doc-length (length doc-list))
		   (doc-str)
		   (space "    ")
		   (width (- (window-width) 8))
		   (column-max (mum-key--count-column-max doc-list))
		   (column (/ width (+ column-max 4)))
		   (split (mum-key--loop-str "-" (+ (* column column-max) 8))))
	  (dotimes (i doc-length)
		(let* ((str (nth i doc-list))
			   (str-length (length str))
			   (d-value (- (+ column-max 2) str-length)))
		  (when (> d-value 0)
			(setq str (concat str (mum-key--loop-str " " d-value))))
		  (setq doc-str (concat doc-str (if (= (% (1+ i) column) 0) (concat str space "\n") (concat str))))))
	  (concat split "\n" doc-str))))

(defun mum-key--make-buffer ()
  "Make show keymap buffer."
  (unless (buffer-live-p mum-key--buffer-handle)
    (setq mum-key--buffer-handle (get-buffer-create mum-key--buffer-name))
    (with-current-buffer mum-key--buffer-handle
	  (setq-local cursor-type nil
				  cursor-in-non-selected-windows nil
				  mode-line-format nil
				  word-wrap nil
				  show-trailing-whitespace nil)
	  (run-hooks 'mum-key--buffer-hook)
	  mum-key--buffer-handle)))

(defun mum-key--insert-content-to-buffer (title body)
  "Insert TITLE and BODY to buffer."
  (with-current-buffer mum-key--buffer-handle
	(erase-buffer)
	(insert (concat "\n" title "\n" body))))

(defun mum-key--show-keymap-buffer ()
  "Show keymap buffer."
  (let* ((window)
		 (width (frame-width))
		 (alist `((window-width . ,width)
				  ;; (window-height . ,height)
				  ;; (side . 'bottom)
				  (slot . 0))))
	(when mum-key--buffer-handle
	  (with-current-buffer mum-key--buffer-handle
		(suppress-keymap mum-key--keymap-mapping)
		(use-local-map mum-key--keymap-mapping)
		;; (set-transient-map mum-key--buffer-handle nil
		;; 				   (lambda () (interactive) (mum-key--close-buffer)))
		(setq-local buffer-read-only nil))
	  (setq window (display-buffer-in-side-window mum-key--buffer-handle alist))
	  (when (window-live-p window)
		(select-window window)))))

(defmacro mum-key--make-call-func (name define)
  "Make call function with NAME DEFINE."
  `(let* ((name-symbol (quote ,name))
		  (define-list (quote ,define)))
	 (unless (null name-symbol)
	   (let* ((name-str (format "%s" name-symbol))
			  (func-name (intern (concat "mum-key:" name-str)))
			  (define-title (car define-list))
			  (define-body (cadr define-list)))
		 `(defun ,func-name (&optional funcp)
			(interactive)
			(mum-key--make-buffer)
			`@(mum-key--insert-content-to-buffer
			   (mum-key--make-content-title ,define-title)
			   (mum-key--make-contentt-to-string
				(if funcp
					(plist-get
					 (mum-key--make-content-hint-and-keymapping (quote ,define-body))
					 :func)
				  (plist-get
				   (mum-key--make-content-hint-and-keymapping (quote ,define-body))
				   :hint))))
			`@(setq mum-key--keymap-mapping
					(plist-get
					 (mum-key--make-content-hint-and-keymapping (quote ,define-body))
					 :mapping))
			(mum-key--show-keymap-buffer)
			)))))

(defmacro mum-key--keymap-define (name define)
  "Define keymap function NAME with DEFINE."
  (eval `(mum-key--make-call-func ,name ,define)))

;; (mum-key--keymap-define elisp
;; 						("test keymap"
;; 						 (("a" vwe@lib--buffer-major-mode "major mode")
;; 						  ("b" vwe@lib--buffer-kill-current "kill current")
;; 						  ("d" buffer-list "buffer list")
;; 						  ("e" point-max "point max")
;; 						  ("f" point-min "point min")
;; 						  ("g" (lambda () (interactive) (message "test mum keymap")) "keymap test message"))))

;; (mum-key--keymap-define test
;; 						("test keymap"
;; 						 (("a" vwe@lib--buffer-current-name "current name")
;; 						  ("f" point-min "point min")
;; 						  ("g" (lambda () (interactive) (message "======================================= message")) "message"))))

;; (mum-key:elisp)
;; (mum-key:test)

;; (accessible-keymaps mum-key--keymap-mapping)
;; (map-keymap (lambda (key)
;; 			  (local-unset-key key)) 'mum-key--keymap-mapping)

(defun mum-key--close-buffer ()
  "Close key buffer side window."
  (interactive)
  (when mum-key--buffer-handle
	(kill-buffer mum-key--buffer-handle)
	(setq mum-key--buffer-handle nil)))

;;;###autoload
;; (define-minor-mode mum-key-mode
;;   "Mum key minor mode."
;;   :init-value nil
;;   :keymap nil
;;   :lighter ""
;;   :group 'mum-key
;;   :global t
;;   (if mum-key-mode ))

(provide 'mum-key)
;;; mum-key.el ends here
