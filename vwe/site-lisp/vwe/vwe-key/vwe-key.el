;;; vwe-key.el ---     Vwe key           -*- lexical-binding: t; -*-

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


;;; Code:

(defgroup vwe-key nil
  "Vwe key."
  :prefix "vwe-key--"
  :group 'vwe)

(defcustom vwe-key--buffer-hook
  '()
  "Hook run when vwe-key buffer is initialized."
  :group 'vwe-key
  :type 'hook)

(defcustom vwe-key--leader-key
  "M-RET"
  "Keymap quit key."
  :group 'vwe-key
  :type 'string)

(defcustom vwe-key--quit-key
  "q"
  "Keymap quit key."
  :group 'vwe-key
  :type 'string)

(defcustom vwe-key--toggle-hint-key
  "SPC"
  "Keymap toggle hint key."
  :group 'vwe-key
  :type 'string)

(defcustom vwe-key--search-cmd-key
  "C-s"
  "Keymap toggle hint key."
  :group 'vwe-key
  :type 'string)

(defcustom vwe-key--toggle-minibuffer-key
  "M-RET"
  "Toggle minibuffer key."
  :group 'vwe-key
  :type 'string)

(defface vwe-key--default-face
  '((t (:foreground "#B0BEC5" :weight bold)))
  "Default face.")

(defface vwe-key--title-face
  '((t (:background "DarkBlue" :foreground "white" :weight bold)))
  "Title face.")

(defface vwe-key--title-other-face
  '((t (:background "#434C5E" :foreground "white" :weight bold)))
  "Title face.")

(defface vwe-key--key-face
  '((t (:foreground "SpringGreen" :weight bold)))
  "Key face.")

(defface vwe-key--hint-face
  '((t (:background "DarkOrange" :foreground "white" :weight bold)))
  "Hint face.")

(defface vwe-key--footer-face
  '((t (:background "#434C5E" :foreground "#B0BEC5" :weight bold)))
  "Footer face.")

(defface vwe-key--keymapping-hint-face
  '((t (:background "DarkOrange" :foreground "white" :weight bold)))
  "Keymapping hint face.")

(defvar vwe-key-mode-p
  nil
  "Mode.")

(defvar vwe-key--buffer-name
  "*vwe-key*"
  "Keymap buffer name.")

(defvar vwe-key--string-title-separator
  "▬"
  "Title and body separator.")

(defvar vwe-key--string-footer-separator
  "▬"
  "Footer and body separator.")

(defvar vwe-key--buffer-handle
  nil
  "Hold keymap buffer.")

(defvar vwe-key--keymap-base-mapping
  (let* ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd vwe-key--quit-key) #'vwe-key--close-buffer)
	(define-key keymap (kbd vwe-key--toggle-hint-key) 'nil)
	keymap)
  "Key mapping.")

(defvar vwe-key--keymap-mapping
  nil
  "Key mapping.")

(defvar vwe-key--keymap-mode-func-alist
  '()
  "Mode and func alist.")

(defvar vwe-key--show-hint-p
  nil
  "If non-nil Show keymapping hint, is t show func name.")

(defvar vwe-key--footer-list
  nil
  "Footer list.")

(defvar vwe-key--line-max-length
  (frame-width)
  "Line max length.")

(defvar vwe-key--max-width
  (frame-width)
  "Max width.")

(defvar vwe-key--frame-min-length
  0
  "Frame min length.")

(defun vwe-key--make-content-title (title)
  "Make content TITLE and add face."
  (let* ((original-title title)
		 (title-make)
		 (other (propertize (format " [%s]    ｡◕‿◕｡ " major-mode) 'face 'vwe-key--title-other-face)))
	(when original-title
	  (if (listp original-title)
		  (let* ((title-str (car original-title))
				 (title-face (plist-get (cdr original-title) :face)))
			(setq title-make (propertize (concat " " (upcase title-str) " ")
										 'face (if title-face title-face 'vwe-key--title-face))))
		(setq title-make (propertize (concat " " (upcase original-title) " ") 'face 'vwe-key--title-face)))
	  (format "%s%s" title-make other))))

(defun vwe-key--make-minibuffer-function-name-by-func-name (func-name)
  "Make minibuffer function name by FUNC-NAME."
  (format "%s-minibufer" func-name))

(defun vwe-key--make-content-hint-and-keymapping-for-minibuffer (func-name body)
  "Make content BODY and define FUNC-NAME keymap for minibuffer."
  (when (listp body)
	(let* ((body-length (length body))
		   (minibuf-condition-list '()))
	  (dotimes (i body-length)
		(let* ((body-item (nth i body))
			   (func (cadr body-item))
			   (func-str (format "%s" func))
			   (hint (car (cddr body-item)))
			   (hint-str (format "%s" hint)))
		  (unless func (setq func (lambda () (interactive) (message "func is nil"))))
		  (unless hint (setq hint-str func-str))

		  (when (symbolp (quote func))
			(setq hint-str (concat (propertize (concat hint-str " ["))
								   (propertize func-str 'face 'vwe-key--key-face)
								   (propertize "]"))
				  minibuf-condition-list (append minibuf-condition-list (list (list hint-str func)))))))

	  (setq minibuf-condition-list (cons (list (concat (propertize "toggle to leader key [")
													   (propertize (format "%s" func-name)
																   'face 'vwe-key--key-face)
													   (propertize "]"))
											   (intern (format "%s" func-name)))
										 minibuf-condition-list))
	  minibuf-condition-list)))

(defun vwe-key--make-content-hint-and-keymapping (func-name body)
  "Make content BODY and define FUNC-NAME keymap."
  (when (listp body)
	(let* ((keymap (make-sparse-keymap))
		   (body-length (length body))
		   (func-str-list)
		   (hint-str-list)
		   (content '(:mapping nil :hint nil :func nil)))
	  (dotimes (i body-length)
		(let* ((body-item (nth i body))
			   (key (car body-item))
			   (func (cadr body-item))
			   (func-str (format "%s" func))
			   (func-keep func-str)
			   (hint (car (cddr body-item)))
			   (hint-str (format "%s" hint))
			   (label-footer (plist-get (cdddr body-item) :footer))
			   (str-face (plist-get (cdddr body-item) :face))
			   (circle (plist-get (cdddr body-item) :circle)))
		  (unless func
			(setq func (lambda () (interactive) (message "func is nil"))))
		  (when circle
			(setq func (lambda () (interactive)
						 (funcall (intern func-keep))
						 (funcall (intern (format "%s" func-name))))))
		  (unless (< (length func-str) vwe-key--max-width)
			(setq func-str (concat (substring func-str 0 (* 2 (/ vwe-key--max-width 3))) "...")))
		  (unless hint
			(setq hint-str func-str))
		  (when (symbolp (quote func))
			(define-key keymap (cond ((stringp key) (kbd key)) ((mapp key) key)) func)
			(if label-footer
				(setq vwe-key--footer-list
					  (append vwe-key--footer-list (list (list key (propertize hint-str
																			   'face (if str-face str-face
																					   'vwe-key--footer-face))))))
			  (setq func-str (concat (propertize key 'face 'vwe-key--key-face)
									 " : "
									 (propertize func-str 'face str-face))
					func-str-list (append func-str-list (list func-str))
					hint-str (concat (propertize key 'face 'vwe-key--key-face)
									 " : "
									 (propertize hint-str 'face str-face))
					hint-str-list (append hint-str-list (list hint-str)))))))

	  (set-keymap-parent keymap vwe-key--keymap-base-mapping)
	  (define-key keymap (kbd vwe-key--toggle-minibuffer-key) (intern (vwe-key--make-minibuffer-function-name-by-func-name func-name)))
	  (define-key keymap (kbd vwe-key--toggle-hint-key) (lambda () (interactive)
														  (if vwe-key--show-hint-p
															  (funcall (intern (format "%s" func-name)))
															(funcall (intern (format "%s" func-name)) t))))
	  (setq vwe-key--keymap-mapping keymap)
	  (plist-put content :mapping keymap)
	  (plist-put content :func func-str-list)
	  (plist-put content :hint hint-str-list)
	  content)))

(defun vwe-key--count-column-max (column)
  "Count COLUMN max length."
  (when (listp column)
	(let* ((max-length 0))
	  (dotimes (i (length column))
		(when (> (length (nth i column)) max-length)
		  (setq max-length (length (nth i column)))))
	  max-length)))

(defun vwe-key--loop-str (str num)
  "Loop STR NUM."
  (let* ((tostr))
	(dotimes (_ num)
	  (setq tostr (concat tostr str)))
	tostr))

(defun vwe-key--make-content-to-string (doc-list)
  "Make and format buffer show contentnnnn with DOC-LIST."
  (when (listp doc-list)
	(let* ((doc-str "")
		   (column-max (+ (vwe-key--count-column-max doc-list) 4))
		   (column-number (if (< vwe-key--max-width column-max) 1 (/ vwe-key--max-width column-max))))

	  (setq vwe-key--frame-min-length column-max
			vwe-key--line-max-length (* column-number column-max))

	  (dotimes (i (length doc-list))
		(let* ((str (if (nth i doc-list) (concat (nth i doc-list) (vwe-key--loop-str " " 4)) " "))
			   (str-length (length str)))

		  (when (< str-length column-max)
			(setq str (concat str (vwe-key--loop-str " " (- column-max str-length)))))

		  (setq doc-str (concat doc-str
								(if (eq (% (1+ i) column-number) 0)
									(concat str "\n")
								  str)))))
	  doc-str)))

(defun vwe-key--make-content-footer ()
  "Make content footer."
  (let* ((custom-str "")
		 (other (propertize (format "[%s] : quit    [%s] : toggle hint/func    ◕‿-｡ "
									vwe-key--quit-key vwe-key--toggle-hint-key)
							'face 'vwe-key--footer-face)))
	(dotimes (i (length vwe-key--footer-list))
	  (setq custom-str (concat custom-str
							   (propertize (concat "[" (format "%s" (car (nth i vwe-key--footer-list))) "]" " : ")
										   'face 'vwe-key--footer-face)
							   (format "%s" (cadr (nth i vwe-key--footer-list)))
							   (propertize "    " 'face 'vwe-key--footer-face))))
	(string-trim (format "%s%s" custom-str other))))

(defun vwe-key--window-get-top (&optional win)
  "Get Top Window.
WIN is Window."
  (interactive)
  (unless win
	(setq win (selected-window)))
  (if (windowp win)
	  (if (window-parent win)
		  (vwe-key--window-get-top (window-parent win))
		(window-parent (window-child win)))))

(defun vwe-key--make-buffer ()
  "Make show keymap buffer."
  (unless (buffer-live-p vwe-key--buffer-handle)
	(setq vwe-key--buffer-handle (get-buffer-create vwe-key--buffer-name))
	(with-current-buffer vwe-key--buffer-handle
	  (let (message-log-max)
		(toggle-truncate-lines 1)
		(message ""))
	  (setq-local cursor-type nil
				  cursor-in-non-selected-windows nil
				  mode-line-format nil
				  word-wrap nil
				  show-trailing-whitespace nil)
	  (run-hooks 'vwe-key--buffer-hook)))
  vwe-key--buffer-handle)

(defun vwe-key--insert-content-to-buffer (title body)
  "Insert TITLE and BODY to buffer."
  (with-current-buffer vwe-key--buffer-handle
	(erase-buffer)
	(insert (concat "\n"
					(format "%s" title) "\n"
					(propertize (vwe-key--loop-str vwe-key--string-title-separator vwe-key--line-max-length)
								'face 'vwe-key--default-face) "\n"
					(format "%s" body) "\n"
					(propertize (vwe-key--loop-str vwe-key--string-footer-separator vwe-key--line-max-length)
								'face 'vwe-key--default-face) "\n"
					(vwe-key--make-content-footer)))
	(goto-char (point-min))))

(defun vwe-key--hide-buffer ()
  "Hide buffer."
  (when (buffer-live-p vwe-key--buffer-handle)
	(quit-windows-on vwe-key--buffer-handle)))

(defun vwe-key--close-buffer ()
  "Close key buffer side window."
  (interactive)
  (when vwe-key--buffer-handle
	(delete-windows-on vwe-key--buffer-handle)
	(kill-buffer vwe-key--buffer-handle)
	(setq vwe-key--buffer-handle nil
		  vwe-key--keymap-mapping nil
		  vwe-key--footer-list nil
		  vwe-key--line-max-length (frame-width)
		  vwe-key--max-width (frame-width))))

(defun vwe-key--show-keymap-buffer ()
  "Show keymap buffer."
  (let* ((alist '((window-width . vwe-key--max-width)
				  (window-height . fit-window-to-buffer)
				  (direction . 'down)
				  (slot . 0))))

	(when vwe-key--buffer-handle
	  (with-current-buffer vwe-key--buffer-handle
		(setq-local buffer-read-only nil)
		(set-transient-map vwe-key--keymap-mapping nil 'vwe-key--close-buffer))
	  (display-buffer-in-side-window vwe-key--buffer-handle alist))))

;;;###autoload
(defmacro vwe-key-define (name define &optional mode leaderkey)
  "Define MODE keymap with NAME and DEFINE.
LEADERKEY is leader key."
  (let* ((name-symbol `,name)
		 (define-list `,define)
		 (mode-list `,mode))
	(unless (null name-symbol)
	  (let* ((name-str (format "%s" name-symbol))
			 (func-name-str (concat "vwe-key:" name-str))
			 (func-name (intern func-name-str))
			 (minibuf-func-name (intern (vwe-key--make-minibuffer-function-name-by-func-name func-name-str)))
			 (define-title (car define-list))
			 (define-body (cadr define-list)))

		(eval `(defun ,minibuf-func-name (&optional func)
				 (interactive
				  (list
				   (completing-read (format "func call (%s):" ,func-name-str)
									(mapcar (lambda (x) (car x))
											(vwe-key--make-content-hint-and-keymapping-for-minibuffer ,func-name-str (quote ,define-body))))))
				 (unless func (setq func (intern ,func-name-str)))
				 (let* ((func-list (vwe-key--make-content-hint-and-keymapping-for-minibuffer ,func-name-str (quote ,define-body))))
				   (call-interactively (cadr (assoc func func-list))))))

		(when (and mode-list (listp mode-list))
		  (dotimes (i (length mode-list))
			(if (plist-member vwe-key--keymap-mode-func-alist (nth i mode-list))
				(progn (plist-put vwe-key--keymap-mode-func-alist (nth i mode-list) `(,func-name ,leaderkey)))
			  (setq vwe-key--keymap-mode-func-alist (append vwe-key--keymap-mode-func-alist
															`(,(nth i mode-list) (,func-name ,leaderkey)))))))
		`(defun ,func-name (&optional funcp)
		   (interactive)
		   (setq vwe-key--max-width (frame-width))
		   (vwe-key--make-buffer)
		   (vwe-key--insert-content-to-buffer
			(vwe-key--make-content-title (quote ,define-title))
			(vwe-key--make-content-to-string
			 (if funcp
				 (progn
				   (setq vwe-key--show-hint-p t)
				   (plist-get
					(vwe-key--make-content-hint-and-keymapping ,func-name-str (quote ,define-body))
					:func))
			   (setq vwe-key--show-hint-p nil)
			   (plist-get
				(vwe-key--make-content-hint-and-keymapping ,func-name-str (quote ,define-body))
				:hint))))
		   (setq vwe-key--keymap-mapping
				 (plist-get
				  (vwe-key--make-content-hint-and-keymapping ,func-name-str (quote ,define-body))
				  :mapping))
		   (if (< vwe-key--max-width vwe-key--frame-min-length)
			   (message "sorry,the frame is too small.")
			 (vwe-key--show-keymap-buffer)))))))

;;;###autoload
(defun vwe-key--define-keymap-on-change-major-mode ()
  "Define keymap on change major mode."
  (when vwe-key--keymap-mode-func-alist
	(let* ((mode-func (plist-get vwe-key--keymap-mode-func-alist major-mode))
		   (key (if (cadr mode-func) (cadr mode-func) vwe-key--leader-key)))
	  (when mode-func
		(define-key (current-local-map) (kbd key) (car mode-func))))))

;;;###autoload
(define-minor-mode vwe-key-mode
  "Vwe Key minor mode."
  :init-value nil
  :group 'vwe-key
  :global t
  (if vwe-key-mode-p
	  (progn
		(setq vwe-key-mode-p nil)
		(vwe-key--close-buffer))
	(setq vwe-key-mode-p t)
	(add-hook 'after-change-major-mode-hook 'vwe-key--define-keymap-on-change-major-mode)
	(add-hook 'read-only-mode-hook 'vwe-key--define-keymap-on-change-major-mode)
	(add-hook 'window-configuration-change-hook 'vwe-key--define-keymap-on-change-major-mode)))

(provide 'vwe-key)
;;; vwe-key.el ends here
