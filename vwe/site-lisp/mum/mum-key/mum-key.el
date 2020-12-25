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


;;; Code:

(defgroup mum-key nil
  "Mum key."
  :prefix "mum-key--"
  :group 'mum)

(defcustom mum-key--buffer-hook
  '()
  "Hook run when mum-key buffer is initialized."
  :group 'mum-key
  :type 'hook)

(defface mum-key--default-face
  '((t (:foreground "#B0BEC5" :weight bold)))
  "Default face.")

(defface mum-key--title-face
  '((t (:background "#434C5E" :foreground "white" :weight bold)))
  "Title face.")

(defface mum-key--key-face
  '((t (:foreground "SpringGreen" :weight bold)))
  "Key face.")

(defface mum-key--hint-face
  '((t (:background "DarkOrange" :foreground "white" :weight bold)))
  "Hint face.")

(defface mum-key--footer-face
  '((t (:background "#434C5E" :foreground "#B0BEC5" :weight bold)))
  "Footer face.")

(defface mum-key--keymapping-hint-face
  '((t (:background "DarkOrange" :foreground "white" :weight bold)))
  "Keymapping hint face.")

(defvar mum-key-mode-p
  nil
  "Mode.")

(defvar mum-key--buffer-name
  "*mum-key*"
  "Keymap buffer name.")

(defvar mum-key--string-title-separator
  "▬"
  "Title and body separator.")

(defvar mum-key--string-footer-separator
  "▬"
  "Footer and body separator.")

(defvar mum-key--buffer-handle
  nil
  "Hold keymap buffer.")

(defvar mum-key--keymap-base-mapping
  (let* ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd "q") 'mum-key--close-buffer)
	keymap)
  "Key mapping.")

(defvar mum-key--keymap-mapping
  nil
  "Key mapping.")

(defvar mum-key--show-hint-p
  nil
  "If non-nil Show keymapping hint, is t show func name.")

(defvar mum-key--footer-list
  nil
  "Footer list.")

(defun mum-key--make-content-title (title)
  "Make content TITLE.
TITLE: [TITLE]"
  (propertize (format "%s Keymap from [%s]    ｡◕‿◕｡ " (upcase title) major-mode)
			  'face 'mum-key--title-face))

(defun mum-key--make-content-hint-and-keymapping (func-name body)
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
			   (hint (car (cddr body-item)))
			   (hint-str (format "%s" hint))
			   (label-footer (plist-get (cdddr body-item) :footer)))
		  (unless func
			(setq func (lambda () (interactive) (message "func is nil"))))
		  (unless (< (length func-str) (frame-width))
			(setq func-str (concat (substring func-str 0 (* 2 (/ (frame-width) 3))) "...")))
		  (unless hint
			(setq hint-str func-str))
		  (when (symbolp (quote func))
			(define-key keymap (cond ((stringp key) (kbd key)) ((mapp key) key)) func)

			(if label-footer
				(setq mum-key--footer-list (append mum-key--footer-list (list (list key hint-str))))
			  (setq func-str (concat (propertize key 'face 'mum-key--key-face)
									 " : " func-str)
					func-str-list (append func-str-list (list func-str))
					hint-str (concat (propertize key 'face 'mum-key--key-face)
									 " : " hint-str)
					hint-str-list (append hint-str-list (list hint-str))))
			)))

	  (set-keymap-parent keymap mum-key--keymap-base-mapping)
	  (define-key keymap (kbd "TAB") (lambda () (interactive)
									   (if mum-key--show-hint-p
										   (funcall (intern (format "%s" func-name)))
										 (funcall (intern (format "%s" func-name)) t))))
	  (setq mum-key--keymap-mapping keymap)

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

(defun mum-key--make-content-to-string (doc-list)
  "Make and format buffer show content with DOC-LIST."
  (when (listp doc-list)
	(let* ((doc-length (length doc-list))
		   (doc-str)
		   (space "    ")
		   (width (- (frame-width) 8))
		   (column-max (mum-key--count-column-max doc-list))
		   (column (/ width (+ column-max 4))))
	  (dotimes (i doc-length)
		(let* ((str (nth i doc-list))
			   (str-length (length str))
			   (d-value (- (+ column-max 2) str-length)))
		  (when (> d-value 0)
			(setq str (concat str (mum-key--loop-str " " d-value))))
		  (setq doc-str (concat doc-str (if (= (% (1+ i) column) 0) (progn (concat str space "\n")) (concat str))))))
	  doc-str)))

(defun mum-key--make-content-footer ()
  "Make content footer."
  (let* ((custom-str ""))
	(dotimes (i (length mum-key--footer-list))
	  (setq custom-str (concat custom-str
							   "["
							   (format "%s" (car (nth i mum-key--footer-list)))
							   "]"
							   " : "
							   (format "%s" (cadr (nth i mum-key--footer-list)))
							   "    ")))
	(propertize (format "%s[q]: quit    [tab]: toggle hint/func    ◕‿-｡ " custom-str)
				'face 'mum-key--footer-face)))

(defun mum-key--window-get-top (&optional win)
  "Get Top Window.
WIN is Window."
  (interactive)
  (unless win
	(setq win (selected-window)))
  (if (windowp win)
	  (if (window-parent win)
		  (mum-key--window-get-top (window-parent win))
		(window-parent (window-child win)))))

(defun mum-key--make-buffer ()
  "Make show keymap buffer."
  (unless (buffer-live-p mum-key--buffer-handle)
    (setq mum-key--buffer-handle (get-buffer-create mum-key--buffer-name))
    (with-current-buffer mum-key--buffer-handle
	  (let (message-log-max)
        (toggle-truncate-lines 1)
        (message ""))
	  (setq-local cursor-type nil
				  cursor-in-non-selected-windows nil
				  mode-line-format nil
				  word-wrap nil
				  show-trailing-whitespace nil)
	  (run-hooks 'mum-key--buffer-hook)))
  mum-key--buffer-handle)

(defun mum-key--insert-content-to-buffer (title body)
  "Insert TITLE and BODY to buffer."
  (with-current-buffer mum-key--buffer-handle
	(erase-buffer)
	(insert (concat "\n"
					title "\n"
					(propertize (mum-key--loop-str mum-key--string-title-separator (* (/ (frame-width) 3) 2))
								'face 'mum-key--default-face) "\n"
					body "\n"
					(propertize (mum-key--loop-str mum-key--string-footer-separator (* (/ (frame-width) 3) 2))
								'face 'mum-key--default-face) "\n"
					(mum-key--make-content-footer)))))

(defun mum-key--hide-buffer ()
  "Hide buffer."
  (when (buffer-live-p mum-key--buffer-handle)
    (quit-windows-on mum-key--buffer-handle)))

(defun mum-key--close-buffer ()
  "Close key buffer side window."
  (interactive)
  (when mum-key--buffer-handle
	(delete-windows-on mum-key--buffer-handle)
	(kill-buffer mum-key--buffer-handle)
	(setq mum-key--buffer-handle nil
		  mum-key--keymap-mapping nil
		  mum-key--footer-list nil)))

(defun mum-key--show-keymap-buffer ()
  "Show keymap buffer."
  (let* ((alist '((window-width . #'frame-width)
				  (window-height . fit-window-to-buffer)
				  (window-min-height . 5)
				  (direction . 'down)
				  (slot . 0))))

	(when mum-key--buffer-handle
	  (with-current-buffer mum-key--buffer-handle
		(setq-local buffer-read-only nil)
		(set-transient-map mum-key--keymap-mapping nil 'mum-key--close-buffer))
	  (display-buffer-in-side-window mum-key--buffer-handle alist))))

;;;###autoload
(defmacro mum-key--keymap-define (name define)
  "Make call function with NAME DEFINE."
  `(let* ((name-symbol (quote ,name))
		  (define-list (quote ,define)))
	 (unless (null name-symbol)
	   (let* ((name-str (format "%s" name-symbol))
			  (func-name-str (concat "mum-key:" name-str))
			  (func-name (intern func-name-str))
			  (define-title (car define-list))
			  (define-body (cadr define-list)))
		 `(defun ,func-name (&optional funcp)
			(interactive)
			(mum-key--make-buffer)
			(mum-key--insert-content-to-buffer
			 (mum-key--make-content-title ,define-title)
			 (mum-key--make-content-to-string
			  (if funcp
				  (progn
					(setq mum-key--show-hint-p t)
					(plist-get
					 (mum-key--make-content-hint-and-keymapping ,func-name-str (quote ,define-body))
					 :func))
				(setq mum-key--show-hint-p nil)
				(plist-get
				 (mum-key--make-content-hint-and-keymapping ,func-name-str (quote ,define-body))
				 :hint))))
			(setq mum-key--keymap-mapping
				  (plist-get
				   (mum-key--make-content-hint-and-keymapping ,func-name-str (quote ,define-body))
				   :mapping))
			(mum-key--show-keymap-buffer))))))

;;;###autoload
(defmacro mum-key-define (name define)
  "Define keymap with NAME DEFINE."
  `(eval (mum-key--keymap-define ,name ,define)))

;;;###autoload
(define-minor-mode mum-key-mode
  "Mum Key minor mode."
  :init-value nil
  :group 'mum-key
  :global t
  (if mum-key-mode-p
	  (progn
		(setq mum-key-mode-p nil)
		(mum-key--close-buffer))
	(setq mum-key-mode-p t)))

(provide 'mum-key)
;;; mum-key.el ends here
