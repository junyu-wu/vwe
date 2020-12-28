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

(defcustom mum-key--leader-key
  "M-RET"
  "Keymap quit key."
  :group 'mum-key
  :type 'hook)

(defcustom mum-key--quit-key
  "q"
  "Keymap quit key."
  :group 'mum-key
  :type 'hook)

(defcustom mum-key--toggle-hint-key
  "SPC"
  "Keymap toggle hint key."
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
	(define-key keymap (kbd mum-key--quit-key) 'mum-key--close-buffer)
	(define-key keymap (kbd mum-key--toggle-hint-key) 'nil)
	keymap)
  "Key mapping.")

(defvar mum-key--keymap-mapping
  nil
  "Key mapping.")

(defvar mum-key--keymap-mode-func-alist
  '()
  "Mode and func alist.")

(defvar mum-key--show-hint-p
  nil
  "If non-nil Show keymapping hint, is t show func name.")

(defvar mum-key--footer-list
  nil
  "Footer list.")

(defvar mum-key--line-max-length
  (frame-width)
  "Line max length.")

(defvar mum-key--max-width
  (frame-width)
  "Max width.")

(defvar mum-key--frame-min-length
  0
  "Frame min length.")

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
		  (unless (< (length func-str) mum-key--max-width)
			(setq func-str (concat (substring func-str 0 (* 2 (/ mum-key--max-width 3))) "...")))
		  (unless hint
			(setq hint-str func-str))
		  (when (symbolp (quote func))
			(define-key keymap (cond ((stringp key) (kbd key)) ((mapp key) key)) func)

			(if label-footer
				(setq mum-key--footer-list (append mum-key--footer-list (list (list key hint-str)))))
			(setq func-str (concat (propertize key 'face 'mum-key--key-face)
								   " : " func-str)
				  func-str-list (append func-str-list (list func-str))
				  hint-str (concat (propertize key 'face 'mum-key--key-face)
								   " : " hint-str)
				  hint-str-list (append hint-str-list (list hint-str)))
			)))

	  (set-keymap-parent keymap mum-key--keymap-base-mapping)
	  (define-key keymap (kbd mum-key--toggle-hint-key) (lambda () (interactive)
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
  "Make and format buffer show contentnnnn with DOC-LIST."
  (when (listp doc-list)
	(let* ((doc-str "")
		   (column-max (+ (mum-key--count-column-max doc-list) 4))
		   (column-number (if (< mum-key--max-width column-max) 1 (/ mum-key--max-width column-max))))

	  (setq mum-key--frame-min-length column-max
			mum-key--line-max-length (* column-number column-max))

	  (dotimes (i (length doc-list))
		(let* ((str (if (nth i doc-list) (concat (nth i doc-list) (mum-key--loop-str " " 4)) " "))
			   (str-length (length str)))

		  (when (< str-length column-max)
			(setq str (concat str (mum-key--loop-str " " (- column-max str-length)))))

		  (setq doc-str (concat doc-str
								(if (eq (% (1+ i) column-number) 0)
									(concat str "\n")
								  str)))))
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
	(propertize (format "%s[%s] : quit    [%s] : toggle hint/func    ◕‿-｡ " custom-str mum-key--quit-key mum-key--toggle-hint-key)
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
					(format "%s" title) "\n"
					(propertize (mum-key--loop-str mum-key--string-title-separator mum-key--line-max-length)
								'face 'mum-key--default-face) "\n"
					(format "%s" body) "\n"
					(propertize (mum-key--loop-str mum-key--string-footer-separator mum-key--line-max-length)
								'face 'mum-key--default-face) "\n"
					(mum-key--make-content-footer)))
	(goto-char (point-min))))

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
		  mum-key--footer-list nil
		  mum-key--line-max-length (frame-width)
		  mum-key--max-width (frame-width))))

(defun mum-key--show-keymap-buffer ()
  "Show keymap buffer."
  (let* ((alist '((window-width . mum-key--max-width)
				  (window-height . fit-window-to-buffer)
				  (direction . 'down)
				  (slot . 0))))

	(when mum-key--buffer-handle
	  (with-current-buffer mum-key--buffer-handle
		(setq-local buffer-read-only nil)
		(set-transient-map mum-key--keymap-mapping nil 'mum-key--close-buffer))
	  (display-buffer-in-side-window mum-key--buffer-handle alist))))

;;;###autoload
(defmacro mum-key-define (name define &optional mode leaderkey)
  "Define MODE keymap with NAME and DEFINE.
LEADERKEY is leader key."
  (let* ((name-symbol `,name)
		 (define-list `,define))
	(unless (null name-symbol)
	  (let* ((name-str (format "%s" name-symbol))
			 (func-name-str (concat "mum-key:" name-str))
			 (func-name (intern func-name-str))
			 (define-title (car define-list))
			 (define-body (cadr define-list)))
		(if (plist-member mum-key--keymap-mode-func-alist mode)
			(progn (plist-put mum-key--keymap-mode-func-alist mode `(,func-name ,leaderkey)))
		  (setq mum-key--keymap-mode-func-alist (append mum-key--keymap-mode-func-alist
														`(,mode (,func-name ,leaderkey)))))
		`(defun ,func-name (&optional funcp)
		   (interactive)
		   (setq mum-key--max-width (frame-width))
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
		   (if (< mum-key--max-width mum-key--frame-min-length)
			   (message "sorry,the frame is too small to display the message.")
			 (mum-key--show-keymap-buffer)))))))

;;;###autoload
(defun mum-key--define-keymap-on-change-major-mode ()
  "Define keymap on change major mode."
  (when mum-key--keymap-mode-func-alist
	(let* ((mode-func (plist-get mum-key--keymap-mode-func-alist major-mode))
		   (key (if (cadr mode-func) (cadr mode-func) mum-key--leader-key)))
	  (when mode-func
		(define-key (current-local-map) (kbd key) (car mode-func))))))

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
	(setq mum-key-mode-p t)
	(add-hook 'after-change-major-mode-hook 'mum-key--define-keymap-on-change-major-mode)))

(provide 'mum-key)
;;; mum-key.el ends here
