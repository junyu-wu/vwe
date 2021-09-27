;;; vwe-move.el --- vwiss emacs move                 -*- lexical-binding: t; -*-

;; Copyright (C) 2021  WuJunyu

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

(defgroup vwe-move nil
  "Customization group for beacon."
  :group 'vwe
  :prefix "vwe-move--")

(defvar vwe-move--keymap
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Move to mark map.")

(defun vwe-move-line--begin-point ()
  "Get current point line or region start indentation."
  (interactive)
  (let* ((pos))
	(if (region-active-p)
		(setq pos (region-beginning))
	  (setq pos (line-beginning-position)))
	pos))

(defun vwe-move-line--end-point ()
  "Get current point line or region end eol."
  (interactive)
  (let* ((pos))
	(if (region-active-p)
		(setq pos (region-end))
	  (setq pos (line-end-position)))
	pos))

(defun vwe-move-line--line-copy (&optional begin end)
  "Copy BEGIN and END region."
  (interactive)
  (let* ((begin-point (or begin (vwe-move-line--begin-point)))
		 (end-point (or end (vwe-move-line--end-point))))
	(buffer-substring-no-properties begin-point end-point)))

(defun vwe-move-line--move-up (&optional index)
  "Move line up INDEX."
  (let* ((begin-point (vwe-move-line--begin-point))
		 (end-point (vwe-move-line--end-point))
		 (cur-line (vwe-move-line--line-copy))
		 (regionp (region-active-p)))
	(delete-region begin-point end-point)
	(when (= (line-beginning-position)
			 (line-end-position))
	  (backward-delete-char 1))
	(when regionp
	  (goto-char begin-point)
	  (newline)
	  (goto-char begin-point)
	  (when (= (line-beginning-position)
			   (line-end-position))
		(backward-delete-char 1)))
	(goto-char (line-beginning-position))
	(forward-line 1)
	(forward-line (or index 1))
	(insert cur-line)
	(newline)
	(forward-line -1)))

(defun vwe-move-line--move-down (&optional index)
  "Move line down INDEX."
  (let* ((begin-point (vwe-move-line--begin-point))
		 (end-point (vwe-move-line--end-point))
		 (cur-line (vwe-move-line--line-copy))
		 (regionp (region-active-p)))
	(delete-region begin-point end-point)
	(when (= (line-beginning-position)
			 (line-end-position))
	  (delete-char 1))
	(when regionp
	  (goto-char begin-point)
	  (newline)
	  (goto-char begin-point)
	  (when (= (line-beginning-position)
			   (line-end-position))
		(backward-delete-char 1)))
	(forward-line (or index 1))
	(if regionp
		(progn
		  (goto-char (line-end-position))
		  (newline)
		  (insert cur-line))
	  (goto-char (line-beginning-position))
	  (insert cur-line)
	  (newline))
	(forward-line -1)))

(defun vwe-move-line--up (&optional index)
  "Move line up INDEX."
  (interactive)
  (vwe-move-line--move-up (if (numberp index) (* -1 index) -1)))

(defun vwe-move-line--down (&optional index)
  "Move line down INDEX."
  (interactive)
  (vwe-move-line--move-down (or index 1)))

(defun vwe-move-line-mode-enable ()
  "Enable mode."
  (define-key vwe-move--keymap (kbd "M-[") #'vwe-move-line--up)
  (define-key vwe-move--keymap (kbd "M-]") #'vwe-move-line--down))

(defun vwe-move-line-mode-disable ()
  "Disable mode.")

;;;###autoload
(define-minor-mode vwe-move-line-mode
  "Vwe move line minor mode."
  :group 'vwe-move
  :keymap vwe-move--keymap
  :global t
  (if vwe-move-line-mode
	  (vwe-move-line-mode-enable)
	(vwe-move-line-mode-disable)))

;;
;; change point
;;
(defvar vwe-move-change--skip-step
  8
  "Skip step.")

(defvar-local vwe-move-change--current-step
  1
  "Skip step.")

(defvar vwe-move-change--keymap
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Chage keymap.")

(defvar vwe-move-change--skip-step-keymap
  (let ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd "C-,") (lambda ()
									 (interactive)
									 (setq vwe-move-change--current-step
										   (1+ vwe-move-change--current-step))
									 (vwe-move-change--goto-last
									  vwe-move-change--current-step)))
	keymap)
  "Chage keymap.")

(defun vwe-move-change--format-undo-list-element (element)
  "Format an Emacs 27.1 style `buffer-undo-list' ELEMENT to regular edit."
  (let* ((formatted element)
		 (args (last element))
		 (formatp (and (consp element)
					   (eq (car element) 'apply)
					   (not (functionp (cadr element)))
					   (eq (nth 4 element) 'undo--wrap-and-run-primitive-undo)))
		 (args-formatp (and formatp
							(consp args)
							(= (length args) 1)
							(consp (car args))
							(= (length (car args)) 1)
							(consp (caar args))
							(numberp (car (caar args)))
							(numberp (cdr (caar args))))))
	(when args-formatp (setq formatted (caar args)))
	formatted))

(defun vwe-move-change--get-undo-list-element-point (element)
  "Get `buffer-undo-list' ELEMENT point."
  (unless (numberp element)
	(let* ((format-element (vwe-move-change--format-undo-list-element element)))
	  (cond ((numberp format-element) format-element) ; position
			((atom format-element) nil) ; command boundary
			((numberp (car format-element)) (cdr format-element)) ; insertion
			((stringp (car format-element)) (abs (cdr format-element))) ; deletion
			((null (car format-element)) (nthcdr 4 format-element)) ; text property
			((atom (car format-element)) nil) ; file modifiy time
			(t nil)))))

(defun vwe-move-change--find-change-point (step)
  "Find last STEP change point."
  (let* ((found-point -1)
		 (undo-len (if buffer-undo-list (length buffer-undo-list) 0))
		 (match-index 0))
	(catch 'break
	  (dotimes (i undo-len)
		(let* ((undo-elem (nth i buffer-undo-list))
			   (pos (vwe-move-change--get-undo-list-element-point undo-elem)))
		  (when  pos
			(setq match-index (1+ match-index))
			(when (= step match-index)
			  (setq found-point pos)
			  (message "last change: %s" vwe-move-change--current-step)
			  (throw 'break found-point))))))
	found-point))

;;;###autoload
(defun vwe-move-change--goto-last (&optional step)
  "Goto last or STEP change."
  (interactive)
  (let* ((undo-status (and buffer-undo-list (not (eq buffer-undo-list t))))
		 (found-point 0))
	(if undo-status
		(progn
		  (let* ((undo-step (or step 1)))
			(setq found-point (vwe-move-change--find-change-point undo-step))
			(if (> found-point 0) (goto-char found-point) (message "none or last change"))))
	  (message "Buffer has not been changed or undo is disabled"))
	found-point))

(defun vwe-move-change--goto-last-cycle ()
  "Goto last change."
  (interactive)
  (let* ((pos (vwe-move-change--goto-last vwe-move-change--current-step)))
	(when pos
	  (set-transient-map vwe-move-change--skip-step-keymap
						 t
						 (lambda ()
						   (setq vwe-move-change--current-step 1))))))

(defun vwe-move-change--enable ()
  "Enable change."
  (define-key vwe-move-change--keymap (kbd "C-,") #'vwe-move-change--goto-last)
  (define-key vwe-move-change--keymap (kbd "C-.") #'vwe-move-change--goto-last-cycle))

(defun vwe-move-change--disable ()
  "Disable change.")

(define-minor-mode vwe-move-change-mode
  "Mark line change mode."
  :group 'vwe-mark
  :keymap vwe-move-change--keymap
  :global t
  (if vwe-move-change-mode
	  (vwe-move-change--enable)
	(vwe-move-change--disable)))

;;
;; win move
;;
(defun vwe-move--windmove-up()
  "Selected current up window."
  (interactive)
  (condition-case nil (windmove-up)
    (error (condition-case nil (windmove-down) (error)))))

(defun vwe-move--windmove-down()
  "Selected current down window."
  (interactive)
  (condition-case nil (windmove-down)
    (error (condition-case nil (windmove-up) (error)))))

(defun vwe-move--windmove-right()
  "Selected current right window."
  (interactive)
  (condition-case nil (windmove-right)
    (error (condition-case nil (windmove-left) (error)))))

(defun vwe-move--windmove-left()
  "Selected current left window."
  (interactive)
  (condition-case nil (windmove-left)
    (error (condition-case nil (windmove-right) (error)))))

;;
;; buffer swap
;;
(defun vwe-move--swap-up-buffer ()
  "Swap current and up buffer."
  (interactive)
  (let ((current-window (selected-window))
		(current-buffer (buffer-name))
		(swaped-window)
		(swaped-buffer))
	(vwe-move--windmove-up)
	(setq swaped-window (selected-window)
		  swaped-buffer (buffer-name))
	(when (and (not (string= swaped-buffer current-buffer)))
	  (set-window-buffer swaped-window current-buffer)
	  (set-window-buffer current-window swaped-buffer))))

(defun vwe-move--swap-down-buffer ()
  "Swap current and down buffer."
  (interactive)
  (let ((current-window (selected-window))
		(current-buffer (buffer-name))
		(swaped-window)
		(swaped-buffer))
	(vwe-move--windmove-down)
	(setq swaped-window (selected-window)
		  swaped-buffer (buffer-name))
	(when (and (not (string= swaped-buffer current-buffer)))
	  (set-window-buffer swaped-window current-buffer)
	  (set-window-buffer current-window swaped-buffer))))

(defun vwe-move--swap-right-buffer ()
  "Swap current and right buffer."
  (interactive)
  (let ((current-window (selected-window))
		(current-buffer (buffer-name))
		(swaped-window nil)
		(swaped-buffer nil))
	(vwe-move--windmove-right)
	(setq swaped-window (selected-window)
		  swaped-buffer (buffer-name))
	(when (and (not (string= swaped-buffer current-buffer)))
	  (set-window-buffer swaped-window current-buffer)
	  (set-window-buffer current-window swaped-buffer))))

(defun vwe-move--swap-left-buffer()
  "Swap current and right buffer."
  (interactive)
  (let ((current-window (selected-window))
		(current-buffer (buffer-name))
		(swaped-window nil)
		(swaped-buffer nil))
	(vwe-move--windmove-left)
	(setq swaped-window (selected-window)
		  swaped-buffer (buffer-name))
	(when (and (not (string= swaped-buffer current-buffer)))
	  (set-window-buffer swaped-window current-buffer)
	  (set-window-buffer current-window swaped-buffer))))

;;
;; buffer switch
;;
(defvar vwe-move--switch-side-buffer-name
  "*vwe-layout:switch-side-tmp-buffer*"
  "Switch side buffer name.")

(defvar vwe-move--switch-buffer-keymap
  (let* ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd "q") #'vwe-move--kill-switch-side-buffer)
	(define-key keymap (kbd "n") #'next-line)
	(define-key keymap (kbd "p") #'previous-line)
	(define-key keymap (kbd "k") #'vwe-move--switch-buffer-kill-current-line-buffer)
	(define-key keymap (kbd "s") (lambda () (interactive)
								   (if (fboundp 'swiper)
									   (swiper)
									 (isearch-forward))))
	(define-key keymap (kbd "TAB") #'vwe-move--switch-buffer)
	(define-key keymap (kbd "1") (lambda () (interactive)
								   (vwe-move--switch-buffer-goto-line 1)))
	(define-key keymap (kbd "2") (lambda () (interactive)
								   (vwe-move--switch-buffer-goto-line 2)))
	(define-key keymap (kbd "3") (lambda () (interactive)
								   (vwe-move--switch-buffer-goto-line 3)))
	(define-key keymap (kbd "4") (lambda () (interactive)
								   (vwe-move--switch-buffer-goto-line 4)))
	(define-key keymap (kbd "5") (lambda () (interactive)
								   (vwe-move--switch-buffer-goto-line 5)))
	(define-key keymap (kbd "6") (lambda () (interactive)
								   (vwe-move--switch-buffer-goto-line 6)))
	(define-key keymap (kbd "7") (lambda () (interactive)
								   (vwe-move--switch-buffer-goto-line 7)))
	(define-key keymap (kbd "8") (lambda () (interactive)
								   (vwe-move--switch-buffer-goto-line 8)))
	(define-key keymap (kbd "9") (lambda () (interactive)
								   (vwe-move--switch-buffer-goto-line 9)))

	keymap)
  "Switch buffer keymap.")

(defface vwe-move--default-face
  '((t (:inherit 'default :weight bold)))
  "Default face.")

(defface vwe-move--info-face
  '((t (:foreground "DarkOrange" :weight bold)))
  "Info face.")

(defface vwe-move--success-face
  '((t (:foreground "SpringGreen" :weight bold)))
  "Success face.")

(defface vwe-move--warning-face
  '((t (:foreground "yellow" :weight bold)))
  "Warning face.")

(defface vwe-move--error-face
  '((t (:foreground "DarkRed" :weight bold)))
  "Error face.")

(defface vwe-move--button-face
  '((t (:foreground "SkyBlue" :weight bold)))
  "Button face.")

(defvar vwe-move--switch-side-show-status
  'buffer
  "Switch side buffer show content `buffer' or `temp' buffer.")

(defvar vwe-move--switch-buffer-buffer-store-list
  nil
  "Buffer store list.")

(defvar vwe-move--switch-buffer-temp-buffer-store-list
  nil
  "Buffer store list.")

(defvar vwe-move--switch-buffer-current-buffer
  nil
  "Current buffer.")

(defun vwe-move--switch-buffer ()
  "Switch buffer."
  (interactive)
  (cond
   ((equal vwe-move--switch-side-show-status 'buffer)
	(setq vwe-move--switch-side-show-status 'temp)
	(vwe-move--display-switch-side-buffer))
   ((equal vwe-move--switch-side-show-status 'temp)
	(setq vwe-move--switch-side-show-status 'buffer)
	(vwe-move--display-switch-side-buffer t))
   (t
	(setq vwe-move--switch-side-show-status 'temp)
	(vwe-move--display-switch-side-buffer))))

(defun vwe-move--build-switch-buffer-headerline ()
  "Build switch buffer headerline."
  (let* ((allbuf (length (buffer-list)))
		 (buflen (length (vwe-move--get-buffer-list)))
		 (tmpbuflen (length (vwe-move--get-buffer-list t)))
		 (hidebuflen (- allbuf buflen tmpbuflen))
		 (split (propertize (format " | ")
							'face 'vwe-move--default-face)))
	(concat
	 (propertize (format "Vwe switch buffer:")
				 'face 'vwe-move--success-face)
	 split
	 (propertize (format "total " )
				 'face 'vwe-move--default-face)
	 (propertize (format "%d" allbuf)
				 'face 'vwe-move--success-face)
	 split
	 (propertize (format "buffer ")
				 'face 'vwe-move--default-face)
	 (propertize (format "%d" buflen)
				 'face 'vwe-move--info-face)
	 split
	 (propertize (format "* buffer ")
				 'face 'vwe-move--default-face)
	 (propertize (format "%d" tmpbuflen)
				 'face 'vwe-move--warning-face)
	 split
	 (propertize (format "hide buffer ")
				 'face 'vwe-move--default-face)
	 (propertize (format "%d" hidebuflen)
				 'face 'vwe-move--button-face)
	 split
	 (propertize (format "buffer/*buffer ")
				 'face 'vwe-move--default-face)
	 (propertize (format "[TAB]")
				 'face 'vwe-move--info-face)
	 (propertize (format " quit ")
				 'face 'vwe-move--default-face)
	 (propertize (format "[q]")
				 'face 'vwe-move--info-face))))

(defun vwe-move--display-switch-side-buffer (&optional tmp?)
  "Make switch buffer cmd buffer.
TMP is tmp buffer."
  (let* ((buffer (if (get-buffer vwe-move--switch-side-buffer-name)
					 (get-buffer vwe-move--switch-side-buffer-name)
				   (get-buffer-create vwe-move--switch-side-buffer-name)))
		 ;; (buffer-length (length (vwe-move--get-buffer-list)))
		 ;; (tmpbuf-length (length (vwe-move--get-buffer-list t)))
		 ;; (headerline (concat (format "buffer total %d | buffer %d | tmp buffer %d | hide buffer %d"
		 ;; (length (buffer-list)) buffer-length tmpbuf-length (- (length (buffer-list)) buffer-length tmpbuf-length))))
		 (headerline (vwe-move--build-switch-buffer-headerline))
		 (bufname-list (if tmp?
						   (mapcar #'buffer-name (vwe-move--get-buffer-list t))
						 (mapcar #'buffer-name (vwe-move--get-buffer-list))))
		 (alist '((window-width . vwe-key--max-width)
				  (window-height . fit-window-to-buffer)
				  (direction . 'down)
				  (slot . 0))))
	(setq vwe-move--switch-buffer-current-buffer (current-buffer))
	(when buffer
	  (with-current-buffer buffer
		(read-only-mode -1)
		(erase-buffer)
		(setq-local mode-line-format nil
					word-wrap nil
					show-trailing-whitespace nil
					header-line-format headerline)
		(vwe-move--switch-buffer-mode 1)
		(setq vwe-move--switch-buffer-temp-buffer-store-list nil
			  vwe-move--switch-buffer-buffer-store-list nil)
		(dotimes (i (length bufname-list))
		  (if tmp?
			  (setq vwe-move--switch-buffer-temp-buffer-store-list (append vwe-move--switch-buffer-temp-buffer-store-list
																		   (list (cons (format "%d" (1+ i)) (nth i bufname-list)))))
			(setq vwe-move--switch-buffer-buffer-store-list (append vwe-move--switch-buffer-buffer-store-list
																	(list (cons (format "%d" (1+ i)) (nth i bufname-list))))))
		  (insert-button (concat (propertize (if (< i 9)
												 (format "%d:" (1+ i))
											   (format ".:"))
											 'face 'vwe-move--warning-face)
								 (propertize (nth i bufname-list)
											 'face 'vwe-move--info-face)
								 "|"
								 (propertize (if (buffer-file-name (get-buffer (nth i bufname-list)))
												 (buffer-file-name (get-buffer (nth i bufname-list)))
											   " tmp buffer ")
											 'face 'vwe-move--success-face)
								 (unless (= (1+ i) (length bufname-list)) "\n"))
						 'action (lambda(_)
								   (vwe-move--kill-switch-side-buffer)
								   (switch-to-buffer (nth i bufname-list)))
						 'follow-link t))
		(goto-char (point-min))
		(read-only-mode t))
	  (select-window (display-buffer-in-side-window buffer alist)))))

(defun vwe-move--switch-get-buffer-name-by-id (id)
  "Get buffer name by ID."
  (when (get-buffer vwe-move--switch-side-buffer-name)
	(let* ((buffer-store-list))
	  (if (equal vwe-move--switch-side-show-status 'temp)
		  (setq buffer-store-list vwe-move--switch-buffer-buffer-store-list)
		(setq buffer-store-list vwe-move--switch-buffer-temp-buffer-store-list))
	  (when buffer-store-list
		(cdr (assoc id buffer-store-list))))))

(defun vwe-move--switch-buffer-goto-line (id)
  "Go to line by ID."
  (when (get-buffer vwe-move--switch-side-buffer-name)
	(goto-line id)))

(defun vwe-move--switch-buffer-kill-current-line-buffer ()
  "Kill current line buffer."
  (interactive)
  (when (get-buffer vwe-move--switch-side-buffer-name)
	(let* ((id (number-to-string (line-number-at-pos))))
	  (when (and (vwe-move--switch-get-buffer-name-by-id id)
				 (get-buffer (vwe-move--switch-get-buffer-name-by-id id)))
		(unless (equal vwe-move--switch-buffer-current-buffer (get-buffer (vwe-move--switch-get-buffer-name-by-id id)))
		  (kill-buffer (vwe-move--switch-get-buffer-name-by-id id)))
		(vwe-move--display-switch-side-buffer)))))

(defun vwe-move--kill-switch-side-buffer ()
  "Kill side buffer."
  (interactive)
  (let* ((buffer (get-buffer vwe-move--switch-side-buffer-name)))
	(when buffer
	  (delete-windows-on buffer)
	  (kill-buffer buffer)
	  (setq vwe-move--switch-side-show-status 'buffer
			vwe-move--switch-buffer-current-buffer nil))))

(defun vwe-move--buffer-list-filter (regexp &optional self?)
  "Find buffer list of REGEXP.
SELF is include curretn buffer."
  (seq-filter 'bufferp
			  (mapcar
			   (lambda (item)
				 (if (and (string-match regexp (buffer-name item))
						  (not (equal vwe-move--switch-side-buffer-name
									  (buffer-name item))))
					 (if self?
						 item
					   (unless (equal item (current-buffer)) item))))
			   (buffer-list))))

(defun vwe-move--get-buffer-list (&optional tmp? self?)
  "Get buffer list.
TMP is asterisk buffers.
SELF is include curretn buffer."
  (if tmp?
	  (vwe-move--buffer-list-filter "^*" self?)
	(vwe-move--buffer-list-filter "^[^\s*]" self?)))

(defun vwe-move--switch-buffer-enable ()
  "Enable mode.")

(defun vwe-move--switch-buffer-disable ()
  "Disable mode."
  (vwe-move--kill-switch-side-buffer))

;;;###autoload
(define-minor-mode vwe-move--switch-buffer-mode
  "Mode."
  :keymap vwe-move--switch-buffer-keymap
  (if vwe-move--switch-buffer-mode
	  (vwe-move--switch-buffer-enable)
    (vwe-move--switch-buffer-disable)))

;;
;; goto line and preview
;;
(defvar vwe-move-goto-line--origin-window
  nil
  "Origin window.")

(defvar vwe-move-goto-line--origin-window-line
  nil
  "Origin window line.")

(defvar vwe-move-goto-line--origin-window-point
  nil
  "Origin window point.")

(defvar vwe-move-goto-line--preveiw-line
  nil
  "Preveiw goto line.")

(defvar vwe-move-goto-line--keymap
  (let ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd "M-* g") #'vwe-move-goto-line--goto)
	(define-key keymap (kbd "M-* r") #'vwe-move-goto-line--recovery)
	keymap)
  "Move to mark map.")

(defun vwe-move-goto-line--preview ()
  "Preview goto line."
  (let* ((cur-input (thing-at-point 'line)))
	(when cur-input
	  (setq vwe-move-goto-line--preveiw-line (string-to-number cur-input)))
	(save-selected-window
	  (when (and vwe-move-goto-line--origin-window
				 vwe-move-goto-line--preveiw-line)
		(select-window vwe-move-goto-line--origin-window)
		(unless (zerop vwe-move-goto-line--preveiw-line)
		  (goto-char (point-min))
		  (forward-line (1- vwe-move-goto-line--preveiw-line)))))))

;;;###autoload
(defun vwe-move-goto-line--goto ()
  "Preview goto LINE."
  (interactive)
  (let* ((win (selected-window))
		 (line (line-number-at-pos))
		 (pos (point)))
	(setq vwe-move-goto-line--preveiw-line nil
		  vwe-move-goto-line--origin-window win
		  vwe-move-goto-line--origin-window-line line
		  vwe-move-goto-line--origin-window-point pos)
	(unwind-protect
		(setq vwe-move-goto-line--preveiw-line (read-number "goto:" (line-number-at-pos)))
	  (set-window-point vwe-move-goto-line--origin-window
						vwe-move-goto-line--origin-window-point))
	(unless (zerop vwe-move-goto-line--preveiw-line)
	  (goto-char (point-min))
	  (forward-line (1- vwe-move-goto-line--preveiw-line)))))

(defun vwe-move-goto-line--recovery ()
  "Preview recovery."
  (interactive)
  (if vwe-move-goto-line--origin-window
	  (progn
		(select-window vwe-move-goto-line--origin-window)
		(when (and vwe-move-goto-line--origin-window
				   (numberp vwe-move-goto-line--origin-window-point))
		  (set-window-point vwe-move-goto-line--origin-window
							vwe-move-goto-line--origin-window-point)))
	(message "not found origin window.")))

(defun vwe-move-goto-line--minibuffer-setup ()
  "Preview hook for minibuffer command."
  (when (memq this-command '(vwe-move-goto-line--goto))
    (add-hook 'post-command-hook #'vwe-move-goto-line--preview nil t)))

(defun vwe-move-goto-line-enable ()
  "Enable preview."
  (add-hook 'minibuffer-setup-hook 'vwe-move-goto-line--minibuffer-setup))

(defun vwe-move-goto-line--disable ()
  "Disable preview."

  (remove-hook 'minibuffer-setup-hook 'vwe-move-goto-line--minibuffer-setup))

;;;###autoload
(define-minor-mode vwe-move-goto-line-mode
  "Mark line preview mode."
  :group 'vwe-mark
  :keymap vwe-move-goto-line--keymap
  :global t
  (if vwe-move-goto-line-mode
	  (vwe-move-goto-line-enable)
	(vwe-move-goto-line--disable)))

;; =============================================================================
;; mark and goto
;; word/symbol,line,expression
;; =============================================================================
;;
;; marker point
;;
(defvar vwe-move-marker-point--keymap
  (let* ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd "M-* m") #'vwe-move-marker-point--marker)
	(define-key keymap (kbd "M-* t") #'vwe-move-marker-point--goto-marker)
	(define-key keymap (kbd "M-* c") #'vwe-move-marker-point--clear-marker)
	keymap)
  "Last mark point.")

(defface vwe-move-marker-point--face
  '((t (:inherit 'error :weight bold)))
  "Posit mark face.")

(defvar-local vwe-move-marker-point--marker-point-overlay
  nil
  "Last mark point overlay.")

(defun vwe-move-marker-point--line-indent (&optional goto)
  "Get current point or GOTO point line indentation."
  (interactive)
  (unless goto (setq goto (point)))
  (let* ((ind-pos (save-excursion (goto-char goto) (back-to-indentation) (point))))
	ind-pos))

(defun vwe-move-marker-point--line-eol (&optional goto)
  "Get current point or GOTO point line eol."
  (interactive)
  (unless goto (setq goto (point)))
  (let* ((eol-pos (save-excursion
					(goto-char goto)
					(end-of-line)
					(skip-chars-backward " \t" (vwe-move-marker-point--line-indent))
					(point))))
	eol-pos))

(defun vwe-move-marker-point--marker ()
  "Mark current point."
  (interactive)
  (let* ((overlay (make-overlay (1- (point)) (point))))
	(setq vwe-move-marker-point--marker-point-overlay overlay)
	(overlay-put overlay 'after-string
				 (propertize (format "[M]")
							 'display '((raise 0.5) (height 0.8))
							 'face 'vwe-move-marker-point--face))))

(defun vwe-move-marker-point--goto-marker ()
  "Got mark list first point."
  (interactive)
  (if vwe-move-marker-point--marker-point-overlay
	  (let* ((goto-point (overlay-end vwe-move-marker-point--marker-point-overlay)))
		(condition-case nil
			(progn (when (numberp goto-point) (goto-char goto-point)))
		  (error nil)))
	(message "current buffer not found mark point.")))

(defun vwe-move-marker-point--clear-marker ()
  "Clear mark."
  (interactive)
  (delete-overlay vwe-move-marker-point--marker-point-overlay))

(define-minor-mode vwe-move-marker-point-mode
  "Point mode."
  :group 'vwe-mark
  :global t
  :keymap vwe-move-marker-point--keymap)

(defvar vwe-move-mark--keymap
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Move to mark map.")

(defvar vwe-move-mark--show-mark-keymap
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Move to mark map.")

(defvar vwe-move-mark-show--keymap
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Move to mark map.")

(defface vwe-move-mark--mark-face
  '((t (:inherit 'error :inverse-video nil)))
  "Position makr hint face."
  :group 'vwe-mark)

(defvar vwe-move-mark--mark-list
  nil
  "Mark list, like '(point1 ... pointn)).")

(defvar vwe-move-mark--is-back
  nil
  "Is backward.")

(defvar vwe-move-mark--type
  'word
  "Mark and goto type, `word', `symbol', `line', `expression', `paragraph'.")

(defun vwe-move-mark--mark ()
  "Mark word, forward."
  (interactive)
  (setq vwe-move-mark--mark-list nil)
  (save-excursion
	(let* ((win-start (window-start))
		   (win-end (window-end))
		   (mark-pos (point))
		   (mark-overlay nil)
		   (mark-index 0)
		   (next t))
	  (vwe-move-mark-show-mode-enable)
	  (when (= mark-pos (point-min))
		(setq mark-pos (1+ mark-pos)))
	  (when (= mark-pos (point-max))
		(setq mark-pos (1- mark-pos)))

	  (while next
		(if vwe-move-mark--is-back
			(progn
			  (cond
			   ((eq vwe-move-mark--type 'word) (backward-word))
			   ((eq vwe-move-mark--type 'symbol) (forward-symbol -1))
			   ((eq vwe-move-mark--type 'line) (forward-line -1))
			   ((eq vwe-move-mark--type 'expression) (backward-sexp))
			   ((eq vwe-move-mark--type 'paragraph) (backward-paragraph))))
		  (progn
			(cond
			 ((eq vwe-move-mark--type 'word) (forward-word))
			 ((eq vwe-move-mark--type 'symbol) (forward-symbol 1))
			 ((eq vwe-move-mark--type 'line) (forward-line))
			 ((eq vwe-move-mark--type 'expression) (forward-sexp))
			 ((eq vwe-move-mark--type 'paragraph) (forward-paragraph)))))

		(if (or (>= mark-pos win-end) (<= mark-pos win-start))
			(setq next nil)
		  (when next
			(setq mark-pos (point)
				  mark-overlay (make-overlay (1- mark-pos) mark-pos)
				  mark-index (1+ mark-index)
				  vwe-move-mark--mark-list (append
											vwe-move-mark--mark-list
											(list (list mark-index
														mark-pos
														mark-overlay))))
			(overlay-put mark-overlay 'after-string
						 (propertize (format "%d" mark-index)
									 'display '((raise 0.5) (height 0.7))
									 'face 'vwe-move-mark--mark-face)))))

	  (dotimes (i (if (> (length vwe-move-mark--mark-list) 10)
					  10
					(length vwe-move-mark--mark-list)))
		(define-key vwe-move-mark-show--keymap
		  (kbd (number-to-string i))
		  (lambda ()
			(interactive)
			(vwe-move-mark--goto-mark i))))
	  (set-transient-map vwe-move-mark--show-mark-keymap nil nil))))

(defun vwe-move-mark--goto-mark (&optional arg)
  "Goto mark ARG."
  (interactive)
  (message "index is %S" arg)
  (message "list length is %d" (length vwe-move-mark--mark-list))
  (let* ((goto (if arg arg (read-number "goto:"))))
	(if vwe-move-mark--is-back
		(progn
		  (cond
		   ((eq vwe-move-mark--type 'word) (backward-word goto))
		   ((eq vwe-move-mark--type 'symbol) (forward-symbol (* goto -1)))
		   ((eq vwe-move-mark--type 'line) (forward-line (* goto -1)))
		   ((eq vwe-move-mark--type 'expression) (backward-sexp goto))
		   ((eq vwe-move-mark--type 'paragraph) (backward-paragraph goto))
		   ))
	  (progn
		(cond
		 ((eq vwe-move-mark--type 'word) (forward-word goto))
		 ((eq vwe-move-mark--type 'symbol) (forward-symbol goto))
		 ((eq vwe-move-mark--type 'line) (forward-line goto))
		 ((eq vwe-move-mark--type 'expression) (forward-sexp goto))
		 ((eq vwe-move-mark--type 'paragraph) (forward-paragraph goto)))))

	(vwe-move-mark--remove-mark-list)
	(setq vwe-move-mark--is-back nil)
	(vwe-move-mark-show-mode-disable)))

(defun vwe-move-mark--remove-mark-list ()
  "Remove mark list."
  (interactive)
  (when vwe-move-mark--mark-list
	(mapc #'delete-overlay
		  (mapcar (lambda (m)
					(caddr m))
				  vwe-move-mark--mark-list))
	(setq vwe-move-mark--mark-list nil))
  (when vwe-move-mark-show-mode
	(vwe-move-mark-show-mode-disable)))

(defun vwe-move-mark--mark-line ()
  "Mark line."
  (interactive)
  (setq vwe-move-mark--type 'line)
  (vwe-move-mark--remove-mark-list)
  (vwe-move-mark--mark))

(defun vwe-move-mark--forward-mark-line ()
  "Forward mark line."
  (interactive)
  (setq vwe-move-mark--is-back nil)
  (vwe-move-mark--mark-line))

(defun vwe-move-mark--backward-mark-line ()
  "Backward mark line."
  (interactive)
  (setq vwe-move-mark--is-back t)
  (vwe-move-mark--mark-line))

(defun vwe-move-mark--mark-word ()
  "Mark word."
  (interactive)
  (setq vwe-move-mark--type 'word)
  (vwe-move-mark--remove-mark-list)
  (vwe-move-mark--mark))

(defun vwe-move-mark--forward-mark-word ()
  "Forward mark word."
  (interactive)
  (setq vwe-move-mark--is-back nil)
  (vwe-move-mark--mark-word))

(defun vwe-move-mark--backward-mark-word ()
  "Backward mark word."
  (interactive)
  (setq vwe-move-mark--is-back t)
  (vwe-move-mark--mark-word))

(defun vwe-move-mark-show-mode-enable ()
  "Enable mode."
  (vwe-move-mark-show-mode 1)
  (setq buffer-read-only t)
  (add-hook 'vwe-editor-edit-mode-hook #'vwe-move-mark-show-mode-disable)
  (define-key vwe-move-mark-show--keymap (kbd "g") #'vwe-move-mark--goto-mark)
  (define-key vwe-move-mark-show--keymap (kbd "q") #'vwe-move-mark--remove-mark-list)
  (define-key vwe-move-mark-show--keymap (kbd "c-g") #'vwe-move-mark--remove-mark-list))

(defun vwe-move-mark-show-mode-disable ()
  "Disable mode."
  (setq buffer-read-only nil)
  (remove-hook 'vwe-editor-edit-mode-hook #'vwe-move-mark-show-mode-disable)
  (vwe-move-mark-show-mode -1)
  (vwe-move-mark--remove-mark-list))

(define-minor-mode vwe-move-mark-show-mode
  "Mark and goto show mode."
  :group 'vwe-move
  :keymap vwe-move-mark-show--keymap
  :global nil
  )

(defun vwe-move-mark-mode-enable ()
  "Enable mode."
  (define-key vwe-move-mark--keymap (kbd "C->") #'vwe-move-mark--forward-mark-word)
  (define-key vwe-move-mark--keymap (kbd "C-<") #'vwe-move-mark--backward-mark-word)
  (define-key vwe-move-mark--keymap (kbd "M-{") #'vwe-move-mark--backward-mark-line)
  (define-key vwe-move-mark--keymap (kbd "M-}") #'vwe-move-mark--forward-mark-line))

(defun vwe-move-mark-mode-disable ()
  "Disable mode.")

(define-minor-mode vwe-move-mark-mode
  "Mark and goto mode."
  :group 'vwe-move
  :keymap vwe-move-mark--keymap
  :global t
  (if vwe-move-mark-mode
	  (vwe-move-mark-mode-enable)
	(vwe-move-mark-mode-disable)))

;;
;; mode
;;
(defun vwe-move-mode-enable ()
  "Enable mode."
  (vwe-move-line-mode 1)
  (vwe-move-change-mode 1)
  (vwe-move-goto-line-mode 1)
  (vwe-move-marker-point-mode 1)
  (vwe-move-mark-mode 1))

(defun vwe-move-mode-disable ()
  "Disable mode."
  (vwe-move-line-mode -1)
  (vwe-move-change-mode -1)
  (vwe-move-goto-line-mode -1)
  (vwe-move-marker-point-mode -1)
  (vwe-move-mark-mode -1))

;;;###autoload
(define-minor-mode vwe-move-mode
  "Vwe tags minor mode."
  :group 'vwe-move
  :keymap vwe-move--keymap
  :global t
  (if vwe-move-mode
	  (vwe-move-mode-enable)
	(vwe-move-mode-disable)))

(provide 'vwe-move)
;;; vwe-move.el ends here
