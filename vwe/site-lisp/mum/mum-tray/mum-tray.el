;;; mum-tray--.el --- Mu tray     -*- lexical-binding: t; -*-

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

(defcustom mum-tray--segments
  '(mum-tray--begin
	mum-tray--segment-space
	mum-tray--segment-modified
	mum-tray--segment-space
	mum-tray--segment-buffer-name
	mum-tray--segment-major-mode
	mum-tray--segment-space
	mum-tray--segment-location
	mum-tray--segment-symbol-count-info
	mum-tray--segment-space
	mum-tray--segment-date
	mum-tray--segment-space
	mum-tray--end)
  "Segments."
  :type 'list
  :group 'mum-tray)

(defcustom mum-tray--refresh-idle-delay
  1
  "Update idle delay of message."
  :type 'double
  :group 'mum-tray)

(defface mum-tray--default-face
  '((t (:foreground "#B0BEC5" :weight bold)))
  "Default face."
  :group 'mum-tray)

(defface mum-tray--major-face
  '((t (:foreground "cyan" :weight bold)))
  "Default face."
  :group 'mum-tray)

(defface mum-tray--buffer-name-face
  '((t (:foreground "SpringGreen" :weight bold)))
  "Default face."
  :group 'mum-tray)

(defface mum-tray--success-face
  '((t (:inherit 'success)))
  "Face used for success status indicators in the mode-line."
  :group 'mum-tray)

(defface mum-tray--info-face
  '((t (:foreground "DarkOrange")))
  "Face for generic status indicators in the mode-line."
  :group 'mum-gray)

(defface mum-tray--warning-face
  '((t (:inherit 'warning)))
  "Face for warning status indicators in the mode-line."
  :group 'mum-tray)

(defface mum-tray--error-face
  '((t (:foreground "OrangeRed")))
  "Face for error status indicators in the mode-line."
  :group 'mum-tray)

(defvar mum-tray--active-p
  nil
  "Tray is active?")

(defvar mum-tray--timer
  nil
  "Timer.")

(defvar mum-tray--last-str
  nil
  "Tray last show str.")

(defvar mum-tray--begin-str
  "➥"
  "Tray last show str.")

(defvar mum-tray--end-str
  "¡"
  "Tray last show str.")

(defvar mum-tray--separator
  " "
  "Separator.")

(defvar-local mum-tray--show-message-buffer-name
  " *Minibuf-0*"
  "Show message buffer.")

(defvar-local mum-tray--buffer
  nil
  "Current buffer.")

;;
;; segment
;;

(defun mum-tray--segment-space ()
  "Space."
  " ")

(defun mum-tray--segemnt-separator ()
  "Separator."
  (propertize mum-tray--separator
			  'face 'mum-tray--default-face))

(defun mum-tray--begin ()
  "End."
  (propertize mum-tray--begin-str
			  'face 'mum-tray--success-face))

(defun mum-tray--end ()
  "End."
  (propertize mum-tray--end-str
			  'face 'mum-tray--info-face))

(defun mum-tray--segment-major-mode ()
  "Display current major mode."
  (unless (minibufferp (current-buffer))
	(propertize
	 (concat " "
			 (or (and (boundp 'delighted-modes)
					  (cadr (assq major-mode delighted-modes)))
				 (format-mode-line mode-name)))
	 'face 'mum-tray--major-face)))

(defun mum-tray--segment-buffer-name ()
  "Display current buffer name."
  (propertize (format "%s" (buffer-name))
			  'face 'mum-tray--buffer-name-face))

(defun mum-tray--segment-location ()
  "Location."
  (propertize (format "%s^%s:%s"
					  (format-mode-line "%p")
					  (format-mode-line "%l")
					  (format-mode-line "%c"))
			  'face 'mum-tray--default-face))

(defun mum-tray--segment-modified ()
  "BUFFER modification or read-only."
  (if (not (string-match-p "\\*.*\\*" (buffer-name)))
	  (let* ((read-only (and buffer-read-only (buffer-file-name mum-tray--buffer)))
			 (modified (buffer-modified-p mum-tray--buffer)))
		(propertize (if read-only "RO" (if modified "MD" "RW"))
					'face `(:inherit
							,(if modified 'mum-tray--error-face
							   (if read-only 'mum-tray--info-face
								 'mum-tray--warning-face)))))
	(propertize "**" 'face 'mum-tray--warning-face)))

(defun mum-tray--segment-symbol-count-info ()
  "Return Symbol Total And Current Symbol Index."
  (let* ((symbol (thing-at-point 'symbol))
		 (cur-bound (bounds-of-thing-at-point 'symbol)))
	(when (and symbol cur-bound)
	  (let*((cur (point))
			(cur-start (car cur-bound))
			(cur-end (cdr cur-bound))
			(cur-length (- cur-end cur-start))
			(total 0)
			(curindex 0))
		(save-excursion
		  (save-restriction
			(when symbol
			  (goto-char (point-min))
			  (setq symbol (concat "\\_<" (regexp-quote symbol) "\\_>"))
			  (while (re-search-forward symbol nil t)
				(let* ((bound (bounds-of-thing-at-point 'symbol))
					   (end (cdr bound))
					   (start (car bound))
					   (len (- end start)))
				  (if (= cur-length len)
					  (progn
						(setq total (+ total 1))
						(if (and (>= cur start) (<= cur end))
							(setq curindex total))))))
			  (propertize (format " T%d:C%d" total curindex)
						  'face 'mum-tray--default-face))))))))

(defun mum-tray--segment-date ()
  "Date."
  (propertize (format-time-string "%y-%m-%d %H:%M %a")
			  'face 'mum-tray--default-face))

;;
;; make message
;;

(defun mum-tray--segments-show-message (segments)
  "Make message with SEGMENTS."
  (let* ((str-list (mapcar (lambda (segment) (funcall `,segment)) segments))
		 (seg-msg ""))
	(dotimes (i (length str-list))
	  (let* ((str (nth i str-list)))
		(when (stringp str)
		  (setq seg-msg (concat seg-msg (format "%s" (nth i str-list)))))))
	(concat (make-string (max 0 (- (frame-width) (string-width seg-msg))) 32)
			seg-msg)))

(defun mum-tray--buffer-show-message (msg)
  "Make show message with MSG and segments."
  (let* ((seg-str (mum-tray--segments-show-message mum-tray--segments))
		 (blanks (- (frame-width) (+ (string-width (string-trim seg-str)) (string-width msg))))
		 (show-msg ""))
	(if (< blanks 0)
		(setq show-msg (concat msg "\n" seg-str))
	  (setq show-msg (concat msg (make-string blanks 32) (string-trim seg-str))))
	show-msg))

;;
;; insert/update message
;;

(defun mum-tray--insert-message ()
  "Insert Minibuf-0 buffer message."
  (let* ((msg (mum-tray--segments-show-message mum-tray--segments)))
	(with-current-buffer mum-tray--show-message-buffer-name
	  (erase-buffer) (insert msg))))

(defun mum-tray--update-message ()
  "Update message."
  (unless (current-message) (mum-tray--insert-message)))

;;
;; advice
;;

(defun mum-tray--message (func &rest args)
  "Message advice around FUNC and ARGS."
  (cond
   ((or (not mum-tray--active-p) inhibit-message) (apply func args))
   (t (if (car args)
		  (apply func "%s" (list (mum-tray--buffer-show-message (apply 'format args))))
		(apply func args)))))

(defun mum-tray--current-message (func &rest args)
  "Current message advice around FUNC and ARGS.."
  (if (car args)
	  (mum-tray--buffer-show-message (apply 'format args))
	(apply func args)))

(defun mum-tray--error-message (func &rest args)
  "Error message advice around FUNC and ARGS.."
  (if (car args)
	  (mum-tray--buffer-show-message (apply 'format args))
	(apply func args)))

(defun mum-tray--x-of-buffer (func &rest args)
  "Begin or end of buffer advice around FUNC and ARGS.."
  (apply func args)
  (message ""))

;;
;; mode
;;

(defun mum-tray-enable ()
  "Enable tray."
  (interactive)
  (setq mum-tray--active-p t
		mum-tray--timer (run-with-timer 0 mum-tray--refresh-idle-delay 'mum-tray--update-message))
  (add-hook 'focus-in-hook 'mum-tray--update-message)
  (advice-add #'message :around #'mum-tray--message)
  (advice-add #'current-message :around #'mum-tray--current-message)
  (advice-add #'error-message-string :around #'mum-tray--x-of-buffer)
  (advice-add #'beginning-of-buffer :around #'mum-tray--x-of-buffer)
  (advice-add #'end-of-buffer :around #'mum-tray--x-of-buffer))

(defun mum-tray-disable ()
  "Disable tray."
  (interactive)
  (when (timerp mum-tray--timer) (cancel-timer mum-tray--timer))
  (setq mum-tray--active-p nil
		mum-tray--timer nil)
  (remove-hook 'focus-in-hook 'mum-tray--update-message)
  (advice-remove 'message #'mum-tray--current-message)
  (advice-remove 'current-message #'mum-tray--error-message)
  (advice-remove #'error-message-string #'mum-tray--x-of-buffer)
  (advice-remove #'beginning-of-buffer #'mum-tray--x-of-buffer)
  (advice-remove #'end-of-buffer #'mum-tray--x-of-buffer)
  (force-mode-line-update)
  (redraw-display)
  (with-current-buffer mum-tray--show-message-buffer-name (erase-buffer)))

;;;###autoload
(define-minor-mode mum-tray-mode
  "Mum modeline tray minor mode."
  :init-value nil
  :keymap nil
  :lighter ""
  :group 'mum-tray
  :global t
  (if mum-tray-mode
	  (mum-tray-enable)
	(mum-tray-disable)))

(provide 'mum-tray)
;;; mum-tray.el ends here
