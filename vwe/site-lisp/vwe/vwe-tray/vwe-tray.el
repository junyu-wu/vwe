;;; vwe-tray--.el --- Mu tray     -*- lexical-binding: t; -*-

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

(defcustom vwe-tray--segments
  '(vwe-tray--begin
	vwe-tray--segment-space
	vwe-tray--segment-modified
	vwe-tray--segment-space
	vwe-tray--segment-buffer-name
	vwe-tray--segment-major-mode
	vwe-tray--segment-space
	vwe-tray--segment-location
	vwe-tray--segment-symbol-count-info
	vwe-tray--segment-space
	vwe-tray--segment-date
	vwe-tray--segment-space
	vwe-tray--end)
  "Segments."
  :type 'list
  :group 'vwe-tray)

(defcustom vwe-tray--refresh-idle-delay
  1
  "Update idle delay of message."
  :type 'double
  :group 'vwe-tray)

(defface vwe-tray--default-face
  '((t (:foreground "#B0BEC5" :weight bold)))
  "Default face."
  :group 'vwe-tray)

(defface vwe-tray--major-face
  '((t (:foreground "cyan" :weight bold)))
  "Default face."
  :group 'vwe-tray)

(defface vwe-tray--buffer-name-face
  '((t (:foreground "SpringGreen" :weight bold)))
  "Default face."
  :group 'vwe-tray)

(defface vwe-tray--success-face
  '((t (:inherit 'success)))
  "Face used for success status indicators in the mode-line."
  :group 'vwe-tray)

(defface vwe-tray--info-face
  '((t (:foreground "DarkOrange")))
  "Face for generic status indicators in the mode-line."
  :group 'vwe-gray)

(defface vwe-tray--warning-face
  '((t (:inherit 'warning)))
  "Face for warning status indicators in the mode-line."
  :group 'vwe-tray)

(defface vwe-tray--error-face
  '((t (:foreground "OrangeRed")))
  "Face for error status indicators in the mode-line."
  :group 'vwe-tray)

(defvar vwe-tray--active-p
  nil
  "Tray is active?")

(defvar vwe-tray--timer
  nil
  "Timer.")

(defvar vwe-tray--last-str
  nil
  "Tray last show str.")

(defvar vwe-tray--begin-str
  "i"
  "Tray last show str.")

(defvar vwe-tray--end-str
  "↵"
  "Tray last show str.")

(defvar vwe-tray--separator
  " "
  "Separator.")

(defvar-local vwe-tray--show-message-buffer-name
  " *Minibuf-0*"
  "Show message buffer.")

(defvar-local vwe-tray--buffer
  nil
  "Current buffer.")

;;
;; segment
;;

(defun vwe-tray--segment-space ()
  "Space."
  " ")

(defun vwe-tray--segemnt-separator ()
  "Separator."
  (propertize vwe-tray--separator
			  'face 'vwe-tray--default-face))

(defun vwe-tray--begin ()
  "End."
  (propertize vwe-tray--begin-str
			  'face 'vwe-tray--success-face))

(defun vwe-tray--end ()
  "End."
  (propertize vwe-tray--end-str
			  'face 'vwe-tray--info-face))

(defun vwe-tray--segment-major-mode ()
  "Display current major mode."
  (unless (minibufferp (current-buffer))
	(propertize
	 (concat " "
			 (or (and (boundp 'delighted-modes)
					  (cadr (assq major-mode delighted-modes)))
				 (format-mode-line mode-name)))
	 'face 'vwe-tray--major-face)))

(defun vwe-tray--segment-buffer-name ()
  "Display current buffer name."
  (propertize (format "%s" (buffer-name))
			  'face 'vwe-tray--buffer-name-face))

(defun vwe-tray--segment-location ()
  "Location."
  (propertize (format "%s^%s:%s"
					  (format-mode-line "%p")
					  (format-mode-line "%l")
					  (format-mode-line "%c"))
			  'face 'vwe-tray--default-face))

(defun vwe-tray--segment-modified ()
  "BUFFER modification or read-only."
  (if (not (string-match-p "\\*.*\\*" (buffer-name)))
	  (let* ((read-only (and buffer-read-only (buffer-file-name vwe-tray--buffer)))
			 (modified (buffer-modified-p vwe-tray--buffer)))
		(propertize (if read-only "RO" (if modified "MD" "RW"))
					'face `(:inherit
							,(if modified 'vwe-tray--error-face
							   (if read-only 'vwe-tray--info-face
								 'vwe-tray--warning-face)))))
	(propertize "**" 'face 'vwe-tray--warning-face)))

(defun vwe-tray--segment-symbol-count-info ()
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
						  'face 'vwe-tray--default-face))))))))

(defun vwe-tray--segment-date ()
  "Date."
  (propertize (format-time-string "%y-%m-%d %H:%M %a")
			  'face 'vwe-tray--default-face))

;;
;; make message
;;

(defun vwe-tray--segments-show-message (segments)
  "Make message with SEGMENTS."
  (let* ((str-list (mapcar (lambda (segment) (funcall `,segment)) segments))
		 (seg-msg ""))
	(dotimes (i (length str-list))
	  (let* ((str (nth i str-list)))
		(when (stringp str)
		  (setq seg-msg (concat seg-msg (format "%s" (nth i str-list)))))))
	(concat (make-string (max 0 (- (frame-width) (string-width seg-msg))) 32)
			seg-msg)))

(defun vwe-tray--buffer-show-message (msg)
  "Make show message with MSG and segments."
  (let* ((seg-str (vwe-tray--segments-show-message vwe-tray--segments))
		 (blanks (- (frame-width) (+ (string-width (string-trim seg-str)) (string-width msg)) 2))
		 (show-msg ""))
	(if (< blanks 0)
		(setq show-msg (concat msg "\n" seg-str))
	  (setq show-msg (concat msg (make-string blanks 32) (string-trim seg-str))))
	show-msg))

;;
;; insert/update message
;;

(defun vwe-tray--insert-message ()
  "Insert Minibuf-0 buffer message."
  (let* ((msg (vwe-tray--segments-show-message vwe-tray--segments)))
	(with-current-buffer vwe-tray--show-message-buffer-name
	  (erase-buffer) (insert msg))))

(defun vwe-tray--update-message ()
  "Update message."
  (unless (current-message) (vwe-tray--insert-message)))

;;
;; advice
;;

(defun vwe-tray--message (func &rest args)
  "Message advice around FUNC and ARGS."
  (cond
   ((or (not vwe-tray--active-p) inhibit-message) (apply func args))
   (t (if (car args)
		  (apply func "%s" (list (vwe-tray--buffer-show-message (apply 'format args))))
		(apply func args)))))

(defun vwe-tray--current-message (func &rest args)
  "Current message advice around FUNC and ARGS.."
  (if (car args)
	  (vwe-tray--buffer-show-message (apply 'format args))
	(apply func args)))

(defun vwe-tray--error-message (func &rest args)
  "Error message advice around FUNC and ARGS.."
  (if (car args)
	  (vwe-tray--buffer-show-message (apply 'format args))
	(apply func args)))

(defun vwe-tray--x-of-buffer (func &rest args)
  "Begin or end of buffer advice around FUNC and ARGS.."
  (apply func args)
  (message ""))

;;
;; mode
;;

(defun vwe-tray-enable ()
  "Enable tray."
  (interactive)
  (setq vwe-tray--active-p t
		vwe-tray--timer (run-with-timer 0 vwe-tray--refresh-idle-delay 'vwe-tray--update-message))
  (add-hook 'focus-in-hook 'vwe-tray--update-message)
  (advice-add #'message :around #'vwe-tray--message)
  (advice-add #'current-message :around #'vwe-tray--current-message)
  (advice-add #'error-message-string :around #'vwe-tray--x-of-buffer)
  (advice-add #'beginning-of-buffer :around #'vwe-tray--x-of-buffer)
  (advice-add #'end-of-buffer :around #'vwe-tray--x-of-buffer))

(defun vwe-tray-disable ()
  "Disable tray."
  (interactive)
  (when (timerp vwe-tray--timer) (cancel-timer vwe-tray--timer))
  (setq vwe-tray--active-p nil
		vwe-tray--timer nil)
  (remove-hook 'focus-in-hook 'vwe-tray--update-message)
  (advice-remove 'message #'vwe-tray--current-message)
  (advice-remove 'current-message #'vwe-tray--error-message)
  (advice-remove #'error-message-string #'vwe-tray--x-of-buffer)
  (advice-remove #'beginning-of-buffer #'vwe-tray--x-of-buffer)
  (advice-remove #'end-of-buffer #'vwe-tray--x-of-buffer)
  (force-mode-line-update)
  (redraw-display)
  (with-current-buffer vwe-tray--show-message-buffer-name (erase-buffer)))

;;;###autoload
(define-minor-mode vwe-tray-mode
  "Vwe modeline tray minor mode."
  :init-value nil
  :keymap nil
  :lighter ""
  :group 'vwe-tray
  :global t
  (if vwe-tray-mode
	  (vwe-tray-enable)
	(vwe-tray-disable)))

(provide 'vwe-tray)
;;; vwe-tray.el ends here
