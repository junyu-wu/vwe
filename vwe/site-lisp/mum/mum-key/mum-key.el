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
  :prefix "mum-key/"
  :group 'mum)

(defcustom mum-key/init-buffer-hook
  '()
  "Hook run when mum-key buffer is initialized."
  :group 'mum-key
  :type 'hook)

(defvar mum-key/key-buffer-name
  "*mum-key*"
  "Mum key mode default buffer name.")

(defvar mum-key/key-buffer-handle
  nil
  "Hold key buffer.")

(defvar mum-key/origin-buffer-handle
  nil
  "Hold origin buffer.")

(defvar mum-key/key-buffer-map
  (let ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd "q") #'mum-key/close-buffer-side-window)
	keymap)
  "Key buffer map.")

(defvar mum-key/side-window-location
  'bottom
  "Side window location.")

(defun mum-key/generate-context-header (&optional list)
  "Generate context header of LIST."
  (when (listp list)
	))

(defun mum-key/generate-context-body (&optional list)
  "Generate context body of LIST."
  (when (listp list)
	))

(defun mum-key/generate-context-footer (&optional list)
  "Generate context footer of LIST."
  (when (listp list)
	))

(defun mum-key/generate-buffer-content (&optional list)
  "Generate buffer content of LIST."
  (when (listp list)
	(let* ((content)
		   (header (mum-key/generate-context-header))
		   (body (mum-key/generate-context-body))
		   (footer (mum-key/generate-context-footer)))
	  (setq content (concat "mum-key\n" header body footer "\nends here"))
	  content)))

(defun mum-key/get-key-buffer ()
  "Make key buffer, buffer is `mum-key/key-buffer-name'."
  (when (buffer-live-p mum-key/key-buffer-handle)
	(delete-windows-on mum-key/key-buffer-handle))

  (setq mum-key/key-buffer-handle (get-buffer-create mum-key/key-buffer-name))

  (with-current-buffer mum-key/key-buffer-handle
    (let (message-log-max)
      (toggle-truncate-lines 1)
      (message ""))

	(insert (mum-key/generate-buffer-content))
	(define-key mum-key/key-buffer-map (kbd "a")
	  (lambda ()
		(interactive)
		(mum-key/close-buffer-side-window)
		))
	(use-local-map mum-key/key-buffer-map)
	;; (use-local-map nil)
	(setq-local cursor-type nil
				cursor-in-non-selected-windows nil
				mode-line-format nil
				word-wrap nil
				show-trailing-whitespace nil
				mode-line-format nil
				major-mode 'special-mode
				buffer-read-only t
				header-line-format nil)
	(run-hooks 'mum-key/init-buffer-hook))
  mum-key/key-buffer-handle)

(defun mum-key/show-buffer-side-window ()
  "Show key side-window."
  (interactive)
  (setq mum-key/origin-buffer-handle (current-buffer))
  (let* ((buffer (mum-key/get-key-buffer))
		 (window)
		 (height 10)
		 (width (frame-width))
		 (alist `((window-width . ,width)
				  (window-height . ,height)
				  (side . ,mum-key/side-window-location)
				  (slot . 0))))
	(setq window (display-buffer-in-side-window buffer alist))
	(when (window-live-p window)
	  (select-window window))
	))

(defun mum-key/close-buffer-side-window ()
  "Close key buffer side window."
  (interactive)
  (kill-buffer mum-key/key-buffer-handle)
  (setq mum-key/key-buffer-handle nil))


(global-set-key (kbd "M-RET") #'mum-key/show-buffer-side-window)

;;;###autoload
(define-minor-mode mum-key-mode
  "Mum key minor mode."
  :init-value nil
  :keymap nil
  :lighter ""
  :group 'mum-key
  :global t
  (if mum-key-mode ))

(provide 'mum-key)
;;; mum-key.el ends here
