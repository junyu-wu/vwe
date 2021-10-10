;;; vwe-editor.el ---  Vwe editor         -*- lexical-binding: t; -*-

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

(defgroup vwe-editor nil
  "Vwe editor."
  :prefix "vwe-editor--"
  :group 'vwe)

(defvar-local vwe-editor-view--keymap
  (let* ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd "SPC") (lambda () (interactive) (vwe-editor-edit-mode t)))
	(define-key keymap (kbd "p") #'previous-line)
	(define-key keymap (kbd "n") #'next-line)
	(define-key keymap (kbd "v") #'scroll-up-command)
	(define-key keymap (kbd "V") #'scroll-down-command)
	(define-key keymap (kbd "k") #'kill-buffer)
	keymap)
  "Keymap.")

(defvar-local vwe-editor-edit--keymap
  (let* ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd "ESC SPC") (lambda () (interactive) (vwe-editor-view-mode t)))
	keymap)
  "Keymap.")

(defvar vwe-editor--mode-activate?
  t
  "Is editor mode?.")

(defvar vwe-editor--mode-type-list
  '('edit 'view)
  "Current buffer editor type.")

(defvar-local vwe-editor--mode-current-type
  'view
  "Current buffer type.")

(defvar-local vwe-editor--timer
  nil
  "Idle time.")

(defvar-local vwe-editor--idle-time
  15
  "Idle time.")

(defvar vwe-editor--idle-toggle-mode
  nil
  "Idle time.")

(defvar vwe-editor--save-toggle-mode
  nil
  "Save buffer toggle mode.")

(defvar vwe-editor--filter-regexp
  "^[*|\s*]"
  "Filter regexp.")

(defvar vwe-editor--last-buffer
  nil
  "Last view buffer.")

(defvar vwe-editor-view-mode-hook
  nil
  "View mode hook.")

(defvar vwe-editor-edit-mode-hook
  nil
  "View mode hook.")

(defvar vwe-editor--ignore-func
  nil
  "Ignore func.")

(defun vwe-editor--find-file (func &rest args)
  "Find FUNC and ARGS file."
  (apply func args)
  (unless (vwe-editor--tmp-buffer-p (current-buffer))
	(vwe-editor-view-mode t)))

(defun vwe-editor-view--save-buffer (&rest _)
  "Active view mode."
  (when vwe-editor--save-toggle-mode
	(vwe-editor-view-mode t)))

(defun vwe-editor-view--active ()
  "Active view mode."
  (interactive)
  (vwe-editor-view-mode t))

(defun vwe-editor--tmp-buffer-p (buffer)
  "Is temp BUFFER."
  (when (and (bufferp buffer)
			 (string-match vwe-editor--filter-regexp
						   (buffer-name buffer)))
	t))

(defun vwe-editor-view--switch-buffer (func &rest args)
  "Call FUNC and ARGS to Switch buffer activate view mode."
  (let* ((from (current-buffer))
		 (to (get-buffer (car args))))
	(cond
	 ((vwe-editor--tmp-buffer-p from) (apply func args))
	 ((vwe-editor--tmp-buffer-p to) (apply func args))
	 ((equal from to) (apply func args))
	 (t (apply func args) (vwe-editor-view-mode t)))))

(defun vwe-editor-view--select-window (func &rest args)
  "Call FUNC and ARGS select window activate view mode."
  (let* ((from (current-buffer))
		 (selected (selected-window))
		 (window (apply func args))
		 (to (window-buffer)))
	(unless (vwe-editor--tmp-buffer-p from)
	  (setq vwe-editor--last-buffer from))
	(when (and (windowp window)
			   (bufferp to)
			   (not (equal to vwe-editor--last-buffer))
			   (not (equal selected window))
			   (not (vwe-editor--tmp-buffer-p from))
			   (not (vwe-editor--tmp-buffer-p to))
			   (not (equal (buffer-name from) (buffer-name to))))
	  (vwe-editor-view-mode t))))

(defun vwe-editor--filter-func ()
  "Filter function."
  (unless (and (not vwe-editor--mode-activate?)
			   (vwe-editor--tmp-buffer-p (current-buffer)))
	(let* ((cmd (or (command-remapping this-original-command)
					this-original-command)))
	  (when (and (memq cmd vwe-editor--ignore-func))
		;; (vwe-editor-mode -1)
		(setq vwe-editor--mode-activate? nil)))))

(defun vwe-editor--restart-mode ()
  "Restart editor mode."
  ;; (vwe-editor-mode 1)
  (setq vwe-editor--mode-activate? t))

;;;###autoload
(define-minor-mode vwe-editor-view-mode
  "Editor view mode."
  :group 'vwe-editor
  :keymap vwe-editor-view--keymap
  (if vwe-editor-view-mode
	  (progn
		(when vwe-editor--mode-activate?
		  (setq vwe-editor--mode-current-type 'view
				buffer-read-only t)
		  (vwe-editor-edit-mode -1))
		(run-hooks 'vwe-editor-view-mode-hook))))

;;;###autoload
(define-minor-mode vwe-editor-edit-mode
  "Editor edit mode."
  :group 'vwe-editor
  :keymap vwe-editor-edit--keymap
  (if vwe-editor-edit-mode
	  (when vwe-editor--mode-activate?
		(setq vwe-editor--mode-current-type 'edit
			  buffer-read-only nil)
		(vwe-editor-view-mode -1)
		(when vwe-editor--idle-toggle-mode
		  (setq vwe-editor--timer (run-with-idle-timer
								   vwe-editor--idle-time
								   t
								   #'vwe-editor-view--active)))
		(add-hook 'pre-command-hook 'vwe-editor--filter-func t t)
		(add-hook 'post-command-hook 'vwe-editor--restart-mode t t)
		(run-hooks 'vwe-editor-edit-mode-hook))
	(when (timerp vwe-editor--timer)
	  (cancel-timer vwe-editor--timer))
	(remove-hook 'pre-command-hook 'vwe-editor--filter-func)
	(remove-hook 'post-command-hook 'vwe-editor--restart-mode)))

(defun vwe-editor-enable ()
  "Enable editor mode."
  (interactive)
  (setq vwe-editor--mode-activate? t
		vwe-editor--last-buffer nil)
  (if (vwe-editor--tmp-buffer-p (current-buffer))
	  (vwe-editor-edit-mode 1)
	(vwe-editor-view-mode 1))
  (advice-add #'find-file :around #'vwe-editor--find-file)
  (advice-add #'save-buffer :after #'vwe-editor-view--save-buffer)
  (advice-add #'switch-to-buffer :around #'vwe-editor-view--switch-buffer)
  (advice-add #'select-window :around #'vwe-editor-view--select-window))

(defun vwe-editor-disable ()
  "Disable editor mode."
  (interactive)
  (vwe-editor-view-mode -1)
  (vwe-editor-edit-mode -1)
  (advice-remove #'find-file #'vwe-editor--find-file)
  (advice-remove #'save-buffer #'vwe-editor-view--save-buffer)
  (advice-remove #'switch-to-buffer #'vwe-editor-view--switch-buffer)
  (advice-remove #'select-window #'vwe-editor-view--select-window)
  (setq vwe-editor--mode-activate? nil
		vwe-editor--mode-current-type nil
		buffer-read-only nil))

;;;###autoload
(define-minor-mode vwe-editor-mode
  "Vwe editor minor mode."
  :init-value nil
  :group 'vwe-editor
  :global t
  (if vwe-editor-mode
	  (vwe-editor-enable)
	(vwe-editor-disable)))

(provide 'vwe-editor)
;;; vwe-editor.el ends here
