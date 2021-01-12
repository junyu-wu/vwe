;;; mum-editor.el ---  Mum editor         -*- lexical-binding: t; -*-

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

(defgroup mum-editor nil
  "Mum editor."
  :prefix "mum-editor--"
  :group 'mum)

(defvar-local mum-editor-view--keymap
  (let* ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd "SPC") (lambda () (interactive) (mum-editor-edit-mode t)))
	keymap)
  "Keymap.")

(defvar-local mum-editor-edit--keymap
  (let* ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd "ESC ESC") (lambda () (interactive) (mum-editor-view-mode t)))
	keymap)
  "Keymap.")

(defvar mum-editor--mode-activate?
  t
  "Is editor mode?.")

(defvar mum-editor--mode-type-list
  '('edit 'view)
  "Current buffer editor type.")

(defvar-local mum-editor--mode-current-type
  'view
  "Current buffer type.")

(defvar-local mum-editor--timer
  nil
  "Idle time.")

(defvar-local mum-editor--idle-time
  15
  "Idle time.")

(defvar mum-editor--idle-toggle-mode
  nil
  "Idle time.")

(defvar mum-editor--save-toggle-mode
  nil
  "Save buffer toggle mode.")

(defvar mum-editor--filter-regexp
  "^[*|\s*]"
  "Filter regexp.")

(defun mum-editor--find-file (func &rest args)
  "Find FUNC and ARGS file."
  (apply func args)
  (mum-editor-view-mode t))

(defun mum-editor-view--save-buffer (&rest _)
  "Active view mode."
  (when mum-editor--save-toggle-mode
	(mum-editor-view-mode t)))

(defun mum-editor-view--active ()
  "Active view mode."
  (interactive)
  (mum-editor-view-mode t))

(defun mum-editor-view--switch-buffer (func &rest args)
  "Call FUNC and ARGS to Switch buffer activate view mode."
  (let* ((from (current-buffer))
		 (to (get-buffer (car args))))
	(cond
	 ((string-match mum-editor--filter-regexp (buffer-name from)) (apply func args))
	 ((string-match mum-editor--filter-regexp (buffer-name to)) (apply func args))
	 ((equal from to) (apply func args))
	 (t (apply func args) (mum-editor-view-mode t)))))

(defun mum-editor-view--select-window (func &rest args)
  "Call FUNC and ARGS select window activate view mode."
  (let* ((from (current-buffer))
		 (to)
		 (selected (selected-window))
		 (window (apply func args)))
	(when (windowp window)
	  (setq to (window-buffer))
	  (when (and (bufferp to)
				 (not (equal selected window))
				 (not (string-match mum-editor--filter-regexp (buffer-name from)))
				 (not (string-match mum-editor--filter-regexp (buffer-name to)))
				 (not (equal (buffer-name from) (buffer-name to))))
		(mum-editor-view-mode t)))))

(define-minor-mode mum-editor-view-mode
  "Editor view mode."
  :group 'mum-editor
  :keymap mum-editor-view--keymap
  (if mum-editor-view-mode
	  (when mum-editor--mode-activate?
		(setq buffer-read-only t
			  mum-editor--mode-current-type 'view)
		(mum-editor-edit-mode -1))))

(define-minor-mode mum-editor-edit-mode
  "Editor edit mode."
  :group 'mum-editor
  :keymap mum-editor-edit--keymap
  (if mum-editor-edit-mode
	  (when mum-editor--mode-activate?
		(setq buffer-read-only nil
			  mum-editor--mode-current-type 'edit)
		(mum-editor-view-mode -1)
		(when mum-editor--idle-toggle-mode
		  (setq mum-editor--timer (run-with-idle-timer mum-editor--idle-time t #'mum-editor-view--active))))
	(when (timerp mum-editor--timer) (cancel-timer mum-editor--timer))))

(defun mum-editor-enable ()
  "Enable editor mode."
  (interactive)
  (setq mum-editor--mode-activate? t)
  (advice-add #'find-file :around #'mum-editor--find-file)
  (advice-add #'save-buffer :after #'mum-editor-view--save-buffer)
  (advice-add #'switch-to-buffer :around #'mum-editor-view--switch-buffer)
  (advice-add #'select-window :around #'mum-editor-view--select-window))

(defun mum-editor-disable ()
  "Disable editor mode."
  (interactive)
  (mum-editor-view-mode -1)
  (mum-editor-edit-mode -1)
  (advice-remove #'find-file #'mum-editor--find-file)
  (advice-remove #'save-buffer #'mum-editor-view--save-buffer)
  (advice-remove #'switch-to-buffer #'mum-editor-view--switch-buffer)
  (advice-remove #'select-window #'mum-editor-view--select-window)
  (setq mum-editor--mode-activate? nil
		buffer-read-only nil
		mum-editor--mode-current-type nil))

;;;###autoload
(define-minor-mode mum-editor-mode
  "Mum editor minor mode."
  :init-value nil
  :group 'mum-editor
  :global t
  (if mum-editor-mode (mum-editor-enable) (mum-editor-disable)))

(provide 'mum-editor)
;;; mum-editor.el ends here
