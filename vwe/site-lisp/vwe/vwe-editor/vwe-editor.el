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

(defvar-local vwe-editor--submode
  'view
  "Current buffer type.")

(defvar vwe-editor--ignore-func
  nil
  "Ignore func.")

(defvar vwe-editor--current-cmd
  nil
  "Current cmd.")

(defun vwe-editor--tmp-buffer-p (buffer)
  "Is temp BUFFER."
  (when (and (bufferp buffer)
			 (string-match "^[*|\s*]" (buffer-name buffer)))
	t))

(defun vwe-editor--filter-func ()
  "Filter function."
  (unless (vwe-editor--tmp-buffer-p (current-buffer))
	(let* ((cmd (or (command-remapping this-original-command)
				 	this-original-command
					this-command)))
	  (when (and (memq cmd vwe-editor--ignore-func))
		(vwe-editor--toggle-submode t)
		(setq vwe-editor--submode 'edit)
		(vwe-editor--toggle-submode)
		(message "cmd is: %s" vwe-editor--current-cmd)
		t))))

(defun vwe-editor--view-submode (&optional deactivate)
  "Activate or DEACTIVATE veiw submode."
  (if deactivate
	  (progn
		(setq buffer-read-only t)
		(local-unset-key (kbd "SPC SPC")))
	(progn
	  (unless (or (vwe-editor--tmp-buffer-p (current-buffer))
				  (vwe-editor--filter-func))
		(setq buffer-read-only t)
		(local-set-key (kbd "SPC SPC") (lambda ()
									 (interactive)
									 (vwe-editor--toggle-submode t)
									 (setq vwe-editor--submode 'edit)
									 (vwe-editor--toggle-submode)))))))

(defun vwe-editor--edit-submode (&optional deactivate)
  "Activate or DEACTIVATE edit submode."
  (if deactivate
	  (progn
		(local-unset-key (kbd "ESC SPC")))
	(progn
	  (setq buffer-read-only nil)
	  (local-set-key (kbd "ESC SPC") (lambda ()
									   (interactive)
									   (vwe-editor--toggle-submode t)
									   (setq vwe-editor--submode 'view)
									   (vwe-editor--toggle-submode))))))

(defun vwe-editor--toggle-submode (&optional deactivate)
  "Toggle editor submode activate or DEACTIVATE."
  (cond
   ((eq vwe-editor--submode 'view) (vwe-editor--view-submode deactivate))
   ((eq vwe-editor--submode 'edit) (vwe-editor--edit-submode deactivate))
   (t (vwe-editor--view-submode deactivate))))

(defun vwe-editor--find-file (func &rest args)
  "Find FUNC and ARGS file."
  (apply func args)
  (unless (and (vwe-editor--tmp-buffer-p (current-buffer)))
	(vwe-editor--toggle-submode t)
	(setq vwe-editor--submode 'view)
	(vwe-editor--toggle-submode)))

(defun vwe-editor--switch-buffer-advice (func &rest args)
  "Call FUNC and ARGS to Switch buffer activate view mode."
  (let* ((from (current-buffer))
		 (to (get-buffer (car args))))
	(cond
	 ((vwe-editor--tmp-buffer-p from) (apply func args))
	 ((vwe-editor--tmp-buffer-p to) (apply func args))
	 ((equal from to) (apply func args))
	 (t (apply func args)
		(vwe-editor--toggle-submode t)
		(setq vwe-editor--submode 'view)
		(vwe-editor--toggle-submode)))))

(defun vwe-editor-enable ()
  "Enable editor mode."
  (interactive)
  (vwe-editor--toggle-submode)
  (advice-add #'find-file :around #'vwe-editor--find-file)
  (advice-add #'switch-to-buffer :around #'vwe-editor--switch-buffer-advice))

(defun vwe-editor-disable ()
  "Disable editor mode."
  (interactive)
  (setq vwe-editor--submode nil)
  (vwe-editor--toggle-submode t)
  (advice-remove #'find-file #'vwe-editor--find-file)
  (advice-remove #'switch-to-buffer #'vwe-editor--switch-buffer-advice))

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
