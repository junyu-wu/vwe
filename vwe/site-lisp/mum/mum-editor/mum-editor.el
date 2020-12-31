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
  "Mum key."
  :prefix "mum-editor--"
  :group 'mum)

(defvar-local mum-editor--keymap
  (let* ((keymap (make-sparse-keymap)))
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

(defun mum-editor--find-file (filename &optional wildcards)
  "Find FILENAME file or WILDCARDS."
  (interactive
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  (when (file-exists-p filename)
	(let* ((file-buffer (find-file-noselect filename nil nil wildcards)))
	  (with-current-buffer file-buffer
		(setq-local buffer-read-only t
					mum-editor--mode-current-type 'view))
	  (switch-to-buffer file-buffer)
	  (if (and mum-editor--mode-activate? (eq mum-editor--mode-current-type 'view))
		  (define-key (current-local-map) (kbd "SPC SPC")
			(lambda () (interactive)
			  (setq-local buffer-read-only nil
						  mum-editor--mode-current-type 'edit)
			  (define-key (current-local-map) (kbd "SPC SPC") nil)
			  (define-key (current-local-map) (kbd "SPC") nil)))))))

;;;###autoload
(define-minor-mode mum-editor-mode
  "Mum editor minor mode."
  :init-value nil
  :group 'mum-editor
  :global t
  (if mum-editor-mode
	  (progn
		(setq mum-editor--mode-activate? t)
		(mum-key--close-buffer))
	(setq mum-editor--mode-activate? nil)))

(provide 'mum-editor)
;;; mum-editor.el ends here
