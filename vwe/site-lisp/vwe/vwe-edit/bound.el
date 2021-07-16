;; bound.el ---                                     -*- lexical-binding: t; -*-

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

;;
;; edit column bound
;;
(defface vwe-edit-bound--show-face
  '((t (:background nil :foreground "#c1ffc1" :weight normal :slant normal)))
  "Column bound face."
  :group 'vwe-edit)

(defcustom vwe-edit-bound--fill-column
  nil
  "Fill column."
  :group 'vwe-edit
  :type 'string)

(defcustom vwe-edit-bound--border-character
  ?|
  "Fill character."
  :group 'vwe-edit
  :type 'character)

(defvar vwe-edit-bound--blank-character
  ?\uE001
  "Blank character.")

(defun vwe-edit-bound--build-char (limit)
  "Build fill column show char with LIMIT."
  (when (and limit (numberp limit))
	(let* (
		   (char nil)
		   (face 'vwe-edit-bound--show-face)
		   (cursorp t))
	  (cond
	   ((> limit 0) (setq char (concat
								(propertize
								 (char-to-string vwe-edit-bound--blank-character)
								 'display '(space :align-to fill-column))
								(char-to-string vwe-edit-bound--border-character)))))
	  (when char (propertize char
							 'face face
							 'cursor cursorp)))))

(defun vwe-edit-bound--show (&optional start end _ignored)
  "Show edit bound between START and END."
  ;; (interactive)
  (let* ((fill-num (1+ (or vwe-edit-bound--fill-column fill-column)))
		 (start (or start (window-start (selected-window))))
		 (end (or end (window-end (selected-window)))))
	(save-match-data
	  (save-excursion
		(let* ((inhibit-point-motion-hooks t)
			   (ol)
			   (limit)
			   (char))
		  (goto-char start)
		  (vwe-edit-bound--remove-between-overlays start end)
		  (condition-case nil
		   (while (search-forward "\n" end t)
			 (goto-char (match-beginning 0))
			 (setq limit (- fill-num (current-column))
				   char (vwe-edit-bound--build-char limit)
				   ol (make-overlay (match-beginning 0) (match-beginning 0)))
			 (when char
			   (overlay-put ol 'after-string char)
			   (overlay-put ol 'ebol t))
			 (goto-char (match-end 0)))
		   (error
			(goto-char (window-start)))))))))

(defun vwe-edit-bound--remove-between-overlays (start end)
  "Remvoe overlays between START and END."
  (mapc #'(lambda (ol)
			(if (overlay-get ol 'ebol)
				(delete-overlay ol)))
        (overlays-in start end)))

(defun vwe-edit-bound--remove-overlays ()
  "Remvoe overlays between START and END."
  (mapc #'(lambda (ol)
			(if (overlay-get ol 'ebol)
				(delete-overlay ol)))
        (overlays-in (point-min) (point-max))))

(defun vwe-edit-bound--redraw (start end &optional ignored)
  "Redraw between START END and IGNORED."
  (vwe-edit-bound--remove-overlays)
  (vwe-edit-bound--show start end ignored))

(defun vwe-edit-bound--redraw-command ()
  "Redraw WIN begin to START for scroll END."
  (vwe-edit-bound--remove-overlays)
  (vwe-edit-bound--show))

(defun vwe-edit-bound--redraw-for-scroll (win start)
  "Redraw WIN begin to START for scroll END."
  (vwe-edit-bound--remove-overlays)
  (vwe-edit-bound--redraw-window win start))

(defun vwe-edit-bound--redraw-window (win &optional start)
  "Redraw WIN begin to START."
  (vwe-edit-bound--show (or start (window-start win))
						(window-end win)))

(defun vwe-edit-bound-mode-enable ()
  "Enable mode."

  (when (boundp 'line-move-visual)
	(if (local-variable-p 'line-move-visual)
		(setq line-move-visual nil)
	  (set (make-local-variable 'line-move-visual) nil)))
  (setq truncate-lines t)
  (vwe-edit-bound--show)

  (add-hook 'after-change-functions #'vwe-edit-bound--redraw t t)
  (add-hook 'before-change-functions #'vwe-edit-bound--redraw nil t)
  (add-hook 'window-scroll-functions #'vwe-edit-bound--redraw-for-scroll nil t)
  (add-hook 'window-configuration-change-hook #'vwe-edit-bound--show)
  (add-hook 'post-self-insert-hook #'vwe-edit-bound--show nil t)
  (add-hook 'post-command-hook #'vwe-edit-bound--redraw-command nil t))

(defun vwe-edit-bound-mode-disable ()
  "Disable mode."
  (vwe-edit-bound--remove-overlays)
  (remove-hook 'after-change-functions #'vwe-edit-bound--redraw)
  (remove-hook 'before-change-functions #'vwe-edit-bound--redraw)
  (remove-hook 'window-scroll-functions #'vwe-edit-bound--redraw-for-scroll)
  (remove-hook 'window-configuration-change-hook #'vwe-edit-bound--show)
  (remove-hook 'post-command-hook #'vwe-edit-bound--redraw-command))

;;;###autoload
(define-minor-mode vwe-edit-bound-mode
  "Bound minor mode."
  :group 'vwe-edit
  :keymap nil
  :global nil
  (if vwe-edit-bound-mode
	  (vwe-edit-bound-mode-enable)
	(vwe-edit-bound-mode-disable)))

(provide 'bound)
;;; bound.el ends here
