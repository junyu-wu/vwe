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
	(when (= (line-beginning-position) (line-end-position)) (backward-delete-char 1))
	(when regionp (goto-char begin-point) (newline) (goto-char begin-point) (when (= (line-beginning-position) (line-end-position)) (backward-delete-char 1)))
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
	(when (= (line-beginning-position) (line-end-position)) (delete-char 1))
	(when regionp (goto-char begin-point) (newline) (goto-char begin-point) (when (= (line-beginning-position) (line-end-position)) (backward-delete-char 1)))
	(forward-line (or index 1))
	(if regionp
		(progn
		  (goto-char (line-end-position))
		  (newline)
		  (insert cur-line))
	  (goto-char (line-beginning-position))
	  (insert cur-line)
	  (newline))
	(forward-line -1)
	))

(defun vwe-move-line--up (&optional index)
  "Move line up INDEX."
  (interactive)
  (vwe-move-line--move-up (if (numberp index) (* -1 index) -1)))

(defun vwe-move-line--down (&optional index)
  "Move line down INDEX."
  (interactive)
  (vwe-move-line--move-down (or index 1)))

;;
;; mode
;;
(defun vwe-move-mode-enable ()
  "Enable mode."
  (define-key vwe-move--keymap (kbd "M-n") #'vwe-move-line--down)
  (define-key vwe-move--keymap (kbd "M-p") #'vwe-move-line--up))

(defun vwe-move-mode-disable ()
  "Disable mode.")

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
