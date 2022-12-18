;;; vwe-mark.el ---   Vwe mark          -*- lexical-binding: t; -*-

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

(defgroup vwe-mark nil
  "Customization group for beacon."
  :group 'vwiss-vwe
  :prefix "vwe-mark--")

(defvar vwe-mark--mode-keymap
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Mark map.")

(defvar-local vwe-mark-multi-edit--list
  nil
  "Multi edit list.")

(defvar-local vwe-mark-multi-edit--overlay-list
  '()
  "Multi edit overlay list.")

(defvar-local vwe-mark-multi-edit--rect-bound-overlay-list
  '()
  "Multi edit rect overlay list.")

(defvar-local vwe-mark-multi-edit--current-point
  nil
  "Rect start point.")

(defvar-local vwe-mark-multi-edit--rect-start-point
  nil
  "Rect start point.")

(defface vwe-mark-multi-edit--default-face
  '((t (:foreground nil :background "#20b2aa" :bold t)))
  "Multi edit marked face."
  :group 'vwiss-vwe)

(defface vwe-mark-multi-edit--rect-face
  '((t (:foreground nil :background "#b22222" :bold t)))
  "Multi edit rect marked face."
  :group 'vwiss-vwe)

;;;###autoload
(defun vwe-mark-multi-edit--chars ()
  "Edit multiple chars in range."
  (interactive)
  (when (region-active-p)
	(let* ((chars '())
		   (start (region-beginning))
		   (end (region-end)))
	  (save-excursion
		(goto-char end)
		(while (>= (point) start)
		  (add-to-list 'chars (cons (1- (point)) (point)))
		  (backward-char)))
	  (setq vwe-mark-multi-edit--list chars)
	  (goto-char end)
	  (vwe-mark-multi-edit--kmacro-start))))

;;;###autoload
(defun vwe-mark-multi-edit--words ()
  "Edit multiple words in range."
  (interactive)
  (when (region-active-p)
	(let* ((words '())
		   (start (region-beginning))
		   (end (region-end)))
	  (save-excursion
		(goto-char end)
		(while (>= (point) start)
		  (when (bounds-of-thing-at-point 'word)
			(add-to-list 'words (bounds-of-thing-at-point 'word) t))
		  (backward-word)))
	  (setq vwe-mark-multi-edit--list words)
	  (goto-char (cdr (car vwe-mark-multi-edit--list)))
	  (vwe-mark-multi-edit--kmacro-start))))

;;;###autoload
(defun vwe-mark-multi-edit--symbols ()
  "Edit multiple symbols in range."
  (interactive)
  (when (region-active-p)
	(let* ((symbol '())
		   (start (region-beginning))
		   (end (region-end)))
	  (save-excursion
		(goto-char end)
		(while (>= (point) start)
		  (when (bounds-of-thing-at-point 'symbol)
			(add-to-list 'symbol (bounds-of-thing-at-point 'symbol) t))
		  (backward-sexp)))
	  (setq vwe-mark-multi-edit--list symbol)
	  (goto-char (cdr (car vwe-mark-multi-edit--list)))
	  (vwe-mark-multi-edit--kmacro-start))))

;;;###autoload
(defun vwe-mark-multi-edit--lines ()
  "Edit multiple lines in range."
  (interactive)
  (when (region-active-p)
	(let* ((line '())
		   (start (region-beginning))
		   (end (region-end)))
	  (save-excursion
		(goto-char end)
		(while (< (point) end)
          (unless (string-match-p "^[ ]*$" (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
            (when (bounds-of-thing-at-point 'line)
              (add-to-list 'vwe-mark-multi-edit--list (bounds-of-thing-at-point 'line) t)))
          (forward-line))
		(while (>= (point) start)
		  (when (bounds-of-thing-at-point 'line)
			(add-to-list 'line (cons (car (bounds-of-thing-at-point 'line)) (1- (cdr (bounds-of-thing-at-point 'line)))) t))
		  (backward-word)))
	  (setq vwe-mark-multi-edit--list line)
	  (goto-char end)
	  (vwe-mark-multi-edit--kmacro-start))))

(defun vwe-mark-multi-edit--make-overlay ()
  "Make overlay."
  (when vwe-mark-multi-edit--list
	(dolist (item vwe-mark-multi-edit--list)
	  (let* ((overlay (make-overlay (car item) (cdr item))))
		(overlay-put overlay 'face 'vwe-mark-multi-edit--default-face)
		(add-to-list 'vwe-mark-multi-edit--overlay-list overlay t)))))

(defun vwe-mark-multi-edit--kmacro-start ()
  "Start kmacro."
  (when (region-active-p)
	(deactivate-mark))
  (remove-hook 'post-command-hook #'vwe-mark-multi-edit--rect-monitor-post-command t)
  (vwe-mark-multi-edit--make-overlay)
  (advice-add 'keyboard-quit :before #'vwe-mark-multi-edit--exit)
  (kmacro-start-macro 0))

(defun vwe-mark-multi-edit--exit ()
  "Keyboard-quit advice."
  (end-kbd-macro)
  (vwe-mark-multi-edit--apply-all)
  (advice-remove 'keyboard-quit #'vwe-mark-multi-edit--exit)

  (when vwe-mark-multi-edit--overlay-list
	(mapc #'delete-overlay vwe-mark-multi-edit--overlay-list))

  (when vwe-mark-multi-edit--rect-bound-overlay-list
	(mapc #'delete-overlay vwe-mark-multi-edit--rect-bound-overlay-list))

  (setq vwe-mark-multi-edit--list nil
		vwe-mark-multi-edit--overlay-list '()
		vwe-mark-multi-edit--rect-bound-overlay-list '()))

(defun vwe-mark-multi-edit--apply-all ()
  "Apply all kmacro."
  (when vwe-mark-multi-edit--overlay-list
	(dolist (item (cdr vwe-mark-multi-edit--overlay-list))
	  (save-excursion
		(when (overlay-end item)
		  (goto-char (overlay-end item))
		  (call-last-kbd-macro))))))

;;;###autoload
(defun vwe-mark-multi-edit--rect-mark ()
  "Rect mark."
  (interactive)
  (setq vwe-mark-multi-edit--rect-start-point (point))
  (setq vwe-mark-multi-edit--rect-bound-overlay-list '())
  (add-hook 'post-command-hook #'vwe-mark-multi-edit--rect-monitor-post-command nil t))

(defun vwe-mark-multi-edit--rect-monitor-post-command ()
  "Rect monitor post command."
  (if (eq this-command 'keyboard-quit)
	  (progn
		(mapc #'delete-overlay vwe-mark-multi-edit--rect-bound-overlay-list)
		(remove-hook 'post-command-hook #'vwe-mark-multi-edit--rect-monitor-post-command t))
	(when vwe-mark-multi-edit--rect-start-point
	  (mapc #'delete-overlay vwe-mark-multi-edit--rect-bound-overlay-list)
	  (let* ((start-point (save-excursion
							(goto-char vwe-mark-multi-edit--rect-start-point)
							(cons (line-number-at-pos) (current-column))))
			 (start-line (car start-point))
			 (start-column (cdr start-point))
			 (current-line (line-number-at-pos))
			 (current-column (current-column))
			 (rect-start-line (min start-line current-line))
             (rect-end-line (max start-line current-line))
             (rect-start-column (min start-column current-column))
             (rect-end-column (max start-column current-column)))
		(dotimes (line (1+ (- rect-end-line rect-start-line)))
          (let ((overlay (make-overlay (save-excursion
                                         (goto-line (+ rect-start-line line))
                                         (move-to-column rect-start-column)
                                         (point))
                                       (save-excursion
                                         (goto-line (+ rect-start-line line))
                                         (move-to-column rect-end-column)
                                         (point)))))
            (overlay-put overlay 'face 'vwe-mark-multi-edit--rect-face)
            (add-to-list 'vwe-mark-multi-edit--rect-bound-overlay-list overlay t)))))))

(defun vwe-mark-multi-edit--rect-symbols ()
  "Multi edit rect symbols."
  (interactive)
  (when vwe-mark-multi-edit--rect-bound-overlay-list
	(dolist (bound-overlay vwe-mark-multi-edit--rect-bound-overlay-list)
	  (when (and (overlay-start bound-overlay) (overlay-end bound-overlay))
		(save-excursion
		  (goto-char (overlay-start bound-overlay))
		  (when (bounds-of-thing-at-point 'symbol)
			(add-to-list 'vwe-mark-multi-edit--list (bounds-of-thing-at-point 'symbol))))))
	(goto-char (cdr (car vwe-mark-multi-edit--list)))
	(vwe-mark-multi-edit--kmacro-start)))

(defun vwe-mark-multi-edit--rect-edit ()
  "Multi edit rect insert STR."
  (interactive)
  (when vwe-mark-multi-edit--rect-bound-overlay-list
	(dolist (bound-overlay vwe-mark-multi-edit--rect-bound-overlay-list)
	  (when (and (overlay-start bound-overlay) (overlay-end bound-overlay))
		(save-excursion
		  (goto-char (overlay-start bound-overlay))
		  (add-to-list 'vwe-mark-multi-edit--list (cons (point) (point)))
		  )))
	(goto-char (cdr (car vwe-mark-multi-edit--list)))
	(vwe-mark-multi-edit--kmacro-start)))

;;;###autoload
(defun vwe-mark-multi-edit--replace ()
  "Replace symbol or region range."
  (interactive)
  (let* ((str (if (region-active-p)
				  (buffer-substring (region-beginning) (region-end))
				(thing-at-point 'symbol)))
		 (point (point))
		 (len (length str))
		 (start (point-min))
		 (end (point-max))
		 (current-overlay (if (region-active-p)
							  (make-overlay (region-beginning) (region-end))
							(make-overlay (car (bounds-of-thing-at-point 'symbol))
										  (cdr (bounds-of-thing-at-point 'symbol))))))
	(save-excursion
	  (goto-char end)
	  (catch 'break
		(while (search-backward str nil t)
		  (let* ((search-start (point))
				 (search-end (+ search-start len)))
			(cond
			 ((= point search-start) (setq point (+ search-start len)))
			 ((= point search-end) (setq point search-end))
			 (t (add-to-list 'vwe-mark-multi-edit--list (cons search-start search-end)))))
		  (when (<= (point) start)
			(throw 'break nil)))))
	(goto-char point)
	(vwe-mark-multi-edit--kmacro-start)
	(overlay-put current-overlay 'face 'vwe-mark-multi-edit--rect-face)
	(add-to-list 'vwe-mark-multi-edit--overlay-list current-overlay)))

;;
;; mode
;;
(defun vwe-mark-mode-enable ()
  "Enable mode.")

(defun vwe-mark-mode-disable ()
  "Disable mode.")

;;;###autoload
(define-minor-mode vwe-mark-mode
  "Vwe mark minor mode."
  :group 'vwe-mark
  :keymap vwe-mark--mode-keymap
  :global t
  (if vwe-mark-mode
	  (vwe-mark-mode-enable)
	(vwe-mark-mode-disable)))

(provide 'vwe-mark)
;;; vwe-mark.el ends here
