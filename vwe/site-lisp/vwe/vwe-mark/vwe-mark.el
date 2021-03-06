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
(require 'cl)

(defgroup vwe-mark nil
  "Customization group for beacon."
  :group 'vwiss-vwe
  :prefix "vwe-mark--")

(defvar vwe-mark--mode-keymap
  (let ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd "C->") #'vwe-mark-position--forward-word)
	(define-key keymap (kbd "C-<") #'vwe-mark-position--backward-word)
	(define-key keymap (kbd "M-* p") #'vwe-mark-line-previous)
	(define-key keymap (kbd "M-* n") #'vwe-mark-line-next)
	keymap)
  "Move to mark map.")

;; =============================================================================
;; word or symbol
;;
;; =============================================================================
(defface vwe-mark--position--face
  '((t (:inherit 'error :inverse-video nil)))
  "Position makr hint face.")

(defmacro vwe-mark-position--create-keymap (cmd)
  "Create keymap CMD."
  `(dotimes (i 10)
	 (define-key vwe-mark-position--move-to-mark-map
	   (kbd (concat "" (number-to-string i)))
	   (lambda()
		 (interactive)
		 (vwe-mark-position--mark-move ,cmd (if (= i 0) 10 i))))))

(defvar vwe-mark-position--move-to-mark-map
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Move to mark map.")

(defun vwe-mark-position--show-mark-and-move (cmd)
  "Show before/after current cursor mark and move.
CMD before/after move func."
  (let ((mark-list))
	(save-mark-and-excursion
	  (call-interactively cmd)
	  (dotimes (i 10)
		(let ((before-point (point))
			  (mark-i (make-overlay (1- (point)) (point))))
		  (call-interactively cmd)
		  (when (/= before-point (point))
			(overlay-put mark-i 'after-string
						 (propertize (format "%d" (1+ i))
									 'display '((raise 0.5))
									 'face 'vwe-mark--position--face))
			(push mark-i mark-list)))))
	(vwe-mark-position--create-keymap cmd)
	(define-key vwe-mark-position--move-to-mark-map
	  (kbd "q") (lambda ()
				  (interactive)
				  (mapc #'delete-overlay mark-list)))
	(set-transient-map vwe-mark-position--move-to-mark-map
					   nil
					   (lambda()
						 (mapc #'delete-overlay mark-list)))))

(defun vwe-mark-position--mark-move (cmd &optional step)
  "Move cursor position.
CMD move func.
STEP move step."
  (let ((move-step 1))
	(when (and (numberp step) (> step 1))
	  (setq move-step step))
	(dotimes (_ move-step)
	  (call-interactively cmd))))

;;;###autoload
(defun vwe-mark-position--forward-word ()
  "Forward word."
  (interactive)
  (forward-word)
  (vwe-mark-position--show-mark-and-move #'forward-word))

;;;###autoload
(defun vwe-mark-position--backward-word ()
  "Backward word."
  (interactive)
  (backward-word)
  (vwe-mark-position--show-mark-and-move #'backward-word))

;; =============================================================================
;; paren
;;
;; =============================================================================

(defvar vwe-mark-paren--level-face-list
  '(("1" . '((t (:foreground "red"))))
	("2" . '((t (:foreground "DeepSkyBlue"))))
	("3" . '((t (:foreground "green"))))
	("4" . '((t (:foreground "yellow"))))
	("5" . '((t (:foreground "cyan"))))
	("6" . '((t (:foreground "orange"))))
	("7" . '((t (:foreground "cyan"))))
	("8" . '((t (:foreground "cyan"))))
	("9" . '((t (:foreground "cyan"))))
	("0" . '((t (:foreground "cyan")))))
  "Face list.")

(defvar vwe-mark-paren--overlay-list
  nil
  "Paren overlay list.")

(defvar vwe-mark-paren--move-to-mark-map
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Move to mark map.")

(defstruct vwe-mark-paren--obj
  "Paren pair obj."
  (level 0) (front) (after) (ovs '()))

(defun vwe-mark-paren--find-nearest-close-paren-of-cur-point (&optional point)
  "Find the nearest close parenthesis of the current point or other position.
POINT is current point or position."
  (unless point
	(setq point (point)))
  (save-excursion
	(condition-case nil
		(progn
		  (let ((code (syntax-class (syntax-after (1- point))))
				(open-code (syntax-class (syntax-after point))))
			(cond ((= code 5) point)
				  ((= open-code 4) (forward-list))
				  ((= code 4) (progn (goto-char (1- point)) (forward-list)))
				  (t (scan-lists point 1 1)))))
	  (error
	   nil))))

(defun vwe-mark-paren--find-nearest-open-paren-of-cur-point (&optional point)
  "Find the nearest open parenthesis of the current point or other position.
POINT is current point or position."
  (save-excursion
	(let ((point (vwe-mark-paren--find-nearest-close-paren-of-cur-point point)))
	  (if point
		  (progn
			(goto-char point)
			(backward-list))
		nil))))

(defun vwe-mark-paren--find-nearest-open-paren-of-cur-point+1 (&optional point)
  "Find the nearest open parenthesis of the current point or other position.
POINT is current point or position."
  (1+ (vwe-mark-paren--find-nearest-open-paren-of-cur-point point)))

(defun vwe-mark-paren--find-top-paren (&optional point)
  "Find top open parenthesis.
POINT is current point or position."
  (unless point
	(setq point (point)))

  (let* ((p point))
	(save-excursion
	  (condition-case nil
		  (progn
			(while t
			  (setq p (scan-lists p 1 1))))
		(error
		 (vwe-mark-paren--find-nearest-open-paren-of-cur-point p))))))

(defmacro vwe-mark-paren--obj-constructor (obj point lv)
  "Paren pair structs constructor.
OBJ.
POINT.
LV."
  `(list 'when (list 'vwe-mark-paren--obj-p ,obj)
		 (list 'save-excursion
			   (list 'setf (list 'vwe-mark-paren--obj-after ,obj) ,point
					 (list 'vwe-mark-paren--obj-front ,obj)
					 (list 'progn
						   (list 'condition-case 'nil
								 (list 'progn
									   (list 'goto-char ,point)
									   (list '1+ (list 'backward-list)))
								 (list 'error
									   'nil)
								 ))
					 (list 'vwe-mark-paren--obj-level ,obj) ,lv))))

(defun vwe-mark-paren--find-children-paren-pair (&optional point level)
  "Find all children parentheses that contain the current point.
POINT is current point or position.
LEVEL is current level."
  (unless point (setq point (point)))
  (when (or (not level) (< level 0)) (setq level 0))
  (let* ((cur-p point) (lv level) (p-list '()))
	(save-excursion
	  (condition-case nil
		  (progn
			(while t
			  (let* ((pp-obj (make-vwe-mark-paren--obj)))
				(setq cur-p (scan-lists cur-p 1 0))
				(eval (vwe-mark-paren--obj-constructor pp-obj cur-p lv))
				(setq p-list (append p-list (list pp-obj))))))
		(error
		 (let* ((pp-list p-list)
				(ppc-list '()))
		   (loop for ppf
				 in pp-list
				 collect
				 (setq ppc-list (append (vwe-mark-paren--find-children-paren-pair
										 (vwe-mark-paren--obj-front ppf) (1+ lv))
										ppc-list))) ;; loop
		   (setq p-list (append p-list ppc-list))))))
	p-list))

(defun vwe-mark-paren--find-paren-pair (&optional point)
  "Find all parentheses that contain the current point.
POINT is current point or position.
LEVEL is current level."
  (unless point
	(setq point (point)))

  (save-excursion
	(let* ((p (vwe-mark-paren--find-top-paren point))
		   (p-list '())
		   (lv 0)
		   (obj (make-vwe-mark-paren--obj)))
	  (when p
		(setf (vwe-mark-paren--obj-front obj) (1+ p)
			  (vwe-mark-paren--obj-after obj) (scan-lists p 1 0)
			  (vwe-mark-paren--obj-level obj) lv)
		(setq p-list (cons obj p-list)
			  p-list (append p-list (vwe-mark-paren--find-children-paren-pair (1+ p) (1+ lv))))
		)
	  p-list)))

(defmacro vwe-mark-paren--overlay-factory (ov str face)
  "Paren pair overlay factory.
OV.
STR.
FACE."
  `(overlay-put ,ov 'face ,face)
  `(overlay-put ,ov 'display '((height 1.5)))
  `(overlay-put ,ov 'after-string
				(propertize ,str
							'display '((raise 0.5))
							'face ,face)))

(defun vwe-mark-paren--find-level-face (lv)
  "Find face.
LV."
  (unless (stringp lv)
	(setq lv (number-to-string lv)))
  (cdr (assoc lv vwe-mark-paren--level-face-list)))

(defun vwe-mark-paren--paren-show-mark-obj (ov-obj index)
  "Make obj mark.
OV-OBJ.
INDEX."
  (when (and ov-obj (vwe-mark-paren--obj-p ov-obj))
	(let* ((front-ov (make-overlay
					  (1- (vwe-mark-paren--obj-front ov-obj))
					  (vwe-mark-paren--obj-front ov-obj)))
		   (after-ov (make-overlay
					  (1- (vwe-mark-paren--obj-after ov-obj))
					  (vwe-mark-paren--obj-after ov-obj)))
		   (lv-face (vwe-mark-paren--find-level-face (vwe-mark-paren--obj-level ov-obj))))
	  (vwe-mark-paren--overlay-factory front-ov (concat (number-to-string index) "a") lv-face)
	  (vwe-mark-paren--overlay-factory after-ov (concat (number-to-string index) "b") lv-face)
	  (setf (vwe-mark-paren--obj-ovs ov-obj) (append (vwe-mark-paren--obj-ovs ov-obj)
													 (list front-ov after-ov))))))

(defun vwe-mark-paren--paren-show-mark (objs)
  "Paren show mark.
OBJS."
  (when (and vwe-mark-paren--overlay-list (overlayp vwe-mark-paren--overlay-list)) (mapc #'delete-overlay vwe-mark-paren--overlay-list))
  (let* ((len (length objs))
		 (ovs '()))
	(dotimes (i len)
	  (vwe-mark-paren--paren-show-mark-obj (nth i objs) i)
	  (setq ovs (cons (vwe-mark-paren--obj-ovs (nth i objs)) ovs)))

	(define-key vwe-mark-paren--move-to-mark-map
	  (kbd "b") (lambda (p)
				  (interactive "ngoto after paren:")
				  (setq p (vwe-mark-paren--obj-after (nth p objs)))
				  (goto-char p)))
	(define-key vwe-mark-paren--move-to-mark-map
	  (kbd "a") (lambda (p)
				  (interactive "ngoto front paren:")
				  (setq p (vwe-mark-paren--obj-front (nth p objs)))
				  (goto-char p)))
	(define-key vwe-mark-paren--move-to-mark-map
	  (kbd "q") (lambda ()
				  (interactive)
				  (mapc (lambda (ov) (mapc #'delete-overlay ov)) ovs)
				  (vwe-mark-paren-mode -1)))
	(setq vwe-mark-paren--overlay-list ovs)))

;;;###autoload
(defun vwe-mark-paren--paren-pair ()
  "Show paren pair mark."
  (interactive)
  (vwe-mark-paren-mode t)
  (vwe-mark-paren--paren-show-mark (vwe-mark-paren--find-paren-pair (point))))

(define-minor-mode vwe-mark-paren-mode
  "Mark paren mode."
  :group 'vwe-mark
  :keymap vwe-mark-paren--move-to-mark-map
  (if vwe-mark-paren-mode
	  t
	(when vwe-mark-paren--overlay-list
	  (mapc #'delete-overlay vwe-mark-paren--overlay-list))
	(setq vwe-mark-paren--overlay-list nil)))

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
