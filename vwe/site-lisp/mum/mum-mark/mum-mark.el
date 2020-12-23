;;; mum-mark.el ---   Mum mark          -*- lexical-binding: t; -*-

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

(defgroup mum-mark nil
  "Customization group for beacon."
  :group 'vwiss-mum
  :prefix "mum-mark--")

;; =============================================================================
;; word or symbol
;;
;; =============================================================================
(defface mum-mark--position--face
  '((t
	 (:background nil :foreground "yellow")))
  "Position makr hint face.")

(defmacro mum-mark--position--create-keymap (cmd)
  "Create keymap.
MAP.
CMD."
  `(dotimes (i 10)
	 (define-key mum-mark--position--move-to-mark-map
	   (kbd (concat "" (number-to-string i)))
	   (lambda()
		 (interactive)
		 (mum-mark--position--mark-move ,cmd (if (= i 0) 10 i))))))

(defvar mum-mark--position--move-to-mark-map
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Move to mark map.")

(defun mum-mark--position--show-mark-and-move (cmd)
  "Show before/after current cursor mark and move.
CMD before/after move func."
  (let ((mark-list))
	(save-mark-and-excursion
	  (call-interactively cmd)
	  (dotimes (i 10)
		(let ((mark-i (make-overlay (1- (point)) (point))))
		  (call-interactively cmd)
		  (overlay-put mark-i 'after-string
					   (propertize (format "%d" (1+ i))
								   'display '((raise 0.5))
								   'face 'mum-mark--position--face))
		  (push mark-i mark-list))))
	(mum-mark--position--create-keymap cmd)
	(define-key mum-mark--position--move-to-mark-map
	  (kbd "q") (lambda ()
				  (interactive)
				  (mapc #'delete-overlay mark-list)))
	(set-transient-map mum-mark--position--move-to-mark-map
					   nil
					   (lambda()
						 (mapc #'delete-overlay mark-list)))))

(defun mum-mark--position--mark-move (cmd &optional step)
  "Move cursor position.
CMD move func.
STEP move step."
  (let ((move-step 1))
	(when (and (numberp step) (> step 1))
	  (setq move-step step))
	(dotimes (_ move-step)
	  (call-interactively cmd))))

;;;###autoload
(defun mum-mark--position--forward-word ()
  "Forward word."
  (interactive)
  (forward-word)
  (mum-mark--position--show-mark-and-move #'forward-word))

;;;###autoload
(defun mum-mark--position--backward-word ()
  "Backward word."
  (interactive)
  (backward-word)
  (mum-mark--position--show-mark-and-move #'backward-word))

;; =============================================================================
;; paren
;;
;; =============================================================================

(defvar mum-mark--paren--level-face-list
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

(defvar mum-mark--paren--move-to-mark-map
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Move to mark map.")

(defstruct mum-mark--paren--obj
  "Paren pair obj."
  (level 0) (front) (after) (ovs '()))

(defun mum-mark--paren--find-nearest-close-paren-of-cur-point (&optional point)
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

(defun mum-mark--paren--find-nearest-open-paren-of-cur-point (&optional point)
  "Find the nearest open parenthesis of the current point or other position.
POINT is current point or position."
  (save-excursion
	(let ((point (mum-mark--paren--find-nearest-close-paren-of-cur-point point)))
	  (if point
		  (progn
			(goto-char point)
			(backward-list))
		nil))))

(defun mum-mark--paren--find-nearest-open-paren-of-cur-point+1 (&optional point)
  "Find the nearest open parenthesis of the current point or other position.
POINT is current point or position."
  (1+ (mum-mark--paren--find-nearest-open-paren-of-cur-point point)))

(defun mum-mark--paren--find-top-paren (&optional point)
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
		 (mum-mark--paren--find-nearest-open-paren-of-cur-point p))))))

(defmacro mum-mark--paren--obj-constructor (obj point lv)
  "Paren pair structs constructor.
OBJ.
POINT.
LV."
  `(list 'when (list 'mum-mark--paren--obj-p ,obj)
		 (list 'save-excursion
			   (list 'setf (list 'mum-mark--paren--obj-after ,obj) ,point
					 (list 'mum-mark--paren--obj-front ,obj)
					 (list 'progn
						   (list 'condition-case 'nil
								 (list 'progn
									   (list 'goto-char ,point)
									   (list '1+ (list 'backward-list)))
								 (list 'error
									   'nil)
								 ))
					 (list 'mum-mark--paren--obj-level ,obj) ,lv))))

(defun mum-mark--paren--find-children-paren-pair (&optional point level)
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
			  (let* ((pp-obj (make-mum-mark--paren--obj)))
				(setq cur-p (scan-lists cur-p 1 0))
				(eval (mum-mark--paren--obj-constructor pp-obj cur-p lv))
				(setq p-list (append p-list (list pp-obj))))))
		(error
		 (let* ((pp-list p-list)
				(ppc-list '()))
		   (loop for ppf
				 in pp-list
				 collect
				 (setq ppc-list (append (mum-mark--paren--find-children-paren-pair
										 (mum-mark--paren--obj-front ppf) (1+ lv))
										ppc-list))) ;; loop
		   (setq p-list (append p-list ppc-list))))))
	p-list))

(defun mum-mark--paren--find-paren-pair (&optional point)
  "Find all parentheses that contain the current point.
POINT is current point or position.
LEVEL is current level."
  (unless point
	(setq point (point)))

  (save-excursion
	(let* ((p (mum-mark--paren--find-top-paren point))
		   (p-list '())
		   (lv 0)
		   (obj (make-mum-mark--paren--obj)))
	  (when p
		(setf (mum-mark--paren--obj-front obj) (1+ p)
			  (mum-mark--paren--obj-after obj) (scan-lists p 1 0)
			  (mum-mark--paren--obj-level obj) lv)
		(setq p-list (cons obj p-list)
			  p-list (append p-list (mum-mark--paren--find-children-paren-pair (1+ p) (1+ lv))))
		)
	  p-list)))

(defmacro mum-mark--paren--overlay-factory (ov str face)
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

(defun mum-mark--paren--find-level-face (lv)
  "Find face.
LV."
  (unless (stringp lv)
	(setq lv (number-to-string lv)))
  (cdr (assoc lv mum-mark--paren--level-face-list)))

(defun mum-mark--paren--paren-show-mark-obj (ov-obj index)
  "Make obj mark.
OV-OBJ.
INDEX."
  (when (and ov-obj (mum-mark--paren--obj-p ov-obj))
	(let* ((front-ov (make-overlay
					  (1- (mum-mark--paren--obj-front ov-obj))
					  (mum-mark--paren--obj-front ov-obj)))
		   (after-ov (make-overlay
					  (1- (mum-mark--paren--obj-after ov-obj))
					  (mum-mark--paren--obj-after ov-obj)))
		   (lv-face (mum-mark--paren--find-level-face (mum-mark--paren--obj-level ov-obj))))
	  (mum-mark--paren--overlay-factory front-ov (concat (number-to-string index) "a") lv-face)
	  (mum-mark--paren--overlay-factory after-ov (concat (number-to-string index) "b") lv-face)
	  (setf (mum-mark--paren--obj-ovs ov-obj) (append (mum-mark--paren--obj-ovs ov-obj)
											   (list front-ov after-ov))))))

(defun mum-mark--paren--paren-show-mark (objs)
  "Paren show mark.
OBJS."
  (let* ((len (length objs))
		 (ovs '()))
	(dotimes (i len)
	  (mum-mark--paren--paren-show-mark-obj (nth i objs) i)
	  (setq ovs (cons (mum-mark--paren--obj-ovs (nth i objs)) ovs)))

	(define-key mum-mark--paren--move-to-mark-map
	  (kbd "b") (lambda (p)
				  (interactive "ngoto after paren:")
				  (setq p (mum-mark--paren--obj-after (nth p objs)))
				  (goto-char p)))
	(define-key mum-mark--paren--move-to-mark-map
	  (kbd "a") (lambda (p)
				  (interactive "ngoto front paren:")
				  (setq p (mum-mark--paren--obj-front (nth p objs)))
				  (goto-char p)))
	(define-key mum-mark--paren--move-to-mark-map
	  (kbd "q") (lambda ()
				  (interactive)
				  (mapc (lambda (ov) (mapc #'delete-overlay ov)) ovs)))
	(set-transient-map mum-mark--paren--move-to-mark-map
					   nil
					   (lambda()
						 (mapc (lambda (ov) (mapc #'delete-overlay ov)) ovs)))))

;;;###autoload
(defun mum-mark--paren--paren-pair ()
  "Show paren pair mark."
  (interactive)
  (mum-mark--paren--paren-show-mark (mum-mark--paren--find-paren-pair (point))))

;;;###autoload
(define-minor-mode mum-mark-mode
  "Mum mark minor mode."
  :init-value nil
  :group 'mum-mark
  :global t
  (define-key global-map (kbd "M-f") #'mum-mark--position--forward-word)
  (define-key global-map (kbd "M-b") #'mum-mark--position--backward-word))

(provide 'mum-mark)
;;; mum-mark.el ends here
