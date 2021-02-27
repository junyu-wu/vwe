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

;; =============================================================================
;; line
;;
;; =============================================================================
(defvar vwe-mark-line--map
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Move to mark map.")

(defvar vwe-mark-line--preview-map
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Move to mark map.")

(defvar vwe-mark-line--current-overlay-list
  nil
  "Current overlay list.")

(defun vwe-mark-line--calculate-mark-number (win-pos)
  "Calculate mark number, WIN-POS if nil reverse calculate."
  (let* ((cur-line (line-number-at-pos))
		 (number))
	(save-excursion
	  (if win-pos
		  (setq number (- (line-number-at-pos (window-end)) cur-line))
		(setq number (- cur-line (line-number-at-pos (window-start)))))
	  number)))

(defun vwe-mark-line--show-mark (win-pos)
  "Show mark with WIN-POS."
  (interactive)
  (when vwe-mark-line--current-overlay-list (mapc #'delete-overlay vwe-mark-line--current-overlay-list))
  (let* ((keymap vwe-mark-line--map)
		 (ov-list)
		 (pos-list (vwe-mark-line--overlay-alist
					(vwe-mark-line--calculate-mark-number win-pos) win-pos)))
	(dotimes (i (length pos-list))
	  (let* ((ov (make-overlay (cadr (nth i pos-list)) (1+ (cadr (nth i pos-list))))))
		(overlay-put ov 'before-string
					 (propertize (format "%d" (car (nth i pos-list)))
								 'display '((raise 0.5))
								 'face 'vwe-mark--position--face))
		(push ov ov-list)
		))
	(define-key keymap (kbd "q") (lambda() (interactive) (mapc #'delete-overlay ov-list) (vwe-mark-line-mode -1)))
	(if win-pos
		(define-key keymap (kbd "g") #'vwe-mark-line-next-move-to)
	  (define-key keymap (kbd "g") #'vwe-mark-line-previous-move-to))
	(setq vwe-mark-line--current-overlay-list ov-list)))

(defun vwe-mark-line--overlay-alist (num win-pos)
  "Move lien to NUM with WIN-POS."
  (let* ((line-pos-list '())
		 (line-pos)
		 (pos-num))
	(dotimes (i num)
	  (if win-pos (setq pos-num (1+ i)) (setq pos-num (* (1+ i) -1)))
	  (save-excursion
		(forward-line pos-num)
		(setq line-pos (point)
			  line-pos-list (append (list (list (1+ i) line-pos)) line-pos-list))))
	line-pos-list))

(defun vwe-mark-line-previous ()
  "Line previous."
  (interactive)
  (when vwe-mark-mode
	(vwe-mark-line-mode t)
	(vwe-mark-line--show-mark nil)))

(defun vwe-mark-line-next ()
  "Line next."
  (interactive)
  (when vwe-mark-mode
	(vwe-mark-line-mode t)
	(vwe-mark-line--show-mark t)))

(defun vwe-mark-line-next-move-to (&optional num)
  "Move to NUM line."
  (interactive "nto:")
  (unless num (set num 1))
  (when (numberp num)
	(forward-line num))
  (vwe-mark-line-next))

(defun vwe-mark-line-previous-move-to (&optional num)
  "Move to NUM line."
  (interactive "nto:")
  (unless num (set num -1))
  (when (numberp num)
	(forward-line (* num -1)))
  (vwe-mark-line-previous))

(defun vwe-mark-line--enable ()
  "Enable.")

(defun vwe-mark-line--disable ()
  "Disable."
  (when vwe-mark-line--current-overlay-list
	(mapc #'delete-overlay vwe-mark-line--current-overlay-list))
  (setq vwe-mark-line--current-overlay-list nil))

(define-minor-mode vwe-mark-line-mode
  "Mark line mode."
  :group 'vwe-mark
  :keymap vwe-mark-line--map
  (if vwe-mark-line-mode
	  (vwe-mark-line--enable)))

;;
;; line preview
;;
(defvar vwe-mark-line--origin-window
  nil
  "Origin window.")

(defvar vwe-mark-line--origin-window-line
  nil
  "Origin window line.")

(defvar vwe-mark-line--origin-window-point
  nil
  "Origin window point.")

(defvar vwe-mark-line--preview-goto-line
  nil
  "Preveiw goto line.")

(defvar vwe-mark-line--preview-keymap
  (let ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd "q") #'vwe-mark-line--preview-recovery)
	(define-key keymap (kbd "g") #'vwe-mark-line--preview-goto-line)
	keymap)
  "Preview temp keymap.")

(defun vwe-mark-line--preview ()
  "Preview goto line."
  (interactive)
  (save-selected-window
	(let* ((input-num-str (thing-at-point 'line)))
	  (when input-num-str
		(setq vwe-mark-line--preview-goto-line (string-to-number input-num-str)))
	  (when (and vwe-mark-line--origin-window vwe-mark-line--preview-goto-line)
		(select-window vwe-mark-line--origin-window)
		(unless (zerop vwe-mark-line--preview-goto-line)
		  (goto-char (point-min))
		  (forward-line (1- vwe-mark-line--preview-goto-line)))
		(set-transient-map vwe-mark-line--preview-keymap nil #'vwe-mark-line--preview-recovery)))
	(message "preview line %s" vwe-mark-line--preview-goto-line)))

(defun vwe-mark-line--preview-goto-line (&optional line)
  "Preview goto LINE."
  (interactive)
  (setq vwe-mark-line--preview-goto-line (if line line (read-number "preview line:")))
  (vwe-mark-line--bulid-preview-origin-snapshot)
  (vwe-mark-line--preview))

(defun vwe-mark-line--preview-dynamic-goto-line ()
  "Preview dynamic goto line."
  (interactive)
  (vwe-mark-line--bulid-preview-origin-snapshot)
  (unwind-protect
	  (setq vwe-mark-line--preview-goto-line (read-number "preview line:"))
	(set-window-point vwe-mark-line--origin-window vwe-mark-line--origin-window-point)))

(defun vwe-mark-line--bulid-preview-origin-snapshot ()
  "Preview goto line."
  (interactive)
  (let* ((window (selected-window))
		 (line-num (line-number-at-pos))
		 (cur-point (point)))
	(setq vwe-mark-line--origin-window window
		  vwe-mark-line--origin-window-line line-num
		  vwe-mark-line--origin-window-point cur-point))
  (message "build exec finished %S" vwe-mark-line--origin-window))

(defun vwe-mark-line--preview-recovery ()
  "Preview recovery."
  (interactive)
  (select-window vwe-mark-line--origin-window)
  (when (and vwe-mark-line--origin-window (numberp vwe-mark-line--origin-window-point))
	(set-window-point vwe-mark-line--origin-window vwe-mark-line--origin-window-point)))

(defun vwe-mark-line--preview-cmd-config ()
  "Preview hook for minibuffer command."
  (when (memq this-command '(vwe-mark-line--preview-dynamic-goto-line))
    (add-hook 'post-command-hook #'vwe-mark-line--preview nil t)))

(defun vwe-mark-line--preview-enable ()
  "Enable preview."
  (define-key vwe-mark-line--preview-map (kbd "M-* r") #'vwe-mark-line--preview-goto-line)
  (define-key vwe-mark-line--preview-map (kbd "M-* d") #'vwe-mark-line--preview-dynamic-goto-line)
  (add-hook 'minibuffer-setup-hook 'vwe-mark-line--preview-cmd-config))

(defun vwe-mark-line--preview-disable ()
  "Disable preview."
  (remove-hook 'minibuffer-setup-hook 'vwe-mark-line--preview-cmd-config))

(define-minor-mode vwe-mark-line-preview-mode
  "Mark line preview mode."
  :group 'vwe-mark
  :keymap vwe-mark-line--preview-map
  :global t
  (if vwe-mark-line-preview-mode
	  (vwe-mark-line--preview-enable)
	(vwe-mark-line--preview-disable)))

;; =============================================================================
;; point
;;
;; =============================================================================
(defvar vwe-mark--point-keymap
  (let* ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd "M-* m") #'vwe-mark--point-mark)
	(define-key keymap (kbd "M-* g") #'vwe-mark--point-goto-mark)
	(define-key keymap (kbd "M-* c") #'vwe-mark--point-clear-mark)
	keymap)
  "Last mark point.")

(defface vwe-mark--point--face
  '((t (:inherit 'error :weight bold)))
  "Posit mark face.")

(defvar-local vwe-mark--point-last-mark-points-overlay
  nil
  "Last mark point overlay.")

(defun vwe-mark--point-line-indent (&optional goto)
  "Get current point or GOTO point line indentation."
  (interactive)
  (unless goto (setq goto (point)))
  (let* ((ind-pos (save-excursion (goto-char goto) (back-to-indentation) (point))))
	ind-pos))

(defun vwe-mark--point-line-eol (&optional goto)
  "Get current point or GOTO point line eol."
  (interactive)
  (unless goto (setq goto (point)))
  (let* ((eol-pos (save-excursion (goto-char goto) (end-of-line) (skip-chars-backward " \t" (vwe-mark--point-line-indent)) (point))))
	eol-pos))

(defun vwe-mark--point-mark ()
  "Mark current point."
  (interactive)
  (let* ((overlay (make-overlay (1- (point)) (point))))
	(setq vwe-mark--point-last-mark-points-overlay overlay)
	(overlay-put overlay 'after-string
				 (propertize (format "[M]")
							 'display '((raise 0.5) (height 0.8))
							 'face 'vwe-mark--point--face))))

(defun vwe-mark--point-goto-mark ()
  "Got mark list first point."
  (interactive)
  (let* ((goto-point (overlay-end vwe-mark--point-last-mark-points-overlay)))
	(condition-case nil
		(progn (when (numberp goto-point) (goto-char goto-point)))
	  (error nil))))

(defun vwe-mark--point-clear-mark ()
  "Clear mark."
  (interactive)
  (delete-overlay vwe-mark--point-last-mark-points-overlay))

(define-minor-mode vwe-mark-point-mode
  "Point mode."
  :group 'vwe-mark
  :global t
  :keymap vwe-mark--point-keymap)

;;
;; change point
;;
(defvar vwe-mark-change--skip-step
  8
  "Skip step.")

(defvar-local vwe-mark-change--current-step
  1
  "Skip step.")

(defvar vwe-mark-change--keymap
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Chage keymap.")

(defvar vwe-mark-change--skip-step-keymap
  (let ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd "C-,") (lambda () (interactive)
									 (setq vwe-mark-change--current-step (1+ vwe-mark-change--current-step))
									 (vwe-mark-change--goto-last vwe-mark-change--current-step)))
	keymap)
  "Chage keymap.")

(defun vwe-mark-change--format-undo-list-element (element)
  "Format an Emacs 27.1 style `buffer-undo-list' ELEMENT to regular edit."
  (let* ((formatted element)
		 (args (last element))
		 (formatp (and (consp element)
					   (eq (car element) 'apply)
					   (not (functionp (cadr element)))
					   (eq (nth 4 element) 'undo--wrap-and-run-primitive-undo)))
		 (args-formatp (and formatp
							(consp args)
							(= (length args) 1)
							(consp (car args))
							(= (length (car args)) 1)
							(consp (caar args))
							(numberp (car (caar args)))
							(numberp (cdr (caar args))))))
	(when args-formatp (setq formatted (caar args)))
	formatted))

(defun vwe-mark-change--get-undo-list-element-point (element)
  "Get `buffer-undo-list' ELEMENT point."
  (unless (numberp element)
	(let* ((format-element (vwe-mark-change--format-undo-list-element element)))
	  (cond ((numberp format-element) format-element) ; position
			((atom format-element) nil) ; command boundary
			((numberp (car format-element)) (cdr format-element)) ; insertion
			((stringp (car format-element)) (abs (cdr format-element))) ; deletion
			((null (car format-element)) (nthcdr 4 format-element)) ; text property
			((atom (car format-element)) nil) ; file modifiy time
			(t nil)))))

(defun vwe-mark-change--find-change-point (step)
  "Find last STEP change point."
  (let* ((found-point -1)
		 (undo-len (if buffer-undo-list (length buffer-undo-list) 0))
		 (match-index 0))
	(catch 'break
	  (dotimes (i undo-len)
		(let* ((undo-elem (nth i buffer-undo-list))
			   (pos (vwe-mark-change--get-undo-list-element-point undo-elem)))
		  (when  pos
			(setq match-index (1+ match-index))
			(when (= step match-index) (setq found-point pos) (message "last change: %s" vwe-mark-change--current-step) (throw 'break found-point))))))
	found-point))

;;;###autoload
(defun vwe-mark-change--goto-last (&optional step)
  "Goto last or STEP change."
  (interactive)
  (let* ((undo-status (and buffer-undo-list (not (eq buffer-undo-list t))))
		 (found-point 0))
	(if undo-status
		(progn
		  (let* ((undo-step (or step 1)))
			(setq found-point (vwe-mark-change--find-change-point undo-step))
			(if (> found-point 0) (goto-char found-point) (message "none or last change"))))
	  (message "Buffer has not been changed or undo is disabled"))
	found-point))

(defun vwe-mark-change--goto-last-cycle ()
  "Goto last change."
  (interactive)
  (let* ((pos (vwe-mark-change--goto-last vwe-mark-change--current-step)))
	(when pos
	  (set-transient-map vwe-mark-change--skip-step-keymap t (lambda () (setq vwe-mark-change--current-step 1))))))

(defun vwe-mark-change--enable ()
  "Enable change."
  (define-key vwe-mark-change--keymap (kbd "C-,") #'vwe-mark-change--goto-last)
  (define-key vwe-mark-change--keymap (kbd "C-.") #'vwe-mark-change--goto-last-cycle))

(defun vwe-mark-change--disable ()
  "Disable change.")

(define-minor-mode vwe-mark-change-mode
  "Mark line change mode."
  :group 'vwe-mark
  :keymap vwe-mark-change--keymap
  :global t
  (if vwe-mark-change-mode
	  (vwe-mark-change--enable)
	(vwe-mark-change--disable)))

;;
;; mode
;;
(defun vwe-mark-mode-enable ()
  "Enable mode."
  (vwe-mark-line-preview-mode 1)
  (vwe-mark-point-mode 1)
  (vwe-mark-change-mode 1))

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
