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
(require 'cl)

(defgroup mum-mark nil
  "Customization group for beacon."
  :group 'vwiss-mum
  :prefix "mum-mark--")

(defvar mum-mark--mode-keymap
  (let ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd "C->") #'mum-mark-position--forward-word)
	(define-key keymap (kbd "C-<") #'mum-mark-position--backward-word)
	(define-key keymap (kbd "M-p") #'mum-mark-line-previous)
	(define-key keymap (kbd "M-n") #'mum-mark-line-next)
	keymap)
  "Move to mark map.")

;; =============================================================================
;; word or symbol
;;
;; =============================================================================
(defface mum-mark--position--face
  '((t (:inherit 'error :inverse-video nil)))
  "Position makr hint face.")

(defmacro mum-mark-position--create-keymap (cmd)
  "Create keymap CMD."
  `(dotimes (i 10)
	 (define-key mum-mark-position--move-to-mark-map
	   (kbd (concat "" (number-to-string i)))
	   (lambda()
		 (interactive)
		 (mum-mark-position--mark-move ,cmd (if (= i 0) 10 i))))))

(defvar mum-mark-position--move-to-mark-map
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Move to mark map.")

(defun mum-mark-position--show-mark-and-move (cmd)
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
									 'face 'mum-mark--position--face))
			(push mark-i mark-list)))))
	(mum-mark-position--create-keymap cmd)
	(define-key mum-mark-position--move-to-mark-map
	  (kbd "q") (lambda ()
				  (interactive)
				  (mapc #'delete-overlay mark-list)))
	(set-transient-map mum-mark-position--move-to-mark-map
					   nil
					   (lambda()
						 (mapc #'delete-overlay mark-list)))))

(defun mum-mark-position--mark-move (cmd &optional step)
  "Move cursor position.
CMD move func.
STEP move step."
  (let ((move-step 1))
	(when (and (numberp step) (> step 1))
	  (setq move-step step))
	(dotimes (_ move-step)
	  (call-interactively cmd))))

;;;###autoload
(defun mum-mark-position--forward-word ()
  "Forward word."
  (interactive)
  (forward-word)
  (mum-mark-position--show-mark-and-move #'forward-word))

;;;###autoload
(defun mum-mark-position--backward-word ()
  "Backward word."
  (interactive)
  (backward-word)
  (mum-mark-position--show-mark-and-move #'backward-word))

;; =============================================================================
;; paren
;;
;; =============================================================================

(defvar mum-mark-paren--level-face-list
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

(defvar mum-mark-paren--overlay-list
  nil
  "Paren overlay list.")

(defvar mum-mark-paren--move-to-mark-map
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Move to mark map.")

(defstruct mum-mark-paren--obj
  "Paren pair obj."
  (level 0) (front) (after) (ovs '()))

(defun mum-mark-paren--find-nearest-close-paren-of-cur-point (&optional point)
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

(defun mum-mark-paren--find-nearest-open-paren-of-cur-point (&optional point)
  "Find the nearest open parenthesis of the current point or other position.
POINT is current point or position."
  (save-excursion
	(let ((point (mum-mark-paren--find-nearest-close-paren-of-cur-point point)))
	  (if point
		  (progn
			(goto-char point)
			(backward-list))
		nil))))

(defun mum-mark-paren--find-nearest-open-paren-of-cur-point+1 (&optional point)
  "Find the nearest open parenthesis of the current point or other position.
POINT is current point or position."
  (1+ (mum-mark-paren--find-nearest-open-paren-of-cur-point point)))

(defun mum-mark-paren--find-top-paren (&optional point)
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
		 (mum-mark-paren--find-nearest-open-paren-of-cur-point p))))))

(defmacro mum-mark-paren--obj-constructor (obj point lv)
  "Paren pair structs constructor.
OBJ.
POINT.
LV."
  `(list 'when (list 'mum-mark-paren--obj-p ,obj)
		 (list 'save-excursion
			   (list 'setf (list 'mum-mark-paren--obj-after ,obj) ,point
					 (list 'mum-mark-paren--obj-front ,obj)
					 (list 'progn
						   (list 'condition-case 'nil
								 (list 'progn
									   (list 'goto-char ,point)
									   (list '1+ (list 'backward-list)))
								 (list 'error
									   'nil)
								 ))
					 (list 'mum-mark-paren--obj-level ,obj) ,lv))))

(defun mum-mark-paren--find-children-paren-pair (&optional point level)
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
			  (let* ((pp-obj (make-mum-mark-paren--obj)))
				(setq cur-p (scan-lists cur-p 1 0))
				(eval (mum-mark-paren--obj-constructor pp-obj cur-p lv))
				(setq p-list (append p-list (list pp-obj))))))
		(error
		 (let* ((pp-list p-list)
				(ppc-list '()))
		   (loop for ppf
				 in pp-list
				 collect
				 (setq ppc-list (append (mum-mark-paren--find-children-paren-pair
										 (mum-mark-paren--obj-front ppf) (1+ lv))
										ppc-list))) ;; loop
		   (setq p-list (append p-list ppc-list))))))
	p-list))

(defun mum-mark-paren--find-paren-pair (&optional point)
  "Find all parentheses that contain the current point.
POINT is current point or position.
LEVEL is current level."
  (unless point
	(setq point (point)))

  (save-excursion
	(let* ((p (mum-mark-paren--find-top-paren point))
		   (p-list '())
		   (lv 0)
		   (obj (make-mum-mark-paren--obj)))
	  (when p
		(setf (mum-mark-paren--obj-front obj) (1+ p)
			  (mum-mark-paren--obj-after obj) (scan-lists p 1 0)
			  (mum-mark-paren--obj-level obj) lv)
		(setq p-list (cons obj p-list)
			  p-list (append p-list (mum-mark-paren--find-children-paren-pair (1+ p) (1+ lv))))
		)
	  p-list)))

(defmacro mum-mark-paren--overlay-factory (ov str face)
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

(defun mum-mark-paren--find-level-face (lv)
  "Find face.
LV."
  (unless (stringp lv)
	(setq lv (number-to-string lv)))
  (cdr (assoc lv mum-mark-paren--level-face-list)))

(defun mum-mark-paren--paren-show-mark-obj (ov-obj index)
  "Make obj mark.
OV-OBJ.
INDEX."
  (when (and ov-obj (mum-mark-paren--obj-p ov-obj))
	(let* ((front-ov (make-overlay
					  (1- (mum-mark-paren--obj-front ov-obj))
					  (mum-mark-paren--obj-front ov-obj)))
		   (after-ov (make-overlay
					  (1- (mum-mark-paren--obj-after ov-obj))
					  (mum-mark-paren--obj-after ov-obj)))
		   (lv-face (mum-mark-paren--find-level-face (mum-mark-paren--obj-level ov-obj))))
	  (mum-mark-paren--overlay-factory front-ov (concat (number-to-string index) "a") lv-face)
	  (mum-mark-paren--overlay-factory after-ov (concat (number-to-string index) "b") lv-face)
	  (setf (mum-mark-paren--obj-ovs ov-obj) (append (mum-mark-paren--obj-ovs ov-obj)
													 (list front-ov after-ov))))))

(defun mum-mark-paren--paren-show-mark (objs)
  "Paren show mark.
OBJS."
  (when (and mum-mark-paren--overlay-list (overlayp mum-mark-paren--overlay-list)) (mapc #'delete-overlay mum-mark-paren--overlay-list))
  (let* ((len (length objs))
		 (ovs '()))
	(dotimes (i len)
	  (mum-mark-paren--paren-show-mark-obj (nth i objs) i)
	  (setq ovs (cons (mum-mark-paren--obj-ovs (nth i objs)) ovs)))

	(define-key mum-mark-paren--move-to-mark-map
	  (kbd "b") (lambda (p)
				  (interactive "ngoto after paren:")
				  (setq p (mum-mark-paren--obj-after (nth p objs)))
				  (goto-char p)))
	(define-key mum-mark-paren--move-to-mark-map
	  (kbd "a") (lambda (p)
				  (interactive "ngoto front paren:")
				  (setq p (mum-mark-paren--obj-front (nth p objs)))
				  (goto-char p)))
	(define-key mum-mark-paren--move-to-mark-map
	  (kbd "q") (lambda ()
				  (interactive)
				  (mapc (lambda (ov) (mapc #'delete-overlay ov)) ovs)
				  (mum-mark-paren-mode -1)))
	(setq mum-mark-paren--overlay-list ovs)))

;;;###autoload
(defun mum-mark-paren--paren-pair ()
  "Show paren pair mark."
  (interactive)
  (mum-mark-paren-mode t)
  (mum-mark-paren--paren-show-mark (mum-mark-paren--find-paren-pair (point))))

(define-minor-mode mum-mark-paren-mode
  "Mark paren mode."
  :group 'mum-mark
  :keymap mum-mark-paren--move-to-mark-map
  (if mum-mark-paren-mode
	  t
	(when mum-mark-paren--overlay-list
	  (mapc #'delete-overlay mum-mark-paren--overlay-list))
	(setq mum-mark-paren--overlay-list nil)))

;; =============================================================================
;; line
;;
;; =============================================================================
(defvar mum-mark-line--map
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Move to mark map.")

(defvar mum-mark-line--current-overlay-list
  nil
  "Current overlay list.")

(defun mum-mark-line--calculate-mark-number (win-pos)
  "Calculate mark number, WIN-POS if nil reverse calculate."
  (let* ((cur-line (line-number-at-pos))
		 (number))
	(save-excursion
	  (if win-pos
		  (setq number (- (line-number-at-pos (window-end)) cur-line))
		(setq number (- cur-line (line-number-at-pos (window-start)))))
	  number)))

(defun mum-mark-line--show-mark (win-pos)
  "Show mark with WIN-POS."
  (interactive)
  (when mum-mark-line--current-overlay-list (mapc #'delete-overlay mum-mark-line--current-overlay-list))
  (let* ((keymap mum-mark-line--map)
		 (ov-list)
		 (pos-list (mum-mark-line--overlay-alist
					(mum-mark-line--calculate-mark-number win-pos) win-pos)))
	(dotimes (i (length pos-list))
	  (let* ((ov (make-overlay (cadr (nth i pos-list)) (1+ (cadr (nth i pos-list))))))
		(overlay-put ov 'before-string
					 (propertize (format "%d" (car (nth i pos-list)))
								 'display '((raise 0.5))
								 'face 'mum-mark--position--face))
		(push ov ov-list)
		))
	(define-key keymap (kbd "q") (lambda() (interactive) (mapc #'delete-overlay ov-list) (mum-mark-line-mode -1)))
	(if win-pos
		(define-key keymap (kbd "g") #'mum-mark-line-next-move-to)
	  (define-key keymap (kbd "g") #'mum-mark-line-previous-move-to))
	(setq mum-mark-line--current-overlay-list ov-list)))

(defun mum-mark-line--overlay-alist (num win-pos)
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

(defun mum-mark-line-previous ()
  "Line previous."
  (interactive)
  (when mum-mark-mode
	(mum-mark-line-mode t)
	(mum-mark-line--show-mark nil)))

(defun mum-mark-line-next ()
  "Line next."
  (interactive)
  (when mum-mark-mode
	(mum-mark-line-mode t)
	(mum-mark-line--show-mark t)))

(defun mum-mark-line-next-move-to (&optional num)
  "Move to NUM line."
  (interactive "nto:")
  (unless num (set num 1))
  (when (numberp num)
	(forward-line num))
  (mum-mark-line-next))

(defun mum-mark-line-previous-move-to (&optional num)
  "Move to NUM line."
  (interactive "nto:")
  (unless num (set num -1))
  (when (numberp num)
	(forward-line (* num -1)))
  (mum-mark-line-previous))

(defun mum-mark-line--enable ()
  "Enable.")

(defun mum-mark-line--disable ()
  "Disable."
  (when mum-mark-line--current-overlay-list
	(mapc #'delete-overlay mum-mark-line--current-overlay-list))
  (setq mum-mark-line--current-overlay-list nil))

(define-minor-mode mum-mark-line-mode
  "Mark line mode."
  :group 'mum-mark
  :keymap mum-mark-line--map
  (if mum-mark-line-mode
	  (mum-mark-line--enable)))

;;
;; line preview
;;
(defvar mum-mark-line--origin-window
  nil
  "Origin window.")

(defvar mum-mark-line--origin-window-line
  nil
  "Origin window line.")

(defvar mum-mark-line--origin-window-point
  nil
  "Origin window point.")

(defvar mum-mark-line--preview-goto-line
  nil
  "Preveiw goto line.")

(defvar mum-mark-line--preview-keymap
  (let ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd "q") #'mum-mark-line--preview-recovery)
	(define-key keymap (kbd "g") #'mum-mark-line--preview-goto-line)
	keymap)
  "Preview temp keymap.")

(defun mum-mark-line--preview ()
  "Preview goto line."
  (interactive)
  (save-selected-window
	(let* ((input-num-str (thing-at-point 'line)))
	  (when input-num-str
		(setq mum-mark-line--preview-goto-line (string-to-number input-num-str)))
	  (when (and mum-mark-line--origin-window mum-mark-line--preview-goto-line)
		(select-window mum-mark-line--origin-window)
		(unless (zerop mum-mark-line--preview-goto-line)
		  (goto-char (point-min))
		  (forward-line (1- mum-mark-line--preview-goto-line)))
		(set-transient-map mum-mark-line--preview-keymap nil  #'mum-mark-line--preview-recovery)))
	(message "preview line %s" mum-mark-line--preview-goto-line)))

(defun mum-mark-line--preview-goto-line (&optional line)
  "Preview goto LINE."
  (interactive)
  (setq mum-mark-line--preview-goto-line (if line line (read-number "preview line:")))
  (mum-mark-line--bulid-preview-origin-snapshot)
  (mum-mark-line--preview))

(defun mum-mark-line--preview-dynamic-goto-line ()
  "Preview dynamic goto line."
  (interactive)
  (mum-mark-line--bulid-preview-origin-snapshot)
  (unwind-protect
	  (setq mum-mark-line--preview-goto-line (read-number "preview line:"))
	(set-window-point mum-mark-line--origin-window mum-mark-line--origin-window-point)))

(defun mum-mark-line--bulid-preview-origin-snapshot ()
  "Preview goto line."
  (interactive)
  (let* ((window (selected-window))
		 (line-num (line-number-at-pos))
		 (cur-point (point)))
	(setq mum-mark-line--origin-window window
		  mum-mark-line--origin-window-line line-num
		  mum-mark-line--origin-window-point cur-point))
  (message "build exec finished %S" mum-mark-line--origin-window))

(defun mum-mark-line--preview-recovery ()
  "Preview recovery."
  (interactive)
  (select-window mum-mark-line--origin-window)
  (when (and mum-mark-line--origin-window (numberp mum-mark-line--origin-window-point))
	(set-window-point mum-mark-line--origin-window mum-mark-line--origin-window-point)))

(defun mum-mark-line--preview-cmd-config ()
  "Preview hook for minibuffer command."
  (when (memq this-command '(mum-mark-line--preview-dynamic-goto-line))
    (add-hook 'post-command-hook #'mum-mark-line--preview nil t)))

(defun mum-mark-line--preview-enable ()
  "Enable preview."
  (define-key mum-mark-line--map (kbd "M-* r") #'mum-mark-line--preview-goto-line)
  (define-key mum-mark-line--map (kbd "M-* d") #'mum-mark-line--preview-dynamic-goto-line)
  (add-hook 'minibuffer-setup-hook 'mum-mark-line--preview-cmd-config))

(defun mum-mark-line--preview-disable ()
  "Disable preview."
  (remove-hook 'minibuffer-setup-hook 'mum-mark-line--preview-cmd-config))

(define-minor-mode mum-mark-line-preview-mode
  "Mark line preview mode."
  :group 'mum-mark
  :keymap mum-mark-line--map
  :global t
  (if mum-mark-line-preview-mode
	  (mum-mark-line--preview-enable)
	(mum-mark-line--preview-disable)))

;; =============================================================================
;; point
;;
;; =============================================================================
(defvar mum-mark--point-keymap
  (let* ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd "M-* m") #'mum-mark--point-mark)
	(define-key keymap (kbd "M-* g") #'mum-mark--point-goto-mark)
	(define-key keymap (kbd "M-* c") #'mum-mark--point-clear-mark)
	keymap)
  "Last mark point.")

(defface mum-mark--point--face
  '((t (:inherit 'error :weight bold)))
  "Posit mark face.")

(defvar-local mum-mark--point-last-mark-points-overlay
  nil
  "Last mark point overlay.")

(defun mum-mark--point-line-indent (&optional goto)
  "Get current point or GOTO point line indentation."
  (interactive)
  (unless goto (setq goto (point)))
  (let* ((ind-pos (save-excursion (goto-char goto) (back-to-indentation) (point))))
	ind-pos))

(defun mum-mark--point-line-eol (&optional goto)
  "Get current point or GOTO point line eol."
  (interactive)
  (unless goto (setq goto (point)))
  (let* ((eol-pos (save-excursion (goto-char goto) (end-of-line) (skip-chars-backward " \t" (mum-mark--point-line-indent)) (point))))
	eol-pos))

(defun mum-mark--point-mark ()
  "Mark current point."
  (interactive)
  (let* ((overlay (make-overlay (1- (point)) (point))))
	(setq mum-mark--point-last-mark-points-overlay overlay)
	(overlay-put overlay 'after-string
				 (propertize (format "[M]")
							 'display '((raise 0.5) (height 0.8))
							 'face 'mum-mark--point--face))))

(defun mum-mark--point-goto-mark ()
  "Got mark list first point."
  (interactive)
  (let* ((goto-point (overlay-end mum-mark--point-last-mark-points-overlay)))
	(condition-case nil
		(progn (when (numberp goto-point) (goto-char goto-point)))
	  (error nil))))

(defun mum-mark--point-clear-mark ()
  "Clear mark."
  (interactive)
  (delete-overlay mum-mark--point-last-mark-points-overlay))

(define-minor-mode mum-mark-point-mode
  "Point mode."
  :group 'mum-mark
  :global t
  :keymap mum-mark--point-keymap)

;;
;; change point
;;
(defvar mum-mark-change--keymap
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Chage keymap.")

(defun mum-mark-change--format-undo-list-element (element)
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

(defun mum-mark-change--get-undo-list-element-point (element)
  "Get `buffer-undo-list' ELEMENT point."
  (unless (numberp element)
	(let* ((format-element (mum-mark-change--format-undo-list-element element)))
	  (cond ((numberp format-element) format-element) ; position
			((atom format-element) nil) ; command boundary
			((numberp (car format-element)) (cdr format-element)) ; insertion
			((stringp (car format-element)) (abs (cdr format-element))) ; deletion
			((null (car format-element)) (nthcdr 4 format-element)) ; text property
			((atom (car format-element)) nil) ; file modifiy time
			(t nil)))))

(defun mum-mark-change--find-change-point (step)
  "Find last STEP change point."
  (let* ((found-point (point))
		 (undo-len (if buffer-undo-list (length buffer-undo-list) 0))
		 (match-index 0))
	(catch 'break
	  (dotimes (i undo-len)
	  (let* ((undo-elem (nth i buffer-undo-list))
			 (pos (mum-mark-change--get-undo-list-element-point undo-elem)))
	  (when pos
		(setq match-index (1+ match-index))
		(when (= step match-index) (setq found-point pos)
			  (throw 'break nil))))))
	found-point))

;;;###autoload
(defun mum-mark-change--goto-last (&optional step)
  "Goto last or STEP change."
  (interactive)
  (let* ((undo-status (and buffer-undo-list (not (eq buffer-undo-list t)))))
	(if undo-status
		(progn
		  (let* ((found-point)
				 (undo-step (or step 1)))
			(setq found-point (mum-mark-change--find-change-point undo-step))
			(when (> found-point 0) (goto-char found-point))))
	  (message "Buffer has not been changed or undo is disabled"))))

(defun mum-mark-change--enable ()
  "Enable change."
  (define-key mum-mark-change--keymap (kbd "C-,") #'mum-mark-change--goto-last))

(defun mum-mark-change--disable ()
  "Disable change.")

(define-minor-mode mum-mark-change-mode
  "Mark line change mode."
  :group 'mum-mark
  :keymap mum-mark-change--keymap
  :global t
  (if mum-mark-change-mode
	  (mum-mark-change--enable)
	(mum-mark-change--disable)))

;;
;; mode
;;
(defun mum-mark-mode-enable ()
  "Enable mode."
  (mum-mark-line-preview-mode 1)
  (mum-mark-point-mode 1)
  (mum-mark-change-mode 1))

(defun mum-mark-mode-disable ()
  "Disable mode.")

;;;###autoload
(define-minor-mode mum-mark-mode
  "Mum mark minor mode."
  :group 'mum-mark
  :keymap mum-mark--mode-keymap
  :global t
  (if mum-mark-mode
	  (mum-mark-mode-enable)
	(mum-mark-mode-disable)))

(provide 'mum-mark)
;;; mum-mark.el ends here
