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

(defun vwe-move-line-mode-enable ()
  "Enable mode."
  (define-key vwe-move--keymap (kbd "M-p") #'vwe-move-line--up)
  (define-key vwe-move--keymap (kbd "M-n") #'vwe-move-line--down))

(defun vwe-move-line-mode-disable ()
  "Disable mode.")

;;;###autoload
(define-minor-mode vwe-move-line-mode
  "Vwe move line minor mode."
  :group 'vwe-move
  :keymap vwe-move--keymap
  :global t
  (if vwe-move-line-mode
	  (vwe-move-line-mode-enable)
	(vwe-move-line-mode-disable)))

;;
;; change point
;;
(defvar vwe-move-change--skip-step
  8
  "Skip step.")

(defvar-local vwe-move-change--current-step
  1
  "Skip step.")

(defvar vwe-move-change--keymap
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Chage keymap.")

(defvar vwe-move-change--skip-step-keymap
  (let ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd "C-,") (lambda () (interactive)
									 (setq vwe-move-change--current-step (1+ vwe-move-change--current-step))
									 (vwe-move-change--goto-last vwe-move-change--current-step)))
	keymap)
  "Chage keymap.")

(defun vwe-move-change--format-undo-list-element (element)
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

(defun vwe-move-change--get-undo-list-element-point (element)
  "Get `buffer-undo-list' ELEMENT point."
  (unless (numberp element)
	(let* ((format-element (vwe-move-change--format-undo-list-element element)))
	  (cond ((numberp format-element) format-element) ; position
			((atom format-element) nil) ; command boundary
			((numberp (car format-element)) (cdr format-element)) ; insertion
			((stringp (car format-element)) (abs (cdr format-element))) ; deletion
			((null (car format-element)) (nthcdr 4 format-element)) ; text property
			((atom (car format-element)) nil) ; file modifiy time
			(t nil)))))

(defun vwe-move-change--find-change-point (step)
  "Find last STEP change point."
  (let* ((found-point -1)
		 (undo-len (if buffer-undo-list (length buffer-undo-list) 0))
		 (match-index 0))
	(catch 'break
	  (dotimes (i undo-len)
		(let* ((undo-elem (nth i buffer-undo-list))
			   (pos (vwe-move-change--get-undo-list-element-point undo-elem)))
		  (when  pos
			(setq match-index (1+ match-index))
			(when (= step match-index) (setq found-point pos) (message "last change: %s" vwe-move-change--current-step) (throw 'break found-point))))))
	found-point))

;;;###autoload
(defun vwe-move-change--goto-last (&optional step)
  "Goto last or STEP change."
  (interactive)
  (let* ((undo-status (and buffer-undo-list (not (eq buffer-undo-list t))))
		 (found-point 0))
	(if undo-status
		(progn
		  (let* ((undo-step (or step 1)))
			(setq found-point (vwe-move-change--find-change-point undo-step))
			(if (> found-point 0) (goto-char found-point) (message "none or last change"))))
	  (message "Buffer has not been changed or undo is disabled"))
	found-point))

(defun vwe-move-change--goto-last-cycle ()
  "Goto last change."
  (interactive)
  (let* ((pos (vwe-move-change--goto-last vwe-move-change--current-step)))
	(when pos
	  (set-transient-map vwe-move-change--skip-step-keymap t (lambda () (setq vwe-move-change--current-step 1))))))

(defun vwe-move-change--enable ()
  "Enable change."
  (define-key vwe-move-change--keymap (kbd "C-,") #'vwe-move-change--goto-last)
  (define-key vwe-move-change--keymap (kbd "C-.") #'vwe-move-change--goto-last-cycle))

(defun vwe-move-change--disable ()
  "Disable change.")

(define-minor-mode vwe-move-change-mode
  "Mark line change mode."
  :group 'vwe-mark
  :keymap vwe-move-change--keymap
  :global t
  (if vwe-move-change-mode
	  (vwe-move-change--enable)
	(vwe-move-change--disable)))

;;
;; win move
;;
(defun vwe-move--windmove-up()
  "Selected current up window."
  (interactive)
  (condition-case nil (windmove-up)
    (error (condition-case nil (windmove-down) (error)))))

(defun vwe-move--windmove-down()
  "Selected current down window."
  (interactive)
  (condition-case nil (windmove-down)
    (error (condition-case nil (windmove-up) (error)))))

(defun vwe-move--windmove-right()
  "Selected current right window."
  (interactive)
  (condition-case nil (windmove-right)
    (error (condition-case nil (windmove-left) (error)))))

(defun vwe-move--windmove-left()
  "Selected current left window."
  (interactive)
  (condition-case nil (windmove-left)
    (error (condition-case nil (windmove-right) (error)))))

;;
;; buffer swap
;;
(defun vwe-move--swap-up-buffer ()
  "Swap current and up buffer."
  (interactive)
  (let ((current-window (selected-window))
		(current-buffer (buffer-name))
		(swaped-window)
		(swaped-buffer))
	(vwe-move--windmove-up)
	(setq swaped-window (selected-window)
		  swaped-buffer (buffer-name))
	(when (and (not (string= swaped-buffer current-buffer)))
	  (set-window-buffer swaped-window current-buffer)
	  (set-window-buffer current-window swaped-buffer))))

(defun vwe-move--swap-down-buffer ()
  "Swap current and down buffer."
  (interactive)
  (let ((current-window (selected-window))
		(current-buffer (buffer-name))
		(swaped-window)
		(swaped-buffer))
	(vwe-move--windmove-down)
	(setq swaped-window (selected-window)
		  swaped-buffer (buffer-name))
	(when (and (not (string= swaped-buffer current-buffer)))
	  (set-window-buffer swaped-window current-buffer)
	  (set-window-buffer current-window swaped-buffer))))

(defun vwe-move--swap-right-buffer ()
  "Swap current and right buffer."
  (interactive)
  (let ((current-window (selected-window))
		(current-buffer (buffer-name))
		(swaped-window nil)
		(swaped-buffer nil))
	(vwe-move--windmove-right)
	(setq swaped-window (selected-window)
		  swaped-buffer (buffer-name))
	(when (and (not (string= swaped-buffer current-buffer)))
	  (set-window-buffer swaped-window current-buffer)
	  (set-window-buffer current-window swaped-buffer))))

(defun vwe-move--swap-left-buffer()
  "Swap current and right buffer."
  (interactive)
  (let ((current-window (selected-window))
		(current-buffer (buffer-name))
		(swaped-window nil)
		(swaped-buffer nil))
	(vwe-move--windmove-left)
	(setq swaped-window (selected-window)
		  swaped-buffer (buffer-name))
	(when (and (not (string= swaped-buffer current-buffer)))
	  (set-window-buffer swaped-window current-buffer)
	  (set-window-buffer current-window swaped-buffer))))

;;
;; buffer switch
;;
(defvar vwe-move--switch-side-buffer-name
  "*vwe-layout:switch-side-tmp-buffer*"
  "Switch side buffer name.")

(defvar vwe-move--switch-buffer-keymap
  (let* ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd "q") #'vwe-move--kill-switch-side-buffer)
	(define-key keymap (kbd "n") #'next-line)
	(define-key keymap (kbd "p") #'previous-line)
	(define-key keymap (kbd "s") (lambda () (interactive) (if (fboundp 'swiper) (swiper) (isearch-forward))))
	(define-key keymap (kbd "TAB") #'vwe-move--switch-buffer)
	keymap)
  "Switch buffer keymap.")

(defface vwe-move--default-face
  '((t (:inherit 'default :weight bold)))
  "Default face.")

(defface vwe-move--info-face
  '((t (:foreground "DarkOrange" :weight bold)))
  "Info face.")

(defface vwe-move--success-face
  '((t (:foreground "SpringGreen" :weight bold)))
  "Success face.")

(defface vwe-move--warning-face
  '((t (:foreground "yellow" :weight bold)))
  "Warning face.")

(defface vwe-move--error-face
  '((t (:foreground "DarkRed" :weight bold)))
  "Error face.")

(defface vwe-move--button-face
  '((t (:foreground "SkyBlue" :weight bold)))
  "Button face.")

(defvar vwe-move--switch-side-show-status
  'buffer
  "Switch side buffer show content `buffer' or `temp' buffer.")

(defun vwe-move--switch-buffer ()
  "Switch buffer."
  (interactive)
  (cond
   ((equal vwe-move--switch-side-show-status 'buffer) (setq vwe-move--switch-side-show-status 'temp) (vwe-move--display-switch-side-buffer))
   ((equal vwe-move--switch-side-show-status 'temp) (setq vwe-move--switch-side-show-status 'buffer) (vwe-move--display-switch-side-buffer t))
   (t (setq vwe-move--switch-side-show-status 'temp) (vwe-move--display-switch-side-buffer))))

(defun vwe-move--build-switch-buffer-headerline ()
  "Build switch buffer headerline."
  (let* ((allbuf (length (buffer-list)))
		 (buflen (length (vwe-move--get-buffer-list)))
		 (tmpbuflen (length (vwe-move--get-buffer-list t)))
		 (hidebuflen (- allbuf buflen tmpbuflen))
		 (split (propertize (format " | ") 'face 'vwe-move--default-face)))
	(concat
	 (propertize (format "Vwe switch buffer:")
				 'face 'vwe-move--success-face)
	 split
	 (propertize (format "total " )
				 'face 'vwe-move--default-face)
	 (propertize (format "%d" allbuf)
				 'face 'vwe-move--success-face)
	 split
	 (propertize (format "buffer ")
				 'face 'vwe-move--default-face)
	 (propertize (format "%d" buflen)
				 'face 'vwe-move--info-face)
	 split
	 (propertize (format "* buffer ")
				 'face 'vwe-move--default-face)
	 (propertize (format "%d" tmpbuflen)
				 'face 'vwe-move--warning-face)
	 split
	 (propertize (format "hide buffer ")
				 'face 'vwe-move--default-face)
	 (propertize (format "%d" hidebuflen)
				 'face 'vwe-move--button-face)
	 split
	 (propertize (format "buffer/*buffer ")
				 'face 'vwe-move--default-face)
	 (propertize (format "[TAB]")
				 'face 'vwe-move--info-face)
	 (propertize (format " quit ")
				 'face 'vwe-move--default-face)
	 (propertize (format "[q]")
				 'face 'vwe-move--info-face))))

(defun vwe-move--display-switch-side-buffer (&optional tmp?)
  "Make switch buffer cmd buffer.
TMP is tmp buffer."
  (let* ((buffer (if (get-buffer vwe-move--switch-side-buffer-name) (get-buffer vwe-move--switch-side-buffer-name) (get-buffer-create vwe-move--switch-side-buffer-name)))
		 ;; (buffer-length (length (vwe-move--get-buffer-list)))
		 ;; (tmpbuf-length (length (vwe-move--get-buffer-list t)))
		 ;; (headerline (concat (format "buffer total %d | buffer %d | tmp buffer %d | hide buffer %d"
		 ;; (length (buffer-list)) buffer-length tmpbuf-length (- (length (buffer-list)) buffer-length tmpbuf-length))))
		 (headerline (vwe-move--build-switch-buffer-headerline))
		 (bufname-list (if tmp?
						   (mapcar #'buffer-name (vwe-move--get-buffer-list t))
						 (mapcar #'buffer-name (vwe-move--get-buffer-list))))
		 (alist '((window-width . vwe-key--max-width)
				  (window-height . fit-window-to-buffer)
				  (direction . 'down)
				  (slot . 0))))
	(when buffer
	  (with-current-buffer buffer
		(read-only-mode -1)
		(erase-buffer)
		(setq-local mode-line-format nil
					word-wrap nil
					show-trailing-whitespace nil
					header-line-format headerline)
		(vwe-move--switch-buffer-mode 1)
		(dotimes (i (length bufname-list))
		  (insert-button (concat  "[" (nth i bufname-list) "]" (unless (= (1+ i) (length bufname-list)) "\n"))
						 'action (lambda(_)
								   (vwe-move--kill-switch-side-buffer)
								   (switch-to-buffer (nth i bufname-list)))
						 'follow-link t))
		(goto-char (point-min))
		(read-only-mode t))
	  (select-window (display-buffer-in-side-window buffer alist)))))

(defun vwe-move--kill-switch-side-buffer ()
  "Kill side buffer."
  (interactive)
  (let* ((buffer (get-buffer vwe-move--switch-side-buffer-name)))
	(when buffer
	  (delete-windows-on buffer)
	  (kill-buffer buffer)
	  (setq vwe-move--switch-side-show-status 'buffer))))

(defun vwe-move--buffer-list-filter (regexp &optional self?)
  "Find buffer list of REGEXP.
SELF is include curretn buffer."
  (seq-filter 'bufferp
			  (mapcar
			   (lambda (item)
				 (if (and (string-match regexp (buffer-name item)) (not (equal vwe-move--switch-side-buffer-name (buffer-name item))))
					 (if self?
						 item
					   (unless (equal item (current-buffer)) item))))
			   (buffer-list))))

(defun vwe-move--get-buffer-list (&optional tmp? self?)
  "Get buffer list.
TMP is asterisk buffers.
SELF is include curretn buffer."
  (if tmp?
	  (vwe-move--buffer-list-filter "^*" self?)
	(vwe-move--buffer-list-filter "^[^\s*]" self?)))

(defun vwe-move--switch-buffer-enable ()
  "Enable mode.")

(defun vwe-move--switch-buffer-disable ()
  "Disable mode."
  (vwe-move--kill-switch-side-buffer))

;;;###autoload
(define-minor-mode vwe-move--switch-buffer-mode
  "Mode."
  :keymap vwe-move--switch-buffer-keymap
  (if vwe-move--switch-buffer-mode
	  (vwe-move--switch-buffer-enable)
    (vwe-move--switch-buffer-disable)))

;;
;; marker point
;;
(defvar vwe-move-marker-point--keymap
  (let* ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd "M-* m") #'vwe-move-marker-point--marker)
	(define-key keymap (kbd "M-* g") #'vwe-move-marker-point--goto-marker)
	(define-key keymap (kbd "M-* c") #'vwe-move-marker-point--clear-marker)
	keymap)
  "Last mark point.")

(defface vwe-move-marker-point--face
  '((t (:inherit 'error :weight bold)))
  "Posit mark face.")

(defvar-local vwe-move-marker-point--marker-point-overlay
  nil
  "Last mark point overlay.")

(defun vwe-move-marker-point--line-indent (&optional goto)
  "Get current point or GOTO point line indentation."
  (interactive)
  (unless goto (setq goto (point)))
  (let* ((ind-pos (save-excursion (goto-char goto) (back-to-indentation) (point))))
	ind-pos))

(defun vwe-move-marker-point--line-eol (&optional goto)
  "Get current point or GOTO point line eol."
  (interactive)
  (unless goto (setq goto (point)))
  (let* ((eol-pos (save-excursion (goto-char goto) (end-of-line) (skip-chars-backward " \t" (vwe-move-marker-point--line-indent)) (point))))
	eol-pos))

(defun vwe-move-marker-point--marker ()
  "Mark current point."
  (interactive)
  (let* ((overlay (make-overlay (1- (point)) (point))))
	(setq vwe-move-marker-point--marker-point-overlay overlay)
	(overlay-put overlay 'after-string
				 (propertize (format "[M]")
							 'display '((raise 0.5) (height 0.8))
							 'face 'vwe-move-marker-point--face))))

(defun vwe-move-marker-point--goto-marker ()
  "Got mark list first point."
  (interactive)
  (let* ((goto-point (overlay-end vwe-move-marker-point--marker-point-overlay)))
	(condition-case nil
		(progn (when (numberp goto-point) (goto-char goto-point)))
	  (error nil))))

(defun vwe-move-marker-point--clear-marker ()
  "Clear mark."
  (interactive)
  (delete-overlay vwe-move-marker-point--marker-point-overlay))

(define-minor-mode vwe-move-marker-point-mode
  "Point mode."
  :group 'vwe-mark
  :global t
  :keymap vwe-move-marker-point--keymap)

;;
;; line preview
;;
(defvar vwe-move-line-preview--origin-window
  nil
  "Origin window.")

(defvar vwe-move-line-preview--origin-window-line
  nil
  "Origin window line.")

(defvar vwe-move-line-preview--origin-window-point
  nil
  "Origin window point.")

(defvar vwe-move-line-preview--line-number
  nil
  "Preveiw goto line.")

(defvar vwe-move-line-preview--transient-keymap
  (let ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd "q") #'vwe-move-line-preview--recovery)
	(define-key keymap (kbd "g") #'vwe-move-line-preview--goto-line)
	keymap)
  "Preview temp keymap.")

(defvar vwe-mark-line-preview--keymap
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Move to mark map.")

(defun vwe-move-line-preview ()
  "Preview goto line."
  (interactive)
  (let* ((input-num-str (thing-at-point 'line)))
	(when input-num-str
	  (setq vwe-move-line-preview--line-number (string-to-number input-num-str)))
	(save-selected-window
	  (when (and vwe-move-line-preview--origin-window vwe-move-line-preview--line-number)
		(select-window vwe-move-line-preview--origin-window)
		(unless (zerop vwe-move-line-preview--line-number)
		  (goto-char (point-min))
		  (forward-line (1- vwe-move-line-preview--line-number)))
		;; (set-transient-map vwe-move-line-preview--transient-keymap nil #'vwe-move-line-preview--recovery)
		))
	(message "preview line %s" vwe-move-line-preview--line-number)))

(defun vwe-move-line-preview--goto-line (&optional line)
  "Preview goto LINE."
  (interactive)
  (setq vwe-move-line-preview--line-number (if line line (read-number "line:")))
  (goto-char (point-min))
  (forward-line (1- vwe-move-line-preview--line-number))
  ;; (vwe-move-line-previe--bulid-preview-origin-snapshot)
  ;; (vwe-move-line-preview)
  )

(defun vwe-move-line-preview--dynamic-goto-line ()
  "Preview dynamic goto line."
  (interactive)
  (vwe-move-line-previe--bulid-preview-origin-snapshot)
  (unwind-protect
	  (setq vwe-move-line-preview--line-number (read-number "line:"))
	(set-window-point vwe-move-line-preview--origin-window vwe-move-line-preview--origin-window-point)))

(defun vwe-move-line-previe--bulid-preview-origin-snapshot ()
  "Preview goto line."
  (interactive)
  (let* ((window (selected-window))
		 (line-num (line-number-at-pos))
		 (cur-point (point)))
	(setq vwe-move-line-preview--origin-window window
		  vwe-move-line-preview--origin-window-line line-num
		  vwe-move-line-preview--origin-window-point cur-point))
  (message "build exec finished %S" vwe-move-line-preview--origin-window))

(defun vwe-move-line-preview--recovery ()
  "Preview recovery."
  (interactive)
  (select-window vwe-move-line-preview--origin-window)
  (when (and vwe-move-line-preview--origin-window (numberp vwe-move-line-preview--origin-window-point))
	(set-window-point vwe-move-line-preview--origin-window vwe-move-line-preview--origin-window-point)))

(defun vwe-move-line-preview--cmd-config ()
  "Preview hook for minibuffer command."
  (when (memq this-command '(vwe-move-line-preview--dynamic-goto-line))
    (add-hook 'post-command-hook #'vwe-move-line-preview nil t)))

(defun vwe-move-line-preview-enable ()
  "Enable preview."
  (define-key vwe-mark-line-preview--keymap (kbd "M-* r") #'vwe-move-line-preview--goto-line)
  (define-key vwe-mark-line-preview--keymap (kbd "M-* d") #'vwe-move-line-preview--dynamic-goto-line)
  (add-hook 'minibuffer-setup-hook 'vwe-move-line-preview--cmd-config))

(defun vwe-move-line-preview--disable ()
  "Disable preview."
  (remove-hook 'minibuffer-setup-hook 'vwe-move-line-preview--cmd-config))

(define-minor-mode vwe-move-line-preview-mode
  "Mark line preview mode."
  :group 'vwe-mark
  :keymap vwe-mark-line-preview--keymap
  :global t
  (if vwe-move-line-preview-mode
	  (vwe-move-line-preview-enable)
	(vwe-move-line-preview--disable)))

;; =============================================================================
;; goto line
;;
;; =============================================================================
(defvar vwe-move-goto-line--keymap (let ((keymap (make-sparse-keymap)))
									 (define-key keymap (kbd "M-* p") #'vwe-move-goto-line-previous)
									 (define-key keymap (kbd "M-* n") #'vwe-move-goto-line-next)
									 keymap)
  "Move to mark map.")

(defvar vwe-move-goto-line--current-overlay-list
  nil
  "Current overlay list.")

(defun vwe-move-goto-line--calculate-marker-number (win-pos)
  "Calculate mark number, WIN-POS if nil reverse calculate."
  (let* ((cur-line (line-number-at-pos))
		 (number))
	(save-excursion
	  (if win-pos
		  (setq number (- (line-number-at-pos (window-end)) cur-line))
		(setq number (- cur-line (line-number-at-pos (window-start)))))
	  number)))

(defun vwe-move-goto-line--show-mark (win-pos)
  "Show mark with WIN-POS."
  (interactive)
  (when vwe-move-goto-line--current-overlay-list (mapc #'delete-overlay vwe-move-goto-line--current-overlay-list))
  (let* ((keymap vwe-move-goto-line--keymap)
		 (ov-list)
		 (pos-list (vwe-move-goto-line--overlay-alist
					(vwe-move-goto-line--calculate-marker-number win-pos) win-pos)))
	(dotimes (i (length pos-list))
	  (let* ((ov (make-overlay (cadr (nth i pos-list)) (1+ (cadr (nth i pos-list))))))
		(overlay-put ov 'before-string
					 (propertize (format "%d" (car (nth i pos-list)))
								 'display '((raise 0.5))
								 'face 'vwe-mark--position--face))
		(push ov ov-list)
		))
	(define-key keymap (kbd "q") (lambda() (interactive) (mapc #'delete-overlay ov-list) (vwe-move-goto-line-mode -1)))
	(if win-pos
		(define-key keymap (kbd "g") #'vwe-move-goto-line-next-move-to)
	  (define-key keymap (kbd "g") #'vwe-move-goto-line-previous-move-to))
	(setq vwe-move-goto-line--current-overlay-list ov-list)))

(defun vwe-move-goto-line--overlay-alist (num win-pos)
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

(defun vwe-move-goto-line-previous ()
  "Line previous."
  (interactive)
  (when vwe-mark-mode
	(vwe-move-goto-line-mode t)
	(vwe-move-goto-line--show-mark nil)))

(defun vwe-move-goto-line-next ()
  "Line next."
  (interactive)
  (when vwe-mark-mode
	(vwe-move-goto-line-mode t)
	(vwe-move-goto-line--show-mark t)))

(defun vwe-move-goto-line-next-move-to (&optional num)
  "Move to NUM line."
  (interactive "nto:")
  (unless num (set num 1))
  (when (numberp num)
	(forward-line num))
  (vwe-move-goto-line-next))

(defun vwe-move-goto-line-previous-move-to (&optional num)
  "Move to NUM line."
  (interactive "nto:")
  (unless num (set num -1))
  (when (numberp num)
	(forward-line (* num -1)))
  (vwe-move-goto-line-previous))

(defun vwe-move-goto-line--enable ()
  "Enable.")

(defun vwe-move-goto-line--disable ()
  "Disable."
  (when vwe-move-goto-line--current-overlay-list
	(mapc #'delete-overlay vwe-move-goto-line--current-overlay-list))
  (setq vwe-move-goto-line--current-overlay-list nil))

(define-minor-mode vwe-move-goto-line-mode
  "Mark line mode."
  :group 'vwe-mark
  :keymap vwe-move-goto-line--keymap
  (if vwe-move-goto-line-mode
	  (vwe-move-goto-line--enable)))

;; =============================================================================
;; mark and goto
;; word/symbol,paren,line,expression
;; =============================================================================
(defvar vwe-mark-and-goto--keymap
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Move to mark map.")

(defvar vwe-mark-and-goto--show-mark-keymap
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Move to mark map.")

(defvar vwe-mark-and-goto-show--keymap
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Move to mark map.")

(defface vwe-mark-and-goto--mark-face
  '((t (:inherit 'error :inverse-video nil)))
  "Position makr hint face."
  :group 'vwe-mark)

(defvar vwe-mark-and-goto--mark-list
  nil
  "Mark list, like '(point1 ... pointn)).")

(defvar vwe-mark-and-goto--is-back
  nil
  "Is backward.")

(defvar vwe-mark-and-goto--type
  'word
  "Mark and goto type, `word', `symbol', `line', `expression', `paragraph',`paren'.")

(defun vwe-mark-and-goto--mark ()
  "Mark word, forward."
  (interactive)
  (setq vwe-mark-and-goto--mark-list nil)
  (let* ((win-start (window-start))
		 (win-end (window-end))
		 (mark-pos (point))
		 (mark-overlay nil)
		 (mark-index 0)
		 (next t))
	(vwe-mark-and-goto-show-mode-enable)
	(when (= mark-pos (point-min))
	  (setq mark-pos (1+ mark-pos)))
	(when (= mark-pos (point-max))
	  (setq mark-pos (1- mark-pos)))

	(save-excursion
	  (while next
		(if vwe-mark-and-goto--is-back
			(progn
			  (cond
			   ((eq vwe-mark-and-goto--type 'word) (backward-word))
			   ((eq vwe-mark-and-goto--type 'symbol) (forward-symbol -1))
			   ((eq vwe-mark-and-goto--type 'line) (forward-line -1))
			   ((eq vwe-mark-and-goto--type 'expression) (backward-sexp))
			   ((eq vwe-mark-and-goto--type 'paragraph) (backward-paragraph))
			   ))
		  (progn
			(cond
			 ((eq vwe-mark-and-goto--type 'word) (forward-word))
			 ((eq vwe-mark-and-goto--type 'symbol) (forward-symbol 1))
			 ((eq vwe-mark-and-goto--type 'line) (forward-line))
			 ((eq vwe-mark-and-goto--type 'expression) (forward-sexp))
			 ((eq vwe-mark-and-goto--type 'paragraph) (forward-paragraph))
			 )))

		(if (or (>= mark-pos win-end) (<= mark-pos win-start))
			(setq next nil)
		  (setq mark-pos (point)
				mark-overlay (make-overlay (1- mark-pos) mark-pos)
				mark-index (1+ mark-index)
				vwe-mark-and-goto--mark-list (append vwe-mark-and-goto--mark-list (list (list mark-index mark-pos mark-overlay))))
		  (overlay-put mark-overlay 'after-string
					   (propertize (format "%d" mark-index)
								   'display '((raise 0.5))
								   'face 'vwe-mark-and-goto--mark-face))))
	  ;; (define-key vwe-mark-and-goto--show-mark-keymap (kbd "q") #'vwe-mark-and-goto--remove-mark-list)
	  ;; (define-key vwe-mark-and-goto--show-mark-keymap (kbd "C-g") #'vwe-mark-and-goto--remove-mark-list)
	  ;; (define-key vwe-mark-and-goto--show-mark-keymap (kbd "g") #'vwe-mark-and-goto--goto-mark)
	  ;; (set-transient-map vwe-mark-and-goto--show-mark-keymap nil nil)
	  )))

(defun vwe-mark-and-goto--goto-mark (&optional arg)
  "Goto mark ARG."
  (interactive)
  (let* ((goto (if arg arg (read-number "goto:"))))
	(if vwe-mark-and-goto--is-back
		(progn
		  (cond
		   ((eq vwe-mark-and-goto--type 'word) (backward-word goto))
		   ((eq vwe-mark-and-goto--type 'symbol) (forward-symbol (* goto -1)))
		   ((eq vwe-mark-and-goto--type 'line) (forward-line (* goto -1)))
		   ((eq vwe-mark-and-goto--type 'expression) (backward-sexp goto))
		   ((eq vwe-mark-and-goto--type 'paragraph) (backward-paragraph goto))
		   ))
	  (progn
		(cond
		 ((eq vwe-mark-and-goto--type 'word) (forward-word goto))
		 ((eq vwe-mark-and-goto--type 'symbol) (forward-symbol goto))
		 ((eq vwe-mark-and-goto--type 'line) (forward-line goto))
		 ((eq vwe-mark-and-goto--type 'expression) (forward-sexp goto))
		 ((eq vwe-mark-and-goto--type 'paragraph) (forward-paragraph goto)))))

	(vwe-mark-and-goto--remove-mark-list)
	(setq vwe-mark-and-goto--is-back nil)
	(vwe-mark-and-goto-show-mode-disable)))

(defun vwe-mark-and-goto--remove-mark-list ()
  "Remove mark list."
  (interactive)
  (when vwe-mark-and-goto--mark-list
	(mapc #'delete-overlay (mapcar (lambda (m) (caddr m)) vwe-mark-and-goto--mark-list))
	(setq vwe-mark-and-goto--mark-list nil))
  (when vwe-mark-and-goto-show-mode
	(vwe-mark-and-goto-show-mode-disable)))

(defun vwe-mark-and-goto--mark-line ()
  "Mark line."
  (interactive)
  (setq vwe-mark-and-goto--type 'line)
  (vwe-mark-and-goto--remove-mark-list)
  (vwe-mark-and-goto--mark))

(defun vwe-mark-and-goto--forward-mark-line ()
  "Forward mark line."
  (interactive)
  (setq vwe-mark-and-goto--is-back nil)
  (vwe-mark-and-goto--mark-line))

(defun vwe-mark-and-goto--backward-mark-line ()
  "Backward mark line."
  (interactive)
  (setq vwe-mark-and-goto--is-back t)
  (vwe-mark-and-goto--mark-line))

(defun vwe-mark-and-goto--mark-word ()
  "Mark word."
  (interactive)
  (setq vwe-mark-and-goto--type 'word)
  (vwe-mark-and-goto--remove-mark-list)
  (vwe-mark-and-goto--mark))

(defun vwe-mark-and-goto--forward-mark-word ()
  "Forward mark word."
  (interactive)
  (setq vwe-mark-and-goto--is-back nil)
  (vwe-mark-and-goto--mark-word))

(defun vwe-mark-and-goto--backward-mark-word ()
  "Backward mark word."
  (interactive)
  (setq vwe-mark-and-goto--is-back t)
  (vwe-mark-and-goto--mark-word))

(defun vwe-mark-and-goto-show-mode-enable ()
  "Enable mode."
  (vwe-mark-and-goto-show-mode 1)
  (setq buffer-read-only t)
  (add-hook 'vwe-editor-edit-mode-hook #'vwe-mark-and-goto-show-mode-disable)
  (define-key vwe-mark-and-goto-show--keymap (kbd "g") #'vwe-mark-and-goto--goto-mark)
  (define-key vwe-mark-and-goto-show--keymap (kbd "q") #'vwe-mark-and-goto--remove-mark-list)
  (define-key vwe-mark-and-goto-show--keymap (kbd "c-g") #'vwe-mark-and-goto--remove-mark-list))

(defun vwe-mark-and-goto-show-mode-disable ()
  "Disable mode."
  (setq buffer-read-only nil)
  (remove-hook 'vwe-editor-edit-mode-hook #'vwe-mark-and-goto-show-mode-disable)
  (vwe-mark-and-goto-show-mode -1)
  (vwe-mark-and-goto--remove-mark-list))

(define-minor-mode vwe-mark-and-goto-show-mode
  "Mark and goto show mode."
  :group 'vwe-move
  :keymap vwe-mark-and-goto-show--keymap
  :global nil
  )

(defun vwe-mark-and-goto-mode-enable ()
  "Enable mode."
  (define-key vwe-mark-and-goto--keymap (kbd "C->") #'vwe-mark-and-goto--forward-mark-word)
  (define-key vwe-mark-and-goto--keymap (kbd "C-<") #'vwe-mark-and-goto--backward-mark-word)
  (define-key vwe-mark-and-goto--keymap (kbd "M-* n") #'vwe-mark-and-goto--forward-mark-line)
  (define-key vwe-mark-and-goto--keymap (kbd "M-* p") #'vwe-mark-and-goto--backward-mark-line))

(defun vwe-mark-and-goto-mode-disable ()
  "Disable mode.")

(define-minor-mode vwe-mark-and-goto-mode
  "Mark and goto mode."
  :group 'vwe-move
  :keymap vwe-mark-and-goto--keymap
  :global t
  (if vwe-mark-and-goto-mode
	  (vwe-mark-and-goto-mode-enable)
	(vwe-mark-and-goto-mode-disable)))

;;
;; mode
;;
(defun vwe-move-mode-enable ()
  "Enable mode."
  (vwe-move-line-mode 1)
  (vwe-move-change-mode 1)
  (vwe-move-line-preview-mode 1)
  (vwe-move-marker-point-mode 1)
  (vwe-mark-and-goto-mode 1))

(defun vwe-move-mode-disable ()
  "Disable mode."
  (vwe-move-line-mode -1)
  (vwe-move-change-mode -1)
  (vwe-move-line-preview-mode -1)
  (vwe-move-marker-point-mode -1)
  (vwe-mark-and-goto-mode -1))

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
