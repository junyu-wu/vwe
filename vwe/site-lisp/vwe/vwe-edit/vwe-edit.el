;;; vwe-edit.el --- vwe edit                         -*- lexical-binding: t; -*-

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
(require 'cl-lib)

(defgroup vwe-edit nil
  "Edit."
  :group 'vwiss-vwe
  :prefix "vwe-edit--")

(defvar vwe-edit-region--keymap
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Region map.")

(defvar vwe-edit-region--edit-keymap
  (let ((keymap (make-sparse-keymap)))
	;; (define-key keymap (kbd "C-g") (lambda () (interactive) (vwe-edit-region-edit-mode -1) (keyboard-quit)))
	(define-key keymap (kbd "C-c C-c") #'vwe-edit-region--edit-finished)
	(define-key keymap (kbd "C-g") #'vwe-edit-region--edit-finished)
	keymap)
  "Region edit map.")

(defface vwe-edit-region--marker-cursor-face
  '((t (:foreground "red")))
  "Region marker face.")

(defface vwe-edit-region--marker-overlay-face
  '((t (:background "DeepSkyBlue")))
  "Region marker face.")

(defvar vwe-edit-region--marker-cursor
  "|"
  "Region mark char.")

(defvar vwe-edit-region--marker-empty-line-fill
  nil
  "Empty line fill.")

(defvar vwe-edit-region--marker-region-overlays
  nil
  "Marker overlay list.")

(defvar vwe-edit-region--marker-cursor-overlays
  nil
  "Marker overlay list.")

(defvar vwe-edit-region--this-command
  nil
  "This command.")

(defvar vwe-edit-region--marker-cursor-id
  0
  "Marker cursor id.")

(defvar vwe-edit-region--command-executed
  nil
  "Command executed.")

(defvar vwe-edit-region--real-cursor-position
  nil
  "Real cursor position.")

(defvar vwe-edit-region--command-ignore-line
  nil
  "Command ignore line.")

(defvar vwe-edit-region--support-command
  '(self-insert-command
	quoted-insert
	previous-line
	next-line
	newline
	newline-and-indent
	forward-char
	forward-word
	backward-char
	backward-word
	upcase-word
	downcase-word
	capitalize-word
	forward-list
	backward-list
	yank
	yank-pop
	kill-word
	kill-line
	backward-kill-word
	delete-char
	delete-forward-char
	delete-backward-char
	org-delete-backward-char
	end-of-line
	set-mark-command
	exchange-point-and-mark
	move-end-of-line
	beginning-of-line
	move-beginning-of-line
	back-to-indentation
	mwim-beginning-of-code-or-line
	mwim-end-of-code-or-line
	comment-dwim-2
	hungry-delete-backward
	hungry-delete-forward)
  "Support command.")

(defun vwe-edit-region--make-marker-cursor-id ()
  "Make marker cursor id."
  (cl-incf vwe-edit-region--marker-cursor-id))

(defun vwe-edit-region--line-number-at-point (&optional point)
  "Get current POINT line number."
  (let* ((pos (or point (point)))
		 (line (line-number-at-pos)))
	(save-excursion
	  (goto-char pos)
	  (setq line (line-number-at-pos)))
	line))

(defun vwe-edit-region--bol-at-point-p (&optional point)
  "POINT is begin of line."
  (let ((pos (or point (point))))
	(save-excursion
	  (goto-char pos)
	  (= (line-beginning-position) pos))))

(defun vwe-edit-region--eol-at-point-p (&optional point)
  "POINT is end of line."
  (let ((pos (or point (point))))
	(save-excursion
	  (goto-char pos)
	  (= (line-end-position) pos))))

(defun vwe-edit-region--column-at-point (&optional point)
  "Get POINT column."
  (let ((pos (or point (point))))
	(save-excursion
	  (goto-char pos)
	  (current-column))))

(defun vwe-edit-region--point-at-column (&optional column)
  "Get COLUMN point."
  (let ((col (or column (current-column))))
	(save-excursion
	  (goto-char (line-end-position))
	  (when (< (current-column) col) (setq col (current-column)))
	  (move-to-column col)
	  (point))))

(defun vwe-edit-region--empty-line-at-point-p (&optional point)
  "POINT is empty line."
  (let ((pos (or point (point))))
	(save-excursion
	  (goto-char pos)
	  (and (vwe-edit-region--bol-at-point-p) (vwe-edit-region--eol-at-point-p)))))

(defun vwe-edit-region--empty-line-at-line-p (&optional line)
  "LINE is empty line."
  (let ((line-num (or line (line-number-at-pos))))
	(save-excursion
	  (goto-char (point-min))
	  (forward-line (1- line-num))
	  (and (vwe-edit-region--bol-at-point-p) (vwe-edit-region--eol-at-point-p)))))

(defun vwe-edit-region--marker-overlay-region (begin end &optional end-cursorp)
  "Make overlay region at BEGIN point column END point column and END-CURSORP point."
  (let* ((begin-line (vwe-edit-region--line-number-at-point begin))
		 (end-line (vwe-edit-region--line-number-at-point end))
		 (begin-column (vwe-edit-region--column-at-point begin))
		 (end-column (vwe-edit-region--column-at-point end))
		 (to-leftp (if (< begin-column end-column) t nil))
		 (to-downp (if (< begin-line end-line) t nil)))
	(save-excursion
	  (goto-char begin)
	  (dotimes (_ (1+ (abs (- end-line begin-line))))
		(let* ((start) (end) (overlay))
		  (if to-leftp
			  (setq start (vwe-edit-region--point-at-column begin-column)
					end (vwe-edit-region--point-at-column end-column))
			(setq start (vwe-edit-region--point-at-column end-column)
				  end (vwe-edit-region--point-at-column begin-column)))
		  (setq overlay (make-overlay start end))
		  (if end-cursorp ;; cursor to mark line end point
			  (vwe-edit-region--marker-overlay-cursor end)
			(vwe-edit-region--marker-overlay-cursor start))
		  (overlay-put overlay 'face 'vwe-edit-region--marker-overlay-face)
		  (setq vwe-edit-region--marker-region-overlays (cons overlay vwe-edit-region--marker-region-overlays))
		  (if to-downp (forward-line 1) (forward-line -1)))))))

(defun vwe-edit-region--remove-marker-region-overlays ()
  "Remove marker overlays."
  (when vwe-edit-region--marker-region-overlays
	(mapc #'delete-overlay vwe-edit-region--marker-region-overlays)
	(setq vwe-edit-region--marker-region-overlays nil)
	(vwe-edit-region--remove-marker-cursor-overlays)))

(defun vwe-edit-region--marker-overlay-cursor (&optional point mcid)
  "Make overlay region at POINT and set MCID."
  (let* ((start-pos (or point (point)))
		 (end-pos (if (vwe-edit-region--eol-at-point-p) start-pos (1+ start-pos)))
		 (overlay)
		 (id (or mcid (vwe-edit-region--make-marker-cursor-id))))
	(if mcid
		(progn
		  (setq overlay (vwe-edit-region--find-cursor-overlay-by-id id))
		  (move-overlay overlay start-pos end-pos))
	  (setq overlay (make-overlay start-pos end-pos))
	  (overlay-put overlay 'vwe-mcid id)
	  (overlay-put overlay 'type 'vwe-mc)
	  (overlay-put overlay 'before-string (propertize vwe-edit-region--marker-cursor 'face 'vwe-edit-region--marker-cursor-face))
	  (setq vwe-edit-region--marker-cursor-overlays (cons overlay vwe-edit-region--marker-cursor-overlays)))))

(defun vwe-edit-region--find-cursor-overlay-by-id (id)
  "Find cursor overlay by ID."
  (when vwe-edit-region--marker-cursor-overlays
	(save-excursion
	  (let ((overlay))
		(dotimes (i (length vwe-edit-region--marker-cursor-overlays))
		  (when (eq (overlay-get (nth i vwe-edit-region--marker-cursor-overlays) 'vwe-mcid) id)
			(setq overlay (nth i vwe-edit-region--marker-cursor-overlays))))
		overlay))))

(defun vwe-edit-region--remove-marker-cursor-overlays (&optional mcid)
  "Remove marker MCID or all cursor overlays."
  (when vwe-edit-region--marker-cursor-overlays
	(if mcid
		(delete-overlay (vwe-edit-region--find-cursor-overlay-by-id mcid))
	  (mapc #'delete-overlay vwe-edit-region--marker-cursor-overlays))
	(setq vwe-edit-region--marker-cursor-overlays nil)))

;;;###autoload
(defun vwe-edit-region--mark-edit ()
  "Edit START point and END pioint region."
  (interactive)
  (when (and mark-active (/= (point) (mark)))
	(let* ((start-pos (mark))
		   (end-pos (point)))
	  (setq vwe-edit-region--real-cursor-position end-pos
			vwe-edit-region--command-ignore-line (vwe-edit-region--line-number-at-point end-pos))
	  (deactivate-mark)
	  (vwe-edit-region--marker-overlay-region start-pos end-pos)
	  (goto-char start-pos)
	  (vwe-edit-region-edit-mode 1))))

(defun vwe-edit-region--marker-overlay-cursor-p (overlay)
  "Marker OVERLAY cursor."
  (eq (overlay-get overlay 'type) 'vwe-mc))

(defun vwe-edit-region--marker-overlay-cursor-total ()
  "Current buffer marker overlay cursor total."
  (1+ (cl-count-if 'vwe-edit-region--marker-overlay-cursor-p (overlays-in (point-min) (point-max)))))

(defun vwe-edit-region--execute-command (cmd)
  "Run CMD, simulating the parts of the command loop for cursors."
  (ignore-errors
	(when cmd
	  (setq this-command cmd)
	  (unless (or (eq this-command 'ignore) (eq this-command 'keyboard-quit)) (call-interactively cmd))
	  (message "command %s executed" cmd))))

(defun vwe-edit-region--execute-command-for-cursor (cmd cursor)
  "Run CMD, simulating the parts of the command loop for CURSOR."
  (let* ((mcid (overlay-get cursor 'vwe-mcid))
		 (start (overlay-start cursor)))
	(save-excursion
	  (goto-char start)
	  (ignore-errors
		(progn
		  (vwe-edit-region--execute-command cmd)
		  (vwe-edit-region--marker-overlay-cursor (point) mcid))))))

(defun vwe-edit-region--execute-command-for-all-cursors (cmd)
  "Run CMD, simulating the parts of the command loop for all cursors."
  (dotimes (i (length vwe-edit-region--marker-cursor-overlays))
	(vwe-edit-region--execute-command-for-cursor cmd (nth i vwe-edit-region--marker-cursor-overlays))))

(defun vwe-edit-region--store-original-command ()
  "Store original command."
  (unless (equal this-command 'vwe-edit-region--edit-finished)
	(let ((cmd (or (command-remapping this-original-command)
				   this-original-command)))
      (setq vwe-edit-region--this-command cmd
			this-command 'ignore))))

(defun vwe-edit-region--execute-original-command ()
  "Edit mode run command."
  (if (eq 1 (vwe-edit-region--marker-overlay-cursor-total))
	  (vwe-edit-region-edit-mode -1)
	(when this-original-command
	  (let* ((cmd (or vwe-edit-region--this-command
					  (command-remapping this-original-command)
					  this-original-command)))
		(when (functionp cmd)
		  (if (or (not (symbolp cmd)) (string-prefix-p "(" (symbol-name cmd)))
			  (vwe-edit-region--execute-command-for-all-cursors cmd)
			(setq cmd (intern (symbol-name cmd)))
			(when (memq cmd vwe-edit-region--support-command)
			  (vwe-edit-region--execute-command-for-all-cursors cmd))))))))

(defun vwe-edit-region--edit-finished ()
  "Edit finished."
  (interactive)
  (vwe-edit-region-edit-mode -1)
  ;; (keyboard-quit)
  )

;;
;; mode
;;
(defun vwe-edit-region-edit-mode-enable ()
  "Enable mode."
  (setq cursor-type nil)
  (add-hook 'pre-command-hook 'vwe-edit-region--store-original-command t t)
  (add-hook 'post-command-hook 'vwe-edit-region--execute-original-command t t))

(defun vwe-edit-region-edit-mode-disable ()
  "Disable mode."
  (remove-hook 'pre-command-hook 'vwe-edit-region--store-original-command t)
  (remove-hook 'post-command-hook 'vwe-edit-region--execute-original-command t)
  (vwe-edit-region--remove-marker-cursor-overlays)
  (vwe-edit-region--remove-marker-region-overlays)
  (setq vwe-edit-region--this-command nil
		vwe-edit-region--marker-cursor-id 0
		vwe-edit-region--real-cursor-position nil
		vwe-edit-region--command-ignore-line nil
		cursor-type 'bar))

;;;###autoload
(define-minor-mode vwe-edit-region-edit-mode
  "Edit region minor mode."
  :group 'vwe-edit-region
  :keymap vwe-edit-region--edit-keymap
  (if vwe-edit-region-edit-mode
	  (vwe-edit-region-edit-mode-enable)
	(vwe-edit-region-edit-mode-disable)))

(defun vwe-edit-region-mode-enable ()
  "Enable mode.")

(defun vwe-edit-region-mode-disable ()
  "Disable mode.")

;;;###autoload
(define-minor-mode vwe-edit-region-mode
  "Region minor mode."
  :group 'vwe-edit-region
  :keymap vwe-edit-region--keymap
  :global t
  (if vwe-edit-region-mode
	  (vwe-edit-region-mode-enable)
	(vwe-edit-region-mode-disable)))

;;
;; toggle case
;;
(defconst vwe-edit-toggle-case--symbol-chars
  "a-zA-Z0-9_-"
  "Word chars.")

(defconst vwe-edit-toggle-case--convert-case-list
  '(("upper case" . vwe-edit-toggle-case--upper-case)
	("lower case" . vwe-edit-toggle-case--lower-case)
	("camel case" . vwe-edit-toggle-case--camel-case)
	("capitalize" . vwe-edit-toggle-case--capitalize)
	("underline" . vwe-edit-toggle-case--underline)
	("hyphen" . vwe-edit-toggle-case--hyphen))
  "Convert function list.")

(defun vwe-edit-toggle-case--find-string-region-point ()
  "Find string start and end point."
  (let* ((start (point))
		 (end (point)))
	(if (use-region-p)
		(setq start (if (> (region-end) (region-beginning)) (region-beginning) (region-end))
			  end (if (> (region-end) (region-beginning)) (region-end) (region-beginning)))
	  (save-excursion
		(setq start (progn (skip-chars-backward vwe-edit-toggle-case--symbol-chars) (point))
			  end (progn (skip-chars-forward vwe-edit-toggle-case--symbol-chars) (point)))))
	(list start end)))

(defun vwe-edit-toggle-case--get-current-string-and-delete ()
  "Gets the symbol near the cursor."
  (let* ((region (vwe-edit-toggle-case--find-string-region-point))
		 (start (car region))
		 (end (cadr region))
		 (str (buffer-substring start end)))
	(delete-region start end)
	str))

(defun vwe-edit-toggle-case--list-p (str)
  "STR is list."
  (let ((case-fold-search nil))
	(> (length (split-string str " ")) 1)))

(defun vwe-edit-toggle-case--lower-case-p (str)
  "STR like foo."
  (let ((case-fold-search nil))
    (string-match "\\`[a-z0-9]+\\'" str)))

(defun vwe-edit-toggle-case--upper-case-p (str)
  "STR like FOO."
  (let ((case-fold-search nil))
    (string-match "\\`[A-Z0-9]+\\'" str)))

(defun vwe-edit-toggle-case--underline-p (str)
  "STR like foo_bar."
  (let ((case-fold-search nil))
    (string-match "_" str)))

(defun vwe-edit-toggle-case--camel-case-p (str)
  "STR like FooBar."
  (let ((case-fold-search nil))
    (or (and (string-match "[A-Z]" str) (string-match "\\`[a-z][a-zA-Z0-9]+\\'" str))
		(and (string-match "[a-z]" str) (string-match "\\`[A-Z][a-zA-Z0-9]+\\'" str)))))

(defun vwe-edit-toggle-case--hyphen-p (str)
  "STR like foo-bar."
  (let ((case-fold-search nil))
	(string-match "-" str)))

(defun vwe-edit-toggle-case--convert-to-upper-case (str)
  "Upper case STR."
  (upcase str))

(defun vwe-edit-toggle-case--convert-to-lower-case (str)
  "Lower case STR."
  (downcase str))

(defun vwe-edit-toggle-case--convert-to-capitalize (str)
  "Capitalize STR."
  (capitalize str))

(defun vwe-edit-toggle-case--convert-to-underline (str)
  "Underline STR."
  (let ((case-fold-search nil))
    (setq str (replace-regexp-in-string "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1_\\2" str))
    (setq str (replace-regexp-in-string "\\([A-Z]+\\)\\([A-Z][a-z]\\)" "\\1_\\2" str))
    (setq str (replace-regexp-in-string "-" "_" str)) ; FOO-BAR => FOO_BAR
    (setq str (replace-regexp-in-string "_+" "_" str))
    (downcase str)))

(defun vwe-edit-toggle-case--convert-to-camel-case (str)
  "Camel case STR."
  (setq str (split-string (vwe-edit-toggle-case--convert-to-underline str) "_"))
  (concat (downcase (car str))
          (mapconcat 'capitalize (cdr str) "")))

(defun vwe-edit-toggle-case--convert-to-hyphen (str)
  "Hyphen STR."
  (let ((case-fold-search nil))
    (setq str (vwe-edit-toggle-case--convert-to-underline str))
    (setq str (replace-regexp-in-string "_" "-" str))))

;;;###autoload
(defun vwe-edit-toggle-case--upper-case ()
  "Upper case."
  (interactive)
  (let* ((pos (vwe-edit-toggle-case--find-string-region-point))
		 (str (vwe-edit-toggle-case--get-current-string-and-delete))
		 (str-listp (vwe-edit-toggle-case--list-p str)))
	(if str-listp
		(setq str (mapconcat #'vwe-edit-toggle-case--convert-to-upper-case (split-string str " ") " "))
	  (setq str (vwe-edit-toggle-case--convert-to-upper-case str)))
	(goto-char (car pos))
	(insert str)))

;;;###autoload
(defun vwe-edit-toggle-case--lower-case ()
  "Lower case."
  (interactive)
  (let* ((pos (vwe-edit-toggle-case--find-string-region-point))
		 (str (vwe-edit-toggle-case--get-current-string-and-delete))
		 (str-listp (vwe-edit-toggle-case--list-p str)))
	(if str-listp
		(setq str (mapconcat #'vwe-edit-toggle-case--convert-to-lower-case (split-string str " ") " "))
	  (setq str (vwe-edit-toggle-case--convert-to-lower-case str)))
	(goto-char (car pos))
	(insert str)))

(defun vwe-edit-toggle-case--capitalize ()
  "Capitalize."
  (interactive)
  (let* ((pos (vwe-edit-toggle-case--find-string-region-point))
		 (str (vwe-edit-toggle-case--get-current-string-and-delete))
		 (str-listp (vwe-edit-toggle-case--list-p str)))
	(if str-listp
		(setq str (mapconcat #'vwe-edit-toggle-case--convert-to-capitalize (split-string str " ") " "))
	  (setq str (vwe-edit-toggle-case--convert-to-capitalize str)))
	(goto-char (car pos))
	(insert str)))

(defun vwe-edit-toggle-case--underline ()
  "Underline."
  (interactive)
  (let* ((pos (vwe-edit-toggle-case--find-string-region-point))
		 (str (vwe-edit-toggle-case--get-current-string-and-delete))
		 (str-listp (vwe-edit-toggle-case--list-p str)))
	(if str-listp
		(setq str (mapconcat #'vwe-edit-toggle-case--convert-to-underline (split-string str " ") " "))
	  (setq str (vwe-edit-toggle-case--convert-to-underline str)))
	(goto-char (car pos))
	(insert str)))

(defun vwe-edit-toggle-case--camel-case ()
  "Camel case."
  (interactive)
  (let* ((pos (vwe-edit-toggle-case--find-string-region-point))
		 (str (vwe-edit-toggle-case--get-current-string-and-delete))
		 (str-listp (vwe-edit-toggle-case--list-p str)))
	(if str-listp
		(setq str (mapconcat #'vwe-edit-toggle-case--convert-to-camel-case (split-string str " ") " "))
	  (setq str (vwe-edit-toggle-case--convert-to-camel-case str)))
	(goto-char (car pos))
	(insert str)))

(defun vwe-edit-toggle-case--hyphen ()
  "Hyphen."
  (interactive)
  (let* ((pos (vwe-edit-toggle-case--find-string-region-point))
		 (str (vwe-edit-toggle-case--get-current-string-and-delete))
		 (str-listp (vwe-edit-toggle-case--list-p str)))
	(if str-listp
		(setq str (mapconcat #'vwe-edit-toggle-case--convert-to-hyphen (split-string str " ") " "))
	  (setq str (vwe-edit-toggle-case--convert-to-hyphen str)))
	(goto-char (car pos))
	(insert str)))

;;;###autoload
(defun vwe-edit-toggle-case--select-convert (&optional func)
  "Select convret FUNC."
  (interactive
   (list (completing-read (format "select convert:")
						  vwe-edit-toggle-case--convert-case-list)))
  (funcall (cdr (assoc func vwe-edit-toggle-case--convert-case-list))))

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
		  (vwe-edit-bound--remove-overlays)
		  (condition-case nil
		   (while (search-forward "\n" end t)
			 (goto-char (match-beginning 0))
			 (setq limit (- fill-num (current-column))
				   char (vwe-edit-bound--build-char limit)
				   ol (make-overlay (match-beginning 0) (match-beginning 0)))
			 (when char
			   (overlay-put ol 'ebol t)
			   (overlay-put ol 'after-string char))
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
  (add-hook 'after-change-functions #'vwe-edit-bound--redraw t)
  (add-hook 'before-change-functions #'vwe-edit-bound--redraw nil)
  (add-hook 'window-scroll-functions #'vwe-edit-bound--redraw-for-scroll nil)
  (add-hook 'window-configuration-change-hook #'vwe-edit-bound--show)
  (add-hook 'post-self-insert-hook #'vwe-edit-bound--show nil)
  (add-hook 'post-command-hook #'vwe-edit-bound--redraw-command nil))

(defun vwe-edit-bound-mode-disable ()
  "Disable mode."
  (vwe-edit-bound--remove-overlays)
  (remove-hook 'after-change-functions #'vwe-edit-bound--redraw)
  (remove-hook 'before-change-functions #'vwe-edit-bound--redraw)
  (remove-hook 'window-scroll-functions #'vwe-edit-bound--redraw-for-scroll)
  (remove-hook 'window-configuration-change-hook #'vwe-edit-bound--show)
  (add-hook 'post-self-insert-hook #'vwe-edit-bound--show)
  (remove-hook 'post-command-hook #'vwe-edit-bound--redraw-command t))

;;;###autoload
(define-minor-mode vwe-edit-bound-mode
  "Bound minor mode."
  :group 'vwe-edit
  :keymap nil
  :global nil
  (if vwe-edit-bound-mode
	  (vwe-edit-bound-mode-enable)
	(vwe-edit-bound-mode-disable)))

(provide 'vwe-edit)
;;; vwe-edit.el ends here
