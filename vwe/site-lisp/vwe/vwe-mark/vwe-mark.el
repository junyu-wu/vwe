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

  (setq vwe-mark-multi-edit--list nil
		vwe-mark-multi-edit--overlay-list '()))

(defun vwe-mark-multi-edit--apply-all ()
  "Apply all kmacro."
  (when vwe-mark-multi-edit--overlay-list
	(dolist (item (cdr vwe-mark-multi-edit--overlay-list))
	  (save-excursion
		(when (overlay-end item)
		  (goto-char (overlay-end item))
		  (call-last-kbd-macro))))))

;;;###autoload
(defun vwe-mark-multi-edit--replace (&optional type)
  "Replace TYPE(symbol, word, defun...) or region range."
  (interactive
   (list
	(completing-read "type:" (mapcar (lambda(item)
									   (car item))
									 '(("word" . 'word)
									   ("symbol" . 'symbol)
									   ("list" . 'list)
									   ("sexp" . 'sexp)
									   ("defun" . 'defun)
									   ("filename" . 'filename)
									   ("url" . 'url)
									   ("email" . 'email)
									   ("uuid" . 'uuid)
									   ("sentence" . 'sentence)
									   ("whitespace" . 'whitespace)
									   ("line" . 'line)
									   ("page" . 'page))))))
  (let* ((type (intern (cdr (assoc type '(("word" . "word")
										  ("symbol" . "symbol")
										  ("list" . "list")
										  ("sexp" . "sexp")
										  ("defun" . "defun")
										  ("filename" . "filename")
										  ("url" . "url")
										  ("email" . "email")
										  ("uuid" . "uuid")
										  ("sentence" . "sentence")
										  ("whitespace" . "whitespace")
										  ("line" . "line")
										  ("page" . "page"))))))
		 (str (if (region-active-p)
				  (buffer-substring (region-beginning) (region-end))
				(thing-at-point type)))
		 (point (point))
		 (len (length str))
		 (start (point-min))
		 (end (point-max))
		 (current-overlay (if (region-active-p)
							  (make-overlay (region-beginning) (region-end))
							(make-overlay (car (bounds-of-thing-at-point type))
										  (cdr (bounds-of-thing-at-point type)))))
		 (total 1))
	(save-excursion
	  (goto-char end)
	  (catch 'break
		(while (search-backward str nil t)
		  (let* ((search-start (point))
				 (search-end (+ search-start len)))
			(cond
			 ((= point search-start) (setq point (+ search-start len)))
			 ((= point search-end) (setq point search-end))
			 (t (progn
				  (add-to-list 'vwe-mark-multi-edit--list (cons search-start search-end))
				  (setq total (1+ total))))))
		  (when (<= (point) start)
			(throw 'break nil)))))
	(goto-char point)
	(vwe-mark-multi-edit--kmacro-start)
	(overlay-put current-overlay 'face 'vwe-mark-multi-edit--rect-face)
	(add-to-list 'vwe-mark-multi-edit--overlay-list current-overlay)
	(message "%S find total: %S" str total)))

;;
;; rect edit
;;
(defvar-local vwe-mark-rect--active-p
  nil
  "Rectangle is active.")

(defvar-local vwe-mark-multi-edit--rect-bound-overlay-list
  '()
  "Multi edit rect overlay list.")

(defvar-local vwe-mark-rect--rectangle-vector
  '(:point-begin nil :point-end nil :line-begin nil :line-end nil :column-begin nil :column-end nil)
  "Rectangle vector.")

(defun vwe-mark-rect--mark-finish ()
  "Finish mark and overlay mark rect."
  (interactive)
  (local-unset-key (kbd "C-m"))
  (local-set-key (kbd "C-m") #'vwe-mark-rect--edit-exit)
  (local-set-key (kbd "C-g") #'vwe-mark-rect--quit)
  (plist-put vwe-mark-rect--rectangle-vector :point-end (point))
  (remove-hook 'post-command-hook #'vwe-mark-rect--post-command t)
  (goto-line (plist-get vwe-mark-rect--rectangle-vector :line-begin))
  (move-to-column (plist-get vwe-mark-rect--rectangle-vector :column-begin))
  (vwe-mark-rect--edit))

(defun vwe-mark-rect--quit ()
  "Remove rect overlay."
  (interactive)
  (remove-hook 'post-command-hook #'vwe-mark-rect--post-command t)
  (advice-remove 'keyboard-quit #'vwe-mark-rect--edit-exit)
  (mapc #'delete-overlay vwe-mark-multi-edit--rect-bound-overlay-list)
  (local-unset-key (kbd "C-m"))
  (local-unset-key (kbd "C-g"))
  (setq vwe-mark-rect--rectangle-vector '(:point-begin nil :point-end nil :line-begin nil :line-end nil :column-begin nil :column-end nil)
		vwe-mark-multi-edit--rect-bound-overlay-list nil))

(defun vwe-mark-rect--flash-overlay ()
  "Flash overlay."
  (mapcar
   (lambda (elem)
	 (overlay-put elem 'face 'vwe-mark-multi-edit--rect-face))
   vwe-mark-multi-edit--rect-bound-overlay-list))

;;;###autoload
(defun vwe-mark-rect--mark ()
  "Mark rect."
  (interactive)
  (setq vwe-mark-multi-edit--rect-bound-overlay-list '())

  (plist-put vwe-mark-rect--rectangle-vector :point-begin (point))
  (plist-put vwe-mark-rect--rectangle-vector :line-begin (line-number-at-pos))
  (plist-put vwe-mark-rect--rectangle-vector :line-end (line-number-at-pos))
  (plist-put vwe-mark-rect--rectangle-vector :column-begin (current-column))
  (plist-put vwe-mark-rect--rectangle-vector :column-end (current-column))

  (local-set-key (kbd "C-m") #'vwe-mark-rect--mark-finish)
  (add-hook 'post-command-hook #'vwe-mark-rect--post-command nil t))

(defun vwe-mark-rect--post-command ()
  "Mark rectangles post command."
  (mapc #'delete-overlay vwe-mark-multi-edit--rect-bound-overlay-list)
  (if (equal this-command 'keyboard-quit)
	  (vwe-mark-rect--quit)
	(let* ((beginl (plist-get vwe-mark-rect--rectangle-vector :line-begin))
		   (endl (progn
				   (plist-put vwe-mark-rect--rectangle-vector :line-end (line-number-at-pos))
				   (plist-get vwe-mark-rect--rectangle-vector :line-end)))
		   (privc (plist-get vwe-mark-rect--rectangle-vector :column-end))
		   (beginc (progn
					 (plist-put vwe-mark-rect--rectangle-vector :column-end (current-column))
					 (plist-get vwe-mark-rect--rectangle-vector :column-begin)))
		   (endc (plist-get vwe-mark-rect--rectangle-vector :column-end))
		   (totall (1+ (abs (- endl beginl))))
		   (line (min beginl endl))
		   (startc (min beginc endc))
		   (stopc (max beginc endc))
		   (totalc (save-excursion
					 (goto-char (point-at-eol))
					 (current-column))))
	  (when (< totalc privc)
		(move-to-column privc t))
	  (dotimes (il totall)
		(save-excursion
		  (goto-line (+ line il))

		  (let ((totalc+ (progn
						   (goto-char (point-at-eol))
						   (current-column)))
				(overlay (make-overlay (progn (move-to-column startc t) (point))
									   (progn (move-to-column stopc t) (point)))))
			(when (>= (current-column) totalc+) (move-to-column (1+ totalc+) t))
			(add-to-list 'vwe-mark-multi-edit--rect-bound-overlay-list overlay t)))))
	(vwe-mark-rect--flash-overlay)))

(defun vwe-mark-rect--inside-rect-p ()
  "Point is inside rect."
  (let* ((beginc (plist-get vwe-mark-rect--rectangle-vector :column-begin))
		 (endc (plist-get vwe-mark-rect--rectangle-vector :column-end))
		 (beginb (min beginc endc))
		 (endb (max beginc endc)))
	(when (or (>= (current-column) beginb) (<= (current-column) endb)) t)))

(defun vwe-mark-rect--edit ()
  "Rect Edit."
  (interactive)
  (when (and vwe-mark-multi-edit--rect-bound-overlay-list (vwe-mark-rect--inside-rect-p))
	(advice-add 'keyboard-quit :before #'vwe-mark-rect--edit-exit)
	(kmacro-start-macro 0)))

(defun vwe-mark-rect--edit-apply-all ()
  "Apply all kmacro."
  (when vwe-mark-multi-edit--rect-bound-overlay-list
	(let* ((beginl (plist-get vwe-mark-rect--rectangle-vector :line-begin))
		   (endl (plist-get vwe-mark-rect--rectangle-vector :line-end))
		   (beginc (plist-get vwe-mark-rect--rectangle-vector :column-begin))
		   (endc (plist-get vwe-mark-rect--rectangle-vector :column-end))
		   (line (min beginl endl))
		   (totall (1+ (abs (- endl beginl)))))
	  (dotimes (il totall)
		(save-excursion
		  (goto-line (+ line il))
		  (move-to-column beginc)
		  (unless (= beginl (line-number-at-pos))
			(call-last-kbd-macro)))))))

(defun vwe-mark-rect--edit-exit ()
  "Keyboard-quit advice."
  (interactive)
  (end-kbd-macro)
  (vwe-mark-rect--edit-apply-all)
  (vwe-mark-rect--quit))

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
