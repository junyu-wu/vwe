;;; vwe-paren.el ---   vwe paren    -*- lexical-binding: t; -*-

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

(defgroup vwe-paren nil
  "Customization group for beacon."
  :group 'vwe
  :prefix "vwe-paren--")

(defvar vwe-paren--keymap
  (let* ((keymap (make-sparse-keymap)))
	keymap)
  "Paren keymap.")

(defvar vwe-paren--delimiter-max-depth
  9
  "Max depth.")

(defface vwe-paren--delimiter-face
  '((t (:inherit 'default :weight bold)))
  "Default face."
  :group 'vwe-paren)

(defface vwe-paren--delimiter-error-face
  '((t (:background "red" :foreground "yellow")))
  "Error face."
  :group 'vwe-paren)

(defmacro vwe-paren--delimiter-define-depth-faces ()
  "Define depth FACES."
  (let ((faces '())
		(light-colors ["#707183" "#7388d6" "#909183" "#709870" "#907373"
                       "#6276ba" "#858580" "#80a880" "#887070"])
        (dark-colors ["grey55" "#93a8c6" "#b0b1a3" "#97b098" "#aebed8"
                      "#b0b0b3" "#90a890" "#a2b6da" "#9cb6ad"]))
    (dotimes (i 9)
      (push `(defface ,(intern (format "vwe-paren--depth-%d-face" i))
               '((default (:inherit vwe-paren--delimiter-face))
                 (((class color) (background light)) :foreground ,(aref light-colors i))
                 (((class color) (background dark)) :foreground ,(aref dark-colors i)))
               ,(format "Nested delimiter face, depth %d." i)
               :group 'vwe-paren)
            faces))
    `(progn ,@faces)))

(defun vwe-paren--delimiter-ignore-paren (pos ppss code)
  "When POS CODE parse PPSS is string/comment/escaped according ignore."
  (or (nth 3 ppss) (nth 4 ppss) (nth 5 ppss)))

(defun vwe-paren--delimiter-pick-face (depth pair)
  "Pick face.
DEPTH is depth at POS, which depth the face to use.
PAIR is paren pair."
  (let* ((face))
	(cond
	 ((<= depth 0) (setq face 'vwe-paren--delimiter-face))
	 ((not pair) (setq face 'vwe-paren--delimiter-error-face))
	 (t (setq face (intern-soft
					(concat "vwe-paren--depth-"
							(number-to-string
							 (if (<= depth vwe-paren--delimiter-max-depth)
								 depth
							   (mod depth 10)))
							"-face")))))
	face))

(defun vwe-paren--delimiter-apply-face (pos depth pair)
  "Apply face for POS.
DEPTH is depth at POS, which depth the face to use.
PAIR is paren pair."
  (let* ((face (vwe-paren--delimiter-pick-face depth pair)))
	(when face (font-lock-prepend-text-property pos (1+ pos) 'face face))))

(defun vwe-paren--delimiter-keywords (end)
  "Paren keywords in region between point and END."
  ;; Remove `mmm-mode' restrictions
  (when (bound-and-true-p mmm-current-submode) (widen))

  (let* ((last-pos (point))
		 (ppss (syntax-ppss)))
	;; (message "last pos is:%d" last-pos)
	;; (message "ppss is:%S" ppss)
	(while (> end (progn (skip-syntax-forward "^()" end) (point)))
	  (let* ((pos (point))
			 (after-syntax (syntax-after pos)))
		;; (message "cur point is:%d" pos)
		;; (message "after syntab is:%S" after-syntax)
		(setq ppss (parse-partial-sexp last-pos pos nil nil ppss)
			  last-pos pos)
		(forward-char)
		(let ((code (car after-syntax))
			  (depth (1+ (nth 0 ppss))))
		  (cond
		   ((vwe-paren--delimiter-ignore-paren pos ppss code) nil)
		   ((= 4 (logand #xFFFF code)) (vwe-paren--delimiter-apply-face pos depth t))
		   ((= 5 (logand #xFFFF code)) (vwe-paren--delimiter-apply-face pos depth t)))))
	  ))
  nil)

(defun vwe-paren--delimiter-enable ()
  "Delimiter enable."
  (font-lock-remove-keywords nil vwe-paren--delimiter-font-lock-keywords)
  (font-lock-add-keywords nil vwe-paren--delimiter-font-lock-keywords 'append)
  (vwe-paren--delimiter-define-depth-faces)
  (when (or (bound-and-true-p syntax-begin-function)
            (bound-and-true-p font-lock-beginning-of-syntax-function))
    (syntax-ppss-flush-cache 0))

  (when (boundp 'syntax-begin-function)
    (set (make-local-variable 'syntax-begin-function) nil))

  (when (boundp 'font-lock-beginning-of-syntax-function)
    (set (make-local-variable 'font-lock-beginning-of-syntax-function) nil))

  (when font-lock-mode
    (if (fboundp 'font-lock-flush)
        (font-lock-flush)
      (with-no-warnings (font-lock-fontify-buffer)))))

(defun vwe-paren--delimiter-disable ()
  "Delimiter disable."
  (font-lock-remove-keywords nil vwe-paren--delimiter-font-lock-keywords))

;;
;; mode
;;
(defconst vwe-paren--delimiter-font-lock-keywords
  '(vwe-paren--delimiter-keywords))

(defun vwe-paren-mode-enable ()
  "Enable mode."
  (vwe-paren--delimiter-enable))

(defun vwe-paren-mode-disable ()
  "Disable mode."
  (vwe-paren--delimiter-disable)
  (vwe-paren-mode -1))

;;;###autoload
(define-minor-mode vwe-paren-mode
  "Vwe paren minor mode."
  :group 'vwe-paren
  :keymap vwe-paren--keymap
  :global t
  (if vwe-paren-mode
	  (vwe-paren-mode-enable)
	(vwe-paren-mode-disable)))

(provide 'vwe-paren)
;;; vwe-paren.el ends here
