;;; vwe-style.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2021

;; Author:  <>
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

(defgroup vwe-style nil
  "Theme options."
  :group 'faces)

(defvar vwe-style--theme-name
  'vwe-style
  "Style theme name.")

(defvar vwe-style--type
  'dark
  "Type is `dark' `ligth' and `grayscale'.")

(defvar vwe-style--load-after-hook
  nil
  "Load after hook.")

(defvar vwe-style--load-before-hook
  nil
  "Load before hook.")

(defconst vwe-style--base-colors
  '(;; name         default    256        16
	(black "#050505"  "#050505"  "black")
	(dark "#393f56"  "black"    "black")
	(white "#f5f5f5"  "#f5f5f5"  "white")
	(gray "#696969"  "#696969"  "gray)")
	(red "#ff0000"  "#ff0000"  "red")
	(dark-red "#8b0000"  "#8b0000"  "red")
	(light-red "#b22222"  "#b22222"  "red")
	(blue "#0000ff"  "#0000ff"  "blue")
	(green "#00ff00"  "#00ff00"  "green")
	(yellow "#ffff00"  "#ffff00"  "yellow")
	(pink "#ff69b4"  "#ff69b4"  "#ff69b4")
	(cyan "#00ffff"  "#00ffff"  "00ffff")
	(orange "#ff8c00"  "#ff8c00"  "orange")
	(teal "#00ff7f"  "#00ff7f"  "green")
	(violet "#8470ff"  "#8470ff"  "#8470ff")
	(magenta "#ff00ff"  "#ff00ff"  "#ff00ff")
	(purple "#bf3eff"  "#bf3eff"  "#bf3eff"))
  "Base color.")

(defun vwe-style--find-color (name colors)
  "Finde COLORS by NAME."
  (unless colors
	(setq colors vwe-style--base-colors))
  (assoc name colors))

(defun vwe-style--build-face-color (colors faces)
  "Build face color by COLORS and FACES."
  (let ((expand-func
         (lambda (func spec)
		   (let (color-list)
             (eval `(let ,(dolist (col colors color-list)
                            (push `(,(car col) ,(funcall func col))
								  color-list))
					  (eval '(backquote ,spec)))))))
        theme-faces)
    (pcase-dolist (`(,face . ,spec) faces)
	  (push `(,face
			  ((((min-colors 16777216)) ; fully graphical envs
                ,(funcall expand-func 'cadr spec))
			   (((min-colors 256))      ; terminal withs 256 colors
                ,(funcall expand-func 'caddr spec))
			   (t                       ; should be only tty-like envs
                ,(funcall expand-func 'cadddr spec))))
            theme-faces))
    theme-faces))

(defun vwe-style--load (name colors faces)
  "Load and setup NAME face by COLORS and FACES."
  (apply #'custom-theme-set-faces
		 'vwe-style-dark
		 (vwe-style--build-face-color colors faces)))

(defun vwe-style--load-theme-file ()
  "Load theme file."
  (when (and (boundp 'custom-theme-load-path) load-file-name)
	(add-to-list 'custom-theme-load-path
				 (file-name-as-directory (file-name-directory load-file-name)))))

(provide 'vwe-style)
;;; vwe-style.el ends here
