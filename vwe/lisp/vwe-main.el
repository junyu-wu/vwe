;;; vwe-main.el --- Custom Setup           -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Wu Junyu

;; Author: Wu Junyu <vistar_w@hotmail.com>
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

;;; Code:

(push (expand-file-name "vwe/lisp" user-emacs-directory) load-path)

;;
;; init before include vwe lib and vwe customize define
;;
(require 'vwe-lib)
(require 'vwe-customize)

;;
;; init emacs
;;

(when (vwe@lib--found-custom-arg "-reset")
  (progn
	(condition-case nil
		(progn
		  (vwe@lib--file-delete (vwe@lib--path-emacs.d "") vwe@custom--reset-ignore-file-list))
	  (error))))

(cond ((vwe@lib--found-custom-arg "-vq") (message "vwe feature config ignore load."))
	  ((vwe@lib--found-custom-arg "-origin") (progn
											   (require 'vwe-base)
											   (message "vwe by emacs origin config.")))
	  ((vwe@lib--found-custom-arg "-base") (progn
											 (require 'vwe-base)
											 (require 'vwe-ui)
											 (require 'vwe-theme)
											 (require 'vwe-layout)
											 (message "vwe base config init.")))
	  (t (progn
		   ;;
		   ;; init ui theme layout and general
		   ;;
		   (require 'vwe-base)
		   (require 'vwe-ui)
		   (require 'vwe-theme)
		   (require 'vwe-layout)
		   (require 'vwe-general)

		   ;;
		   ;; init edit
		   ;;
		   (require 'vwe-org)
		   (require 'vwe-markdown)

		   ;;
		   ;; init prog and language
		   ;;
		   (require 'vwe-prog)

		   ;;
		   ;; inti misc
		   ;;
		   (require 'vwe-misc)

		   ;;
		   ;; init keybindings
		   ;;
		   (require 'vwe-keybinds)

		   ;;
		   ;; after init
		   ;;
		   (require 'vwe-after-init)

		   (message "vwe all config init."))))


(provide 'vwe-main)
;;; vwe-main.el ends here
