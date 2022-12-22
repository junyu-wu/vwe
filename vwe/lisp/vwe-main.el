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

(when (vwe@lib--found-custom-arg "-vr")
  (progn
	(condition-case nil
		(progn
		  (vwe@lib--file-delete (vwe@lib--path-emacs.d "")
								vwe@custom--reset-ignore-file-list))
	  (error))))

(cond ((vwe@lib--found-custom-arg "-vq")
	   (vwe@lib--log "No configuration loaded."))
	  ((vwe@lib--found-custom-arg "-vb")
	   (progn
		 (require 'vwe-base)))
	  ((vwe@lib--found-custom-arg "-vs")
	   (progn
		 (require 'vwe-base)
		 (require 'vwe-ui)
		 (vwe@lib--log "Initialization of Simple configuration is complete.")))
	  (t (progn
		   ;;
		   ;; init ui theme layout and general
		   ;;
		   (require 'vwe-base)
		   (require 'vwe-ui)
		   (require 'vwe-general)

		   ;;
		   ;; inti lang
		   ;;
		   (require 'vwe-lang)

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

		   ;;
		   ;; after init
		   ;;
		   (when vwe@custom--test-init?
			 (require 'vwe-test-init)))))

(vwe@lib--log "Initialization finished.")

(provide 'vwe-main)
;;; vwe-main.el ends here
