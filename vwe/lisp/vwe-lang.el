;;; vwe-lang.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  WuJunyu

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
;; ***************************************************************************
;; lib
;; ***************************************************************************

;; ***************************************************************************
;; config
;; ***************************************************************************
(vwe@lib--package nil
				  (progn
					(push '("\\.action\\'" . conf-mode) auto-mode-alist)))

;;
;; `text-mode'
;;
(vwe@lib--package 'text-mode
				  (push '("\\.txt\\'" . org-mode) auto-mode-alist)
				  nil nil nil nil t)

;;
;; init edit
;;
(require 'vwe-org)
(require 'vwe-markdown)

;;
;; init prog and language
;;
(require 'vwe-prog)

(vwe@lib--log "Initialization of Languages configuration is complete.")

(provide 'vwe-lang)
;;; vwe-lang.el ends here
