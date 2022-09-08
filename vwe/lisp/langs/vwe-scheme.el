;;; vwe-scheme.el ---   scheme dev     -*- lexical-binding: t; -*-

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

;;
;; `geiser' repl 交互模式
;;
(vwe@lib--pkg geiser
  :variable ((setq scheme-program-name "guile"
				   geiser-active-implementations '(guile))))

(provide 'vwe-scheme)
;;; vwe-scheme.el ends here
