;;; vwe-shell.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2022  WuJunyu

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
;; `shell-script-mode'
;;
(vwe@lib--package 'sh-script
				  nil
				  (progn
					;;
					;; `company-shell'
					;;
					(vwe@lib--package 'company-shell nil nil
									  (with-eval-after-load 'company
										(add-hook 'sh-mode-hook
												  (lambda ()
													(vwe@pkg--company-make-mode-local-backends
													 '(company-shell company-shell-env company-dabbrev-code) '(company-capf))))))))

(provide 'vwe-shell)
;;; vwe-shell.el ends here
