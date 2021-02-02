;;; vwe-csharp.el ---   c# dev       -*- lexical-binding: t; -*-

;; Copyright (C) 2019  WuJunyu

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

;; mono git clone https://github.com/mono/mono.git
;; apt-get install autoconf libtool automake build-essential gettext cmake python

;;; Code:

;;
;; `csharp-mode'
;;
(vwe@lib--package 'csharp-mode
				  (push '("\\.cs\\'" . csharp-mode) auto-mode-alist)
				  ;;
				  ;; `omnisharp' c#后端
				  ;;
				  (vwe@lib--package 'omnisharp
									(add-hook 'csharp-mode-hook (lambda ()
																  (omnisharp-mode)
																  (add-to-list 'company-backends 'company-omnisharp)))))

(provide 'vwe-csharp)
;;; vwe-csharp.el ends here
