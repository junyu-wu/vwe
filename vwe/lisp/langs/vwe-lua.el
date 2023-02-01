;;; vwe-lua.el ---                                   -*- lexical-binding: t; -*-

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

(vwe@lib--pkg lua-mode
  :init ((autoload 'lua-mode "lua-mode" "Lua editing mode." t)
		 (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
		 (add-to-list 'auto-mode-alist '("\\.nse$" . lua-mode))
		 (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))
  :config ((vwe@lib--pkg company-lua
			 :init ((add-hook 'lua-mode-hook
							  (lambda ()
								(vwe@pkg--company-make-mode-local-backends
								 'company-lua)))))))

(provide 'vwe-lua)
;;; vwe-lua.el ends here
