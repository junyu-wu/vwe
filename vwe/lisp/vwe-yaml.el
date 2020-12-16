;;; vwe-yaml.el --- Yaml                -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Wu Junyu

;; Author: Wu Junyu <vistar_w@hotmail.com>
;; Keywords: languages, files

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

;; gem install psych

;;; Code:

(use-package yaml-mode
  :mode
  ("\\.yml\\'" . yaml-mode)
  :hook
  (yaml-mode . (lambda () (define-key yaml-mode-map "\C-m"
								 'newline-and-indent)))
  :config
  (use-package yaml-imenu
	:hook
	(yaml-mode . yaml-imenu-enable)
	:config
	(which-func-mode t)))

(provide 'vwe-yaml)
;;; vwe-yaml.el ends here
