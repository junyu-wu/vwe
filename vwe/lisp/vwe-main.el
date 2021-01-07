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

;;
;; init before
;;
(require 'vwe-lib)
(require 'vwe-customize)
(require 'vwe-package)

;;
;; init emacs
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

(require 'vwe-lsp)

(require 'vwe-lisp)
(require 'vwe-assembly)
(require 'vwe-clang)
(require 'vwe-golang)
(require 'vwe-python)
(require 'vwe-ruby)
(require 'vwe-java)
(require 'vwe-web)
(require 'vwe-clojure)
(require 'vwe-csharp)
(require 'vwe-rust)
(require 'vwe-bat)
(require 'vwe-scheme)
(require 'vwe-ahk)
(require 'vwe-yaml)

;;
;; inti misc
;;
(require 'vwe-misc)

;;
;; init keybindings
;;
(require 'vwe-keybinds)

(provide 'vwe-main)
;;; vwe-main.el ends here
