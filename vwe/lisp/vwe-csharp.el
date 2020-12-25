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

;; omnisharp-roslyn
;; sudo apt install apt-transport-https dirmngr gnupg ca-certificates
;; sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
;; echo "deb https://download.mono-project.com/repo/debian stable-stretch main" | sudo tee /etc/apt/sources.list.d/mono-official-stable.list
;; sudo apt update
;; apt install mono-devel mono-complete mono-dbg mono-xsp4 ca-certificates-mono mono-xsp
;; apt install referenceassemblies-pcl

;;; Code:

(use-package csharp-mode
  :mode
  ("\\.cs\\'" . csharp-mode)
  :config
  ;; c#后端
  (use-package omnisharp
	:hook
	(csharp-mode . (lambda ()
					 (omnisharp-mode)
					 (add-to-list 'company-backends 'company-omnisharp))))
	:init
	(setq omnisharp-server-executable-path "omnisharp"))

(provide 'vwe-csharp)
;;; vwe-csharp.el ends here
