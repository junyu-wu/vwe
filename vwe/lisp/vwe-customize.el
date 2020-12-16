;;; vwe-customize.el --- vwiss emacs customize    -*- lexical-binding: t; -*-

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

(defgroup vwe nil
  "Vwiss Emacs customization."
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/junyu-wu/.emacs.d"))

(defgroup vwe-user nil
  "Vwiss Emacs user customization."
  :group 'vwe)

(defgroup vwe-base nil
  "Vwiss Emacs base customization."
  :group 'vwe)

(defgroup vwe-ui nil
  "Vwiss Emacs ui customization."
  :group 'vwe)

(defgroup vwe-net nil
  "Vwiss Emacs net customization."
  :group 'vwe)

;; ***************************************************************************
;; user
;; ***************************************************************************
(defcustom vwe@custom--user-name
  "WuJunyu"
  "Set user full name."
  :group 'vwe-user
  :type 'string)

(defcustom vwe@custom--user-mail
  "vistar_w@hotmail.com"
  "Set user email address."
  :group 'vwe-user
  :type 'string)

(defcustom vwe@custom--user-home-path
  "~"
  "Set user home directory."
  :group 'vwe-user
  :type 'string)

(defcustom vwe@custom--user-workspace-path
  "~"
  "Workspace."
  :group 'vwe-user
  :type 'string)

(defcustom vwe@custom--user-note-path
  "~/note"
  "Note root."
  :group 'vwe-user
  :type 'string)

(defcustom vwe@custom--user-media-path
  "~/media"
  "Note root."
  :group 'vwe-user
  :type 'string)

(defcustom vwe@custom--user-mail-path
  "~/mail"
  "Note root."
  :group 'vwe-user
  :type 'string)

(defcustom vwe@custom--user-backup-path
  "~/backup"
  "Note root."
  :group 'vwe-user
  :type 'string)

(defcustom vwe@custom--user--tmp-path
  "/tmp"
  "Note root."
  :group 'vwe-user
  :type 'string)

(defcustom vwe@custom--user-homepage
  "https://github.com/junyu-wu/.emacs.d"
  "Homepage."
  :group 'vwe-user
  :type 'string)

;; ***************************************************************************
;; base
;; ***************************************************************************
(defcustom vwe@custom--startup-mini?
  nil
  "Mini startup."
  :group 'vwe-base
  :type 'boolean)

(defcustom vwe@custom--banner-path
  "assets/icons/vwemacs-logo.png"
  "Banner Path."
  :group 'vwe-base
  :type 'string)

(defcustom vwe@custom--source
  "china-melpa"
  "Source name."
  :group 'vwe-base
  :type 'string)

(defcustom vwe@custom--quit-ask?
  nil
  "Ask for confirmation when closing."
  :group 'vwe-base
  :type 'boolean)

(defcustom vwe@custom--config-etc-path
  (concat user-emacs-directory "etc/")
  "Note root."
  :group 'vwe-base
  :type 'string)

(defcustom vwe@custom--config-cache-path
  (concat user-emacs-directory ".cache/")
  "Note root."
  :group 'vwe-base
  :type 'string)

(defcustom vwe@custom--config-lisp-path
  (concat user-emacs-directory "lisp/")
  "Note root."
  :group 'vwe-base
  :type 'string)

(defcustom vwe@custom--config-non-source-pkg-path
  (concat user-emacs-directory "site-lisp/")
  "Note root."
  :group 'vwe-base
  :type 'string)

(defcustom vwe@custom--daemon-activated?
  nil
  "Is deamon activated?"
  :group 'vwe-base
  :type 'boolean)

;; ***************************************************************************
;; theme
;; ***************************************************************************
(defcustom vwe@custom--theme-gui
  'doom-ephemeral
  "Default gui theme."
  :group 'vwe-ui
  :type 'symbol)

(defcustom vwe@custom--theme-tty
  'doom-dracula
  "Default tty theme."
  :group 'vwe-ui
  :type 'symbol)

(defcustom vwe@custom--theme-load-before-hook nil
  "Functions to run before load theme."
  :group 'vwiss-ui
  :type 'hook)

(defcustom vwe@custom--theme-load-after-hook nil
  "Functions to run after load theme."
  :group 'vwiss-ui
  :type 'hook)

;; ***************************************************************************
;; frame
;; ***************************************************************************
(defcustom vwe@custom--frame-width
  85
  "Default width."
  :group 'vwe-ui
  :type 'integer)

(defcustom vwe@custom--frame-height
  35
  "Default hieght."
  :group 'vwe-ui
  :type 'integer)

(defcustom vwe@custom--frame-max?
  nil
  "Startup Frame max."
  :group 'vwe
  :type 'boolean)

(defcustom vwe@custom--frame-menu-bar?
  nil
  "Show menu bar."
  :group 'vwe
  :type 'string)

(defcustom vwe@custom--frame-tool-bar?
  nil
  "Show tool bar."
  :group 'vwe
  :type 'string)

(defcustom vwe@custom--frame-scroll-bar?
  nil
  "Show scorll bar."
  :group 'vwe
  :type 'string)

;; ***************************************************************************
;; font
;; ***************************************************************************
(defcustom vwe@custom--font-ascii
  "Hack"
  "Default font."
  :group 'vwe-ui
  :type 'string)

(defcustom vwe@custom--font-ascii-size
  14
  "Default font size."
  :group 'vwe-ui
  :type 'integer)

(defcustom vwe@custom--font-non-ascii
  "Source Han Sans SC"
  "Default font."
  :group 'vwe-ui
  :type 'string)

(defcustom vwe@custom--font-non-ascii-size
  14
  "Default font size."
  :group 'vwe-ui
  :type 'integer)

;; ***************************************************************************
;; headerline and modeline
;; ***************************************************************************
(defcustom vwe@custom--headerline-show?
  t
  "Is show headre-line."
  :group 'vwe-ui
  :type 'boolean)

;; ***************************************************************************
;; daemon
;; ***************************************************************************
(defcustom vwe@custom--daemon-startup
  t
  "Daemon status."
  :group 'vwe-base
  :type 'boolean)

;; ***************************************************************************
;; server & net
;; ***************************************************************************
(defcustom vwe@custom--server-startup?
  nil
  "Server status."
  :group 'vwe-net
  :type 'boolean)

(defcustom vwe@custom--proxy-http
  "127.0.0.1:1080"
  "Network proxy."
  :group 'vwe-net
  :type 'string)

(defcustom vwe@custom--proxy-socks
  "127.0.0.1:1080"
  "Socks proxy server."
  :group 'vwe-net
  :type 'string)

(defcustom vwe@custom--proxy-socks-version
  5
  "Socks proxy v4 or v5."
  :group 'vwe-net
  :type 'integer)

;; ***************************************************************************
;; buffer
;; ***************************************************************************
(defcustom vwe@custom--buffer-tabs?
  nil
  "Show header tabs."
  :group 'vwe-base
  :type 'boolean)

(defcustom vwe@custom--buffer-headerline?
  t
  "Show header line."
  :group 'vwe-base
  :type 'boolean)

(defcustom vwe@custom--buffer-auto-save?
  nil
  "Auto save?"
  :group 'vwe-base
  :type 'boolean)

(defcustom vwe@custom--buffer-make-backup?
  nil
  "Make backup?"
  :group 'vwe-base
  :type 'boolean)

(defcustom vwe@custom--buffer-filter-list
  '("\\*shell[0-9]*\\*\\|\\*terminal*\\|\\*terminal\\(<[0-9]*>\\)\\*\\'"
	"\\*treemacs.*\\'"
	"\\*company-box.*\\'"
	"\\*dashboard\\*\\'"
	"\\*mu4e-[0-9 a-z A-Z].*\\'"
    "*scratch*"
    "*Messages*")
  "Buffer filter list."
  :group 'vwe-base
  :type 'string)

(defcustom vwe@custom--modeline--hide-list
  '("*scratch*"
    "*Messages*"
    "*dashboard*")
  "Hide modeline list.
If nil hide all buffers."
  :group 'vwe-base
  :type 'string)

;; ***************************************************************************
;; debug
;; ***************************************************************************
(defcustom vwe@custom--debug?
  nil
  "Is debug running?"
  :group 'vwe-base
  :type 'boolean)

(defcustom vwe@custom--debug-max-lisp-eval-depth
  10000
  "Is debug running?"
  :group 'vwe-base
  :type 'integer)

;; ***************************************************************************
;; lib
;; ***************************************************************************
 (defun vwe@custom--vwe-customize ()
	"Open custom file."
	(interactive)
	(customize-group 'vwe))

(provide 'vwe-customize)
;;; vwe-customize.el ends here
