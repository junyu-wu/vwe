;;; vwe-proxy.el --- Vwe proxy                       -*- lexical-binding: t; -*-

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

(require 'url-gw)
(require 'socks)

(defgroup vwe-proxy nil
  "Vwe proxy."
  :prefix "vwe-proxy--"
  :group 'proxy)

(defcustom vwe-proxy--http-proxy-for-env
  '("127.0.0.1" 1080)
  "Environment variable http proxy."
  :type 'list
  :group 'vwe-proxy)

(defcustom vwe-proxy--url-proxy-services
  '(("http"  . "127.0.0.1:1080")
    ("https" . "127.0.0.1:1080")
    ("ftp"   . "127.0.0.1:1080")
    ("no_proxy" . "127.0.0.1"))
  "Url proxy services."
  :type 'alist
  :group 'vwe-proxy)

(defcustom vwe-proxy--socks-proxy-for-env
  '("Default" "127.0.0.1" 1080 5)
  "Environment variable socks proxy."
  :type 'list
  :group 'vwe-proxy)

(defcustom vwe-proxy--socks-non-proxy-alist
  '("localhost" "192.168.*" "10.*")
  "Non proxy alist."
  :type 'list
  :group 'vwe-proxy)

(defvar vwe-proxy--current-type
  nil
  "Current proxy type.")

(defvar vwe-proxy--globalp
  nil
  "Is global.")

(defun vwe-proxy--http-proxy-enable (&optional global?)
  "Enable local or GLOBAL http proxy."
  (setenv "HTTP_PROXY" (format "%s:%s"
							   (car vwe-proxy--http-proxy-for-env)
							   (cadr vwe-proxy--http-proxy-for-env)))
  (setenv "HTTPS_PROXY" (getenv "HTTP_PROXY"))
  (if global?
	  (setq vwe-proxy--current-type "http")
	(setq-local vwe-proxy--current-type "http"))
  (message "[%s] http proxy enable" (getenv "HTTP_PROXY")))

(defun vwe-proxy--http-proxy-disable (&optional global?)
  "Disable local or GLOBAL http proxy."
  (setenv "HTTP_PROXY" nil)
  (setenv "HTTPS_PROXY" (getenv "HTTP_PROXY"))
  (if global?
	  (setq vwe-proxy--current-type nil)
	(setq-local vwe-proxy--current-type nil))
  (message "http proxy disable"))

(defun vwe-proxy--url-proxy-enable (&optional global?)
  "Enable local or GLOBAL URL proxy."
  (if global?
	  (setq url-proxy-services vwe-proxy--url-proxy-services
			vwe-proxy--current-type "url")
	(setq-local url-proxy-services vwe-proxy--url-proxy-services
				vwe-proxy--current-type "url"))
  (message "url proxy enable"))

(defun vwe-proxy--url-proxy-disable (&optional global?)
  "Disable local or GLOBAL URL proxy."
  (if global?
	  (setq url-proxy-services nil
			vwe-proxy--current-type nil)
	(setq-local url-proxy-services nil
				vwe-proxy--current-type nil))
  (message "url proxy disable"))

(defun vwe-proxy--socks-proxy-enable (&optional global?)
  "Enable local or GLOBAL Socks proxy."
  (require 'url-gw)
  (require 'socks)
  (if global?
	  (setq url-gateway-method 'socks
			socks-noproxy vwe-proxy--socks-non-proxy-alist
			socks-server vwe-proxy--socks-proxy-for-env
			vwe-proxy--current-type "socks")
	(setq-local url-gateway-method 'socks
				socks-noproxy vwe-proxy--socks-non-proxy-alist
				socks-server vwe-proxy--socks-proxy-for-env
				vwe-proxy--current-type "socks"))
  (message "socks proxy %s enable" vwe-proxy--socks-proxy-for-env))

(defun vwe-proxy--socks-proxy-disable (&optional global?)
  "Disable local or GLOBAL Socks proxy."
  (if global?
	  (setq url-gateway-method 'native
			vwe-proxy--current-type nil)
	(setq-local url-gateway-method 'native
				vwe-proxy--current-type nil))
  (message "socks proxy disable"))

;;;###autoload
(defun vwe-proxy--enable (&optional type)
  "Enable enable proxy for TYPE."
  (interactive
   (list
    (completing-read (format "type (%s):" vwe-proxy--current-type)
					 '("http" "url" "socks"))))
  (cond
   ((equal type "http") (vwe-proxy--http-proxy-enable))
   ((equal type "url") (vwe-proxy--url-proxy-enable))
   ((equal type "socks") (vwe-proxy--socks-proxy-enable))
   (t (vwe-proxy--http-proxy-enable)))
  (vwe-proxy-mode t))

(defun vwe-proxy--disable ()
  "Enable disable proxy."
  (interactive)
  (when vwe-proxy-mode
	(cond
	 ((equal vwe-proxy--current-type "http") (vwe-proxy--http-proxy-disable))
	 ((equal vwe-proxy--current-type "url") (vwe-proxy--url-proxy-disable))
	 ((equal vwe-proxy--current-type "socks") (vwe-proxy--socks-proxy-disable))
	 (t (vwe-proxy--http-proxy-disable)))
	(vwe-proxy-mode -1)))

;;;###autoload
(define-minor-mode vwe-proxy-mode
  "Vwe proxy mode."
  :global t
  :group 'vwe-proxy)

;;;###autoload
(defun vwe-proxy--enable-global (&optional type)
  "Enable global enable proxy for TYPE."
  (interactive
   (list
    (completing-read (format "type (%s):" vwe-proxy--current-type)
					 '("http" "url" "socks"))))
  (cond
   ((equal type "http") (vwe-proxy--http-proxy-enable t))
   ((equal type "url") (vwe-proxy--url-proxy-enable t))
   ((equal type "socks") (vwe-proxy--socks-proxy-enable t))
   (t (vwe-proxy--http-proxy-enable t)))
  (global-vwe-proxy-mode t))

(defun vwe-proxy--disable-global ()
  "Enable global disable proxy."
  (interactive)
  (when global-vwe-proxy-mode
	(cond
	 ((equal vwe-proxy--current-type "http") (vwe-proxy--http-proxy-disable t))
	 ((equal vwe-proxy--current-type "url") (vwe-proxy--url-proxy-disable t))
	 ((equal vwe-proxy--current-type "socks") (vwe-proxy--socks-proxy-disable t))
	 (t (vwe-proxy--http-proxy-disable t)))
	(global-vwe-proxy-mode -1)))

;;;###autoload
(define-minor-mode global-vwe-proxy-mode
  "Global vwe proxy mode."
  :global t
  :group 'vwe-proxy)

(provide 'vwe-proxy)
;;; vwe-proxy.el ends here
