;;; mum-proxy.el --- Mum proxy                       -*- lexical-binding: t; -*-

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

(defgroup mum-proxy nil
  "Mum proxy."
  :prefix "mum-proxy--"
  :group 'proxy)

(defcustom mum-proxy--http-proxy-for-env
  '("127.0.0.1" 1080)
  "Environment variable http proxy."
  :type 'list
  :group 'mum-proxy)

(defcustom mum-proxy--url-proxy-services
  '(("http"  . "127.0.0.1:1080")
    ("https" . "127.0.0.1:1080")
    ("ftp"   . "127.0.0.1:1080")
    ("no_proxy" . "127.0.0.1"))
  "Url proxy services."
  :type 'alist
  :group 'mum-proxy)

(defcustom mum-proxy--socks-proxy-for-env
  '("Default" "127.0.0.1" 1080 5)
  "Environment variable socks proxy."
  :type 'list
  :group 'mum-proxy)

(defcustom mum-proxy--socks-non-proxy-alist
  '("localhost" "192.168.*" "10.*")
  "Non proxy alist."
  :type 'list
  :group 'mum-proxy)

(defvar mum-proxy--current-type
  nil
  "Current proxy type.")

(defvar mum-proxy--globalp
  nil
  "Is global.")

(defun mum-proxy--http-proxy-enable (&optional global?)
  "Enable local or GLOBAL http proxy."
  (setenv "HTTP_PROXY" (format "%s:%s"
							   (car mum-proxy--http-proxy-for-env)
							   (cadr mum-proxy--http-proxy-for-env)))
  (setenv "HTTPS_PROXY" (getenv "HTTP_PROXY"))
  (if global?
	  (setq mum-proxy--current-type "http")
	(setq-local mum-proxy--current-type "http"))
  (message "[%s] http proxy enable" (getenv "HTTP_PROXY")))

(defun mum-proxy--http-proxy-disable (&optional global?)
  "Disable local or GLOBAL http proxy."
  (setenv "HTTP_PROXY" nil)
  (setenv "HTTPS_PROXY" (getenv "HTTP_PROXY"))
  (if global?
	  (setq mum-proxy--current-type nil)
	(setq-local mum-proxy--current-type nil))
  (message "http proxy disable"))

(defun mum-proxy--url-proxy-enable (&optional global?)
  "Enable local or GLOBAL URL proxy."
  (if global?
	  (setq url-proxy-services mum-proxy--url-proxy-services
			mum-proxy--current-type "url")
	(setq-local url-proxy-services mum-proxy--url-proxy-services
				mum-proxy--current-type "url"))
  (message "url proxy enable"))

(defun mum-proxy--url-proxy-disable (&optional global?)
  "Disable local or GLOBAL URL proxy."
  (if global?
	  (setq url-proxy-services nil
			mum-proxy--current-type nil)
	(setq-local url-proxy-services nil
				mum-proxy--current-type nil))
  (message "url proxy disable"))

(defun mum-proxy--socks-proxy-enable (&optional global?)
  "Enable local or GLOBAL Socks proxy."
  (require 'url-gw)
  (require 'socks)
  (if global?
	  (setq url-gateway-method 'socks
			socks-noproxy mum-proxy--socks-non-proxy-alist
			socks-server mum-proxy--socks-proxy-for-env
			mum-proxy--current-type "socks")
	(setq-local url-gateway-method 'socks
				socks-noproxy mum-proxy--socks-non-proxy-alist
				socks-server mum-proxy--socks-proxy-for-env
				mum-proxy--current-type "socks"))
  (message "socks proxy %s enable" mum-proxy--socks-proxy-for-env))

(defun mum-proxy--socks-proxy-disable (&optional global?)
  "Disable local or GLOBAL Socks proxy."
  (if global?
	  (setq url-gateway-method 'native
			mum-proxy--current-type nil)
	(setq-local url-gateway-method 'native
				mum-proxy--current-type nil))
  (message "socks proxy disable"))

;;;###autoload
(defun mum-proxy--enable (&optional type)
  "Enable enable proxy for TYPE."
  (interactive
   (list
    (completing-read (format "type (%s):" mum-proxy--current-type)
					 '("http" "url" "socks"))))
  (cond
   ((equal type "http") (mum-proxy--http-proxy-enable))
   ((equal type "url") (mum-proxy--url-proxy-enable))
   ((equal type "socks") (mum-proxy--socks-proxy-enable))
   (t (mum-proxy--http-proxy-enable)))
  (mum-proxy-mode t))

(defun mum-proxy--disable ()
  "Enable disable proxy."
  (interactive)
  (when mum-proxy-mode
	(cond
	 ((equal mum-proxy--current-type "http") (mum-proxy--http-proxy-disable))
	 ((equal mum-proxy--current-type "url") (mum-proxy--url-proxy-disable))
	 ((equal mum-proxy--current-type "socks") (mum-proxy--socks-proxy-disable))
	 (t (mum-proxy--http-proxy-disable)))
	(mum-proxy-mode -1)))

;;;###autoload
(define-minor-mode mum-proxy-mode
  "Mum proxy mode."
  :global t
  :group 'mum-proxy)

;;;###autoload
(defun mum-proxy--enable-global (&optional type)
  "Enable global enable proxy for TYPE."
  (interactive
   (list
    (completing-read (format "type (%s):" mum-proxy--current-type)
					 '("http" "url" "socks"))))
  (cond
   ((equal type "http") (mum-proxy--http-proxy-enable t))
   ((equal type "url") (mum-proxy--url-proxy-enable t))
   ((equal type "socks") (mum-proxy--socks-proxy-enable t))
   (t (mum-proxy--http-proxy-enable t)))
  (global-mum-proxy-mode t))

(defun mum-proxy--disable-global ()
  "Enable global disable proxy."
  (interactive)
  (when global-mum-proxy-mode
	(cond
	 ((equal mum-proxy--current-type "http") (mum-proxy--http-proxy-disable t))
	 ((equal mum-proxy--current-type "url") (mum-proxy--url-proxy-disable t))
	 ((equal mum-proxy--current-type "socks") (mum-proxy--socks-proxy-disable t))
	 (t (mum-proxy--http-proxy-disable t)))
	(global-mum-proxy-mode -1)))

;;;###autoload
(define-minor-mode global-mum-proxy-mode
  "Global mum proxy mode."
  :global t
  :group 'mum-proxy)

(provide 'mum-proxy)
;;; mum-proxy.el ends here
