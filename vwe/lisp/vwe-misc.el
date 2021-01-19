;;; vwe-misc.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  WuJunyu

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
;; ************************************************************************
;; lib
;; ************************************************************************

;; ************************************************************************
;; config
;; ************************************************************************
(use-package youdao-dictionary)

(use-package english-teacher
  :load-path
  (lambda () (vwe@lib--path-vwe-site-lisp "english-teacher"))
  :init
  (setq english-teacher-show-result-function
		'english-teacher-eldoc-show-result-function))

(use-package cal-china-x
  :after
  calendar
  :commands cal-china-x-setup
  :init
  (cal-china-x-setup)
  :config
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays
		cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")
									   (holiday-lunar 7 7 "七夕节")
									   (holiday-lunar 7 15 "中元节")
									   (holiday-fixed 3 8 "妇女节")
									   (holiday-fixed 3 12 "植树节")
									   (holiday-fixed 5 4 "青年节")
									   (holiday-fixed 6 1 "儿童节")
									   (holiday-fixed 9 10 "教师节"))
		holiday-other-holidays '((holiday-fixed 2 14 "情人节")
								 (holiday-fixed 4 1 "愚人节")
								 (holiday-fixed 12 25 "圣诞节")
								 (holiday-float 5 0 2 "母亲节")
								 (holiday-float 6 0 3 "父亲节")
								 (holiday-float 11 4 4 "感恩节"))
		calendar-holidays (append cal-china-x-important-holidays
								  cal-china-x-general-holidays
								  holiday-other-holidays)))

(use-package w3m
  :hook
  (w3m-mode-hook . w3m-mode-hook-setup)
  :init
  (setq w3m-coding-system 'utf-8
		w3m-file-coding-system 'utf-8
		w3m-file-name-coding-system 'utf-8
		w3m-output-coding-system 'utf-8
		w3m-input-coding-system 'utf-8
		w3m-terminal-coding-system 'utf-8
		w3m-imagick-convert-program nil
		w3m-use-cookies t
		w3m-cookie-accept-bad-cookies t
		w3m-command-arguments '("-F" "-cookie")
		w3m-home-page "google.com"
		w3m-default-display-inline-image t
		w3m-default-toggle-inline-images t
		w3m-use-form t
		w3m-mailto-url-function 'compose-mail
		browse-url-browser-function 'w3m
		w3m-default-display-inline-images t
		w3m-use-tab nil
		w3m-search-default-engine "google")
    :config
  (setq browse-url-generic-program "google-chrome"
		browse-url-browser-function 'browse-url-generic))

(use-package wget
  :ensure nil
  :after
  w3m
  :load-path
  (lambda ()
	  (vwe@lib--path-vwe-site-lisp "wget"))
  :init
  (setq wget-process-buffer t
		wget-download-directory (vwe@lib--path-cache "download")
		wget-download-log-file "download.log"))

(use-package wttrin
  :init
  (setq wttrin-default-accept-language '("Accept-Language" . "zh-CN")
		wttrin-default-cities '("shenzhen" "xian"))
  :config
  (defun wttrin-fetch-raw-string (query)
	"Get the weather information based on your QUERY."
	(let ((url-user-agent "curl"))
      (add-to-list 'url-request-extra-headers wttrin-default-accept-language)
      (with-current-buffer
          (url-retrieve-synchronously
           (concat "http://wttr.in/" query "?A")
           (lambda (status) (switch-to-buffer (current-buffer))))
		(decode-coding-string (buffer-string) 'utf-8)))))

(use-package emms
  :init
  (emms-all)
  (emms-default-players)
  (setq emms-source-file-default-directory vwe@custom--user-media-path
		emms-lyrics-dir (concat vwe@custom--user-media-path "/lyrics")
		emms-player-list '(emms-player-mpg321
  						   emms-player-ogg123
  						   emms-player-mplayer
  						   emms-player-vlc)
		emms-score-file (vwe@lib--path-cache "emms/scores" t)
		emms-stream-bookmarks-file (vwe@lib--path-cache "emms/streams" t)
		emms-history-file (vwe@lib--path-cache "emms/history" t)
		emms-cache-file (vwe@lib--path-cache "emms/cache" t)
		emms-info-functions '(emms-info-mp3info)
		emms-score-enabled-p t
		emms-browser-default-browse-type 'info-album
		emms-stream-default-action "play"
		emms-mode-line-icon-color "Gold1"
		emms-mode-line-icon-before-format "["
		emms-mode-line-format " %s"
		emms-playing-time-display-format " %s]"))

(provide 'vwe-misc)
;;; vwe-misc.el ends here
