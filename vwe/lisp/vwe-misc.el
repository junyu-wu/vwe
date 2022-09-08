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

;;
;; `youdao-dictionary'
;;
(vwe@lib--pkg youdao-dictionary)

;;
;; `english-teacher'
;;
(vwe@lib--pkg english-teacher
  :variable ((setq english-teacher-show-result-function 'english-teacher-eldoc-show-result-function))
  :path (vwe@lib--path-vwe-site-lisp "english-teacher"))

;;
;; `cal-china-x'
;;
(vwe@lib--pkg cal-china-x
  :init ((require 'cal-china-x)
		 (add-hook 'calendar-mode-hook (lambda ()
										 (setq calendar-mark-holidays-flag t)
										 (cal-china-x-setup))))
  :variable ((setq cal-china-x-important-holidays cal-china-x-chinese-holidays
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
											 holiday-other-holidays))))

;;
;; `eww'
;;
(vwe@lib--pkg eww)

;;
;; `wget'
;;
(vwe@lib--pkg wget
  :init ((add-hook 'w3m-mode-hook (lambda () (vwe@lib--package-load 'wget))))
  :variable ((setq wget-process-buffer t
				   wget-download-directory (vwe@lib--path-cache "download")
				   wget-download-log-file "download.log"))
  :path (vwe@lib--path-vwe-site-lisp "wget"))

;;
;; `wttrin'
;;
(vwe@lib--pkg wttrin
  :config ((defun wttrin-fetch-raw-string (query)
			 "Get the weather information based on your QUERY."
			 (let ((url-user-agent "curl"))
			   (add-to-list 'url-request-extra-headers wttrin-default-accept-language)
			   (with-current-buffer
				   (url-retrieve-synchronously
					(concat "http://wttr.in/" query "?A")
					(lambda (status) (switch-to-buffer (current-buffer))))
				 (decode-coding-string (buffer-string) 'utf-8)))))
  :variable ((setq wttrin-default-accept-language '("Accept-Language" . "zh-CN")
				   wttrin-default-cities '("shenzhen" "xian"))))

;;
;; `emms'
;;
(vwe@lib--pkg emms
  :config ((emms-all)
		   (emms-default-players))
  :variable ((setq emms-source-file-default-directory vwe@custom--user-media-path
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
				   emms-playing-time-display-format " %s]")))

;;
;; `nov' 电子书阅读器
;;
(vwe@lib--pkg nov
  :init ((add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))
  :variable ((setq nov-save-place-file (concat (vwe@lib--path-emacs.d) ".cache/nov/nov-splaces"))
			 (when vwe@lib--sys-win-p
			   (setq process-coding-system-alist
					 (cons `(,nov-unzip-program . (gbk . gbk))
						   process-coding-system-alist)))))

(vwe@lib--log "Initialization of Misc configuration is complete.")

(provide 'vwe-misc)
;;; vwe-misc.el ends here
