;;; vwe-layout.el ---  Vwe layout              -*- lexical-binding: t; -*-

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

(defgroup vwe-layout nil
  "Window layout."
  :group 'windows)

;;
;; zoom
;;
(defcustom vwe-layout--zoom-size
  #'vwe-layout--window-zoom-size-callback
  "Zoom size."
  :type 'list
  :group 'vwe-layout)

(defcustom vwe-layout--zoom-ignored-major-modes nil
  "List of ignored major modes."
  :type 'symbol
  :group 'vwe-layout)

(defcustom vwe-layout--zoom-ignored-buffer-names nil
  "List of ignored buffer names."
  :type 'string
  :group 'vwe-layout)

(defcustom vwe-layout--zoom-ignored-buffer-name-regexps nil
  "List of ignored buffer name regexps."
  :type 'regexp
  :group 'vwe-layout)

(defcustom vwe-layout--zoom-ignore-predicates nil
  "List of additional predicates that allow to ignore windows."
  :type 'function
  :group 'vwe-layout)

(defvar vwe-layout--last-window
  nil
  "Track of the currently selected window.")

(defvar vwe-layout--zoom-balance-p
  nil
  "Windows `ratio' or `balance'.")

(defun vwe-layout--text-scale-increase ()
  "Text scale increase."
  (interactive)
  (text-scale-increase 1))

(defun vwe-layout--text-scale-decrease ()
  "Text scale decrease."
  (interactive)
  (text-scale-increase -1))

(defun vwe-layout--text-scale-adjust ()
  "Text scale adjust."
  (interactive)
  (text-scale-increase 0))

(defun vwe-layout--window-height-enlarge (&optional delta)
  "Window height enlarge DELTA."
  (interactive)
  (unless delta (setq delta 1))
  (condition-case nil (enlarge-window delta)
	(error (set-frame-height (selected-frame) (+ (frame-height (selected-frame)) delta)))))

(defun vwe-layout--window-height-shrink (&optional delta)
  "Window height shrink DELTA."
  (interactive)
  (unless delta (setq delta 1))
  (condition-case nil (shrink-window delta)
	(error (set-frame-height (selected-frame) (- (frame-height (selected-frame)) delta)))))

(defun vwe-layout--window-width-enlarge (&optional delta)
  "Window width enlarge DELTA."
  (interactive)
  (unless delta (setq delta 1))
  (condition-case nil (enlarge-window delta t)
	(error (set-frame-width (selected-frame) (+ (frame-width (selected-frame)) delta)))))

(defun vwe-layout--window-width-shrink (&optional delta)
  "Window width shrink DELTA."
  (interactive)
  (unless delta (setq delta 1))
  (condition-case nil (shrink-window delta t)
	(error (set-frame-width (selected-frame) (- (frame-width (selected-frame)) delta)))))

(defun vwe-layout--window-zoom-size-callback ()
  "Window zoom size callback."
  (cond ((> (frame-pixel-width) 1280) '(0.75 . 0.75))
        (t                            '(0.5 . 0.5))))

(defun vwe-layout--get-windows-info-to-string ()
  "Get window info string in frame."
  (format "%s" (list (default-value 'track-mouse)
                     (mapcar (lambda (window) (list window
                                                    (window-total-width)
                                                    (window-total-height)))
                             (window-list)))))

(defun vwe-layout--zoom-window-ignored-p ()
  "Check whether the selected window will be ignored or not."
  (or
   (frame-root-window-p (selected-window))
   (window-minibuffer-p)
   (member major-mode vwe-layout--zoom-ignored-major-modes)
   (member (buffer-name) vwe-layout--zoom-ignored-buffer-names)
   (catch 'ignored
     (dolist (regex vwe-layout--zoom-ignored-buffer-name-regexps)
       (when (string-match regex (buffer-name))
         (throw 'ignored t))))
   (catch 'ignored
     (dolist (predicate vwe-layout--zoom-ignore-predicates)
       (when (funcall predicate)
         (throw 'ignored t))))))

(defun vwe-layout--windows-zoom (&optional ignored)
  "Redisplay windows zoom.
Argument IGNORED is ignored."
  (let* ((windows-info (vwe-layout--get-windows-info-to-string)))
	(unless (equal (frame-parameter nil 'vwe-layout--zoom-parameter) windows-info)
	  (set-frame-parameter nil 'vwe-layout--zoom-parameter windows-info)
	  (with-selected-window (if (or (equal (selected-window) vwe-layout--last-window)
									(window-minibuffer-p)
									(default-value 'track-mouse))
								vwe-layout--last-window
							  (selected-window))
		(setq vwe-layout--last-window (selected-window))
		(vwe-layout--windows-zoom-redisplay)))))

(defun vwe-layout--windows-zoom-redisplay ()
  "Resize windows zoom."
  (let* ((vwe-layout-zoom-mode nil)
		 (window-configuration-change-hook nil)
		 (window-combination-resize t)
		 (window-resize-pixelwise t))
	(balance-windows)
	(unless vwe-layout--zoom-balance-p
	  (unless (vwe-layout--zoom-window-ignored-p)
		(progn (vwe-layout--windows-zoom-resize) (vwe-layout--windows-zoom-resize t))
		(unless (derived-mode-p 'image-mode)
          (scroll-right (window-hscroll))
		  (when (and truncate-lines
					 (> (current-column) (- (window-body-width) hscroll-margin)))
			(scroll-left (- (current-column) (/ (window-body-width) 2)))))))))

(defun vwe-layout--windows-zoom-resize (&optional horizontal?)
  "Resize windows.
HORIZONTAL horizontal or vertical."
  (let* ((size (if (funcall vwe-layout--zoom-size) (funcall vwe-layout--zoom-size) vwe-layout--zoom-size))
		 (custom-size (if horizontal? (car size) (cdr size)))
		 (frame-size (if horizontal? (frame-width) (frame-height)))
		 (win-size (if (floatp custom-size)
					   ;; ratios
					   (if horizontal? (window-total-width) (window-total-height))
					 ;; absolute
					 (if horizontal? (window-body-width) (window-body-height))))
		 (win-ratio (if (floatp custom-size) (round (* custom-size frame-size)) custom-size))
		 (delta (window-resizable nil (max (- win-ratio win-size) 0) horizontal?)))
	(window-resize nil delta horizontal?)))

(defun vwe-layout--zoom-type-toggle ()
  "Toggle zoom type."
  (interactive)
  (if vwe-layout--zoom-balance-p
	  (setq vwe-layout--zoom-balance-p nil)
	(setq vwe-layout--zoom-balance-p t))
  (vwe-layout--windows-zoom))

(defun vwe-layout--zoom-enable ()
  "Enable mode."
  (add-function :after pre-redisplay-function #'vwe-layout--windows-zoom)
  ;; disable mouse resizing
  (advice-add #'mouse-drag-mode-line :override #'ignore)
  (advice-add #'mouse-drag-vertical-line :override #'ignore)
  (advice-add #'mouse-drag-header-line :override #'ignore)
  ;; update the layout once loaded
  (dolist (frame (frame-list))
    (with-selected-frame frame
      (vwe-layout--windows-zoom))))

(defun vwe-layout--zoom-disable ()
  "Disable mode."
  (remove-function pre-redisplay-function #'vwe-layout--windows-zoom)
  (advice-remove #'mouse-drag-mode-line #'ignore)
  (advice-remove #'mouse-drag-vertical-line #'ignore)
  (advice-remove #'mouse-drag-header-line #'ignore)
  (dolist (frame (frame-list))
    (balance-windows frame)))

;;;###autoload
(define-minor-mode vwe-layout-zoom-mode
  "Zoom Mode."
  :global t
  (if vwe-layout-zoom-mode
	  (vwe-layout--zoom-enable)
    (vwe-layout--zoom-disable)))
;;
;; move
;;
(defun vwe-layout--windmove-up()
  "Selected current up window."
  (interactive)
  (condition-case nil (windmove-up)
    (error (condition-case nil (windmove-down) (error)))))

(defun vwe-layout--windmove-down()
  "Selected current down window."
  (interactive)
  (condition-case nil (windmove-down)
    (error (condition-case nil (windmove-up) (error)))))

(defun vwe-layout--windmove-right()
  "Selected current right window."
  (interactive)
  (condition-case nil (windmove-right)
    (error (condition-case nil (windmove-left) (error)))))

(defun vwe-layout--windmove-left()
  "Selected current left window."
  (interactive)
  (condition-case nil (windmove-left)
    (error (condition-case nil (windmove-right) (error)))))

;;
;; buffer swap
;;
(defun vwe-layout--swap-up-buffer ()
  "Swap current and up buffer."
  (interactive)
  (let ((current-window (selected-window))
		(current-buffer (buffer-name))
		(swaped-window)
		(swaped-buffer))
	(vwe-layout--windmove-up)
	(setq swaped-window (selected-window)
		  swaped-buffer (buffer-name))
	(when (and (not (string= swaped-buffer current-buffer)))
	  (set-window-buffer swaped-window current-buffer)
	  (set-window-buffer current-window swaped-buffer))))

(defun vwe-layout--swap-down-buffer ()
  "Swap current and down buffer."
  (interactive)
  (let ((current-window (selected-window))
		(current-buffer (buffer-name))
		(swaped-window)
		(swaped-buffer))
	(vwe-layout--windmove-down)
	(setq swaped-window (selected-window)
		  swaped-buffer (buffer-name))
	(when (and (not (string= swaped-buffer current-buffer)))
	  (set-window-buffer swaped-window current-buffer)
	  (set-window-buffer current-window swaped-buffer))))

(defun vwe-layout--swap-right-buffer ()
  "Swap current and right buffer."
  (interactive)
  (let ((current-window (selected-window))
		(current-buffer (buffer-name))
		(swaped-window nil)
		(swaped-buffer nil))
	(vwe-layout--windmove-right)
	(setq swaped-window (selected-window)
		  swaped-buffer (buffer-name))
	(when (and (not (string= swaped-buffer current-buffer)))
	  (set-window-buffer swaped-window current-buffer)
	  (set-window-buffer current-window swaped-buffer))))

(defun vwe-layout--swap-left-buffer()
  "Swap current and right buffer."
  (interactive)
  (let ((current-window (selected-window))
		(current-buffer (buffer-name))
		(swaped-window nil)
		(swaped-buffer nil))
	(vwe-layout--windmove-left)
	(setq swaped-window (selected-window)
		  swaped-buffer (buffer-name))
	(when (and (not (string= swaped-buffer current-buffer)))
	  (set-window-buffer swaped-window current-buffer)
	  (set-window-buffer current-window swaped-buffer))))

;;
;; buffer switch
;;
(defvar vwe-layout--switch-side-buffer-name
  "*vwe-layout:switch-side-tmp-buffer*"
  "Switch side buffer name.")

(defvar vwe-layout--switch-buffer-keymap
  (let* ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd "q") #'vwe-layout--kill-switch-side-buffer)
	(define-key keymap (kbd "n") #'next-line)
	(define-key keymap (kbd "p") #'previous-line)
	(define-key keymap (kbd "s") (lambda () (interactive) (if (fboundp 'swiper) (swiper) (isearch-forward))))
	(define-key keymap (kbd "TAB") #'vwe-layout--switch-buffer)
	keymap)
  "Switch buffer keymap.")

(defface vwe-layout--default-face
  '((t (:inherit 'default :weight bold)))
  "Default face.")

(defface vwe-layout--info-face
  '((t (:foreground "DarkOrange" :weight bold)))
  "Info face.")

(defface vwe-layout--success-face
  '((t (:foreground "SpringGreen" :weight bold)))
  "Success face.")

(defface vwe-layout--warning-face
  '((t (:foreground "yellow" :weight bold)))
  "Warning face.")

(defface vwe-layout--error-face
  '((t (:foreground "DarkRed" :weight bold)))
  "Error face.")

(defface vwe-layout--button-face
  '((t (:foreground "SkyBlue" :weight bold)))
  "Button face.")

(defvar vwe-layout--switch-side-show-status
  'buffer
  "Switch side buffer show content `buffer' or `temp' buffer.")

(defun vwe-layout--switch-buffer ()
  "Switch buffer."
  (interactive)
  (cond
   ((equal vwe-layout--switch-side-show-status 'buffer) (setq vwe-layout--switch-side-show-status 'temp) (vwe-layout--display-switch-side-buffer))
   ((equal vwe-layout--switch-side-show-status 'temp) (setq vwe-layout--switch-side-show-status 'buffer) (vwe-layout--display-switch-side-buffer t))
   (t (setq vwe-layout--switch-side-show-status 'temp) (vwe-layout--display-switch-side-buffer))))

(defun vwe-layout--build-switch-buffer-headerline ()
  "Build switch buffer headerline."
  (let* ((allbuf (length (buffer-list)))
		 (buflen (length (vwe-layout--get-buffer-list)))
		 (tmpbuflen (length (vwe-layout--get-buffer-list t)))
		 (hidebuflen (- allbuf buflen tmpbuflen))
		 (split (propertize (format " | ") 'face 'vwe-layout--default-face)))
	(concat
	 (propertize (format "Vwe switch buffer:")
				 'face 'vwe-layout--success-face)
	 split
	 (propertize (format "total " )
				 'face 'vwe-layout--default-face)
	 (propertize (format "%d" allbuf)
				 'face 'vwe-layout--success-face)
	 split
	 (propertize (format "buffer ")
				 'face 'vwe-layout--default-face)
	 (propertize (format "%d" buflen)
				 'face 'vwe-layout--info-face)
	 split
	 (propertize (format "* buffer ")
				 'face 'vwe-layout--default-face)
	 (propertize (format "%d" tmpbuflen)
				 'face 'vwe-layout--warning-face)
	 split
	 (propertize (format "hide buffer ")
				 'face 'vwe-layout--default-face)
	 (propertize (format "%d" hidebuflen)
				 'face 'vwe-layout--button-face)
	 split
	 (propertize (format "buffer/*buffer ")
				 'face 'vwe-layout--default-face)
	 (propertize (format "[TAB]")
				 'face 'vwe-layout--info-face)
	 (propertize (format " quit ")
				 'face 'vwe-layout--default-face)
	 (propertize (format "[q]")
				 'face 'vwe-layout--info-face))))

(defun vwe-layout--display-switch-side-buffer (&optional tmp?)
  "Make switch buffer cmd buffer.
TMP is tmp buffer."
  (let* ((buffer (if (get-buffer vwe-layout--switch-side-buffer-name) (get-buffer vwe-layout--switch-side-buffer-name) (get-buffer-create vwe-layout--switch-side-buffer-name)))
		 ;; (buffer-length (length (vwe-layout--get-buffer-list)))
		 ;; (tmpbuf-length (length (vwe-layout--get-buffer-list t)))
		 ;; (headerline (concat (format "buffer total %d | buffer %d | tmp buffer %d | hide buffer %d"
		 ;; (length (buffer-list)) buffer-length tmpbuf-length (- (length (buffer-list)) buffer-length tmpbuf-length))))
		 (headerline (vwe-layout--build-switch-buffer-headerline))
		 (bufname-list (if tmp?
						   (mapcar #'buffer-name (vwe-layout--get-buffer-list t))
						 (mapcar #'buffer-name (vwe-layout--get-buffer-list))))
		 (alist '((window-width . vwe-key--max-width)
				  (window-height . fit-window-to-buffer)
				  (direction . 'down)
				  (slot . 0))))
	(when buffer
	  (with-current-buffer buffer
		(read-only-mode -1)
		(erase-buffer)
		(setq-local mode-line-format nil
					word-wrap nil
					show-trailing-whitespace nil
					header-line-format headerline)
		(vwe-layout--switch-buffer-mode 1)
		(dotimes (i (length bufname-list))
		  (insert-button (concat  "[" (nth i bufname-list) "]" (unless (= (1+ i) (length bufname-list)) "\n"))
						 'action (lambda(_)
								   (vwe-layout--kill-switch-side-buffer)
								   (switch-to-buffer (nth i bufname-list)))
						 'follow-link t))
		(goto-char (point-min))
		(read-only-mode t))
	  (select-window (display-buffer-in-side-window buffer alist)))))

(defun vwe-layout--kill-switch-side-buffer ()
  "Kill side buffer."
  (interactive)
  (let* ((buffer (get-buffer vwe-layout--switch-side-buffer-name)))
	(when buffer
	  (delete-windows-on buffer)
	  (kill-buffer buffer)
	  (setq vwe-layout--switch-side-show-status 'buffer))))

(defun vwe-layout--buffer-list-filter (regexp &optional self?)
  "Find buffer list of REGEXP.
SELF is include curretn buffer."
  (seq-filter 'bufferp
			  (mapcar
			   (lambda (item)
				 (if (and (string-match regexp (buffer-name item)) (not (equal vwe-layout--switch-side-buffer-name (buffer-name item))))
					 (if self?
						 item
					   (unless (equal item (current-buffer)) item))))
			   (buffer-list))))

(defun vwe-layout--get-buffer-list (&optional tmp? self?)
  "Get buffer list.
TMP is asterisk buffers.
SELF is include curretn buffer."
  (if tmp?
	  (vwe-layout--buffer-list-filter "^*" self?)
	(vwe-layout--buffer-list-filter "^[^\s*]" self?)))

(defun vwe-layout--switch-buffer-enable ()
  "Enable mode.")

(defun vwe-layout--switch-buffer-disable ()
  "Disable mode."
  (vwe-layout--kill-switch-side-buffer))

;;;###autoload
(define-minor-mode vwe-layout--switch-buffer-mode
  "Mode."
  :keymap vwe-layout--switch-buffer-keymap
  (if vwe-layout--switch-buffer-mode
	  (vwe-layout--switch-buffer-enable)
    (vwe-layout--switch-buffer-disable)))

;;
;; mode
;;
(defun vwe-layout--enable ()
  "Enable mode."
  (vwe-layout-zoom-mode t))

(defun vwe-layout--disable ()
  "Disable mode."
  (vwe-layout-zoom-mode -1))

;;;###autoload
(define-minor-mode vwe-layout-mode
  "Vwe layout mode."
  :global t
  (if vwe-layout-mode
	  (vwe-layout--enable)
    (vwe-layout--disable)))

(provide 'vwe-layout)
;;; vwe-layout.el ends here
