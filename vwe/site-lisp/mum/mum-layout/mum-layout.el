;;; mum-layout.el ---  Mum layout              -*- lexical-binding: t; -*-

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

(defgroup mum-layout nil
  "Window layout."
  :group 'windows)

;;
;; zoom
;;
(defcustom mum-layout--zoom-size
  #'mum-layout--window-zoom-size-callback
  "Zoom size."
  :type 'list
  :group 'mum-layout)

(defcustom mum-layout--zoom-ignored-major-modes nil
  "List of ignored major modes."
  :type 'symbol
  :group 'mum-layout)

(defcustom mum-layout--zoom-ignored-buffer-names nil
  "List of ignored buffer names."
  :type 'string
  :group 'mum-layout)

(defcustom mum-layout--zoom-ignored-buffer-name-regexps nil
  "List of ignored buffer name regexps."
  :type 'regexp
  :group 'mum-layout)

(defcustom mum-layout--zoom-ignore-predicates nil
  "List of additional predicates that allow to ignore windows."
  :type 'function
  :group 'mum-layout)

(defvar mum-layout--last-window
  nil
  "Track of the currently selected window.")

(defun mum-layout--text-scale-increase ()
  "Text scale increase."
  (interactive)
  (text-scale-increase 1))

(defun mum-layout--text-scale-decrease ()
  "Text scale decrease."
  (interactive)
  (text-scale-increase -1))

(defun mum-layout--text-scale-adjust ()
  "Text scale adjust."
  (interactive)
  (text-scale-increase 0))

(defun mum-layout--window-height-enlarge (&optional delta)
  "Window height enlarge DELTA."
  (interactive)
  (unless delta (setq delta 1))
  (condition-case nil (enlarge-window delta)
	(error (set-frame-height (selected-frame) (+ (frame-height (selected-frame)) delta)))))

(defun mum-layout--window-height-shrink (&optional delta)
  "Window height shrink DELTA."
  (interactive)
  (unless delta (setq delta 1))
  (condition-case nil (shrink-window delta)
	(error (set-frame-height (selected-frame) (- (frame-height (selected-frame)) delta)))))

(defun mum-layout--window-width-enlarge (&optional delta)
  "Window width enlarge DELTA."
  (interactive)
  (unless delta (setq delta 1))
  (condition-case nil (enlarge-window delta t)
	(error (set-frame-width (selected-frame) (+ (frame-width (selected-frame)) delta)))))

(defun mum-layout--window-width-shrink (&optional delta)
  "Window width shrink DELTA."
  (interactive)
  (unless delta (setq delta 1))
  (condition-case nil (shrink-window delta t)
	(error (set-frame-width (selected-frame) (- (frame-width (selected-frame)) delta)))))

(defun mum-layout--window-zoom-size-callback ()
  "Window zoom size callback."
  (cond ((> (frame-pixel-width) 1280) '(0.75 . 0.75))
        (t                            '(0.5 . 0.5))))

(defun mum-layout--get-windows-info-to-string ()
  "Get window info string in frame."
  (format "%s" (list (default-value 'track-mouse)
                     (mapcar (lambda (window) (list window
                                                    (window-total-width)
                                                    (window-total-height)))
                             (window-list)))))

(defun mum-layout--zoom-window-ignored-p ()
  "Check whether the selected window will be ignored or not."
  (or
   (frame-root-window-p (selected-window))
   (window-minibuffer-p)
   (member major-mode mum-layout--zoom-ignored-major-modes)
   (member (buffer-name) mum-layout--zoom-ignored-buffer-names)
   (catch 'ignored
     (dolist (regex mum-layout--zoom-ignored-buffer-name-regexps)
       (when (string-match regex (buffer-name))
         (throw 'ignored t))))
   (catch 'ignored
     (dolist (predicate mum-layout--zoom-ignore-predicates)
       (when (funcall predicate)
         (throw 'ignored t))))))

(defun mum-layout--windows-zoom (&optional ignored)
  "Redisplay windows zoom.
Argument IGNORED is ignored."
  (let* ((windows-info (mum-layout--get-windows-info-to-string)))
	(unless (equal (frame-parameter nil 'mum-layout--zoom-parameter) windows-info)
	  (set-frame-parameter nil 'mum-layout--zoom-parameter windows-info)
	  (with-selected-window (if (or (equal (selected-window) mum-layout--last-window)
									(window-minibuffer-p)
									(default-value 'track-mouse))
								mum-layout--last-window
							  (selected-window))
		(setq mum-layout--last-window (selected-window))
		(mum-layout--windows-zoom-redisplay)))))

(defun mum-layout--windows-zoom-redisplay ()
  "Resize windows zoom."
  (let* ((mum-layout-zoom-mode nil)
		 (window-configuration-change-hook nil)
		 (window-combination-resize t)
		 (window-resize-pixelwise t))
	(balance-windows)
    (unless (mum-layout--zoom-window-ignored-p)
      (progn (mum-layout--windows-zoom-resize) (mum-layout--windows-zoom-resize t))
      (unless (derived-mode-p 'image-mode)
        (scroll-right (window-hscroll))
		(when (and truncate-lines
				   (> (current-column) (- (window-body-width) hscroll-margin)))
		  (scroll-left (- (current-column) (/ (window-body-width) 2))))))))

(defun mum-layout--windows-zoom-resize (&optional horizontal?)
  "Resize windows.
HORIZONTAL horizontal or vertical."
  (let* ((size (if (funcall mum-layout--zoom-size) (funcall mum-layout--zoom-size) mum-layout--zoom-size))
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

(defun mum-layout--zoom-enable ()
  "Enable mode."
  (add-function :after pre-redisplay-function #'mum-layout--windows-zoom)
  ;; disable mouse resizing
  (advice-add #'mouse-drag-mode-line :override #'ignore)
  (advice-add #'mouse-drag-vertical-line :override #'ignore)
  (advice-add #'mouse-drag-header-line :override #'ignore)
  ;; update the layout once loaded
  (dolist (frame (frame-list))
    (with-selected-frame frame
      (mum-layout--windows-zoom))))

(defun mum-layout--zoom-disable ()
  "Disable mode."
  (remove-function pre-redisplay-function #'mum-layout--windows-zoom)
  (advice-remove #'mouse-drag-mode-line #'ignore)
  (advice-remove #'mouse-drag-vertical-line #'ignore)
  (advice-remove #'mouse-drag-header-line #'ignore)
  (dolist (frame (frame-list))
    (balance-windows frame)))

;;;###autoload
(define-minor-mode mum-layout-zoom-mode
  "Zoom Mode."
  :global t
  (if mum-layout-zoom-mode
	  (mum-layout--zoom-enable)
    (mum-layout--zoom-disable)))
;;
;; move
;;
(defun mum-layout--windmove-up()
  "Selected current up window."
  (interactive)
  (condition-case nil (windmove-up)
    (error (condition-case nil (windmove-down) (error)))))

(defun mum-layout--windmove-down()
  "Selected current down window."
  (interactive)
  (condition-case nil (windmove-down)
    (error (condition-case nil (windmove-up) (error)))))

(defun mum-layout--windmove-right()
  "Selected current right window."
  (interactive)
  (condition-case nil (windmove-right)
    (error (condition-case nil (windmove-left) (error)))))

(defun mum-layout--windmove-left()
  "Selected current left window."
  (interactive)
  (condition-case nil (windmove-left)
    (error (condition-case nil (windmove-right) (error)))))

;;
;; buffer swap
;;
(defun mum-layout--swap-up-buffer ()
  "Swap current and up buffer."
  (interactive)
  (let ((current-window (selected-window))
		(current-buffer (buffer-name))
		(swaped-window)
		(swaped-buffer))
	(mum-layout--windmove-up)
	(setq swaped-window (selected-window)
		  swaped-buffer (buffer-name))
	(when (and (not (string= swaped-buffer current-buffer)))
	  (set-window-buffer swaped-window current-buffer)
	  (set-window-buffer current-window swaped-buffer))))

(defun mum-layout--swap-down-buffer ()
  "Swap current and down buffer."
  (interactive)
  (let ((current-window (selected-window))
		(current-buffer (buffer-name))
		(swaped-window)
		(swaped-buffer))
	(mum-layout--windmove-down)
	(setq swaped-window (selected-window)
		  swaped-buffer (buffer-name))
	(when (and (not (string= swaped-buffer current-buffer)))
	  (set-window-buffer swaped-window current-buffer)
	  (set-window-buffer current-window swaped-buffer))))

(defun mum-layout--swap-right-buffer ()
  "Swap current and right buffer."
  (interactive)
  (let ((current-window (selected-window))
		(current-buffer (buffer-name))
		(swaped-window nil)
		(swaped-buffer nil))
	(mum-layout--windmove-right)
	(setq swaped-window (selected-window)
		  swaped-buffer (buffer-name))
	(when (and (not (string= swaped-buffer current-buffer)))
	  (set-window-buffer swaped-window current-buffer)
	  (set-window-buffer current-window swaped-buffer))))

(defun mum-layout--swap-left-buffer()
  "Swap current and right buffer."
  (interactive)
  (let ((current-window (selected-window))
		(current-buffer (buffer-name))
		(swaped-window nil)
		(swaped-buffer nil))
	(mum-layout--windmove-left)
	(setq swaped-window (selected-window)
		  swaped-buffer (buffer-name))
	(when (and (not (string= swaped-buffer current-buffer)))
	  (set-window-buffer swaped-window current-buffer)
	  (set-window-buffer current-window swaped-buffer))))

;;
;; buffer switch
;;
(defvar mum-layout--switch-side-buffer-name
  "*mum-layout:switch-side-tmp-buffer*"
  "Switch side buffer name.")

(defvar mum-layout--switch-buffer-keymap
  (let* ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd "q") #'mum-layout--kill-switch-side-buffer)
	(define-key keymap (kbd "n") #'next-line)
	(define-key keymap (kbd "p") #'previous-line)
	(define-key keymap (kbd "s") (lambda () (interactive) (if (fboundp 'swiper) (swiper) (isearch-forward))))
	(define-key keymap (kbd "TAB") #'mum-layout--switch-buffer)
	keymap)
  "Switch buffer keymap.")

(defvar mum-layout--switch-side-show-status
  'buffer
  "Switch side buffer show content `buffer' or `temp' buffer.")

(defun mum-layout--switch-buffer ()
  "Switch buffer."
  (interactive)
  (cond
   ((equal mum-layout--switch-side-show-status 'buffer) (setq mum-layout--switch-side-show-status 'temp) (mum-layout--display-switch-side-buffer))
   ((equal mum-layout--switch-side-show-status 'temp) (setq mum-layout--switch-side-show-status 'buffer) (mum-layout--display-switch-side-buffer t))
   (t (setq mum-layout--switch-side-show-status 'temp) (mum-layout--display-switch-side-buffer))))

(defun mum-layout--display-switch-side-buffer (&optional tmp?)
  "Make switch buffer cmd buffer.
TMP is tmp buffer."
  (let* ((buffer (if (get-buffer mum-layout--switch-side-buffer-name) (get-buffer mum-layout--switch-side-buffer-name) (get-buffer-create mum-layout--switch-side-buffer-name)))
		 (buffer-length (length (mum-layout--get-buffer-list)))
		 (tmpbuf-length (length (mum-layout--get-buffer-list t)))
		 (headerline (format "buffer total %d | buffer is %d | tmp buffer is %d"
							 (length (buffer-list)) buffer-length tmpbuf-length))
		 (bufname-list (if tmp?
						   (mapcar #'buffer-name (mum-layout--get-buffer-list t))
						 (mapcar #'buffer-name (mum-layout--get-buffer-list))))
		 (alist '((window-width . mum-key--max-width)
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
		(mum-layout--switch-buffer-mode 1)
		(dotimes (i (length bufname-list))
		  (insert-button (concat  "[" (nth i bufname-list) "]" (unless (= (1+ i) (length bufname-list)) "\n"))
						 'action (lambda(_)
								   (mum-layout--kill-switch-side-buffer)
								   (switch-to-buffer (nth i bufname-list)))
						 'follow-link t))
		(goto-char (point-min))
		(read-only-mode t))
	  (select-window (display-buffer-in-side-window buffer alist)))))

(defun mum-layout--kill-switch-side-buffer ()
  "Kill side buffer."
  (interactive)
  (let* ((buffer (get-buffer mum-layout--switch-side-buffer-name)))
	(when buffer
	  (delete-windows-on buffer)
	  (kill-buffer buffer)
	  (setq mum-layout--switch-side-show-status 'buffer))))

(defun mum-layout--buffer-list-filter (regexp &optional self?)
  "Find buffer list of REGEXP.
SELF is include curretn buffer."
  (seq-filter 'bufferp
			  (mapcar
			   (lambda (item)
				 (if (and (string-match regexp (buffer-name item)) (not (equal mum-layout--switch-side-buffer-name (buffer-name item))))
					 (if self?
						 item
					   (unless (equal item (current-buffer)) item))))
			   (buffer-list))))

(defun mum-layout--get-buffer-list (&optional tmp? self?)
  "Get buffer list.
TMP is asterisk buffers.
SELF is include curretn buffer."
  (if tmp?
	  (mum-layout--buffer-list-filter "^*" self?)
	(mum-layout--buffer-list-filter "^[^\s*]" self?)))

(defun mum-layout--switch-buffer-enable ()
  "Enable mode."
  )

(defun mum-layout--switch-buffer-disable ()
  "Disable mode."
  (mum-layout--kill-switch-side-buffer))

;;;###autoload
(define-minor-mode mum-layout--switch-buffer-mode
  "Mode."
  :keymap mum-layout--switch-buffer-keymap
  (if mum-layout--switch-buffer-mode
	  (mum-layout--switch-buffer-enable)
    (mum-layout--switch-buffer-disable)))

;;
;; mode
;;
(defun mum-layout--enable ()
  "Enable mode."
  (mum-layout-zoom-mode t))

(defun mum-layout--disable ()
  "Disable mode."
  (mum-layout-zoom-mode -1))

;;;###autoload
(define-minor-mode mum-layout-mode
  "Mum layout mode."
  :global t
  (if mum-layout-mode
	  (mum-layout--enable)
    (mum-layout--disable)))

(provide 'mum-layout)
;;; mum-layout.el ends here
