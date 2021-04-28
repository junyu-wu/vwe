;;; vwe-golang.el --- Golang Programming    -*- lexical-binding: t; -*-

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
;; go get golang.org/x/tools/cmd/goimports
;; go get golang.org/x/tools/cmd/godoc
;; go get golang.org/x/tools/cmd/guru
;; go get golang.org/x/tools/cmd/gorename
;; go get golang.org/x/lint/golint
;; go get github.com/rogpeppe/godef
;; go get github.com/nsf/gocode  ;; gocode set autobuild true
;; go get -u github.com/josharian/impl
;; go get -u github.com/davidrjenni/reftools/cmd/fillstruct
;; go get -u github.com/cweill/gotests/...
;; go get -u github.com/kisielk/errcheck
;; go get github.com/mdempsky/unconvert
;; go get golang.org/x/tools/gopls@latest  ;; go lsp

;;; Code:
;; ***************************************************************************
;; lib
;; ***************************************************************************

;; ***************************************************************************
;; config
;; ***************************************************************************

;;
;; `go-mode'
;;
(vwe@lib--package 'go-mode
				  (push '("\\.go\\'" . go-mode) auto-mode-alist)
				  (progn
					(define-key go-mode-map (kbd "M-.") #'godef-jump)
					(add-hook 'before-save-hook #'gofmt-before-save)
					(with-eval-after-load 'exec-path-from-shell
					  (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))

					;;
					;; `company-go'
					;;
					(vwe@lib--package 'company-go nil nil
					  				  (add-hook 'go-mode-hook (lambda() (set (make-local-variable 'company-backends) '(company-go)) (company-mode))))

					;;
					;; `go-eldoc'
					;;
					(vwe@lib--package 'go-eldoc (add-hook 'go-mode-hook #'go-eldoc-setup))

					;;
					;; `go-guru' go代码编辑扩展
					;;
					(vwe@lib--package 'go-guru (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)))
				  (setq go-command (concat (getenv "GOROOT") "/bin/go")
						gofmt-command (concat (getenv "GOPATH") "/bin/goimports")
						company-go-gocode-command (concat (getenv "GOPATH") "/bin/gocode")
						flycheck-go-gofmt-executable (concat (getenv "GOPATH") "/bin/goimports")
						flycheck-go-golint-executable (concat (getenv "GOPATH") "/bin/golint")
						flycheck-go-build-executable (concat (getenv "GOROOT") "/bin/go")
						flycheck-go-vet-executable (concat (getenv "GOROOT") "/bin/go")
						flycheck-go-test-executable (concat (getenv "GOROOT") "/bin/go")))

(provide 'vwe-golang)
;;; vwe-golang.el ends here
