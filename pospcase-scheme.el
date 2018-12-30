;;; pospcase-scheme.el: Pospcase extension for Scheme -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 gatchapin

;; Author: gatchapin
;; Keywords: languages, faces
;; URL: https://github.com/gatchapin2/pospcase-scheme

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'pospcase-font-lock)

(defun pospcase-match-scheme-define (limit)
  "Matcher iterator for a list of symbols, but last `cdr' can be
a symbol too."
  (pospcase--call-iterator
   (cl-loop with result
            for exp on (if pospcase--fence-start
                           (cl-loop for temp on (car (pospcase-read (point)))
                                    if (equal temp pospcase--fence-start)
                                    return (list temp)
                                    if (equal (car temp) pospcase--fence-start)
                                    return temp)
                           (car (pospcase-read (point))))
            if (and (numberp (car (cdr exp)))
                    (numberp (cdr (cdr exp))))
            return (append result
                           (if (symbolp (car exp))
                               (list (pospcase--list (cdr exp)))))
            if (symbolp (caar exp))
            collect (pospcase--list (cdr (car exp))) into result
            finally return result)
   limit))

(defalias #'pospcase-match-scheme-lambda #'pospcase-match-scheme-define)

(unless (memq 'scheme-define pospcase-defstruct-group)
  (add-to-list 'pospcase-defstruct-group 'scheme-define))

(unless (memq 'scheme-lambda pospcase-list-group)
  (add-to-list 'pospcase-list-group 'scheme-lambda))

(let ((bools '("#[ft]\\_>" . " t")))
  (unless (member bools pospcase--elispify-alist)
    (add-to-list 'pospcase--elispify-alist bools)))

(defun pospcase-font-lock-scheme-setup ()
  "Enable `pospcase' code highlighting for `scheme-mode'."
  (interactive)
  (pospcase-font-lock 'scheme-mode
                      '(`(define (,name . ,args) . ,_))
                      '((heading-keyword . (font-lock-keyword-face))
                        (name . (font-lock-function-name-face))
                        ((args . scheme-define) . (font-lock-variable-name-face))))
  (pospcase-font-lock 'scheme-mode
                      '(`(lambda ,(and (pred consp) args) . ,_))
                      '((heading-keyword . (font-lock-keyword-face))
                        ((args . scheme-lambda) . (font-lock-variable-name-face))))
  (pospcase-font-lock 'scheme-mode
                      '(`(lambda ,(and (pred symbolp) arg) . ,_))
                      '((heading-keyword . (font-lock-keyword-face))
                        (arg . (font-lock-variable-name-face))))
  (pospcase-font-lock 'scheme-mode
                      '(`(let ,binds . ,_)
                        `(let* ,binds . ,_)
                        `(letrec ,binds . ,_)
                        `(letrec* ,binds . ,_))
                      '((heading-keyword . (font-lock-keyword-face))
                        ((binds . list/1) . (font-lock-variable-name-face))))
  (pospcase-font-lock 'scheme-mode
                      '(`(let ,(and (pred symbolp) name) ,binds . ,_))
                      '((heading-keyword . (font-lock-keyword-face))
                        (name . (font-lock-function-name-face))
                        ((binds . list/1) . (font-lock-variable-name-face))))
  (add-hook 'scheme-mode-hook #'pospcase-font-lock-scheme-keywords-add))

(provide 'pospcase-scheme)
