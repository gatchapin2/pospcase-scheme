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

(add-to-list 'pospcase-defstruct-group 'scheme-define)
(add-to-list 'pospcase--elispify-alist '("#[ft]" . " t"))

(defun pospcase-font-lock-scheme-setup ()
  "Enable `pospcase' code highlighting for `scheme-mode'."
  (interactive)
  (pospcase-font-lock 'scheme-mode
                      '(`(define (,name . ,args) . ,_))
                      '((heading-keyword . (font-lock-keyword-face))
                        (name . (font-lock-function-name-face))
                        ((args . scheme-define) . (font-lock-variable-name-face))))
  (pospcase-font-lock 'scheme-mode
                      '(`(let ,binds . ,_)
                        `(let* ,binds . ,_)
                        `(letrec ,binds . ,_)
                        `(letrec* ,binds . ,_))
                      '((heading-keyword . (font-lock-keyword-face))
                        ((binds . list/1) . (font-lock-variable-name-face))))
  (add-hook 'scheme-mode-hook #'pospcase-font-lock-scheme-keywords-add))

(provide 'pospcase-scheme)
