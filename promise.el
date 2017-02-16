;;; promise.el --- Promises/A+                       -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017  chuntaro

;; Author: chuntaro <chuntaro@sakura-games.jp>
;; URL: https://github.com/chuntaro/emacs-promise
;; Package-Requires: ((emacs "25") (async "1.9"))
;; Version: 1.1
;; Keywords: async promise convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; The original JavaScript code is:
;;
;; Copyright (c) 2014 Forbes Lindesay
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Commentary:

;; This is a simple implementation of Promises/A+.
;;
;; This implementation ported the following Promises/A+ implementation faithfully.
;; https://github.com/then/promise
;;
;; * The same API as JavaScript version Promise can be used.
;;  * then, catch, resolve, reject, all, race, etc...
;; * supports "thenable"
;; * supports "Inheritance of Promise"
;; * supports "rejection-tracking"
;;
;; Usage:
;; See `promise-examples.el' for details.
;;  https://raw.githubusercontent.com/chuntaro/emacs-promise/master/examples/promise-examples.el
;;  You can check the operation while downloading and running it interactively.
;;
;; (require 'promise)
;;
;; ;; Please be sure to enable it when developing.
;; (promise-rejection-tracking-enable '((all-rejections . t)))
;;
;; (defun do-something-async (delay-sec value)
;;   "Return `Promise' to resolve the value asynchronously."
;;   (promise-new (lambda (resolve _reject)
;;                  (run-at-time delay-sec
;;                               nil
;;                               (lambda ()
;;                                 (funcall resolve value))))))
;;
;; (defun example4 ()
;;   "All processes are asynchronous Promise chain."
;;   (promise-chain (do-something-async 1 33)
;;     (then (lambda (result)
;;             (message "first result: %s" result)
;;             (do-something-async 1 (* result 2))))
;;
;;     (then (lambda (second-result)
;;             (message "second result: %s" second-result)
;;             (do-something-async 1 (* second-result 2))))
;;
;;     (then (lambda (third-result)
;;             (message "third result: %s" third-result)))))

;;; Code:

(require 'promise-es6-extensions)
(require 'promise-done)
(require 'promise-finally)
(require 'promise-rejection-tracking)

;;;###autoload
(defmacro promise-chain (&rest body)
  "Extract the following code...

    (promise-chain (promise-new ...)
      (then
       (lambda (value)
         ...))

      (promise-catch
       (lambda (reason)
         ...))

      (done
       (lambda (value)
         ...)))

as below.

    (let ((promise (promise-new ...)))
      (setf promise (promise-then promise
                                  (lambda (value)
                                    ...)))

      (setf promise (promise-catch promise
                                   (lambda (value)
                                     ...)))

      (setf promise (promise-done promise
                                  (lambda (reason)
                                    ...))))"
  (declare (indent 1) (debug t))
  `(let ((promise ,(car body)))
     ,@(mapcar (lambda (sexp)
                 (let ((fn (car sexp))
                       (args (cdr sexp)))
                   (cl-case fn
                     (promise-new
                      `(setf promise ,@sexp))
                     ((promise-then
                       promise-catch
                       promise-done
                       promise-finally)
                      `(setf promise (,fn promise ,@args)))
                     (then
                      `(setf promise (promise-then promise ,@args)))
                     (done
                      `(setf promise (promise-done promise ,@args)))
                     (finally
                      `(setf promise (promise-finally promise ,@args)))
                     (otherwise
                      sexp))))
               (cdr body))))

;;
;; Promise version of various utility functions
;;

(require 'url-http)

(defun promise:run-at-time (time function &rest args)
  "Return `Promise' to perform action at specified time."
  (promise-new
   (lambda (resolve _reject)
     (run-at-time time nil
                  (lambda ()
                    (funcall resolve (apply function args)))))))

(defun promise:delay (time &optional value)
  "Return `Promise' to delay specified time."
  (promise-new
   (lambda (resolve _reject)
     (run-at-time time
                  nil
                  (lambda ()
                    (funcall resolve value))))))

(defun promise:time-out (time &optional reason)
  "Return `Promise' to reject after specified time."
  (promise-new
   (lambda (_resolve reject)
     (run-at-time time nil
                  (lambda ()
                    (funcall reject reason))))))

(defun promise:make-process (program &rest args)
  "Generate an asynchronous process and
return Promise to resolve in that process."
  (promise-new
   (lambda (resolve reject)
     (let ((buffer (generate-new-buffer (concat " *" program "*"))))
       (make-process :name program
                     :buffer buffer
                     :command (cons program args)
                     :sentinel (lambda (process event)
                                 (if (string= event "finished\n")
                                     (funcall resolve process)
                                   (funcall reject event))))))))

(defun promise:make-process-string (program &rest args)
  "Generate an asynchronous process and
return Promise to resolve with the output result."
  (promise-then (apply #'promise:make-process program args)
                (lambda (process)
                  (with-current-buffer (process-buffer process)
                    (buffer-string)))))

(defun promise:url-retrieve (url)
  "Return `Promise' to resolve with response body of HTTP request."
  (promise-new
   (lambda (resolve reject)
     (url-retrieve url
                   (lambda (status)
                     ;; All errors are reliably captured and rejected with appropriate values.
                     (if (plist-get status :error)
                         (funcall reject (plist-get status :error))
                       (condition-case ex
                           (with-current-buffer (current-buffer)
                             (if (not (url-http-parse-headers))
                                 (funcall reject (buffer-string))
                               (search-forward-regexp "\n\\s-*\n" nil t)
                               (funcall resolve (buffer-substring (point) (point-max)))))
                         (error (funcall reject ex)))))))))

(require 'xml)                          ; for `xml-parse-region'

(defun promise:xml-retrieve (url)
  "Return `Promise' to resolve with XML object obtained by HTTP request."
  (promise-new
   (lambda (resolve reject)
     (url-retrieve url
                   (lambda (status)
                     ;; All errors are reliably captured and rejected with appropriate values.
                     (if (plist-get status :error)
                         (funcall reject (plist-get status :error))
                       (condition-case ex
                           (with-current-buffer (current-buffer)
                             (if (not (url-http-parse-headers))
                                 (funcall reject (buffer-string))
                               (search-forward-regexp "\n\\s-*\n" nil t)
                               (funcall resolve (xml-parse-region))))
                         (error (funcall reject ex)))))))))

(declare-function async-start "async.el" (start-func &optional finish-func))
(declare-function async-when-done "async.el" (proc &optional _change))

(defun promise:async-start (start-func &optional finish-func)
  "Return Promise to resolve with the `async-start' return value."
  (promise-new
   (lambda (resolve reject)
     (set-process-sentinel (async-start start-func
                                        (lambda (result)
                                          (when finish-func
                                            (funcall finish-func result))
                                          (funcall resolve result)))
                           (lambda (process event)
                             (condition-case reason
                                 (async-when-done process event)
                               (error (funcall reject reason))))))))

(provide 'promise)
;;; promise.el ends here
