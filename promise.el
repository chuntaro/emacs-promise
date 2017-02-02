;;; promise.el --- Promises/A+ for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017  chuntaro

;; Author: chuntaro <chuntaro@sakura-games.jp>
;; URL: https://github.com/chuntaro/emacs-promise
;; Package-Requires: ((emacs "25"))
;; Version: 1.0
;; Keywords: convenience

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
;;
;; (require 'promise)
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
                      (list 'setf 'promise sexp))
                     ((promise-then
                       promise-catch
                       promise-done
                       promise-finally)
                      (list 'setf 'promise (cl-list* fn 'promise args)))
                     (then
                      (list 'setf 'promise
                            (cl-list* 'promise-then 'promise args)))
                     (done
                      (list 'setf 'promise
                            (cl-list* 'promise-done 'promise args)))
                     (finally
                      (list 'setf 'promise
                            (cl-list* 'promise-finally 'promise args)))
                     (otherwise
                      sexp))))
               (cdr body))))

(provide 'promise)
;;; promise.el ends here
