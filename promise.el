;;; promise.el --- This is a simple implementation of Promises/A+.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  chuntaro

;; Author: chuntaro <chuntaro@sakura-games.jp>
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

;;; Commentary:

;; This implementation ported the following `Promise' implementation faithfully.
;; https://github.com/then/promise

;;; Code:

(require 'promise-es6-extensions)
(require 'promise-done)
(require 'promise-finally)

(defmacro promise-chain (&rest body)
  "Extract the following code...

    (promise-chain (promise-new ...)
      (then (lambda (value) ...))

      (promise-catch (lambda (reason) ...))

      (done (lambda (value) ...)))

as below.

    (let ((promise (promise-new ...)))
      (setf promise (promise-then promise (lambda (value) ...)))

      (setf promise (promise-catch promise (lambda (value) ...)))

      (setf promise (promise-done promise (lambda (reason) ...))))"
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
