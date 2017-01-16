;;; promise-es6-extensions.el --- Porting es6-extensions.js  -*- lexical-binding: t; -*-

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

;; This implementation ported the following implementation faithfully.
;; https://github.com/then/promise/blob/master/src/es6-extensions.js
;;
;; This file contains the ES6 extensions to the core Promises/A+ API.
;; (promise-resolve value)
;; (promise-all [values...])
;; (promise-reject value)
;; (promise-race [values...])
;; (promise-catch ((this promise-class) on-rejected))

;;; Code:

(require 'promise-core)

(defsubst promise--then-function ()
  (ignore-errors
    (cl--generic-method-function (cl-find-method #'promise-then
                                                 '()
                                                 '(promise-class t t)))))

;; Static Functions

(defun promise--value (value)
  (let ((p (promise-new #'ignore)))
    (setf (_state p) 1
          (_value p) value)
    p))

(defconst promise--t (promise--value t))
(defconst promise--nil (promise--value nil))
(defconst promise--zero (promise--value 0))
(defconst promise--emptystring (promise--value ""))

(defun promise-resolve (value)
  (cond
   ((promise-class-p value) value)

   ((eq value t) promise--t)
   ((eq value nil) promise--nil)
   ((eq value 0) promise--zero)
   ((eq value "") promise--emptystring)

   ((promise--is-object value)
    (condition-case ex
        (let ((then (ignore-errors (promise--find-then-function value))))
          (if (functionp then)
              (promise-new (lambda (resolve reject)
                             (promise-then value resolve reject)))
            (promise--value value)))
      (error (promise-new (lambda (_resolve reject)
                            (funcall reject ex))))))
   (t
    (promise--value value))))

(defun promise-all (arr)
  (let ((args (cl-coerce arr 'vector)))

    (promise-new
     (lambda (resolve reject)
       (if (zerop (length args))
           (funcall resolve [])
         (let ((remaining (length args)))
           (cl-labels
               ((res (i val)
                     (cl-block nil
                       (when (and val (promise--is-object val))
                         (cond
                          ((and (promise-class-p val)
                                (eq (promise--find-then-function val)
                                    (promise--then-function)))
                           (while (= (_state val) 3)
                             (setf val (_value val)))
                           (when (= (_state val) 1)
                             (cl-return (res i (_value val))))
                           (when (= (_state val) 2)
                             (funcall reject (_value val)))
                           (promise-then val
                                         (lambda (val)
                                           (res i val))
                                         reject)
                           (cl-return))
                          (t
                           (let ((then (ignore-errors
                                         (promise--find-then-function val))))
                             (when (functionp then)
                               (let ((p (promise-new
                                         (lambda (resolve reject)
                                           (promise-then val
                                                         resolve
                                                         reject)))))
                                 (promise-then p
                                               (lambda (val)
                                                 (res i val))
                                               reject)
                                 (cl-return)))))))
                       (setf (aref args i) val)
                       (when (zerop (cl-decf remaining))
                         (funcall resolve args)))))
             (cl-loop for i from 0
                      for arg across args
                      do (res i arg)))))))))

(defun promise-reject (value)
  (promise-new (lambda (_resolve reject)
                 (funcall reject value))))

(defun promise-race (values)
  (promise-new (lambda (resolve reject)
                 (cl-loop for value across (cl-coerce values 'vector)
                          do (promise-then (promise-resolve value)
                                           resolve
                                           reject)))))

(cl-defmethod promise-catch ((this promise-class) on-rejected)
  (promise-then this nil on-rejected))

(provide 'promise-es6-extensions)
;;; promise-es6-extensions.el ends here
