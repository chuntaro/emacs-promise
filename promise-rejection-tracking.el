;;; promise-rejection-tracking.el --- Porting rejection-tracking.js  -*- lexical-binding: t; -*-

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

(require 'promise-core)

(defun promise--warn (message &rest args)
  (display-warning 'promise (apply #'format-message message args)))

(defvar promise--default-whitelist '(void-function
                                     void-variable
                                     wrong-type-argument
                                     args-out-of-range))

(defvar promise--enabled nil)

(defun promise-rejection-tracking-disable ()
  (setf promise--enabled nil
        promise--on-handle nil
        promise--on-reject nil))

(defun promise-rejection-tracking-enable (&optional options)
  (when promise--enabled (promise-rejection-tracking-disable))
  (setf promise--enabled t)
  (let ((id -1)
        (display-id -1)
        (rejections (make-hash-table)))
    (cl-flet*
        ((rejections (id symbol) (alist-get symbol (gethash id rejections)))
         (rejections-set (id symbol value)
                         (setf (alist-get symbol (gethash id rejections))
                               value))
         (options (sym) (alist-get sym options))

         (on-unhandled (id)
                       (when (or (options 'all-rejections)
                                 (promise--match-whitelist
                                  (rejections id 'error)
                                  (or (options 'whitelist)
                                      promise--default-whitelist)))
                         (rejections-set id 'display-id (cl-incf display-id))
                         (cond
                          ((options 'on-unhandled)
                           (rejections-set id 'logged t)
                           (funcall (options 'on-unhandled)
                                    (rejections id 'display-id)
                                    (rejections id 'error)))
                          (t
                           (rejections-set id 'logged t)
                           (promise--log-error (rejections id 'display-id)
                                               (rejections id 'error))))))
         (on-handled (id)
                     (when (rejections id 'logged)
                       (cond
                        ((options 'on-handled)
                         (funcall (options 'on-handled)
                                  (rejections id 'display-id)
                                  (rejections id 'error)))
                        ((not (rejections id 'on-unhandled))
                         (promise--warn "Promise Rejection Handled (id:%d):"
                                        (rejections id 'display-id))
                         (promise--warn "  This means you can ignore any previous messages of the form \"Possible Unhandled Promise Rejection\" with id %d."
                                        (rejections id 'display-id)))))))
      (setf promise--on-handle
            (lambda (promise)
              (when (and (= (_state promise) 2) ; IS REJECTED
                         (gethash (_rejection-id promise) rejections))
                (if (rejections (_rejection-id promise) 'logged)
                    (on-handled (_rejection-id promise))
                  (cancel-timer (rejections (_rejection-id promise) 'timeout)))
                (remhash (_rejection-id promise) rejections)))
            promise--on-reject
            (lambda (promise err)
              (when (zerop (_deferred-state promise)) ; not yet handled
                (setf (_rejection-id promise) (cl-incf id))
                (puthash (_rejection-id promise)
                         `((display-id . nil)
                           (error . ,err)
                           (timeout . ,(run-at-time
                                        (if (promise--match-whitelist
                                             err promise--default-whitelist)
                                            0.1
                                          2)
                                        nil
                                        (lambda ()
                                          (on-unhandled (_rejection-id promise)))))
                           (logged . nil))
                         rejections)))))))

(defun promise--log-error (id error)
  (promise--warn "Possible Unhandled Promise Rejection (id:%d):" id)
  (display-warning 'promise (prin1-to-string error)))

(defun promise--match-whitelist (error list)
  (cl-some (lambda (cls)
             (eq (or (and (consp error)
                          (car error))
                     error)
                 cls))
           list))

(provide 'promise-rejection-tracking)
;;; promise-rejection-tracking.el ends here
