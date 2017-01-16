;;; promise-done.el --- Porting done.js  -*- lexical-binding: t; -*-

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

(cl-defmethod promise-done ((this promise-class) &optional on-fulfilled on-rejected)
  (let ((self (if (or on-fulfilled on-rejected)
                  (promise-then this on-fulfilled on-rejected)
                this)))
    (promise-then self nil (lambda (err)
                             (run-at-time 0 nil
                                          (lambda ()
                                            (if (listp err)
                                                (signal (car err) (cdr err))
                                              (error err)))))))
  nil)

(provide 'promise-done)
;;; promise-done.el ends here
