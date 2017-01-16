;;; promise-finally.el --- Porting finally.js  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  chuntaro

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

;;; Commentary:

;; This implementation ported the following implementation faithfully.
;; https://github.com/then/promise/blob/master/src/finally.js

;;; Code:

(require 'promise-es6-extensions)

(cl-defmethod promise-finally ((this promise-class) f)
  (promise-then this
                (lambda (value)
                  (promise-then (promise-resolve (funcall f))
                                (lambda (_) value)))
                (lambda (err)
                  (promise-then (promise-resolve (funcall f))
                                (lambda (_) err)))))

(provide 'promise-finally)
;;; promise-finally.el ends here
