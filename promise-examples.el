;;; promise-examples.el --- Examples using `promise.el'.  -*- lexical-binding: t; -*-

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

;; This file is examples using `promise.el'.
;;
;; To execute this, move the point after the last parenthesis of the following
;; Lisp code and press C-x C-e. (Launch the new Emacs and run (launcher))
;;
;; (start-process "emacs" nil (file-truename (expand-file-name invocation-name invocation-directory)) "-Q" "-L" default-directory "-l" "promise-examples" "--execute" "(launcher)")

;;; Code:

(require 'promise)
(require 'url-http)
(require 'xml)
(require 'dom)

(defun do-something ()
  "Return `Promise' to resolve the value synchronously."
  (promise-new (lambda (resolve _reject)
                 (let ((value 33))
                   (funcall resolve value)))))

(defun example1 ()
  "Resolved Promise keeps returning the same value."
  (let ((promise (do-something)))
    (promise-then promise
                  (lambda (value)
                    (message "Got a value: %s" value)))

    (promise-then promise
                  (lambda (value)
                    (message "Got the same value again: %s" value)))))

(defun example2 ()
  "Promise chain."
  (let ((promise (do-something)))
    (setf promise (promise-then promise
                                (lambda (result)
                                  (message "first result: %s" result)
                                  88)))

    (setf promise (promise-then promise
                                (lambda (second-result)
                                  (message "second result: %s" second-result)
                                  99)))

    (setf promise (promise-then promise
                                (lambda (third-result)
                                  (message "third result: %s" third-result))))))

(defun example3 ()
  "Same result as `example2'.
`promise-chain' macro is a syntax sugar for easy writing."
  (promise-chain (do-something)
    (then (lambda (result)
            (message "first result: %s" result)
            88))

    (then (lambda (second-result)
            (message "second result: %s" second-result)
            99))

    (then (lambda (third-result)
            (message "third result: %s" third-result)))))

(defun do-something-async (delay-sec value)
  "Return `Promise' to resolve the value asynchronously."
  (promise-new (lambda (resolve _reject)
                 (run-at-time delay-sec
                              nil
                              (lambda ()
                                (funcall resolve value))))))

(defun example4 ()
  "All processes are asynchronous Promise chain."
  (promise-chain (do-something-async 1 33)
    (then (lambda (result)
            (message "first result: %s" result)
            (do-something-async 1 (* result 2))))

    (then (lambda (second-result)
            (message "second result: %s" second-result)
            (do-something-async 1 (* second-result 2))))

    (then (lambda (third-result)
            (message "third result: %s" third-result)))))

(defvar a-dummy)

(defun example5 ()
  "Catch the error."
  (promise-chain (do-something-async 1 33)
    (then (lambda (result)
            (message "first result: %s" result)
            (setf a-dummy (/ 1 0)))) ; An `(arith-error)' occurs here.

    (then (lambda (second-result)
            (message "second result: %s" second-result)
            (do-something-async 1 (* second-result 2)))
          (lambda (reason)
            (message "catch the error: %s" reason)))))

(defun example6 ()
  "Same result as `example5'."
  (promise-chain (do-something-async 1 33)
    (then (lambda (result)
            (message "first result: %s" result)
            (setf a-dummy (/ 1 0)))) ; An `(arith-error)' occurs here.

    (then nil
          (lambda (reason)
            (message "catch the error: %s" reason)))))

(defun example7 ()
  "Same result as `example6'. `promise-catch' is a syntax sugar."
  (promise-chain (do-something-async 1 33)
    (then (lambda (result)
            (message "first result: %s" result)
            (setf a-dummy (/ 1 0)))) ; An `(arith-error)' occurs here.

    (promise-catch (lambda (reason)
                     (message "catch the error: %s" reason)))))

(defun example8 ()
  "How to use `promise-race'."
  (promise-chain (promise-race (vector (do-something-async 2 "2 seccods")
                                       (do-something-async 1 "1 second")
                                       (do-something-async 3 "3 secconds")))
    (then (lambda (result)
            (message "result: %s" result)))))

(defun timeout (time)
  "Return `Promise' which times out after the specified time."
  (promise-new (lambda (_resolve reject)
                 (run-at-time time
                              nil
                              (lambda ()
                                (funcall reject "time out"))))))

(defun example9 ()
  "How to time out using `promise-race'."
  (promise-chain (promise-race (vector (timeout 2)
                                       (do-something-async 3 "3 seconds")))
    (then (lambda (result)
            (message "result: %s" result)))

    (promise-catch (lambda (reason)
                     (message "promise-catch: %s" reason)))))

(defun example10 ()
  "How to use `promise-all'."
  (promise-chain (promise-all (vector (do-something-async 2 "2 seccods")
                                      (do-something-async 1 "1 second")
                                      (do-something-async 3 "3 secconds")))
    (then (lambda (results)
            (message "result[0]: %s" (aref results 0))
            (message "result[1]: %s" (aref results 1))
            (message "result[2]: %s" (aref results 2))))))

(defun do-randomthing-async ()
  "Return `Promise' to resolve the random value asynchronously."
  (promise-new (lambda (resolve _reject)
                 (run-at-time 1
                              nil
                              (lambda ()
                                (funcall resolve (random 100)))))))

(defun example11 ()
  "Branching to `resolve' or `reject' depending on the result."
  (promise-chain (do-randomthing-async)
    (then (lambda (result)
            (if (>= result 50)
                (promise-resolve (format "enough (%d >= 50)" result))
              (promise-reject (format "short (%d < 50)" result)))))

    (then (lambda (result)
            (message "result: %s" result)))

    (promise-catch (lambda (reason)
                     (message "promise-catch: %s" reason)))))

;;
;; Example using `url-retrieve'
;;

(defun xml-retrieve (url)
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

(defun get-text-first-tag (xml tag)
  "Returns the first text that matches TAG in XML."
  (decode-coding-string (dom-text (cl-first (dom-by-tag xml tag)))
                        'utf-8))

(defun get-short-text-first-tag (xml tag)
  "Abbreviate the text obtained by `get-text-first-tag'."
  (let* ((text (get-text-first-tag xml tag))
         (short-text (substring text 0 (min 64 (length text)))))
    (concat short-text " ...")))

(defun wait-seconds (seconds fn &rest args)
  "Return `Promise' to wait a specified number of seconds."
  (promise-new (lambda (resolve _reject)
                 (run-at-time seconds
                              nil
                              (lambda ()
                                (funcall resolve (apply fn args)))))))

(defun example12 ()
  "Example using `xml-retrieve'."
  (let ((wikipedia-url (concat "https://en.wikipedia.org/w/api.php"
                               "?format=xml&action=query&prop=extracts"
                               "&exintro=&explaintext=&titles=")))
    (promise-chain (promise-all
                    (vector
                     (xml-retrieve (concat wikipedia-url (url-encode-url "GNU")))
                     ;; Request after 2 seconds for load reduction.
                     (wait-seconds 2
                                   #'xml-retrieve
                                   (concat wikipedia-url (url-encode-url "Emacs")))))
      (then (lambda (xmls)
              (message "%s" (get-short-text-first-tag (aref xmls 0) 'extract))
              (message "%s" (get-short-text-first-tag (aref xmls 1) 'extract))))

      (promise-catch (lambda (reason)
                       (message "promise-catch: %s" reason))))))

;;
;; Asynchronous Processes
;;

(defun make-grep-process (&rest args)
  (promise-new
   (lambda (resolve reject)
     (make-process :name "grep"
                   :buffer "*grep-result*"
                   :command (cl-list* "grep" args)
                   :sentinel (lambda (_process event)
                               (if (string= event "finished\n")
                                   (with-current-buffer "*grep-result*"
                                     (funcall resolve (buffer-string)))
                                 (funcall reject event)))))))

(defun example13 ()
  (promise-chain (make-grep-process "make-process" "*.el")
    (then (lambda (result)
            (message "grep result:\n%s" result)))

    (promise-catch (lambda (reason)
                     (message "promise-catch: %s" reason)))))

(defun example14 ()
  (promise-chain (make-grep-process "string not in source" "promise.el")
    (then (lambda (result)
            (message "grep result:\n%s" result)))

    (promise-catch (lambda (reason)
                     (message "promise-catch: %s" reason)))))

;;
;; Thenable
;;
;; This `emacs-promise' makes `thenable' an OBJECT whose `promise-then' is defined.
;; OBJECT must be created with `defstruct' or `defclass'.
;;

(cl-defstruct thenable
  value)

(cl-defmethod promise-then ((this thenable) &optional resolve reject)
  "The signature of this method must be the same."
  (run-at-time 1 nil (lambda ()
                       (if (thenable-value this)
                           (funcall resolve (concat "[" (upcase (thenable-value this)) "]"))
                         (funcall reject "failed: thenable")))))

(defun example15 ()
  "Thenable must be passed to `promise-resolve'."
  (promise-chain (promise-resolve (make-thenable :value "This is `thenable'"))
    (then (lambda (result)
            (message "result: %s" result)))

    (promise-catch (lambda (reason)
                     (message "promise-catch: %s" reason)))))

;;
;; Inheritance of Promise
;;

(defclass simple-logger (promise-class)
  ((call-count :accessor call-count :initform 0))
  :documentation "Record the number of times `promise-then' was called.")

(cl-defmethod promise-then ((this simple-logger) &optional on-fulfilled on-rejected)
  (let ((new-promise   ; `promise-then' always returns a new promise."
         (cl-call-next-method this
                              (lambda (result)
                                (message "%d: result: %s"
                                         (1+ (call-count this))
                                         result)
                                (funcall on-fulfilled result))
                              on-rejected)))
    (setf (call-count new-promise) (1+ (call-count this)))
    new-promise))

(defun example16 ()
  (promise-chain (make-instance 'simple-logger
                                :fn (lambda (resolve _reject)
                                      (let ((value 33))
                                        (funcall resolve value))))
    (then (lambda (result)
            (* result 2)))

    (then (lambda (second-result)
            (setf a-dummy (/ 1 0))   ; An `(arith-error)' occurs here.
            (* second-result 2)))

    (then (lambda (third-result)
            ;; Do not reach
            (message "third result: %s" third-result)))

    ;; In the `promise-chain', the `promise' variable is defined.
    (message "* type-of promise: %s" (promise--type-of promise))
    (message "* `promise-then' total call count: %d" (call-count promise))))

;;
;; Unhandled Rejections
;;

(defun example17 ()
  "An example where Promise swallows an error."
  (promise-chain (do-something-async 1 33)
    (then (lambda (result)
            (message "first result: %s" result)
            (setf a-dummy (/ 1 0)))) ; An `(arith-error)' occurs here.

    ;; Oops! I forgot to capture the error!
    ;; Nothing is displayed except for the first result.
    (then (lambda (second-result)
            (message "second result: %s" second-result)))))

(require 'promise-rejection-tracking)

(defun example18 ()
  "Example of `rejection-tracking'."

  ;; Enable `rejection-tracking'.
  ;; The option should always specify (all-rejections . t).
  (promise-rejection-tracking-enable '((all-rejections . t)))
  ;; Since this has a penalty of execution speed,
  ;; it should be effective only during development.

  (promise-chain (do-something-async 1 33)
    (then (lambda (result)
            (message "first result: %s" result)
            (setf a-dummy (/ 1 0)))) ; An `(arith-error)' occurs here.

    ;; if rejection-tracking is enabled,
    ;; an error will be displayed in a few seconds!
    (then (lambda (second-result)
            (message "second result: %s" second-result)))))

;;
;; Launcher
;;

(defun launcher ()
  "A launcher that runs each example."
  (require 'ido)
  (switch-to-buffer "*Messages*")

  (let (nums)
    (mapatoms
     (lambda (x)
       (when (fboundp x)
         (let ((name (symbol-name x)))
           (when (string-match "^example\\([0-9]+\\)$" name)
             (push (match-string 1 name) nums))))))
    (cl-callf cl-sort nums #'< :key #'string-to-number)
    (cl-loop
     (let* ((num (ido-completing-read "What number of examples do you run?: example"
                                      nums))
            (example (intern (concat "example" num))))
       (message "***** example%s *****" num)
       (funcall example)
       (sit-for 1)))))

;;; promise-examples.el ends here
