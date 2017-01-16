Promises/A+ for Emacs
=====================

This is a simple implementation of Promises/A+.  

This implementation ported the following Promises/A+ implementation faithfully.  
https://github.com/then/promise

* The same API as JavaScript version Promise can be used.
 * then, catch, resolve, reject, all, race, etc...
* supports "thenable"
* supports "Inheritance of Promise"
* supports "rejection-tracking"

Usage
-----

See ![promise-examples.el](https://github.com/chuntaro/emacs-promise/blob/master/promise-examples.el) for details.


```emacs-lisp
(require 'promise)
(require 'url-http)
(require 'xml)
(require 'dom)

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
```
