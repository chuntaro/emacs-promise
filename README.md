Promises/A+ for Emacs
=====================

This is a simple implementation of [Promises/A+](https://promisesaplus.com/).  

This implementation ported the following Promises/A+ implementation faithfully.  
https://github.com/then/promise

* The same API as JavaScript version Promise can be used.
 * then, catch, resolve, reject, all, race, etc...
* supports "thenable"
* supports "Inheritance of Promise"
* supports "rejection-tracking"

For detailed tutorials on its use, see [www.promisejs.org](http://www.promisejs.org/) (JavaScript).

*Promises/A+ for Emacs* is used in [Async/Await for Emacs](https://github.com/chuntaro/emacs-async-await), so you can use Async/Await for asynchronous programming.

Installation
------------

You can install from MELPA using package.el.  
The package name is **promise**.

Usage
-----

See [promise-examples.el](https://github.com/chuntaro/emacs-promise/blob/master/examples/promise-examples.el) for details.


```emacs-lisp
(require 'promise)

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
```

An example using `url-retrieve 'as a more complicated example.

```emacs-lisp
(require 'promise)
(require 'url-http)
(require 'xml)
(require 'dom)

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
  "Truncate the text obtained with `get-text-first-tag'."
  (concat (truncate-string-to-width (get-text-first-tag xml tag) 64)
          " ..."))

(defun wait-seconds (seconds fn &rest args)
  "Return `Promise' to execute the function after the specified time."
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

Tests
-----

```
$ cask install
$ cask exec ert-runner
```
