;;; -*- lexical-binding: t -*-

(require 'f)
(require 'ert-async)

(defvar root-test-path
  (f-dirname (f-this-file)))

(defvar root-code-path
  (f-parent root-test-path))

(add-to-list 'load-path root-code-path)

(require 'promise)
