(defpackage #:file
  (:use :cl)
  (:export #:read-file-bytes-to-list #:read-bytes-to-list))

(in-package #:file)

;;; Utilities for reading file data and processing them at the byte-level

(defun read-file-bytes-to-list (file-path)
  (with-open-file (stream file-path :direction :input :element-type '(unsigned-byte 8))
    (read-bytes-to-list stream)))

(defun read-bytes-to-list (stream)
  (let ((b (read-byte stream nil)))
    (if b
        (cons b (read-bytes-to-list stream))
        nil)))
