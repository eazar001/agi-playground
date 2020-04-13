(defpackage #:dir
  (:use :cl #:file)
  (:export #:read-view-dir))

(in-package #:dir)

;;; Extraction for critical data in DIR (directory) files

(defun read-view-dir (file)
  (let ((bytes (partition-triplets (read-file-bytes-to-list file))))
    (remove-if (lambda (x) (equal x (list 15 1048575)))
               (mapcar #'read-view-byte-triplet bytes))))

(defun partition-triplets (bytes)
  (if bytes
      (destructuring-bind (a b c . tail) bytes
        (cons (list a b c) (partition-triplets tail)))
      nil))

;;; Reads three hex-encoded bytes from a VIEWDIR file in order to extract info about positioning and offsets in
;;; the corresponding VOL file(s); this reader format is for AGI version 2
(defun read-view-byte-triplet (bytes)
  ;; we only expect three bytes at a time here encoded as hex strings
  (let* ((first-byte (car bytes))
         (second-byte (cadr bytes))
         (third-byte (caddr bytes))
         (vol (ash (logand first-byte 240) -4))
         (nibble (logand first-byte 15))
         (offset-tail (logior (ash second-byte 8) third-byte))
         (offset (logior (ash nibble 16) offset-tail)))
    ;; data structure contains the number of the volume that contains this data and the offset in that volume
    (list vol offset)))
