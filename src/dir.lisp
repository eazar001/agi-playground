(defpackage #:dir
  (:use :cl #:file))

(in-package #:dir)

;;; Extraction for critical data in DIR (directory) files

;;; Reads three hex-encoded bytes from a VIEWDIR file in order to extract info about positioning and offsets in
;;; the corresponding VOL file(s); this reader format is for AGI version 2
(defun read-view-byte-triplet (byte-strings)
  ;; we only expect three bytes at a time here encoded as hex strings
  (let* ((chars (apply #'concatenate 'list byte-strings))
         (non-vol-chars (cdr chars))
         (vol (car (multiple-value-list (parse-integer (string (car chars)) :radix 16))))
         (offset (car (multiple-value-list (parse-integer (concatenate 'string non-vol-chars) :radix 16)))))
    ;; data structure contains the number of the volume that contains this data and the offset in that volume
    (list vol offset)))
