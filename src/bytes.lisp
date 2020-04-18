(defpackage #:bytes
  (:use :cl)
  (:export #:nibble))

(in-package #:bytes)

(defun nibble (byte section)
  "Takes the first nibble of a byte if provide :hi, and the second nibble if :lo"
  (cond ((eql section :hi) (ash (logand 240 byte) -4))
        ((eql section :lo) (logand 15 byte))))
