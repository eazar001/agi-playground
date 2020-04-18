(defpackage #:bytes
  (:use :cl)
  (:export #:nibble))

(in-package #:bytes)

;;; Provides utilities for working with and reading data at the byte, nibble, or bit-level

(defun nibble (byte section &key (endian :big))
  "Takes the first nibble of a byte if provide :hi, and the second nibble if :lo; default is big-endian."
  (let ((section-input (if (eql endian :little)
                           (cond ((eql section :hi) :lo)
                                 ((eql section :lo) :hi))
                           section)))

    (cond ((eql section-input :hi) (ash (logand 240 byte) -4))
          ((eql section-input :lo) (logand 15 byte)))))
