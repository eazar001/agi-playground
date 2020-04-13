(defpackage #:decryption
  (:use :cl #:file)
  (:export #:decrypt-object-file))

(in-package #:decryption)

;;; Encryption/Decryption routines for AGI resources

;;; The first 3 bytes is a header with bytes 0 - 1 being the offset of the inventory area (+ base of 3).
;;; The third byte, or byte 2, specifies the maximum number of animated objects (unsure what this does).
;;; Every successive sequence of 3 bytes refers to the offset of string i (where i starts at 0), using
;;; the first two bytes (+ 3 like the header), and the third byte referring to the starting room of the
;;; current inventory object pointed to.
;;;
;;; NB: the first two bytes in every 3 byte sequence alway refers to an offset in LITTLE-ENDIAN order!!!

;;; This is primarily used for the OBJECT resource file that ships with games
(defun decrypt-object-file (file)
  "Decrypt an object file with a given file path."
  (decrypt-file "Avis Durgan" file))

(defun decrypt-file (key-string file)
  "Decrypt a file with a given keystring to XOR the bytes with."
  (let ((key-bytes (key-string-to-bytes key-string))
        (source-bytes (read-file-bytes-to-list file)))
    (xor-decrypt key-bytes source-bytes)))

;;; simple xor decryption algorithm with given decryption key-bytes and encrypted source-file bytes
;;; bytes are encoded as a list of unsigned bytes
(defun xor-decrypt (key-bytes source-bytes)
  (let* ((source-len (list-length source-bytes))
         (key-len (list-length key-bytes))
         (c (multiple-value-list (floor (/ source-len key-len))))
         (key-cycles (+ (car c) (numerator (cadr c))))
         (key-cycle-bytes (apply #'concatenate 'list (loop for i from 1 to key-cycles
                                                        collect (loop for b in key-bytes collect b)))))
    (mapcar #'logxor key-cycle-bytes source-bytes)))

(defun key-string-to-bytes (key-string)
  (loop for c in (concatenate 'list key-string) collect (char-code c)))

;;; this should only really be used for debugging with a hex editor
;;; simply decrypt the object file as with decrypt-object-file but instead send the bytes
;;; to a new output file
(defun decrypt-object-to-file (file-path new-file-path)
  (with-open-file (stream new-file-path :direction :output :element-type '(unsigned-byte 8))
    (dolist (b (loop for b in (decrypt-object-file file-path) collect b))
      (write-byte b stream))))
