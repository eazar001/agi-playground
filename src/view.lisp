(defpackage #:view
  (:use :cl #:file :bytes))

(in-package #:view)

;; Extraction of VIEW resources

(defun read-view-header (bytes)
  (destructuring-bind (u0 u1 num-loops desc1 desc2 floop1 floop2 sloop1 sloop2 tloop1 tloop2) bytes
    nil))

(defun read-loop-header (bytes)
  ;; byte 0 should contain the number of cells in the loop
  ;; fcel1 and fcel2 contain the position of the first cell in bytes 1 and 2
  ;; scel1 and scel2 contain the position (if any) of the second cell in bytes 3 and 4
  ;; tcel1 and tcel2 contain the position (if any) of the third cell in bytes 5 and 6
  (destructuring-bind (num-cells fcel1 fcel2 scel1 scel2 tcel1 tcel2) bytes
    nil))

(defun read-cel-header (bytes)
  (destructuring-bind (cel-width cel-height cel-mirroring) bytes
    nil))

;;; reads one byte of pixel-data
(defun read-pixel-data-byte (byte)
  (let ((color (ash (logand byte 240) -4))
        (num-pixels (logand byte 15)))
    (list color num-pixels)))

(defun read-cel-data (byte)
  "Reads a list of bytes and transforming them into a list of pairs containing pixel RLE data."
  (let ((color (nibble byte :hi))
        (num-pixels (nibble byte :lo)))
    (list color num-pixels)))
