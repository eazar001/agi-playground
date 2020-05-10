(defpackage #:view
  (:use :cl #:sdl2 #:file :bytes)
  (:export #:read-cel-data))

(in-package #:view)

(defvar test-image (list #x4F #x4A #x00 #x41 #x0F #x08 #x41 #x00 #x41 #x0F #x08 #x41 #x00 #x41 #x0F #x08 #x41 #x00 #x41 #x0F #x08 #x41 #x00 #x41 #x0F #x08 #x41 #x00 #x41 #x0F #x08 #x41 #x00 #x41 #x0F #x08 #x41 #x00 #x41 #x08 #x81 #x72 #x81 #x72 #x09 #x41 #x00 #x41 #x08 #x81 #x72 #x81 #x72 #x09 #x41 #x00 #x41 #x08 #x81 #x71 #x82 #x71 #x0A #x41 #x00 #x41 #x08 #x81 #x72 #x81 #x72 #x09 #x41 #x00 #x41 #x09 #x81 #x71 #x01 #x81 #x71 #x09 #x41 #x00 #x41 #x08 #x81 #x72 #x81 #x72 #x09 #x41 #x00 #x41 #x08 #x81 #x71 #x82 #x71 #x0A #x41 #x00 #x41 #x08 #x81 #x72 #x81 #x72 #x09 #x41 #x00 #x41 #x09 #x81 #x71 #x81 #x72 #x09 #x41 #x00 #x41 #x08 #x81 #x72 #x81 #x72 #x09 #x41 #x00 #x41 #x07 #x82 #x75 #x09 #x41 #x00 #x41 #x06 #x82 #x77 #x08 #x41 #x00 #x41 #x05 #x82 #x73 #x83 #x73 #x07 #x41 #x00 #x41 #x04 #x82 #x73 #x03 #x82 #x73 #x06 #x41 #x00 #x41 #x03 #x82 #xF3 #x05 #x82 #xF3 #x05 #x41 #x00 #x41 #x03 #x81 #x73 #x07 #x82 #x73 #x04 #x41 #x00 #x41 #x03 #x81 #x73 #x08 #x81 #x73 #x04 #x41 #x00 #x41 #x03 #x84 #x07 #x85 #x04 #x41 #x00 #x41 #x04 #x81 #x73 #x05 #x82 #x73 #x05 #x41 #x00 #x41 #x05 #x81 #x73 #x03 #x82 #x73 #x06 #x41 #x00 #x41 #x06 #x81 #x73 #x83 #x73 #x07 #x41 #x00 #x41 #x07 #x81 #x71 #xF1 #x72 #x81 #x72 #x08 #x41 #x00 #x41 #x08 #x81 #xF1 #x72 #x81 #x71 #x09 #x41 #x00 #x41 #x0F #x08 #x41 #x00 #x41 #x0F #x08 #x41 #x00 #x41 #x0F #x08 #x41 #x00 #x41 #x0F #x08 #x41 #x00 #x41 #x0F #x08 #x41 #x00 #x4F #x4A #x00))

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

(defun draw-cel-data (pixels)
  nil)

(defun read-cel-data ()
  (mapcar #'convert-cel-data test-image))

(defun convert-cel-data (byte)
  "Reads a list of bytes and transforming them into a list of pairs containing pixel RLE data."
  (let ((color (nibble byte :hi))
        (num-pixels (nibble byte :lo)))
    (list color num-pixels)))
