;;; Encryption/Decrytion routines for AGI resources


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
  (loop for c in (concatenate 'list key-string) collecting (char-code c)))

(defun read-file-bytes-to-list (filepath)
  (with-open-file (stream filepath :direction :input :element-type '(unsigned-byte 8))
    (read-bytes-to-list stream)))

(defun read-bytes-to-list (stream)
  (let ((b (read-byte stream nil)))
    (if b
	(cons b (read-bytes-to-list stream))
	nil)))
