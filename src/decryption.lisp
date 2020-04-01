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
	 (key-cycles (car (multiple-value-list (/ (float source-len) key-len))))
	 (key-rem (mod source-len key-len))
	 (last-cycle (subseq key-bytes 0 key-rem))
	 (first-cycles (loop for y from 1 to key-cycles
			  collecting (loop for x in key-bytes
					collecting x)))
	 (all-key-cycles (append (apply #'concatenate 'list first-cycles) last-cycle)))
    (mapcar #'logxor all-key-cycles source-bytes)))

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
