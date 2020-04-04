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

(defun read-file-bytes-to-list (file-path)
  (with-open-file (stream file-path :direction :input :element-type '(unsigned-byte 8))
    (read-bytes-to-list stream)))

(defun read-bytes-to-list (stream)
  (let ((b (read-byte stream nil)))
    (if b
	(cons b (read-bytes-to-list stream))
	nil)))

(defun extract-objects (file)
  "Extracts (index, inventory string, room-location) triplets from an encrypted OBJECT resource file into a list."
  (let* ((decrypted-bytes (decrypt-object-file file))
	 (header-data (extract-object-header-data decrypted-bytes))
	 ;; (max-animated-objects (first header-data))
	 ;; this is the final sequence of bytes we're truly interested in
	 (bytes (second header-data))
	 (meta-bytes (third header-data))
	 (inventory-start (fourth header-data)))

    (let ((strings (mapcar (lambda (codes) (concatenate 'string (mapcar #'code-char codes)))
			   (parse-object-codes bytes)))
	  (object-rooms (get-object-room-pairs meta-bytes 0 inventory-start)))

      (mapcar (lambda (r s) (list (first r) (second r) s))
	      object-rooms strings))))

(defun parse-object-codes (bytes)
  (let ((r (do* ((remaining-bytes (cons 0 bytes) (rest remaining-bytes))
		 (next-char (first bytes) (first remaining-bytes))
		 (current-byte-seq nil (if (/= 0 next-char)
					   (cons next-char current-byte-seq)
					   current-byte-seq)))
		((or (eq next-char 0) (eq next-char nil))
		 (list (rest remaining-bytes) (reverse current-byte-seq))))))

    (if (first r)
	(cons (second r) (parse-object-codes (first r)))
	(list (second r)))))

;;; gives us (index, room-location)
(defun get-object-room-pairs (triplets i inventory-start)
  (let* ((f (first triplets))
	 (fst (cond ((= (length f) 1) (concatenate 'string "0" f))
		    (t f))))

    (cond ((not fst) nil)
	  ((< (+ (parse-integer (concatenate 'string (second triplets) fst) :radix 16) 3) inventory-start)
	   (get-object-room-pairs (cdddr triplets) i inventory-start))
	  (t (cons
		(list i (parse-integer (third triplets) :radix 16))
		(get-object-room-pairs (cdddr triplets) (1+ i) inventory-start))))))

(defun extract-object-header-data (bytes)
  (let* ((header (subseq bytes 0 3))
	 ;; this is the offset for the start point of all inventory items
	 (inventory-offset (+ (first header) (second header)))
	 ;; the true starting index is the offset plus the base byte-length of the header plus two chars
	 ;; for the first inventory item char
	 (inventory-start (+ inventory-offset 3 2))
	 (inventory-metadata (mapcar (lambda (x) (write-to-string x :base 16))
				     (subseq bytes 3 (+ inventory-offset 3))))
	 (inventory-data (subseq bytes inventory-start))
	 ;; this is the maximum number of animated objects
	 (max-animated-objects (third header)))
    (list max-animated-objects inventory-data inventory-metadata inventory-start)))

;;; this should only really be used for debugging with a hex editor
;;; simply decrypt the object file as with decrypt-object-file but instead send the bytes
;;; to a new output file
(defun decrypt-object-to-file (file-path new-file-path)
  (with-open-file (stream new-file-path :direction :output :element-type '(unsigned-byte 8))
    (loop for b in (decrypt-object-file file-path)
       collect (write-byte b stream))))
