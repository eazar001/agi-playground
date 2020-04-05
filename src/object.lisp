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

