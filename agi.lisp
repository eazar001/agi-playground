;;;; Experimental playground for AGI interpreter

(require :sdl2)

(defvar *interpreter-delay-time* nil)


(defun start ()
  "Initializes main state and starts the work-cycle for the interpreter loop."
  (let* ((state (init))
	 (vars (getf state :vars))
	 (flags (getf state :flags))
	 (strings (getf state :strings))
	 (mode (getf state :mode)))
    (work-cycle vars flags strings mode 0 'on)))

;;; initialization of the state of the interpreter as a list of variables, memory, flags, etc.
(defun init ()
  (let ((mem (init-mem)))
    (append (list :mode 'program-control) mem)))

;;; initializes the memory region for variables, flag variables, and string variables
;;; variables = 256, flags = 256, strings = 12
;;; variables 0 - 26 and flags 0 - 16 are reserved for internal use
(defun init-mem ()
  (let ((vars (build-mem-region "V" :init nil :end 255))
	(flags (build-mem-region "F" :init nil :end 255))
	(strings (build-mem-region "S" :init nil :end 12)))
    ;; initialize player's score to 0
    (set-mem vars :v3 0)
    ;; by default, initialize sound to 'on' for now
    (set-mem flags :f9 'on)
    (list :vars vars :flags flags :strings strings)))

;;; construct a region of memory
;;; end specifies the ending index, and init specifies the initialization value
(defun build-mem-region (prefix &key (end 255) (init nil))
  (let ((numbers (loop for x from end downto 0 collecting x)))
    (build-mem-pairs prefix init numbers)))

(defun build-mem-pairs (prefix init numbers)
  (reduce (lambda (acc n) (build-mem-pair prefix init acc n)) numbers :initial-value nil))

(defun build-mem-pair (prefix init acc n)
  (let ((p (intern (concatenate 'string prefix (write-to-string n)) "KEYWORD")))
    (cons p (cons init acc))))

(defun loop-delay ()
  (if *interpreter-delay-time*
      (sleep *interpreter-delay-time*)
      (progn (setf *interpreter-delay-time* 0.005)
	     (loop-delay))))

;;; set the value of a flag in a specified region (variables, flag, strings)
(defun set-mem (region key new-value)
  (if (member key region)
      (setf (getf region key) new-value)))

(defun get-mem (region key)
  (getf region key))

(defun poll-keyboard ()
  nil)

;;; for any object in animate.obj that received and executed a start_update or draw command, we need to
;;; recalculate the direction of movement
(defun recalculate-obj-directions ()
    nil)

(defun update-status-line ()
    nil)

;;; was new.room.n or new.room.p executed?
(defun new-room-p ()
    nil)

(defun update-controlled-objects ()
  nil)

(defun execute ()
  ;; replace stub with necessary updates
  nil
  ;; then run the logic0 script
  (execute-logic0))

(defun execute-logic0 ()
  ;; should execute logic0 script here
  nil
  ;; then run update post-logic0 routines
  (logic0-post-update))

(defun logic0-post-update ()
  nil)

(defun key-string-to-bytes (key-string)
  (loop for c in (concatenate 'list key-string) collecting (char-code c)))

(defun decrypt-object-file (file)
  (decrypt-file "Avis Durgan" file))

(defun decrypt-file (key-string file)
  "Decrypt a file with a given keystring to XOR the bytes with."
  (let ((key-bytes (key-string-to-bytes key-string))
	(source-bytes (read-file-bytes-to-list file)))
    (xor-decrypt key-bytes source-bytes)))

(defun xor-decrypt (key-bytes source-bytes)
  (let* ((source-len (list-length source-bytes))
	 (key-len (list-length key-bytes))
	 (key-cycles (car (multiple-value-list (/ (float source-len) key-len))))
	 (key-rem (mod source-len key-len))
	 (last-cycle (subseq key-bytes 0 key-rem))
	 (first-cycles (loop for y from 1 to key-cycles collecting (loop for x in key-bytes collecting x)))
	 (all-key-cycles (append (apply #'concatenate 'list first-cycles) last-cycle)))
    (mapcar #'logxor all-key-cycles source-bytes)))

(defun read-file-bytes-to-list (filepath)
  (with-open-file (stream filepath :direction :input :element-type '(unsigned-byte 8))
    (read-bytes-to-list stream)))

(defun read-bytes-to-list (stream)
  (let ((b (read-byte stream nil)))
    (if b
	(cons b (read-bytes-to-list stream))
	nil)))

;;; set ego motion depending on interpreter mode state
;;; should throw exception on out of domain state
(defun set-ego-motion (mode vars &optional v6)
  (cond ((eq mode 'program-control) (get-mem vars :v6))
	((eq mode 'player-control) (set-mem vars :v6 v6))))

(defun work-cycle (vars flags strings mode prev-score prev-snd)
  ;; have not decided on how to interpret delay yet
  (loop-delay)
  ;; clear the input buffer
  (clear-input)
  ;; these f2 and f4 are set to false at the beginning of each cycle
  (set-mem flags :f2 nil)
  (set-mem flags :f4 nil)
  (format t "~a~%~a~%~a~%mode: ~a~%" vars flags strings mode)
  ;; poll for user input
  (poll-keyboard)
  ;; set-ego-motion
  (set-ego-motion mode vars)
  (recalculate-obj-directions)
  (cond ((/= (get-mem vars :v3) prev-score) (update-status-line))
	((not (eq (get-mem flags :f9) prev-snd)) (update-status-line))))
