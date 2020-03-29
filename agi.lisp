;;;; Experimental playground for AGI interpreter

(require :sdl2)

(defvar *interpreter-delay* nil)

;;; initialization of the state of the interpreter as a list of variables, memory, flags, etc.
(defun init ()
  (let ((mem (init-mem)))
    (append (list :mode "program-control") mem)))

;;; initializes the memory region for variables, flag variables, and string variables
;;; variables = 256, flags = 256, strings = 12
;;; variables 0 - 26 and flags 0 - 16 are reserved for internal use
(defun init-mem ()
  (let ((vars (build-mem-region "V" :init nil :end 255))
	(flags (build-mem-region "F" :init nil :end 255))
	(strings (build-mem-region "S" :init "" :end 12)))
    (list :vars vars :flags flags :strings strings)))

;;; construct a region of memory
;;; end specifies the ending index, and init specifies the initialization value
(defun build-mem-region (prefix &key (end 255) (init nil))
  (let ((numbers (loop for x from 0 to end collecting x)))
    (reverse (build-mem-pairs prefix init numbers))))

(defun build-mem-pairs (prefix init numbers)
  (reduce (lambda (acc n) (build-mem-pair prefix init acc n)) numbers :initial-value nil))

(defun build-mem-pair (prefix init acc n)
  (let ((p (intern (concatenate 'string prefix (write-to-string n)) "KEYWORD")))
    (cons init (cons p acc))))

(defun loop-delay ()
  (if *interpreter-delay*
      (sleep *interpreter-delay*)
      (progn (setf *interpreter-delay* 0.005)
	     (loop-delay))))

;;; set the value of a flag in a specified region (variables, flag, strings)
(defun set-mem (region key new-value)
  (if (member key region)
      (setf (getf region key) new-value)))

(defun start ()
  (let* ((state (init))
	 (vars (getf state :vars))
	 (flags (getf state :flags))
	 (strings (getf state :strings))
	 (mode (getf state :mode)))
    (work-cycle vars flags strings mode)))

(defun poll-keyboard ()
  nil)

;;; set ego motion depending on interpreter mode state
;;; should throw exception on out of domain state
(defun set-ego-motion (mode vars &optional v6)
  (cond ((string= mode "program-control") (getf vars :v6))
	((string= mode "player-control") (set-mem vars :v6 v6))))

(defun work-cycle (vars flags strings mode)
  ;; have not decided on how to interpret delay yet
  (loop-delay)
  ;; clear the input buffer
  (clear-input)
  ;; these f2 and f4 are set to false at the beginning of each cycle
  (set-mem flags :f2 nil)
  (set-mem flags :f4 nil)
  ;; poll for user input
  (poll-keyboard)
  (set-ego-motion mode vars))

