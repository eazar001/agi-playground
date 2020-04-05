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

;;; set ego motion depending on interpreter mode state
;;; should throw exception on out of domain state
(defun set-ego-motion (mode vars &optional v6)
  (cond ((eql mode 'program-control) (get-mem vars :v6))
	((eql mode 'player-control) (set-mem vars :v6 v6))))

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
	((not (eql (get-mem flags :f9) prev-snd)) (update-status-line))))
