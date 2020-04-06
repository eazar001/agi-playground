(defsystem #:agi
  :depends-on (#:sdl2)
  :components ((:file "agi" :depends-on ("object"))
	       (:file "object" :depends-on ("decryption"))
	       (:file "decryption")))
