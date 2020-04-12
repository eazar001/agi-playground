(defsystem #:agi
  :description "Experiments for hacking Sierra's AGI engine."
  :author "Ebrahim Azarisooreh <ebrahim.azarisooreh@gmail.com>"
  :pathname "src"
  :depends-on (#:sdl2)
  :components ((:file "agi" :depends-on ("object"))
               (:file "object" :depends-on ("decryption"))
               (:file "decryption" :depends-on ("file"))
               (:file "file")))
