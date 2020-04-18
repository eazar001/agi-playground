(defsystem #:agi
  :description "Experiments for hacking Sierra's AGI engine."
  :author "Ebrahim Azarisooreh <ebrahim.azarisooreh@gmail.com>"
  :pathname "src"
  :depends-on (#:sdl2)
  :components ((:file "agi" :depends-on ("object"))
               (:file "object" :depends-on ("decryption"))
               (:file "view" :depends-on ("file" "bytes"))
               (:file "decryption" :depends-on ("file"))
               (:file "dir" :depends-on ("file" "bytes"))
               (:file "bytes")
               (:file "file")))
