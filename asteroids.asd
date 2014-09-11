;;;; asteroids.asd

(asdf:defsystem #:asteroids
  :serial t
  :description "An improved asteroids game with sounds"
  :author "stacksmith stack@sdf.org"
  :license "Do Whatever The Fuck You Want (DWTFYF)"
  :depends-on (#:lispbuilder-sdl
               #:lispbuilder-sdl-gfx
               #:lispbuilder-sdl-mixer)
  :components ((:file "package")
               (:file "asteroids")))

