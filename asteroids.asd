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
               (:file "asteroids")
               (:static-file "sounds/banjo.mp3")
               (:static-file "sounds/biding.mp3")
               (:static-file "sounds/explode1.wav")
               (:static-file "sounds/explode2.wav")
               (:static-file "sounds/explode3.wav")
               (:static-file "sounds/fire.wav")
               (:static-file "sounds/haters.mp3")
               (:static-file "sounds/life.wav")
               (:static-file "sounds/lsaucer.wav")
               (:static-file "sounds/music.mp3")
               (:static-file "sounds/phaser.wav")
               (:static-file "sounds/sfire.wav")
               (:static-file "sounds/ssaucer.wav")
               (:static-file "sounds/thrust.wav")
               (:static-file "sounds/thumphi.wav")
               (:static-file "sounds/thumplo.wav")))

