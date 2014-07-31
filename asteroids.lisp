;; asteroids
;; 
; Controls: 
;; Rotate a f   Thrust j  Fire <spacebar>
;;
;; TODO:
;; -convert timers to ms
;;
(ql:quickload "lispbuilder-sdl")
(ql:quickload "lispbuilder-sdl-gfx")
(ql:quickload "lispbuilder-sdl-mixer")


(defpackage :asteroids
  (:use :cl :sdl )
  (:export main)
  (:export *explosion-color*)
)

(in-package :asteroids)

(defconstant +degrees+ 0.0175)
(defparameter *screen-width* 800)
(defparameter *screen-height* 600)
 
(defparameter *window* nil)
(defparameter *thrust-factor* 0.005)
(defparameter *friction* 0.995)
(defparameter *missile-velocity* 0.8)
(defparameter *rock-sides* 12)
 
(defparameter *powerup-max-age* 9)
(defparameter *explosion-max-radius* 0.1)
(defparameter *explosion-color* (sdl:color :r 180 :g 30 :b 30)) 
(defparameter *lr-map* 0) ;map left=1 right=2 both=3
(defparameter *is-thrusting* nil)

;(defconstant *but-left*   :sdl-key-a)
;(defparameter *but-right*  :sdl-key-d)
;(defparameter *but-fire*   :sdl-key-space)
;(defparameter *but-thrust* :sdl-key-j)

(defparameter *sound* nil)
(defvar *world* nil)
(defvar *audio-path* (make-pathname :host (pathname-host #.(or *compile-file-truename*
							     *load-truename*))
                                  :directory (pathname-directory #.(or *compile-file-truename*
                                                                       *load-truename*))))
(defclass sound ()
  ((opened :initform nil :accessor opened)
   (sample :initform nil :accessor sample)) )

(defmethod reset ((sound sound))
  (sdl-mixer:halt-sample))

(defmethod initialize ((sound sound))
  (sdl-mixer:OPEN-AUDIO)
  (setf (opened sound) (sdl-mixer:open-audio :chunksize 1024 :enable-callbacks nil))
  (format t "mixer opened...")
  (setf (sample sound)
	(mapcar #'sdl-mixer:load-sample 
		'("sounds/explode1.wav" "sounds/explode2.wav" "sounds/explode3.wav" "sounds/fire.wav" 
		  "sounds/thrust.wav"   "sounds/thumplo.wav"  "sounds/thumphi.wav" "sounds/lsaucer.wav"
		  "sounds/ssaucer.wav")))
  (sdl-mixer:allocate-channels 16)
  (play-explode1 sound)
  )
(defmethod shut-down ((sound sound))
  (sdl-mixer:Halt-Music)
  ;(sdl-mixer:free music)
  (sdl-mixer:close-audio)

)
(defmethod play-explode1 ((sound sound))
  (sdl-mixer:play-sample (first (sample sound))))

(defmethod play-explode2 ((sound sound))
  (sdl-mixer:play-sample (second (sample sound))))

(defmethod play-explode3 ((sound sound))
  (sdl-mixer:play-sample (third (sample sound))))

(defmethod play-fire ((sound sound))
  (sdl-mixer:play-sample (fourth (sample sound))))

(defmethod play-thrust ((sound sound))
  (sdl-mixer:play-sample (fifth (sample sound)) :channel 1 :loop t))

(defmethod play-thrust-stop ((sound sound))
  (sdl-mixer:halt-sample :channel 1))

(defmethod play-thump-stop ((sound sound))
  (sdl-mixer:halt-sample :channel 2))
(defmethod play-thumplo ((sound sound)) 
  (play-thump-stop sound)
  (sdl-mixer:play-sample (sixth (sample sound)) :channel 2 ))
(defmethod play-thumphi ((sound sound))
  (play-thump-stop sound)
  (sdl-mixer:play-sample (seventh (sample sound)) :channel 2))

(defmethod play-ufo1 ((sound sound))
  (sdl-mixer:play-sample (eighth (sample sound)) :channel 4 :loop t))
(defmethod play-ufo1-stop ((sound sound))
  (sdl-mixer:halt-sample :channel 4))

(defmethod play-ufo2 ((sound sound))
  (sdl-mixer:play-sample (ninth (sample sound)) :channel 4 :loop t))
(defmethod play-ufo2-stop ((sound sound))
  (sdl-mixer:halt-sample :channel 4))


(defun xy-off-sum (a b)
  (mapcar #'+ a b))

(defun xy-off-scale (v factor)
  (mapcar #'* v (list factor factor)))

(defun xy-off-subtract (a b)
  (mapcar #'- a b))

(defun xy-off-create (angle-rad magnitude)
  "Create a 'xy-off'"
  (list (* magnitude (sin angle-rad))
	(* magnitude (cos angle-rad))))
;;; distance between point a and point b
;;; parameters must be lists of 2 numbers (x y)
(defun my-distance (a b)
  (sqrt (+ (expt (- (first a) (first b)) 2)
	   (expt (- (second a) (second b)) 2))))


(defun radial-point-from (p radius angle)
  (point :x (+ (* radius (sin angle)) (x p))
         :y (+ (* radius (cos angle)) (y p))))

#+nil
(defun calc-angle (a b)
  (destructuring-bind (x y) (xy-off-subtract b a)
    (rad->deg (atan x y))))

;;-------------------------------------------------------------------
;; O B J E C T S
;;; 

;;-------------------------------------------------------------------
;; T I M E R
;;
;; A timer keeps a target tick that we are waiting for.
(defclass timer ()
  (( target :initarg :seconds :initform 0 :accessor target)))

(defmethod initialize-instance :after ((timer timer) &key)
  (setf (target timer) (+ (system-ticks) (* 1000 (target timer)))))

(defmethod donex ((timer timer))
  (<= (target timer) (system-ticks)))

(defmethod set-seconds ((timer timer) seconds)
  (setf (target timer) (+  (system-ticks) (* 1000 seconds))))
;;-------------------------------------------------------------------
;; M O B 
;;
(defclass mob ()
  ((pos :initform '(0.5 0.5) :initarg :pos  :accessor pos)
   (radius :initform 0 :initarg :radius :accessor radius :writer set-radius)
   (velocity :initform '(0 0) :initarg :velocity  :accessor velocity)))

(defmethod map-coords ((mob mob))
  "create a point from mob's fractional coordinates"
  (point :x (* *screen-width* (first (pos mob)))
	 :y (* *screen-height* (second (pos mob)))))

(defmethod map-radius ((mob mob))
  (round (* (radius mob) *screen-width*))) ;radius is required to be an int


(defmethod intersects-p ((mob mob) (other mob))
  (< (my-distance (pos mob) (pos other))
     (+ (radius mob) (radius other))))

(defmethod render ((mob mob))
  (values))


;;-------------------------------------------------------------------
;; R O C K
;;
(defclass rock (mob)
  ((size :initarg :size :initform 1 :reader size)
   (radii :initform nil :accessor radii)
   (rotation :initform (- (random .04) 0.02) :accessor rotation)
   (direction :initform 0 :accessor direction)))

(defmethod initialize-instance :after ((rock rock) &key)
  (let ((radius (nth (size rock) '(nil 0.07 0.04 0.01)))
        (spd (nth (size rock) '(nil 0.05 0.15 0.25))))
(print radius)
    (set-radius radius rock)
    (setf (radii rock)
          (loop for i from 0 below *rock-sides*
	     collect (round (* (- 1.0 (random 0.3))
			       (map-radius rock)))))
    (setf (velocity rock)
          `(,(- (random (* 2 spd)) spd) ,(- (random (* 2 spd)) spd)))))



(defmethod render ((rock rock))
   (draw-polygon (loop for i from 0
                      for r in (radii rock)
                  collect (radial-point-from (map-coords rock) r
                                             (+ (direction rock)
                                                (* i (/ 360 *rock-sides*) +degrees+))))
                :color *green* ))
;;-------------------------------------------------------------------
;; M I S S I L E
;; 
(defclass missile (mob)
  ((super :initform nil :initarg :super :accessor super-p)
   (lifetime :initform (make-instance 'timer :seconds 1) :accessor lifetime)))

(defmethod render ((missile missile))
  (let ((coords (map-coords missile)))
    (draw-circle coords 1 :color *white*)
    (when (super-p missile)
      (draw-circle coords (+ (random 3))
		   :color *magenta*))))
;;-------------------------------------------------------------------
;;E X P L O S I O N
;;

(defclass explosion (mob)())

(defmethod render ((explosion explosion))
  (let ((coords (map-coords explosion))
        (radius (map-radius explosion)))
    (draw-circle coords radius :color *explosion-color* :aa t)
    (draw-circle coords
                 (+ radius (random 3))
                 :color *explosion-color* :aa t)))
;;-------------------------------------------------------------------
;; P O W E R U P
(defclass powerup (mob)
  ((lifetime :initform (make-instance 'timer :seconds *powerup-max-age*) :accessor lifetime)))

(defmethod initialize-instance :after ((powerup powerup) &key)
  (set-radius 0.02 powerup))
;;-------------------------------------------------------------------
(defclass powerup-missile (powerup) ())

(defmethod render ((powerup powerup-missile))
  (let ((coords (map-coords powerup))
        (radius (map-radius powerup)))
    (draw-circle coords radius
                 :color *magenta* :aa t)
    (draw-circle coords (round (* radius 0.3))
                 :color *white* :aa t)))
;;-------------------------------------------------------------------
(defclass powerup-freeze (powerup) ())

(defmethod render ((powerup powerup-freeze))
  (let ((coords (map-coords powerup))
        (radius (map-radius powerup)))
    (draw-circle coords radius
                 :color *cyan* :aa t)
    (draw-polygon (loop for i from 0 to 11
                    collect (radial-point-from coords
                                               (round (* radius (if (= (mod i 2) 0)
								    0.7 0.2)))
					       (* i 0.525)))
                  :color *white* :aa t)))
;;-------------------------------------------------------------------
(defclass powerup-shield (powerup) ())

(defmethod render ((powerup powerup-shield))
  (let ((coords (map-coords powerup))
        (radius (map-radius powerup)))
    (draw-circle coords radius
                 :color *green*)
    (draw-polygon `(,(radial-point-from coords (round (* radius 0.8)) 0.7)
                    ,(radial-point-from coords (round (* radius 0.8)) 0)
                    ,(radial-point-from coords (round (* radius 0.8)) -0.7)
                    ,(radial-point-from coords (round (* radius 0.8)) -2.3625)
                    ,(radial-point-from coords (round (* radius 0.8)) 2.3625))
                  :color *white*  :aa t)))

;;-------------------------------------------------------------------
(defun make-random- (&key pos velocity)
  (make-instance (case (random 3)
                   (0 'powerup-missile)
                   (1 'powerup-freeze)
                   (2 'powerup-shield))
                 :pos pos :velocity velocity))
;;-------------------------------------------------------------------

(defun polygon1 ( coords radius direction)
  "return a list of vertices (points) suitable for draw-polygon"
  (let ((nose (radial-point-from coords radius direction))
	(left (radial-point-from coords radius (- direction 2.45)))
	(right (radial-point-from coords radius (+ direction 2.45)))
	(tail-right (radial-point-from coords (* -.5 radius) (- direction 0.7) ))
	(tail-left (radial-point-from coords (* -.5 radius) (+ direction 0.7) ))
	;(tail (radial-point-from coords (round (* radius 0.5)) (+ direction 180)))
	)
    (list nose left tail-left tail-right right)))
#+nil
(defun polygon2 ( coords radius direction)
  (let ((nose (radial-point-from coords radius direction))
	(left (radial-point-from coords radius (- direction 2.45)))
	(right (radial-point-from coords radius (+ direction 2.45)))
	(tail-right (radial-point-from coords (* -.5 radius) (- direction 0.7) ))
	(tail-left (radial-point-from coords (* -.5 radius) (+ direction 0.7) ))
	;(tail (radial-point-from coords (round (* radius 0.5)) (+ direction 180)))
	)
    (list nose left  left tail-left  tail-left tail-right  tail-right right  right nose )))
;; Draw a list of line segments represented as a linear list of pairs of points
#+nil(defun draw-list (list &key (color *green*))
  (loop for i on list by #'cddr do
       ;(print (car i) )
      (draw-line (car i) (cadr i) :color color :aa t)
       ))

;;-------------------------------------------------------------------
;; S H I P
;;
(defclass ship (mob) 
  ((timers :initform (make-hash-table) :accessor timers)
   (acceleration :initform '(0 0) :accessor acceleration-of)
   (direction :initform 0 :initarg :direction :accessor direction)
   (rotation :initform 0.0 :accessor rotation) 
   ))

(defmethod initialize-instance :after ((ship ship) &key)
  (set-radius 0.015 ship))

(defmethod render ((ship ship))
  (let* ((coords (map-coords ship))
	 (radius (map-radius ship))
	 (direction (direction ship)))
    
    (draw-polygon (polygon1 coords radius direction) :color *green* :aa t)
    ;(draw-list (polygon2 coords radius direction))
    (if *is-thrusting* ;draw thrusting jets
	(let ((tail (radial-point-from coords (round (* radius 0.5)) (+ direction pi))))
	  (draw-line tail
		     (radial-point-from tail (round (* radius (random 1.0))) 
					(+ direction pi (- (random 0.5) 0.25)))
		     :color *red* :aa nil))
       	)
    
    (when (powerup-active-p ship 'shield)
      (draw-circle coords
		   (round (+ radius (random 3)))
		   :color *blue*))))

(defun ship-fire (ship)
  (let ((missile (make-instance 'missile 
				:pos (pos ship)
				:super (powerup-active-p ship 'super-missiles) )))
    (setf (velocity missile) (xy-off-create (direction ship) *missile-velocity*))
    missile )
  
)


(defmethod thrust ((ship ship))
  "Set ship's acceleration using *thrust-factor* and ship's direction"
  (setf (acceleration-of ship) (xy-off-create (direction ship) *thrust-factor*)) )

(defmethod thrust-0 ((ship ship))
  "Set ship's acceleration to null"
  (setf (acceleration-of ship) '(0 0)))

;; questionable - powerups should be independent objects, not tangled with the ship...
(defmethod powerup-active-p ((ship ship) powerup)
  (let ((timer (gethash powerup (timers ship) nil)))
    (and timer
         (not (donex timer)))))






(defmethod add-super-missiles ((ship ship) &key (seconds 0))
  (if (powerup-active-p ship 'super-missiles)
    (set-seconds (gethash 'super-missiles (timers ship)) seconds)
    (setf (gethash 'super-missiles (timers ship))
          (make-instance 'timer :seconds seconds))))

(defmethod add-shield ((ship ship) &key (seconds 0))
  (if (powerup-active-p ship 'shield)
    (set-seconds (gethash 'shield (timers ship)) seconds)
    (setf (gethash 'shield (timers ship))
          (make-instance 'timer :seconds seconds)))
)

;;-------------------------------------------------------------------
;; X - S H I P
(defclass x-ship (mob) 
  ((lifetime :initform (make-instance 'timer :seconds 1) :accessor lifetime)
   (direction :initarg :direction :accessor direction)
   (fade :initform 255 :accessor fade)))

(defmethod render ((x-ship x-ship))
  (draw-polygon (polygon1 (map-coords x-ship) 
		       (map-radius x-ship) 
		       (direction x-ship))
		:color ( color
			 :r (decf (fade x-ship))
			 :g 0
			 :b 0)))
;;-------------------------------------------------------------------
;; W O R L D
;;
(defclass world ()
  ((mobs :initform nil :accessor mobs)
   (ship :initform nil :accessor ship) ;note: also in mobs!
   (timers :initform (make-hash-table) :accessor timers)
   (level :initform 0 :accessor level)
   (num-of-rocks :initform 0 :accessor num-of-rocks)
   (score :initform 0 :accessor score)
   (best-level :initform 0 :accessor best-level)
   (high-score :initform 0 :accessor high-score)
   (lives :initform 1  :accessor lives)
   (paused :initform nil :accessor paused)
   (ambient :initform (make-instance 'ambient :period 1000) :accessor ambient)))

(defmethod reset ((world world))
  (setf (mobs world) nil)
  (setf (ship world) nil)	
  (setf (paused world) nil)
  (setf (level world) 0)
  (setf (score world) 0)
  (setf (lives world) 1)
  (setf (num-of-rocks world) 0)

  (reset *sound*))
;; Adding to world: cons to mob list and track ship and rock-count
;;
(defmethod add-to ((world world) (mob mob))
  (setf (mobs world) (cons mob (mobs world)))
  (values mob))

(defmethod add-to :after ((world world) (rock rock))
  (incf (num-of-rocks world)))

(defmethod add-to :after ((world world) (ship ship))
  (setf (ship world) ship))

(defmethod add-to :after ((world world) (powerup powerup))
  (play-ufo1 *sound* ))
;; Removing from world
(defmethod remove-from ((world world) (mob mob))
  (setf (mobs world) (remove mob (mobs world))))

(defmethod remove-from :after ((world world) (rock rock))
  (decf (num-of-rocks world)))

(defmethod remove-from :after ((world world) (ship ship))
  (setf (ship world) nil))

(defmethod remove-from :after ((world world) (powerup powerup))
  (play-ufo1-stop *sound*))









(defmethod start-next-level ((world world))
  (with-accessors ((level level)
                   (best-level best-level)
                   (mobs mobs)
                   (timers timers)
                   (ship ship))
      world
    (incf level)
    (setf best-level (max best-level level))
    (setf mobs nil)
    (setf timers (make-hash-table))
    (setf (num-of-rocks world) 0)	;
    (dotimes (i level)
      (add-to world (make-instance 'rock :pos `(,(random 1.0) ,(random 1.0)))))
    (add-to world (or ship (make-instance 'ship))) ;keep existing ship or create a new one
    (add-shield (ship world) :seconds 6)
    ))

(defmethod level-cleared-p ((world world))
  ;(print (num-of-rocks world))
  (< (num-of-rocks world) 1))

;;-------------------------------------------------------------------
;; UPDATE multimethod
;;
;; all mobs continue moving and wrapping...
(defmethod update ((mob mob) time-delta (world world))
  (setf (pos mob)
        (mapcar (lambda (x) (mod x 1))
                (xy-off-sum (pos mob) 
			    (xy-off-scale (velocity mob) time-delta)))))

;; update missile
(defmethod update :after ((missile missile) time-delta (world world))
  (if (donex (lifetime missile))
      (remove-from world missile))
#+nil  (let ((remaining (- (timeout missile) time-delta)))
    (if (<= remaining 0)
	(remove-from world missile)
	(setf (timeout missile) remaining)))
  (dolist (mob (mobs world))
    (when (and (not (eq missile  mob))
	       (intersects-p missile mob))
      (collide missile mob world))))

;; update x-ship
(defmethod update :after  ((x-ship x-ship) time-delta (world world))
  (if (donex (lifetime x-ship))
      (remove-from world x-ship)
      (setf (direction x-ship ) ;spinning out of control...
	      (+ (direction x-ship) 0.05)))
)
;; update rock.  rocks also rotate.  Note: if frozen, mob update not called.
(defmethod update ((rock rock) time-delta (world world))
  (declare (ignore time-delta))
  (when (not (frozen-p world))
    (incf (direction rock) (rotation rock))
    (call-next-method)))

;; update explosion
(defmethod update :after ((explosion explosion) time-delta (world world))
  (when (> (incf (radius explosion) time-delta)
           *explosion-max-radius*)
    (remove-from world explosion)))

;; update powerup
(defmethod update :after ((powerup powerup) time-delta (world world))

  (when (donex (lifetime powerup)) 
    (remove-from world powerup)))

;; update ship
(defmethod update :around ((ship ship) time-delta (world world))
  ;; lr-map contains left/right button mapping, for rollover...
  (cond 
    ((= *lr-map* 0) (setf (rotation ship) 0))
    ((= *lr-map* 1) (setf (rotation ship) 1)) 
    ((= *lr-map* 2) (setf (rotation ship) -1)) 
    (t (setf (rotation (ship world)) 0)))

  (setf (direction ship )
	(+ (direction ship) (* 3.5  time-delta (rotation ship))))
  
  (if *is-thrusting*
      (thrust ship)
      (thrust-0 ship))

  (setf (velocity ship)
	(xy-off-scale (xy-off-sum (velocity ship)
				  (acceleration-of ship))
		      *friction*))

 #+nil
 (maphash (lambda (name timer)
             (declare (ignore name))
             (update-timer timer time-delta))
           (timers ship))
  (call-next-method)
  (ship-moved world ship))

(defmethod ship-moved ((world world) (ship ship))
  (dolist (mob (mobs world))
    (when (and (not (eq ship mob))
               (intersects-p ship mob))
      (collide ship mob world))
    ;; if a collision destroyed the ship, stop checking for collisions
    #+nil
    (when (not (in-world-p world ship))
      (return ship))))





(defmethod after ((world world) timer-name &key (seconds 0) do)
  (multiple-value-bind (timer exists) (gethash timer-name (timers world))
    (if exists
      (when (donex timer)
        (remhash timer-name (timers world))
        (when (functionp do)
          (funcall do)))
      (setf (gethash timer-name (timers world))
            (make-instance 'timer :seconds seconds)))))


(defmethod update-world ((world world))
  (update-ambient (ambient *world*) )
  
  
  #+nil  (maphash (lambda (name timer)
		    (declare (ignore name))
		    (update-timer timer (sdl:dt)))
		  (timers world))
  (dolist (mob (mobs world))
    
    (update mob (
dt) world))
  ;; start next level 3 seconds after clearing
  (when (level-cleared-p world)
    (after world
           'cleared
           :seconds 3
           :do (lambda ()
                 (incf (lives world))
                 (start-next-level world)
		 (reset-ambient (ambient world))
		 (play-ufo1-stop *sound*) ;ugly
		)))

  ;; restart level 3 seconds after death - game over if no more lives  

  (unless (ship world)
    (after world
	   'death
	   :seconds 3
	   :do (lambda ()
		 (if (< (lives world) 1)
		     (setf (level world) 0) ; game over
		     (let ((ship (make-instance 'ship)))
		       
		       (add-to world ship)
		       (add-shield ship :seconds 6)
		       (reset *sound*)))))))








(defmethod frozen-p ((world world))
  (let ((timer (gethash 'freeze (timers world) nil)))
    (and timer
         (not (donex timer)))))

;; Attract screen
(defmethod render-attract ((world world))
  (sdl-gfx:draw-string-solid-* "Asteroids"
			       (round (* 1/2 (- *screen-width* 171)))
			       (round (* 1/4 (- *screen-height* 18)))
			       :color *green*)
  (sdl-gfx:draw-string-solid-* (format nil
				       "High score: ~d"
				       (high-score world))
			       (round (* 1/2 (- *screen-width* 171)))
			       (round (* 1/2 (- *screen-height* 18)))
			       :color *green*)
  (sdl-gfx:draw-string-solid-* (format nil "Best level: ~d" (best-level world))
			       (round (* 1/2 (- *screen-width* 171)))
			       (round (* 3/4 (- *screen-height* 18)))
			       :color *green*)
  (sdl-gfx:draw-string-solid-* (format nil "Controls: [a] left   [f] right   [j] thrust   [space] fire" )
			       50
			       (round (* 7/8 (- *screen-height* 18)))
			       :color *green*)
  (sdl-gfx:draw-string-solid-* (format nil "Press [p] to play" )
			       50
			       (round (* 15/16 (- *screen-height* 18)))

			       :color *green*)
)
(defmethod render-world ((world world))
  (clear-display *black*)
  (if (= (level world) 0)
      (render-attract world)
      (render-world-game world))
)
(defmethod render-world-game ((world world))
    ;; hud
  (sdl-gfx:draw-string-solid-* (format nil "Level ~d" (level world))
                               10 10
                               :color *green*)
  (sdl-gfx:draw-string-solid-* (format nil "Lives ~d" (lives world))
                               10 (- *screen-height* 28)
                               :color *green*)
  (sdl-gfx:draw-string-solid-* (format nil "Score ~d" (score world))
                               (- *screen-width* 127) (- *screen-height* 28)
                               :color *green*)
  (sdl-gfx:draw-string-solid-* (format nil  "[P]ause [Q]uit")
                               (- *screen-width* 127) 10
                               :color *green*)
   
  
  (set-clip-rect (rectangle :x 0 :y 0 :w *screen-width* :h *screen-height*)
		 :surface *default-display*)
  (dolist (mob (mobs world))
    (render mob))
  (set-clip-rect nil :surface *default-display*)
    ;; pause text

  #+nil (when (paused world)
    (sdl-gfx:draw-string-solid-* "PAUSED"
				 (round (* 1/2 (- *screen-width* 54)))
				 (round (* 1/2 (- *screen-height* 18)))
				 :color *green*)))

;;----------------------------





(defmethod break-down ((rock rock) (world world))
  (with-slots ((pos pos) size) rock
    (if (= size 3)
      ;; gradually reduce the probability of powerups appearing
      (if (< (random 100) (/ 100 (+ 4 (* (level world) 0.3))))
	  (add-to world (make-random-powerup 
				       :pos pos :velocity (velocity rock))))
      (progn
	(add-to world (make-instance 'rock :pos pos :size (1+ size)))
	(add-to world (make-instance 'rock :pos pos :size (1+ size)))))))








(defmethod collide ((mob mob) (other mob) (world world)) t)

(defmethod collide :before ((ship ship) (powerup powerup-shield) (world world))
  (add-shield ship :seconds 6))

(defmethod collide :before ((ship ship) (powerup powerup-missile) (world world))
  (add-super-missiles ship :seconds 6))

(defmethod collide :before ((ship ship) (powerup powerup) (world world))
  (remove-from world powerup)
  (add-score world powerup))

(defmethod collide :before ((ship ship) (powerup powerup-freeze) (world world))
  (add-freeze world :seconds 6))

;; ship-rock collision
(defmethod collide :before ((ship ship) (rock rock) (world world))
  (unless (powerup-active-p ship 'shield)
    (play-explode1 *sound*)
    (play-explode2 *sound*)
    (play-explode3 *sound*)
    (play-ufo1-stop *sound*) ;ugly: powerup not removed from world
    (add-to world (make-instance 'x-ship 
				 :pos (pos ship)
				 :radius (radius ship)
				 :velocity (velocity ship)
				 :direction (direction ship)))
    (remove-from world ship)
    (add-to world (make-instance 'explosion :pos (pos ship) :velocity (velocity ship)))
    (decf (lives world))
 
  
    ))

(defmethod collide :before ((missile missile) (rock rock) (world world))
  (remove-from world rock)
  (when (not (super-p missile )) (remove-from world missile))
    (break-down rock world) ;it adds smaller rocks to world
  (add-to world (make-instance 'explosion :pos (pos rock) :velocity (velocity rock)))
  (add-score world rock)
  (case (size rock)
    (1 (play-explode1 *sound*))
    (2 (play-explode2 *sound*))
    (3 (play-explode3 *sound*))))


(defmethod shoot ((ship ship) (world world))
  "Fire a missile using ship's direction"
  (add-to world (ship-fire ship))
  (play-fire *sound*)
)




(defmethod add-score ((world world) (score number))
  (setf (high-score world)
        (max (incf (score world) score)
             (high-score world))))

(defmethod add-score ((world world) (powerup powerup))
  (add-score world (* (level world) 10)))

(defmethod add-score ((world world) (rock rock))
  (add-score world (nth (size rock) '(nil 1 2 5))))






(defmethod add-freeze ((world world) &key (seconds 0))
  (if (frozen-p world)
    (set-seconds (gethash 'freeze (timers world)) seconds)
    (setf (gethash 'freeze (timers world))
          (make-instance 'timer :seconds seconds))))






(defmethod in-world-p ((world world) (mob mob))
  (find mob (mobs world)))



;; Written by pjb with minor modification for finishing the output
;; from me.
(defmacro handling-errors (&body body)
  `(handler-case (progn ,@body)
     (simple-condition
         (err)
       (format *error-output* "~&~A: ~%" (class-name (class-of err)))
       (apply (function format) *error-output*
              (simple-condition-format-control   err)
              (simple-condition-format-arguments err))
       (format *error-output* "~&")
       (finish-output))
     (condition
         (err)
       (format *error-output* "~&~A: ~%  ~S~%"
               (class-name (class-of err)) err)
       (finish-output))))

;; Written by pjb with minor modification for finishing the output and
;; starting from zero in the history.
(defun repl ()
  (do ((+eof+ (gensym))
       (hist 0 (1+ hist)))
      (nil)
    (format t "~%~A[~D]> " (package-name *package*) hist)
    (finish-output)
    (handling-errors
     (setf +++ ++
           ++ +
           + -
           - (read *standard-input* nil +eof+))
     (when (or (eq - +eof+)
               (member - '((quit) (exit) (continue)) :test #'equal))
       (return-from repl))
     (setf /// //
           // /
           / (multiple-value-list (eval -)))
     (setf *** **
           ** *
           * (first /))
     (format t "~& --> ~{~S~^ ;~%     ~}~%" /)
     (finish-output))))



(defun key-processor-attract (world key &key down)
  (if down
      (case key
	(:sdl-key-q (SDL:PUSH-QUIT-EVENT))
	(:sdl-key-p (progn (reset world) (start-next-level world))))))

(defun key-processor-game (world key &key down)
  (if down
      (case key
	(:sdl-key-q (reset world))
	(:sdl-key-a (setf *lr-map* (boole boole-ior *lr-map* 1)))		
	(:sdl-key-f (setf *lr-map* (boole boole-ior *lr-map* 2)))
	(:sdl-key-j (setf *is-thrusting* t) (play-thrust *sound*) )
	(:sdl-key-space (if (ship world)
			    (shoot (ship world) world)))
     
	)
      (case key
	(:sdl-key-escape (repl)		;(push-quit-event)
	 )
	(:sdl-key-a (setf *lr-map* (boole boole-and *lr-map* 2)))
	(:sdl-key-f (setf *lr-map* (boole boole-and *lr-map* 1)))
	(:sdl-key-j (setf *is-thrusting* nil) (play-thrust-stop *sound*)))))

(defun key-processor (world key &key down)
  (if (= (level world) 0)
      (key-processor-attract world key :down down)
      (key-processor-game world key :down down)))


(defclass ambient ()
  ((target  :initform 1000 :accessor target)
   (phasex   :initform 1   :accessor phasex)
   (period  :initform 1500 :accessor period :initarg :period)))

;(defmethod initialize-instance :after ((ambient ambient) &key)  (setf (nt) (period ambient)))

(defmethod update-ambient ((ambient ambient) )
  (with-slots ((target target) (phasex phasex) (period period)) ambient
    (let ((sdl-ticks (sdl-get-ticks) ))
      (if (< target sdl-ticks)
	  (progn 
;(print (- sdl-ticks target))
	    (if (evenp (incf phasex))
		(play-thumplo *sound*)
		(play-thumphi *sound*))
	    ;(setf (target (- period (- sdl-ticks target))))
	    (setf target  (+ sdl-ticks period))
	    (if (> period 400) (decf period 40))))))) 

(defmethod reset-ambient ((ambient ambient))
  (setf (period ambient) 1500)
  (setf (target ambient) 0)
  (setf (phasex ambient) 1)
)
(defun sample-finished-action ()
  (sdl-mixer:register-sample-finished
   (lambda (channel)
     (declare (ignore channel))
     nil)))

(defun music-finished-action ()
  (sdl-mixer:register-music-finished
   (lambda ())))



(defun main ()
  (setf *world* (make-instance 'world))
  (let ((world *world*))

    (with-init (sdl-init-video sdl-init-audio)
      (setf *window*
	    (window *screen-width* *screen-height*
		    :title-caption "asteroids"
		    :icon-caption "asteroids"
;		    :fps (make-instance 'fps-fixed :target-frame-rate 120 :world *world*)
;		    :fps (make-instance 'fps-unlocked :ps-fn 'physics)
;		    :fps (make-instance 'fps-timestep :world *world*)
		    ))
      (sdl-gfx:initialise-default-font sdl-gfx:*font-9x18*)
					;(format t "initialized...")
      (setf (frame-rate) 120)
					;(clear-display *black*)

      
      (setf *sound* (make-instance 'sound))
      (initialize *sound*)
      
      (with-events ()
	(:quit-event ()
		     (shut-down *sound*)
		     t)
	(:key-down-event (:key key) (key-processor world key :down t))
	(:key-up-event (:key key) (key-processor world key :down nil))
	(:idle () 
;;(print (get-ticks))
	       (if  (> (level world) 0)
		    (update-world world) 
		      ;(format t "~%~5$ ~5$" (float (sdl:dt)) x)
		      )
	       (render-world world)
	       (update-display))))))
