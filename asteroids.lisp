;;;; ASTeroids
;; Note: vector seems to mean an x-y delta pair...
; Controls: 
;; Rotate a f   Thrust j  Fire <spacebar>
;;
;; TODO:
;; separate gameloop from attract mode loop
;;
(ql:quickload "lispbuilder-sdl")
(ql:quickload "lispbuilder-sdl-gfx")

(defpackage :asteroids
  (:use :cl :sdl)
  (:export main)
  (:export *explosion-color*)
)

(in-package :asteroids)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defparameter *window* nil)
(defparameter *window-width* 640)
(defparameter *thrust-factor* 0.01)
(defparameter *friction* 0.99)
(defparameter *missile-velocity* 0.8)
(defparameter *rock-sides* 12)
 
(defparameter *ticks* 0)

(defparameter *powerup-max-age* 9)
(defparameter *explosion-max-radius* 0.1)
(defparameter *explosion-color* (sdl:color :r 120 :g 30 :b 30)) 
(defparameter *lr-map* 0) ;map left=1 right=2 both=3
(defparameter *is-thrusting* nil)

;(defconstant *but-left*   :sdl-key-a)
;(defparameter *but-right*  :sdl-key-d)
;(defparameter *but-fire*   :sdl-key-space)
;(defparameter *but-thrust* :sdl-key-j)

(defun xy-off-sum (a b)
  (mapcar #'+ a b))

(defun xy-off-scale (v factor)
  (mapcar #'* v (list factor factor)))

(defun xy-off-subtract (a b)
  (mapcar #'- a b))

(defun xy-off-create (angle-deg magnitude)
  "Create a 'xy-off'"
  (list (* magnitude (sin (deg->rad angle-deg)))
	(* magnitude (cos (deg->rad angle-deg)))))
;;; distance between point a and point b
;;; parameters must be lists of 2 numbers (x y)
(defun my-distance (a b)
  (sqrt (apply #'+
               (mapcar (lambda (x)
                         (expt x 2))
                       (xy-off-subtract a b)))))

(defun square-from-midpoint (point radius)
  (rectangle-from-midpoint-* (x point)
                             (y point)
                             (* radius 2)
                             (* radius 2)))

(defun deg->rad (degs)
  (* degs (/ pi 180)))

(defun rad->deg (rads)
  (* rads (/ 180 pi)))

(defun radial-point-from (p radius angle)
  (point :x (+ (* radius (sin (deg->rad angle))) (x p))
         :y (+ (* radius (cos (deg->rad angle))) (y p))))

(defun calc-angle (a b)
  (destructuring-bind (x y) (xy-off-subtract b a)
    (rad->deg (atan x y))))

(defun get-ticks ()
  (let ((ticks (shiftf *ticks* (sdl-get-ticks))))
    (* (- *ticks* ticks) 0.001)))

(defun relative-coords (x y)
"convert pixel coordinates to fractional coordinates"
    (list (/ x *screen-width*) (/ y *screen-height*)))
;;-------------------------------------------------------------------
;; O B J E C T S
;;; 
(defclass mob ()
  ((pos :initarg :pos :initform '(0.5 0.5) :accessor pos)
   (radius :initarg :radius :accessor radius)
   (velocity :initarg :velocity :initform '(0 0) :accessor velocity)))



(defmethod map-coords ((mob mob))
  "create a point from mob's fractional coordinates"
  (destructuring-bind (x y) (pos mob)
    (point :x (round (* x *screen-width*))
           :y (round (* y *screen-height*)))))

(defmethod map-radius ((mob mob))
  (round (* (radius mob) *screen-width*)))


(defmethod intersects-p ((mob mob) (other mob))
  (< (my-distance (pos mob) (pos other))
     (+ (radius mob) (radius other))))

(defmethod render ((mob mob))
  (values))


;;-------------------------------------------------------------------
;; R O C K
;;
(defclass rock (mob)
  ((size :initarg :size :initform 'big :reader size)
   (radii :initform nil :accessor radii)
   (rotation :initform (* (- (random 1.0) 0.5) 3) :accessor rotation)
   (direction :initform 0 :accessor direction)
   (pos :initform `(,(random 1.0) ,(random 1.0)))))

(defmethod initialize-instance :after ((rock rock) &key)
  (let ((radius (cdr (assoc (size rock)
                            '((big . 0.1) (medium . 0.06) (small . 0.015)))))
        (spd (cdr (assoc (size rock)
                         '((big . 0.05) (medium . 0.15) (small . 0.25))))))
    (setf (radius rock) radius)
    (setf (radii rock)
          (loop for i from 0 below *rock-sides*
            collect (round (* (- 1.0 (random 0.4))
                              (map-radius rock)))))
    ;(print (radii rock))
    (setf (velocity rock)
          `(,(- (random (* 2 spd)) spd) ,(- (random (* 2 spd)) spd)))))



(defmethod render ((rock rock))
   (draw-polygon (loop for i from 0
                      for r in (radii rock)
                  collect (radial-point-from (map-coords rock) r
                                             (+ (direction rock)
                                                (* i (/ 360 *rock-sides*)))))
                :color *white*))



;;-------------------------------------------------------------------
;; B U L L E T
;; 
(defclass missile (mob)
  ((radius :initform 0.001)
   (ship :initarg :ship :accessor ship)
   (timeout :initform 1 :accessor timeout)))


(defmethod render ((missile missile))
  (let ((coords (map-coords missile))
        (radius (map-radius missile)))
    (draw-circle coords radius
                 :color *red*)
    (when (super-p missile)
          (draw-circle coords (+ (random 3))
                       :color *magenta*))))

(defmethod super-p ((missile missile))
  (powerup-active-p (ship missile) 'super-missiles))


;;-------------------------------------------------------------------
;;E X P L O S I O N
;;


(defclass explosion (mob)
  ((radius :initform 0)))

(defmethod render ((explosion explosion))
  (let ((coords (map-coords explosion))
        (radius (map-radius explosion)))
    (draw-circle coords radius :color *explosion-color* :aa t)
    (draw-circle coords
                 (+ radius (random 3))
                 :color *explosion-color* :aa t)))


;;-------------------------------------------------------------------
(defclass powerup (mob)
  ((radius :initform 0.03)
   (age :initform 0 :accessor age)))


;;-------------------------------------------------------------------
(defclass missile-powerup (powerup) ())

;;-------------------------------------------------------------------
(defclass freeze-powerup (powerup) ())

;;-------------------------------------------------------------------
(defclass shield-powerup (powerup) ())

(defmethod render ((powerup shield-powerup))
  (let ((coords (map-coords powerup))
        (radius (map-radius powerup)))
    (draw-circle coords radius
                 :color *green*)
    (draw-polygon `(,(radial-point-from coords (round (* radius 0.8)) 40)
                    ,(radial-point-from coords (round (* radius 0.8)) 0)
                    ,(radial-point-from coords (round (* radius 0.8)) -40)
                    ,(radial-point-from coords (round (* radius 0.8)) -135)
                    ,(radial-point-from coords (round (* radius 0.8)) 135))
                  :color *white*)))

;;-------------------------------------------------------------------
;; S H I P
;;
(defclass ship (mob)
  ((timers :initform (make-hash-table) :accessor timers)
   (acceleration :initform '(0 0) :accessor acceleration-of)
   (direction :initform 0 :accessor direction)
   (radius :initform 0.02)
   (rotation :initform 0.0 :accessor rotation)) )

(defmethod render ((ship ship))
  (let* ((coords (map-coords ship))
         (radius (map-radius ship))
         (direction (direction ship))
         (nose (radial-point-from coords radius direction))
         (left (radial-point-from coords radius (- direction 140)))
         (right (radial-point-from coords radius (+ direction 140)))
	 (tail-right (radial-point-from coords (* -.5 radius) (- direction 40) ))
	 (tail-left (radial-point-from coords (* -.5 radius) (+ direction 40) ))
         (tail (radial-point-from coords (round (* radius 0.5)) (+ direction 180))))
    
    (draw-polygon (list nose left tail-left tail-right right)
                  :color *green* :aa t)
    (if *is-thrusting* ;draw thrusting jets
	(draw-line tail (radial-point-from tail 
					   (round (* radius (random 1.0))) 
					   (+ direction 180 (- (random 20) 10)))
		   :color *red* :aa nil))
   
    (when (powerup-active-p ship 'shield)
      (draw-circle coords
		   (round (+ radius (random 3)))
		   :color *blue*))))



(defmethod add-shield ((ship ship) &key (seconds 0))
  (if (powerup-active-p ship 'shield)
    (add-seconds (gethash 'shield (timers ship)) seconds)
    (setf (gethash 'shield (timers ship))
          (make-instance 'timer :seconds seconds))))

(defmethod thrust ((ship ship))
  "Set ship's acceleration using *thrust-factor* and ship's direction"
  (setf (acceleration-of ship) (xy-off-create (direction ship) *thrust-factor*)) )

(defmethod thrust-0 ((ship ship))
  "Set ship's acceleration to null"
  (setf (acceleration-of ship) '(0 0)))








(defmethod add-super-missiles ((ship ship) &key (seconds 0))
  (if (powerup-active-p ship 'super-missiles)
    (add-seconds (gethash 'super-missiles (timers ship)) seconds)
    (setf (gethash 'super-missiles (timers ship))
          (make-instance 'timer :seconds seconds))))



;;-------------------------------------------------------------------
(defclass timer ()
  ((remaining :initarg :seconds :initform 0 :accessor remaining)))

(defmethod done ((timer timer))
  (<= (ceiling (remaining timer)) 0))

(defmethod add-seconds ((timer timer) seconds)
  (incf (remaining timer) seconds))

(defmethod update-timer ((timer timer) time-delta)
  (unless (done timer)
    (decf (remaining timer) time-delta)))

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
   (lives :initform 0 :accessor lives)
   (paused :initform nil :accessor paused)))

(defmethod reset ((world world))
  (setf (mobs world) nil)
  (setf (ship world) nil)	
  (setf (paused world) nil)
  (setf (level world) 0)
  (setf (score world) 0)
  (setf (lives world) 0)
  (setf *ticks* (sdl-get-ticks))
  (setf (num-of-rocks world) 0)


)
;; Adding to world: cons to mob list and track ship and rock-count
;;
(defmethod add-to ((world world) (mob mob))
  (setf (mobs world) (cons mob (mobs world)))
  (values mob))

(defmethod add-to :after ((world world) (rock rock))
  (incf (num-of-rocks world)))

(defmethod add-to :after ((world world) (ship ship))
  (setf (ship world) ship))

;; Removing from world
(defmethod remove-from ((world world) (mob mob))
  (setf (mobs world) (remove mob (mobs world))))


(defmethod remove-from :after ((world world) (rock rock))
  (decf (num-of-rocks world)))


(defmethod remove-from :after ((world world) (ship ship))
  (setf (ship world) nil))

;; update missile
(defmethod update :before  ((missile missile) time-delta (world world))
  (let ((remaining (- (timeout missile) time-delta)))
    (if (<= remaining 0)
	(remove-from world missile)
	(setf (timeout missile) remaining)))
  (dolist (mob (mobs world))
    (when (and (not (eq missile  mob))
	       (intersects-p missile mob))
      (collide missile mob world))))








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
    (setf (num-of-rocks world) 0)	;ss 
    (dotimes (i level)
      (add-to world (make-instance 'rock)))
    (add-to world (or ship (make-instance 'ship))) ;keep existing ship or create a new one
    (add-shield (ship world) :seconds 6)))

(defmethod level-cleared-p ((world world))
  ;(print (num-of-rocks world))
  (< (num-of-rocks world) 1))


(defmethod update-world ((world world) time-delta)
  (maphash (lambda (name timer)
             (declare (ignore name))
             (update-timer timer time-delta))
           (timers world))
  (dolist (mob (mobs world))
    (update mob time-delta world))
  ;; start next level 3 seconds after clearing
  (when (level-cleared-p world)
    (after world
           'cleared
           :seconds 3
           :do (lambda ()
                 (incf (lives world))
                 (start-next-level world))))
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
                     (add-shield ship :seconds 6)))))))

(defmethod after ((world world) timer-name &key (seconds 0) do)
  (multiple-value-bind (timer exists) (gethash timer-name (timers world))
    (if exists
      (when (done timer)
        (remhash timer-name (timers world))
        (when (functionp do)
          (funcall do)))
      (setf (gethash timer-name (timers world))
            (make-instance 'timer :seconds seconds)))))





(defmethod frozen-p ((world world))
  (let ((timer (gethash 'freeze (timers world) nil)))
    (and timer
         (not (done timer)))))


(defmethod render-world ((world world))
  (clear-display *black*)
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
  (sdl-gfx:draw-string-solid-* (format nil
                                       "~a [Q]uit"
                                       (if (= (level world) 0)
                                           "[P]lay"
                                           "[P]ause"))
                               (- *screen-width* 127) 10
                               :color *green*)
  (if (= (level world) 0)
    ;; title screen
    (progn
      (sdl-gfx:draw-string-solid-* "Asteroids"
                                   (round (* 1/2 (- *screen-width* 81)))
                                   (round (* 1/4 (- *screen-height* 18)))
                                   :color *green*)
      (sdl-gfx:draw-string-solid-* (format nil
                                           "High score: ~d"
                                           (high-score world))
                                   (round (* 1/2 (- *screen-width* 171)))
                                   (round (* 1/2 (- *screen-height* 18)))
                                   :color *green*)
      (sdl-gfx:draw-string-solid-* (format nil "Best level: ~d" (best-level world))
                                   (round (* 1/2 (- *screen-width* 135)))
                                   (round (* 3/4 (- *screen-height* 18)))
                                   :color *green*))
    (progn
      ;; game world
      (set-clip-rect (rectangle :x 0 :y 0 :w *screen-width* :h *screen-height*)
                     :surface *default-display*)
      (dolist (mob (mobs world))
        (render mob))
      (set-clip-rect nil :surface *default-display*)
      ;; pause text
      (when (paused world)
        (sdl-gfx:draw-string-solid-* "PAUSED"
                                     (round (* 1/2 (- *screen-width* 54)))
                                     (round (* 1/2 (- *screen-height* 18)))
                                     :color *green*)))))

;;----------------------------


(defun random-powerup (&key pos)
  (make-instance (case (random 3)
                   (0 'missile-powerup)
                   (1 'freeze-powerup)
                   (2 'shield-powerup))
                 :pos pos))

(defmethod break-down ((rock rock) (world world))
  (with-slots ((pos pos) (size size)) rock
    (if (eq size 'small)
      ;; gradually reduce the probability of powerups appearing
      (if (< (random 100) (/ 100 (+ 4 (* (level world) 0.3))))
          `(,(random-powerup :pos pos))
          nil)
      (let ((smaller (cond
                     ((eq size 'big) 'medium)
                     ((eq size 'medium) 'small))))
        `(,(make-instance 'rock :pos pos :size smaller)
          ,(make-instance 'rock :pos pos :size smaller))))))


;; all mobs continue moving and wrapping...
(defmethod update ((mob mob) time-delta (world world))
  (setf (pos mob)
        (mapcar (lambda (x) (mod x 1))
                (xy-off-sum (pos mob) 
			    (xy-off-scale (velocity mob) time-delta)))))

;; rocks also rotate.  Note: if frozen, mob update not called.
(defmethod update ((rock rock) time-delta (world world))
  (declare (ignore time-delta))
  (when (not (frozen-p world))
    (incf (direction rock) (rotation rock))
    (call-next-method)))



(defmethod update ((explosion explosion) time-delta (world world))
  (when (> (incf (radius explosion) time-delta)
           *explosion-max-radius*)
    (remove-from world explosion)))

(defmethod update ((powerup powerup) time-delta (world world))
  (when (> (ceiling (incf (age powerup) time-delta))
           *powerup-max-age*) 
    (remove-from world powerup)))

(defmethod update :around ((ship ship) time-delta (world world))
  ;; lr-map contains left/right button mapping, for rollover...
  (cond 
    ((= *lr-map* 0) (setf (rotation (ship world)) 0))
    ((= *lr-map* 1) (setf (rotation (ship world)) 1)) 
    ((= *lr-map* 2) (setf (rotation (ship world)) -1)) 
    (t (setf (rotation (ship world)) 0)))

  (setf (direction (ship world))
	(+ (direction (ship world)) (* 200 time-delta (rotation (ship world)))))
  
  (if *is-thrusting*
      (thrust (ship world))
      (thrust-0 (ship world)))

  (setf (velocity ship)
	(xy-off-scale (xy-off-sum (velocity ship)
				  (acceleration-of ship))
		      *friction*))

  (maphash (lambda (name timer)
             (declare (ignore name))
             (update-timer timer time-delta))
           (timers ship))
  (call-next-method)
  (ship-moved world ship))


(defmethod collide ((mob mob) (other mob) (world world)) t)

(defmethod collide :before ((ship ship) (powerup shield-powerup) (world world))
  (add-shield ship :seconds 6))

(defmethod collide :before ((ship ship) (powerup missile-powerup) (world world))
  (add-super-missiles ship :seconds 6))



(defmethod shoot ((ship ship) (world world))
  "Fire a missile using ship's direction"
  (let ((missile (make-instance 'missile :pos (pos ship)
                                       :ship ship)))
    (setf (velocity missile) (xy-off-create (direction ship) *missile-velocity*))
    (add-to world missile)))




(defmethod add-score ((world world) (score number))
  (setf (high-score world)
        (max (incf (score world) score)
             (high-score world))))

(defmethod add-score ((world world) (powerup powerup))
  (add-score world (* (level world) 10)))

(defmethod add-score ((world world) (rock rock))
  (add-score world (cdr (assoc (size rock)
                               '((big . 1) (medium . 2) (small . 5))))))

(defmethod collide :before ((ship ship) (powerup powerup) (world world))
  (remove-from world powerup)
  (add-score world powerup))

(defmethod powerup-active-p ((ship ship) powerup)
  (let ((timer (gethash powerup (timers ship) nil)))
    (and timer
         (not (done timer)))))



(defmethod render ((powerup missile-powerup))
  (let ((coords (map-coords powerup))
        (radius (map-radius powerup)))
    (draw-circle coords radius
                 :color *magenta*)
    (draw-circle coords (round (* radius 0.3))
                 :color *white*)))

(defmethod add-freeze ((world world) &key (seconds 0))
  (if (frozen-p world)
    (add-seconds (gethash 'freeze (timers world)) seconds)
    (setf (gethash 'freeze (timers world))
          (make-instance 'timer :seconds seconds))))

(defmethod collide :before ((ship ship) (powerup freeze-powerup) (world world))
  (add-freeze world :seconds 6))

(defmethod render ((powerup freeze-powerup))
  (let ((coords (map-coords powerup))
        (radius (map-radius powerup)))
    (draw-circle coords radius
                 :color *cyan*)
    (draw-polygon (loop for i from 0 to 11
                    collect (radial-point-from coords
                                               (round (* radius (if (= (mod i 2) 0)
                                                                       0.7
                                                                       0.2)))
                                               (* i 30)))
                  :color *white*)))



(defmethod collide :before ((ship ship) (rock rock) (world world))
  (unless (powerup-active-p ship 'shield)
    (remove-from world ship)
    (add-to world (make-instance 'explosion :pos (pos ship)))
    (decf (lives world))))

(defmethod in-world-p ((world world) (mob mob))
  (find mob (mobs world)))

(defmethod ship-moved ((world world) (ship ship))
  (dolist (mob (mobs world))
    (when (and (not (eq ship mob))
               (intersects-p ship mob))
      (collide ship mob world))
    ;; if a collision destroyed the ship, stop checking for collisions
    (when (not (in-world-p world ship))
      (return ship))))







(defmethod collide :before ((missile missile) (rock rock) (world world))
  (remove-from world rock)
  (when (not (super-p missile))
    (remove-from world missile))
  (mapcar (lambda (mob)
            (add-to world mob))
          (break-down rock world))
  (add-to world (make-instance 'explosion :pos (pos rock)))
  (add-score world rock))




















(defun main ()
  (with-init ()
    (setf *window*
          (window 640 480
                  :title-caption "asteroids"
                  :icon-caption "asteroids"))
    (sdl-gfx:initialise-default-font sdl-gfx:*font-9x18*)
    (setf (frame-rate) 60)
    (clear-display *black*)
    (let ((world (make-instance 'world)) )
      (with-events ()
        (:quit-event () t)
	#+nil
        (:mouse-motion-event (:x x :y y)
			     (when (ship world)
			       (setf (direction (ship world))
				     (calc-angle (pos (ship world)) (relative-coords x y)))))
        #+nil
	(:mouse-button-down-event (:x x :y y)
				  )
        (:mouse-button-up-event ()
				)
        (:key-down-event (:key key)
			 (case key  
			   (:sdl-key-q (reset world))
			   (:sdl-key-a (setf *lr-map* (1+ *lr-map*)))		
			   (:sdl-key-f (setf *lr-map* (+ *lr-map* 2)))
			   (:sdl-key-j (setf *is-thrusting* t))
			   (:sdl-key-space (if (ship world)
					       (shoot (ship world) world)))))

	(:key-up-event (:key key)
		       (case key
			 (:sdl-key-escape (push-quit-event))
			 (:sdl-key-p (if (= (level world) 0)
					 (progn 
					   (reset world)
					   (start-next-level world))))
			 (:sdl-key-a (setf *lr-map* (1- *lr-map*)))
			 (:sdl-key-f (setf *lr-map* (- *lr-map* 2)))
			 (:sdl-key-j (setf *is-thrusting* nil))))
 
        (:idle () 
	       (update-world world (get-ticks))
	       (render-world world)
	       (update-display)))
      
      )))
