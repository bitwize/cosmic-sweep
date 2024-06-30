(c-declare "#include \"cosmic-sweep-backend.c\"")

(import (srfi 27))

(define JIFFY-DURATION 0.02)

(define CS-KEY-LEFT 1)
(define CS-KEY-RIGHT 2)
(define CS-KEY-THRUST 4)
(define CS-KEY-FIRE 8)

(define WINDOW-WIDTH 720)
(define WINDOW-HEIGHT 540)

(define BULLET-VEL 10)
(define BULLET-SPAWN-TIME 20)
(define BULLET-FLY-TIME 36)

(define THRUST-ACCEL (cons 0 -0.05))
(define ALIEN-VEL (cons 1. 0.))

(define CELL-THROB-TIME 16)

(define CELL-THROB-DIA-TABLE
  (vector
   7.5
   8.456708580912725
   9.267766952966369
   9.809698831278217
   10.
   9.809698831278217
   9.267766952966369
   8.456708580912725
   7.5
   6.543291419087276
   5.732233047033631
   5.190301168721784
   5.
   5.190301168721783
   5.732233047033631
   6.543291419087274))

(define SCORE-POS (cons 700 25))

(define SCORE-DIGIT-WIDTH 15)

(define DIGITS
  (vector
   '(((0 . 0)
      (10 . 0)
      (10 . -20)
      (0 . -20)
      (0 . 0)))
   '(((10 . 0)
      (10 . -20)))
   '(((10 . 0)
      (0 . 0)
      (0 . -10)
      (10 . -10)
      (10 . -20)
      (0 . -20)))
   '(((0 . 0)
      (10 . 0)
      (10 . -20)
      (0 . -20))
     ((0 . -10)
      (10 . -10)))
   '(((0 . -20)
      (0 . -10)
      (10 . -10))
     ((10 . -20)
      (10 . 0)))
   '(((0 . 0)
      (10 . 0)
      (10 . -10)
      (0 . -10)
      (0 . -20)
      (10 . -20)))
   '(((0 . -20)
      (0 . 0)
      (10 . 0)
      (10 . -10)
      (0 . -10)))
   '(((0 . -20)
      (10 . -20)
      (10 . 0)))
   '(((0 . 0)
      (10 . 0)
      (10 . -20)
      (0 . -20)
      (0 . 0))
     ((0 . -10)
      (10 . -10)))
   '(((10 . 0)
      (10 . -20)
      (0 . -20)
      (0 . -10)
      (10 . -10)))))

;; Things we need to track:
;;   * player's ship
;;     + position
;;     + velocity
;;     + angle
;;     + time since last bullet spawn
;;   * player's bullets
;;     + position
;;     + velocity
;;     + angle
;;     + time in flight
;;   * cells
;;     + position
;;     + velocity
;;   * aliens (if any)
;;     + position
;;     + velocity
;;   * # of lives
;;   * score

(define-structure
  object-config
  pos vel angle)

(define-structure
  game-state
  ship bullets cells aliens lives score)

(define-structure
  ship
  config shoot-timer current-bullet)

(define-structure
  bullet
  config index fly-timer)

(define-structure
  cell
  config throb-timer cluster)

(define-structure
  alien
  config)

(define init-window (c-lambda () int "init_window"))
(define show-backbuf (c-lambda () void "show_backbuf"))
(define process-events (c-lambda () void "process_events"))
(define poll-keys (c-lambda () unsigned-int "poll_keys"))
(define clear-screen (c-lambda () void "clear_screen"))
(define (draw-pline vec len color)
  (let ((len (min len (u32vector-length vec))))
    ((c-lambda (scheme-object size_t unsigned-int) void "draw_pline((uint32_t *)___BODY_AS(___arg1,___tSUBTYPED), ___arg2, ___arg3);") vec len color)))


;; A utility function ~draw-dlist~ iterates over a display list, packing
;; each coordinate pair into a buffer to be sent to ~draw-pline~ in the
;; display backend. In this way we can pass lines to be drawn from Scheme
;; to X11 without having a complex FFI to X11 in Scheme-land.

(define (draw-dlist dlist)
  (let* ((dv (make-u32vector 16384)))
    (for-each
     (lambda (pline)
       (let*
	   ((count 
	     (let loop ((l (cdr pline))
			(n 0))
	       (cond
		((null? l) n)
		(else
		 (u32vector-set!
		  dv
		  n
		  (bitwise-ior
		   (arithmetic-shift
		    (bitwise-and (caar l) #xffff)
		    16)
		   (bitwise-and (cdar l) #xffff)))
		 (loop (cdr l) (+ n 1)))))))
	 (draw-pline dv count (car pline))))
     dlist)))

;; These utility functions do common operations like translation and
;; rotation on points, or entire polylines, to allow sprites to be
;; moved and rotated. Maybe someday I'll put in actual linear
;; transforms...

(define (translate-point! pt1 pt2)
  (set-car! pt1 (+ (car pt1) (car pt2)))
  (set-cdr! pt1 (+ (cdr pt1) (cdr pt2))))

(define (translate-point pt1 pt2)
  (cons (+ (car pt1) (car pt2))
	(+ (cdr pt1) (cdr pt2))))

(define (translate-pline* pline x y)
  (cons (car pline)
	(map
	 (lambda (coords)
	   (cons (+ x (car coords))
		 (+ y (cdr coords))))
	 (cdr pline))))

(define (translate-pline pline point)
  (translate-pline* pline (car point) (cdr point)))

(define (rotate-point coords angle)
  (let ((x (car coords)) (y (cdr coords)))
    (cons
     (- (* x (cos angle)) (* y (sin angle)))
     (+ (* x (sin angle)) (* y (cos angle))))))

(define (rotate-point! coords angle)
  (let ((x (car coords)) (y (cdr coords)))
    (set-car!
     coords
     (inexact->exact (round (- (* x (cos angle)) (* y (sin angle))))))
    (set-cdr!
     coords
     (inexact->exact (round (+ (* x (sin angle)) (* y (cos angle))))))))

(define (rotate-pline pline angle)
  (cons (car pline)
	(map
	 (lambda (coords)
	   (let* ((rot (rotate-point coords angle)))
	     (cons (inexact->exact (round (car rot)))
		   (inexact->exact (round (cdr rot))))))
	 (cdr pline))))

(define (point->integer-coords pt)
  (cons (inexact->exact (round (car pt)))
	(inexact->exact (round (cdr pt)))))

(define (ship-sprite config)
  (let* ((pos (object-config-pos config))
	 (x (inexact->exact (round (car pos))))
	 (y (inexact->exact (round (cdr pos))))
	 (angle (object-config-angle config)))
    (translate-pline*
     (rotate-pline
      `(4
	(0 . -10)
	(-5 . 10)
	(0 . 8)
	(5 . 10)
	(0 . -10))
      angle)
     x y)))

(define (bullet-sprite config)
  (let* ((pos (object-config-pos config))
	 (x (inexact->exact (round (car pos))))
	 (y (inexact->exact (round (cdr pos)))))
    (translate-pline*
      `(7
	(0 . 0)
	(0 . 0))
      x y)))

(define (cell-sprites cell)
  (let* ((config (cell-config cell))
	 (pos (object-config-pos config))
	 (x (inexact->exact (round (car pos))))
	 (y (inexact->exact (round (cdr pos))))
	 (cell-pts '((1 . 0)
		     (.8660254037844387 . .49999999999999994)
		     (.5000000000000001 . .8660254037844386)
		     (6.123233995736766e-17 . 1.)
		     (-.4999999999999998 . .8660254037844387)
		     (-.8660254037844385 . .5000000000000003)
		     (-1. . 1.2246467991473532e-16)
		     (-.8660254037844388 . -.4999999999999997)
		     (-.5000000000000004 . -.8660254037844384)
		     (-1.8369701987210297e-16 . -1.)
		     (.49999999999999933 . -.866025403784439)
		     (.8660254037844384 . -.5000000000000004)
		     (1 . 0)))
	 (cell-sp
	  (map
	   (lambda (x)
	     (cons
	      (inexact->exact (round (*
				      (vector-ref
				       CELL-THROB-DIA-TABLE
				       (cell-throb-timer cell))
				      (car x))))
	      (inexact->exact (round (*
				      (vector-ref
				       CELL-THROB-DIA-TABLE
				       (cell-throb-timer cell))
				      (cdr x))))))
	   cell-pts))
	 (clus (cell-cluster cell)))
    (cond
     ((= clus 1)
      (list
       (translate-pline*
	(cons 5 cell-sp)
	x y)))
     ((= clus 2)
      (list
       (translate-pline*
	(cons 3 cell-sp)
	x y)
       (translate-pline*
	(cons 3 cell-sp)
	(- x 10)
	y)))
     ((= clus 3)
      (list
       (translate-pline*
	(cons 6 cell-sp)
	x y)
       (translate-pline*
	(cons 6 cell-sp)
	(- x 10)
	y)
       (translate-pline*
	(cons 6 cell-sp)
	(- x 5)
	(inexact->exact (round (- y (* 5 (sqrt 0.5))))))))
     (else '()))))

(define (alien-sprite alien)
  (let* ((alien-pts '((10. . 0)
		      (8.660254037844387 . 6.499999999999999)
		      (5.000000000000001 . 11.2583302491977)
		      (6.123233995736766e-16 . 13.)
		      (-4.999999999999998 . 11.258330249197703)
		      (-8.660254037844385 . 6.500000000000005)
		      (-10. . 1.5920408388915593e-15)
		      (-8.660254037844388 . -6.499999999999997)
		      (-5.000000000000004 . -11.258330249197699)
		      (-1.8369701987210297e-15 . -13.)
		      (4.9999999999999933 . -11.258330249197708)
		      (8.660254037844384 . -6.500000000000006)
		      (10. . 0)))
	 (alien-eye-pts '((4 . 0)
			  (3.464101615137755 . 1.9999999999999998)
			  (2.0000000000000004 . 3.4641016151377544)
			  (2.4492935982947064e-16 . 4.)
			  (-1.9999999999999991 . 3.464101615137755)
			  (-3.464101615137754 . 2.0000000000000013)
			  (-4. . 4.898587196589413e-16)
			  (-3.4641016151377553 . -1.999999999999999)
			  (-2.0000000000000018 . -3.4641016151377535)
			  (-7.347880794884119e-16 . -4.)
			  (1.9999999999999973 . -3.464101615137756)
			  (3.4641016151377535 . -2.0000000000000018)
			  (4 . 0)))
	 (alien-antenna1-pts '((5 . -11)
			       (3 . -14)
			       (3 . -16)
			       (6 . -20)))
	 (alien-antenna2-pts '((5 . 11)
			       (3 . 14)
			       (3 . 16)
			       (6 . 20)))
	 (config (alien-config alien))
	 (pos (object-config-pos config))
	 (x (inexact->exact (round (car pos))))
	 (y (inexact->exact (round (cdr pos))))
	 (angle (object-config-angle config)))
    (list
     (translate-pline*
      (rotate-pline
       (cons 1 alien-pts)
       angle)
      x y)
     (translate-pline*
      (cons 1 (map point->integer-coords alien-eye-pts))
      x y)
     (translate-pline*
      (rotate-pline
       (cons 1 alien-antenna1-pts)
       angle)
      x y)
     (translate-pline*
      (rotate-pline
       (cons 1 alien-antenna2-pts)
       angle)
      x y))))

(define (score-sprite score)
  (define (score-sprite1 score accum point)
    (cond
     ((zero? score)
      (if (null? accum)
	  (map
	   (lambda (x)
	     (translate-pline
	      (cons 7 x)
	      point))
	   (vector-ref DIGITS 0))
	  accum))
     (else
      (let* ((digit (abs (remainder score 10))))
	(score-sprite1
	 (quotient score 10)
	 (append
	  (map
	   (lambda (x)
	     (translate-pline
	      (cons 7 x)
	      point))
	   (vector-ref DIGITS digit))
	  accum)
	 (cons (- (car point) SCORE-DIGIT-WIDTH) (cdr point)))))))
  (score-sprite1 score '() SCORE-POS))
	

;; The display function composes a display list from the current game
;; state which is then fed to the /display backend/. The backend is
;; mostly written in C and handles the setup, teardown, draw calls,
;; and event handling with X11. Because this is a vector game, each
;; element of a display list consists of a color and a list of =x,y=
;; coordinate pairs specifying a polyline which the display backend
;; draws to the screen as connected line segments.

(define (get-dlist state)
  (define (get-bullet-sprites bullets)
    (let* ((l (vector-length bullets)))
      (let loop ((i 0)
		 (dl '()))
	(cond ((>= i l) dl)
	      (else
	       (loop (+ i 1)
		     (if (not (vector-ref bullets i))
			 dl
			 (cons (bullet-sprite
				(bullet-config (vector-ref bullets i)))
			       dl))))))))
  
  (define (get-cell-sprites cells)
    (let* ((l (vector-length cells)))
      (let loop ((i 0)
		 (dl '()))
	(cond ((>= i l) dl)
	      (else
	       (loop (+ i 1)
		     (if (not (vector-ref cells i))
			 dl
			 (append (cell-sprites
				  (vector-ref cells i))
				 dl))))))))
  (define (get-alien-sprites aliens)
    (let* ((l (vector-length aliens)))
      (let loop ((i 0)
		 (dl '()))
	(cond ((>= i l) dl)
	      (else
	       (loop (+ i 1)
		     (if (not (vector-ref aliens i))
			 dl
			 (append (alien-sprite
				  (vector-ref aliens i))
				 dl))))))))
  
  (append
   (list
    (ship-sprite (ship-config (game-state-ship state))))
   (get-cell-sprites (game-state-cells state))
   (get-bullet-sprites (game-state-bullets state))
   (get-alien-sprites (game-state-aliens state))
   (score-sprite (game-state-score state))))

(define (key-pressed? keys key)
  (not (zero? (bitwise-and keys key))))

;; Wrap a point's coordinates to be within the bounds of the window.

(define (wrap-point! pt)
  (let loop ()
    (if (>= (car pt) WINDOW-WIDTH)
	(begin (set-car! pt (- (car pt) WINDOW-WIDTH))
	       (loop))))
  (let loop ()
    (if (>= (cdr pt) WINDOW-HEIGHT)
	(begin (set-cdr! pt (- (cdr pt) WINDOW-HEIGHT))
	       (loop))))
  (let loop ()
    (if (< (car pt) 0)
	(begin (set-car! pt (+ (car pt) WINDOW-WIDTH))
	       (loop))))
  (let loop ()
    (if (< (cdr pt) 0)
	(begin (set-cdr! pt (+ (cdr pt) WINDOW-HEIGHT))
	       (loop)))))

;; Simple collision function. Determines if pt1 collides with pt2, to
;; a certain tolerance. Uses simple AABB around pt2.

(define (collide? pt1 pt2 tolerance)
  (let* ((x1 (car pt1)) (y1 (cdr pt1))
	 (x2 (car pt2)) (y2 (cdr pt2)))
    (and
     (>= x1 (- x2 tolerance))
     (< x1 (+ x2 tolerance))
     (>= y1 (- y2 tolerance))
     (< y1 (+ y2 tolerance)))))


(define (vector-for-each/index f v)
  (let* ((l (vector-length v)))
    (do ((i 0 (+ i 1)))
	((>= i l))
      (f i (vector-ref v i)))))

(define (bullet-check proc bullets conf tolerance)
  (vector-for-each/index
   (lambda (j b)
     (if b
	 (let* ((b-pos (object-config-pos (bullet-config b))))
	   (if (collide? b-pos (object-config-pos conf) tolerance)
	       (proc j b)))))
   bullets))

;; Update the game state. Very imperative and mutates the state.

(define (update-game! state)
  (let* ((ship (game-state-ship state))
	 (shipc (ship-config ship))
	 (pos
	  (object-config-pos
	   shipc))
	 (vel (object-config-vel shipc))
	 (angle (object-config-angle shipc))
	 (bullets (game-state-bullets state))
	 (cells (game-state-cells state))
	 (aliens (game-state-aliens state))
	 (keys (poll-keys)))
    ;; move ship
    
    (translate-point! pos vel)

    ;; move bullets, update bullet counters
    (vector-for-each/index
     (lambda (i b)
       (if b
	   (let* 
	       ((conf (bullet-config b)))
	     (translate-point!
	      (object-config-pos conf)
	      (object-config-vel conf))
	     (wrap-point! (object-config-pos conf))
	     (bullet-fly-timer-set! b (+ (bullet-fly-timer b) 1))
	     (if (>= (bullet-fly-timer b) BULLET-FLY-TIME)
		 (vector-set! bullets i #f)))))
     bullets)
    ;; move and animate "cells"

    ;; Cells have an associated "cell throb timer" that is initialized
    ;; randomly and loops from 0-15, incrementing each frame, that
    ;; controls the "throbbing" animation of the cells. A bit of
    ;; Brownian motion is also applied to each cell so they wiggle
    ;; about.

    (vector-for-each/index
     (lambda (i c)
       (if c
	   (let* 
	       ((conf (cell-config c))
		(brownian-displacement (lambda () (* 0.25 (- (random-integer 3) 1)))))
	     (translate-point!
	      (object-config-pos conf)
	      (object-config-vel conf))
	     (translate-point!
	      (object-config-pos conf)
	      (cons
	       (brownian-displacement)
	       (brownian-displacement)))
	     (wrap-point! (object-config-pos conf))
	     (cell-throb-timer-set! c (+ (cell-throb-timer c) 1))
	     (if (>= (cell-throb-timer c) CELL-THROB-TIME)
		 (cell-throb-timer-set! c 0))
	     ;; Check for collision between cell and a bullet. Delete
	     ;; both if collision found.
	     (bullet-check
	      (lambda (j b)
		(if (>
		     (cell-cluster c)
		     1)
		    (cell-cluster-set! c (- (cell-cluster c) 1))
		    (vector-set! cells i #f))
		(vector-set! bullets j #f)
		(game-state-score-set! state
				       (+
					(game-state-score state)
					50)))
	      bullets
	      (cell-config c)
	      10.0)
	     ;; Check for cell-cell collision. Make cells stick together.
	     (vector-for-each/index
	      (lambda (j c2)
		(if (and c2 (not (= j i)))
		    (let* ((c-pos (object-config-pos conf))
			   (c2-pos (object-config-pos (cell-config c2))))
		      (if (collide? c2-pos c-pos 15.0)
			  (begin
			    (cell-cluster-set!
			     c
			     (+ (cell-cluster c)
				(cell-cluster c2)))
			    (if (> (cell-cluster c) 3)
				(begin
				  (vector-set! cells i #f)
				  (vector-set! aliens i
					       (make-alien
						(cell-config c)))))
			    (vector-set! cells j #f)))
		      (if (collide? c2-pos c-pos 150.0)
			  (begin
			    (let* ((roll (random-integer 150)))
			      (if (< roll (cell-cluster c2))
				  (let* ((dx (- (car c2-pos) (car c-pos)))
					 (dy (- (cdr c2-pos) (cdr c-pos)))
					 (dist (sqrt (+ (* dx dx) (* dy dy)))))
				    (if (> dist 0)
					(translate-point! c-pos (cons (/ dx dist)  (/ dy dist))))))))))))
	      cells))))
     cells)
    (vector-for-each/index
     (lambda (i a)
       (if a
	   (let* ((conf (alien-config a))
		  (apos (object-config-pos conf)))
	     (bullet-check
	      (lambda (j b)
		(vector-set! aliens i #f)
		(vector-set! bullets j #f)
		(game-state-score-set! state
				       (+
					(game-state-score state)
					200)))
	      bullets
	      conf
	      15.0)
	     (translate-point! apos (object-config-vel conf))
	     (object-config-vel-set! conf
				     (rotate-point ALIEN-VEL (object-config-angle conf)))
	     (object-config-angle-set! conf
				       (atan (- (cdr pos) (cdr apos))
					     (- (car pos) (car apos))))
	     (wrap-point! apos))))
     aliens)
    (wrap-point! pos)
    (if (key-pressed? keys CS-KEY-FIRE)
	(begin
	  (if (zero? (ship-shoot-timer ship))
	      (let* ((b (vector-ref bullets (ship-current-bullet ship))))
		(if (not b)
		    (begin
		      (vector-set!
		       bullets
		       (ship-current-bullet ship)
		       (make-bullet
			(let* ((bullet-ang (- angle 1.570796)))
			  (make-object-config
			   (cons (car pos) (cdr pos))
			   (cons (* BULLET-VEL (cos bullet-ang))
				 (* BULLET-VEL (sin bullet-ang)))
			   angle))
			(ship-current-bullet ship)
			0))
		      (ship-current-bullet-set!
		       ship
		       (remainder
			(+ (ship-current-bullet ship) 1)
			(vector-length bullets)))))))
	  (ship-shoot-timer-set! ship (remainder
				       (+ (ship-shoot-timer ship) 1)
				       BULLET-SPAWN-TIME)))
	(ship-shoot-timer-set! ship 0))
    (if (key-pressed? keys CS-KEY-THRUST)
	(translate-point! vel (rotate-point THRUST-ACCEL angle)))
    (if (key-pressed? keys 1)
	(object-config-angle-set! shipc (- angle 0.05))
	(if (key-pressed? keys 2)
	    (object-config-angle-set! shipc (+ angle 0.05))))))

;; Generate an array of cells at random locations.

(define (make-cells n)
  (let* ((v (make-vector n)))
    (do ((i 0 (+ i 1)))
	((>= i n))
      (vector-set! v i
		   (make-cell
		    (make-object-config
		     (cons
		      (random-integer WINDOW-WIDTH)
		      (random-integer WINDOW-HEIGHT))
		     (cons 0 0)
		     0)
		    (random-integer CELL-THROB-TIME)
		    1)))
    v))

(define (main)
  (let* ((tdiff 0)
	 (game-state (make-game-state
		      (make-ship
		       (make-object-config (cons 360 270) (cons 0 0) 0)
		       0 0)
		      (make-vector 5 #f)
		      (make-cells 50) (make-vector 50 #f) 0 0)))
    (let loop ((t1 (time->seconds (current-time)))
	       (t2 (time->seconds (current-time))))
      (let loop ((tdiff2 (+ tdiff (- t2 t1))))
	(cond
	 ((< tdiff2 JIFFY-DURATION) (set! tdiff tdiff2))
	 (else
	  (update-game! game-state)
	  (loop (- tdiff2 JIFFY-DURATION)))))
      (process-events)
      (clear-screen)
      (draw-dlist (get-dlist game-state))
      (show-backbuf)
      (thread-sleep! JIFFY-DURATION)
      (loop t2 (time->seconds (current-time))))))
