(c-declare "#include \"cosmic-sweep-backend.c\"")
;; Things we need to track:
;;   * player's ship
;;     + position
;;     + velocity
;;     + angle
;;   * player's bullets
;;     + position
;;     + velocity
;;     + angle
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

(define init-window (c-lambda () int "init_window"))
(define show-backbuf (c-lambda () void "show_backbuf"))
(define process-events (c-lambda () void "process_events"))
(define poll-keys (c-lambda () unsigned-int "poll_keys"))
(define clear-screen (c-lambda () void "clear_screen"))
(define (draw-pline vec len color)
  (let ((len (min len (u32vector-length vec))))
    ((c-lambda (scheme-object size_t unsigned-int) void "draw_pline((uint32_t *)___BODY_AS(___arg1,___tSUBTYPED), ___arg2, ___arg3);") vec len color)))


;; The display function composes a series of display lists which are then
;; fed to the /display backend/. The backend is mostly written in C and
;; handles the setup, teardown, draw calls, and event handling with
;; X11. Because this is a vector game, a display list consists of a
;; color and a list of =x,y= coordinate pairs specifying a
;; polyline which the display backend draws to the screen as connected
;; line segments.

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

(define (translate-point! pt1 pt2)
  (set-car! pt1 (+ (car pt1) (car pt2)))
  (set-cdr! pt1 (+ (cdr pt1) (cdr pt2))))

(define (translate-point pt1 pt2)
  (cons (+ (car pt1) (car pt2))
	(+ (cdr pt1) (cdr pt2))))

(define (translate-pline pline x y)
  (cons (car pline)
	(map
	 (lambda (coords)
	   (cons (+ x (car coords))
		 (+ y (cdr coords))))
	 (cdr pline))))

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


(define (ship-sprite config)
  (let* ((pos (object-config-pos config))
	 (x (inexact->exact (round (car pos))))
	 (y (inexact->exact (round (cdr pos))))
	 (angle (object-config-angle config)))
    (translate-pline
     (rotate-pline
      `(4
	(0 . -10)
	(-5 . 10)
	(0 . 8)
	(5 . 10)
	(0 . -10))
      angle)
     x y)))

(define (get-dlist state)
  (list (ship-sprite (game-state-ship state))))
(define JIFFY-DURATION 0.02)

(define CS-KEY-LEFT 1)
(define CS-KEY-RIGHT 2)
(define CS-KEY-THRUST 4)
(define CS-KEY-FIRE 8)

(define WINDOW-WIDTH 720)
(define WINDOW-HEIGHT 540)

(define (key-pressed? keys key)
  (not (zero? (bitwise-and keys key))))

(define THRUST-ACCEL (cons 0 -0.05))

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

(define (update-game! state)
  (let* ((ship (game-state-ship state))
	 (pos
	  (object-config-pos
	   ship))
	 (vel (object-config-vel ship))
	 (angle (object-config-angle ship))
	 (keys (poll-keys)))
    (translate-point! pos vel)
    (wrap-point! pos)
    (if (key-pressed? keys CS-KEY-THRUST)
	(translate-point! vel (rotate-point THRUST-ACCEL angle)))
    (if (key-pressed? keys 1)
	(object-config-angle-set! ship (- angle 0.05))
	(if (key-pressed? keys 2)
	    (object-config-angle-set! ship (+ angle 0.05))))))

(define (main)
  (let* ((tdiff 0)
	 (game-state (make-game-state
		      (make-object-config (cons 360 270) (cons 0 0) 0)
		      #f #f #f 0 0)))
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
