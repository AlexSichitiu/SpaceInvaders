;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |HtC Simple Data Final Project - Space Invaders|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; ====================
;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-SPEED 3)  ;speed (not velocity) in pixels per tick
(define TANK-SPEED 5)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define SPAWN-PROB 1)     ;percentage chance of invader spawing per tick

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; ====================
;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))


(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))


(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right

#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


;; ListOfInvader is one of:
;; - empty
;; - (cons Invader ListOfInvader)
;; interp. a list of current invaders in the game
(define LOI1 empty)
(define LOI2 (list I1))
(define LOI3 (list I1 (make-invader 100 100 12)))

#;
(define (fn-for-loinvader loinvader)
  (cond [(empty? loinvader) ...]
        [else 
         (... (fn-for-invader (first loinvader))
              (fn-for-loinvader (rest loinvader)))]))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                               ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


;; ListOfMissile is one of:
;; - empty
;; - (cons Missile ListOfMissile)
;; interp. a list of current missiles in the game
(define LOM1 empty)
(define LOM2 (list M1))
(define LOM3 (list M1 M2))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) ...]
        [else 
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))



;; ====================
;; Functions:


;; Game -> Game
;; call to run a Space Invaders-inspired game; start the world with (main (make-game empty empty (make-tank (/ WIDTH 2) 0)))

(define (main s)
  (big-bang s                  ; Game
    (on-tick   update-game)    ; Game -> Game
    (to-draw   render)         ; Game -> Image
    (stop-when game-over?)     ; Game -> Boolean
    (on-key    control-tank))) ; Game KeyEvent -> Game


;; Game -> Game
;; update the given game status by: updating the position and velocities of current entities,
;;                                  removing destroyed missles/invaders 
;;                                  spawning new invaders
(check-random (update-game (make-game empty empty (make-tank 0 0)))
              (make-game (if (<= (random 100) SPAWN-PROB)
                             (cons (make-invader (random WIDTH) 0 1) empty)
                             empty)
                         empty
                         (make-tank 0 0)))
(check-random (update-game (make-game (list (make-invader 100 100 1)
                                            (make-invader 200 200 1))
                                      (list (make-missile 0 -1)
                                            (make-missile 100 110)
                                            (make-missile 50 50))
                                      (make-tank 0 1)))
              (make-game (if (<= (random 100) SPAWN-PROB)
                             (cons (make-invader (random WIDTH) 0 1)
                                   (cons (make-invader (+ 200 INVADER-SPEED) (+ 200 INVADER-SPEED) 1) empty))
                             (list (make-invader (+ 200 INVADER-SPEED) (+ 200 INVADER-SPEED) 1)))
                         (list (make-missile 50 (- 50 MISSILE-SPEED)))
                         (make-tank (* TANK-SPEED 1) 1)))
                         
                                                                                                                           
;(define (update-game s) G1) ;stub

;<template as function composition>

(define (update-game s)
  (spawn-invader (destroy (move s))))


;; Game -> Game
;; randomly spawn new invaders at the top of the screen at a random x that is on Natural[0, WIDTH]. Uses SPAWN-PROB.
(check-random (spawn-invader G0) (make-game (if (<= (random 100) SPAWN-PROB)
                                                (list (make-invader (random WIDTH) 0 1))
                                                empty)
                                            empty
                                            T0))
                                            
;(define (spawn-invader s) G1) ;stub

;<used template from Game>

(define (spawn-invader s)
  (if (<= (random 100) SPAWN-PROB)
      (make-game (cons (make-invader (random WIDTH) 0 1)
                       (game-invaders s))
                 (game-missiles s)
                 (game-tank s))
      s))


;; Game -> Game
;; remove invaders and missles that have collided with each other and remove missiles that have flown off-screen from the given game
(check-expect (destroy (make-game empty empty (make-tank 0 0)))
              (make-game empty empty (make-tank 0 0)))
(check-expect (destroy (make-game (list (make-invader 100 100 -1)
                                        (make-invader 50 50 1))
                                  (list (make-missile 100 100)
                                        (make-missile 50 HEIGHT))
                                  (make-tank 100 1)))
              (make-game (list (make-invader 50 50 1))
                         (list (make-missile 50 HEIGHT))
                         (make-tank 100 1)))
(check-expect (destroy (make-game (list (make-invader 100 100 -1)
                                        (make-invader 50 50 1))
                                  (list (make-missile 100 100)
                                        (make-missile 50 (- 0 1)))
                                  (make-tank 100 1)))
              (make-game (list (make-invader 50 50 1))
                         empty
                         (make-tank 100 1)))
(check-expect (destroy (make-game (list (make-invader 100 100 -1)
                                        (make-invader 50 50 1))
                                  (list (make-missile 200 200)
                                        (make-missile 10 10))
                                  (make-tank 100 1)))
              (make-game (list (make-invader 100 100 -1)
                               (make-invader 50 50 1))
                         (list (make-missile 200 200)
                               (make-missile 10 10))
                         (make-tank 100 1)))
(check-expect (destroy (make-game (list (make-invader 100 100 -1)
                                        (make-invader 50 50 1))
                                  (list (make-missile 200 200)
                                        (make-missile 10 -1))
                                  (make-tank 100 1)))
              (make-game (list (make-invader 100 100 -1)
                               (make-invader 50 50 1))
                         (list (make-missile 200 200))
                         (make-tank 100 1)))

;(define (destroy s) G1) ;stub

;<template as function composition>

(define (destroy s)
  (collision (offscreen-missiles s)))


;; Game -> Game
;; checks whether a missile and an invader have collided and removes both if they have
(check-expect (collision (make-game empty empty (make-tank 0 0)))
              (make-game empty empty (make-tank 0 0)))
(check-expect (collision (make-game (list (make-invader 0 0 1))
                                    (list (make-missile (/ WIDTH 2) HEIGHT))
                                    (make-tank 0 0)))
              (make-game (list (make-invader 0 0 1))
                         (list (make-missile (/ WIDTH 2) HEIGHT))
                         (make-tank 0 0)))
(check-expect (collision (make-game (list (make-invader 0 0 1)
                                          (make-invader 100 100 1))
                                    (list (make-missile (/ WIDTH 2) HEIGHT)
                                          (make-missile 100 100))
                                    (make-tank 0 0)))
              (make-game (list (make-invader 0 0 1))
                         (list (make-missile (/ WIDTH 2) HEIGHT))
                         (make-tank 0 0)))


;(define (collision s) G0) ;stub

;<used template from function composition>

(define (collision s)
  (make-game (collision-invaders (game-invaders s) (game-missiles s))
             (collision-missiles (game-invaders s) (game-missiles s))
             (game-tank s)))


;; ListOfInvader ListOfMissile -> ListOfInvader
;; cross reference a list of invaders against a list of missles to see if there are any collisions and produce a list with collided invaders removed
;; CROSS PRODUCT OF TYPE COMMENTS TABLE
;;
;;                                         loinvader
;;                                    empty           (cons Invader ListOfInvader)                
;;                                                   
;; l           empty                 loinvader      loinvader
;; o                                             -----------------
;; m   (cons Missile ListOfMissile)              |  (if (check-list-invader (first loinvader) lom)
;;                                   loinvader   |      (collision-invaders (rest loinvader) lom)
;;                                               |      (cons (first loinvader)(collision-invaders (rest loinvader) lom))
(check-expect (collision-invaders empty empty) empty)
(check-expect (collision-invaders (list (make-invader 0 0 -1)) empty)
              (list (make-invader 0 0 -1)))
(check-expect (collision-invaders empty (list (make-missile 0 0))) empty)
(check-expect (collision-invaders (list (make-invader 0 0 1))
                                  (list (make-missile 0 0)))
              empty)
(check-expect (collision-invaders (list (make-invader 0 0 1))
                                  (list (make-missile 100 100)))
              (list (make-invader 0 0 1)))
(check-expect (collision-invaders (list (make-invader 0 0 1)
                                        (make-invader 100 100 -1))
                                  (list (make-missile 0 0)))
              (list (make-invader 100 100 -1)))
(check-expect (collision-invaders (list (make-invader 0 0 1)
                                        (make-invader 0 0 1))
                                  (list (make-missile 0 0)))
              empty)
(check-expect (collision-invaders (list (make-invader 0 0 1)
                                        (make-invader 50 50 1))
                                  (list (make-missile 100 100)
                                        (make-missile 0 0)))
              (list (make-invader 50 50 1)))
(check-expect (collision-invaders (list (make-invader 0 0 1)
                                        (make-invader 50 50 1))
                                  (list (make-missile 100 100)
                                        (make-missile 50 50)))
              (list (make-invader 0 0 1)))
(check-expect (collision-invaders (list (make-invader 100 100 1)
                                        (make-invader 50 50 1))
                                  (list (make-missile 100 100)
                                        (make-missile 50 50)))
              empty)

;(define (collision-invaders loinvader lom) empty) ;stub

;<used template from functions on 2 one-of data>

(define (collision-invaders loinvader lom)
  (cond [(or (empty? loinvader) (empty? lom))
         loinvader]
        [else
         (if (check-list-invader (first loinvader) lom)
             (collision-invaders (rest loinvader) lom)
             (cons (first loinvader)(collision-invaders (rest loinvader) lom)))]))


;; Invader ListOfMissle -> Boolean
;; produces true if the given invader collides with any of the missiles in the list
(check-expect (check-list-invader (make-invader 0 0 1) empty) false)
(check-expect (check-list-invader (make-invader 0 0 1) (list (make-missile 0 0))) true)
(check-expect (check-list-invader (make-invader 0 0 1) (list (make-missile 100 100))) false)
(check-expect (check-list-invader (make-invader 0 0 1) (list (make-missile 100 100) (make-missile 0 0))) true)
(check-expect (check-list-invader (make-invader 0 0 1) (list (make-missile 100 100) (make-missile 50 50))) false)

;(define (check-list-invader invader lom) false) ;stub

;<used template from ListOfMissile with extra parameter>

(define (check-list-invader invader lom)
  (cond [(empty? lom) false]
        [else 
         (or  (collision? invader (first lom))
              (check-list-invader invader (rest lom)))]))

;; Invader Missile -> Boolean
;; produce true if given missile and invader have both x and y within HIT-RANGE of one another. If x or y distance is exactly HIT-RANGE, produce true.
(check-expect (collision? (make-invader 0 0 1)
                          (make-missile 100 100))
              (and (>= HIT-RANGE (abs (- 100 0)))
                   (>= HIT-RANGE (abs (- 100 0)))))
(check-expect (collision? (make-invader 0 0 1) (make-missile 0 0)) true)
(check-expect (collision? (make-invader (/ HIT-RANGE 2) (/ HIT-RANGE 2) 1)
                          (make-missile 0 0))
              true)
(check-expect (collision? (make-invader HIT-RANGE HIT-RANGE 1) (make-missile 0 0)) true)
(check-expect (collision? (make-invader HIT-RANGE 100 1) (make-missile 0 0)) false)
(check-expect (collision? (make-invader 100 HIT-RANGE 1) (make-missile 0 0)) false)

;(define (collision? invader m) false) ;stub

(define (collision? invader m)
  (and (>= HIT-RANGE
           (abs (- (invader-x invader)
                   (missile-x m))))
       (>= HIT-RANGE
           (abs (- (invader-y invader)
                   (missile-y m)))))) 


;; ListOfInvader ListOfMissile -> ListOfInvader
;; cross reference a list of missiles against a list of invaders to see if there are any collisions and produce a list with collided missiles removed
;; CROSS PRODUCT OF TYPE COMMENTS TABLE
;;
;;                                         loinvader
;;                                    empty           (cons Invader ListOfInvader)                
;;                                                   
;; l           empty                 lom             lom
;; o                                             -----------------
;; m   (cons Missile ListOfMissile)              |  (if (check-list-missile (first lom) loinvader)
;;                                   lom         |      (collision-missiles loinvader (rest lom))
;;                                               |      (cons (first lom)(collision-missiles loinvader (rest lom)))
(check-expect (collision-missiles empty empty)
              empty)
(check-expect (collision-missiles (list (make-invader 0 0 -1)) empty)
              empty)
(check-expect (collision-missiles empty (list (make-missile 0 0)))
              (list (make-missile 0 0)))
(check-expect (collision-missiles (list (make-invader 0 0 1))
                                  (list (make-missile 0 0)))
              empty)
(check-expect (collision-missiles (list (make-invader 0 0 1))
                                  (list (make-missile 100 100)))
              (list (make-missile 100 100)))
(check-expect (collision-missiles (list (make-invader 100 100 -1)
                                        (make-invader 0 0 1))
                                  (list (make-missile 0 0)))
              empty)
(check-expect (collision-missiles (list (make-invader 0 0 1))
                                  (list (make-missile 0 0)
                                        (make-missile 0 0)))
              empty)
(check-expect (collision-missiles (list (make-invader 0 0 1)
                                        (make-invader 50 50 1))
                                  (list (make-missile 50 50)
                                        (make-missile 0 0)))
              empty)
(check-expect (collision-missiles (list (make-invader 0 0 1)
                                        (make-invader 50 50 1))
                                  (list (make-missile 100 100)
                                        (make-missile 50 50)))
              (list (make-missile 100 100)))
(check-expect (collision-missiles (list (make-invader 100 100 1)
                                        (make-invader 50 50 1))
                                  (list (make-missile 100 100)
                                        (make-missile 50 50)))
              empty)

;(define (collision-missiles loinvader lom) empty) ;stub

;<used template from functions on 2 one-of data>

(define (collision-missiles loinvader lom)
  (cond [(or (empty? loinvader) (empty? lom))
         lom]
        [else
         (if (check-list-missile (first lom) loinvader)
             (collision-missiles loinvader (rest lom))
             (cons (first lom)(collision-missiles loinvader (rest lom))))]))


;; Missile ListOfInvader -> Boolean
;; produces true if the given missile collides with any of the invaders in the list
(check-expect (check-list-missile (make-missile 0 0) empty) false)
(check-expect (check-list-missile (make-missile 0 0) (list (make-invader 0 0 1))) true)
(check-expect (check-list-missile (make-missile 0 0) (list (make-invader 100 100 1))) false)
(check-expect (check-list-missile (make-missile 0 0) (list (make-invader 100 100 1) (make-invader 0 0 1))) true)
(check-expect (check-list-missile (make-missile 0 0) (list (make-invader 100 100 1) (make-invader 50 50 1))) false)

;(define (check-list-missile m loinvader) false) ;stub

;<used template from ListOfInvader with extra parameter>

(define (check-list-missile m loinvader)
  (cond [(empty? loinvader) false]
        [else 
         (or  (collision? (first loinvader) m)
              (check-list-missile m (rest loinvader)))]))


;; Game -> Game
;; removes missiles that have flown offscreen from the given game
(check-expect (offscreen-missiles (make-game empty empty (make-tank 0 0)))
              (make-game empty empty (make-tank 0 0)))
(check-expect (offscreen-missiles (make-game empty
                                             (list (make-missile 0 0)
                                                   (make-missile (/ WIDTH 2) HEIGHT)
                                                   (make-missile 10 -10))
                                             (make-tank 0 0)))
              (make-game empty
                         (list (make-missile (/ WIDTH 2) HEIGHT))
                         (make-tank 0 0)))
                                                                              
;(define (offscreen-missiles s) G0) ;stub

;<used template from Game>

(define (offscreen-missiles s)
  (make-game (game-invaders s)
             (remove-missiles (game-missiles s))
             (game-tank s)))


;; ListOfMissile -> ListOfMissile
;; given a list of missiles, remove those that have flown offscreen to produce the new list
(check-expect (remove-missiles empty) empty)
(check-expect (remove-missiles (list (make-missile 0 -10))) empty)
(check-expect (remove-missiles (list (make-missile 0 0)
                                     (make-missile (/ WIDTH 2) HEIGHT)
                                     (make-missile 10 -10)))
              (list (make-missile (/ WIDTH 2) HEIGHT)))

;(define (remove-missiles lom) empty) stub

;<used template from ListOfMissile

(define (remove-missiles lom)
  (cond [(empty? lom) empty]
        [else 
         (if (remove? (first lom))
             (remove-missiles (rest lom))
             (cons (first lom) (remove-missiles (rest lom))))]))


;; Missile -> Boolean
;; produce true if a missile has flown off the top of the screen (i.e. missile-y <= 0)
(check-expect (remove? (make-missile 0 HEIGHT)) false)
(check-expect (remove? (make-missile 0 0)) true)
(check-expect (remove? (make-missile 0 -10)) true)

;(define (remove? m) false) ;stub

;<used template from Missile>

(define (remove? m)
  (<= (missile-y m) 0))


;; Game -> Game
;; move invader, missle and tank positions according to their velocities and change their velocities as necessary
(check-expect (move (make-game empty empty (make-tank 100 0)))
              (make-game empty empty (make-tank 100 0)))
(check-expect (move (make-game(list (make-invader 100 100 -1)
                                    (make-invader 200 200 1))
                              (list (make-missile 100 400)
                                    (make-missile 50 300))
                              (make-tank (/ WIDTH 2) 1)))
              (make-game (list (make-invader (+ (* -1 INVADER-SPEED) 100) (+ 100 (* 1 INVADER-SPEED)) -1)
                               (make-invader (+ (* 1 INVADER-SPEED) 200) (+ 200 (* 1 INVADER-SPEED)) 1))
                         (list (make-missile 100 (- 400 MISSILE-SPEED))
                               (make-missile 50 (- 300 MISSILE-SPEED)))
                         (make-tank (+ 150 (* TANK-SPEED 1)) 1)))

;(define (move s) G1) ;stub

;<used template from Game>

(define (move s)
  (make-game (move-loinvader (game-invaders s))
             (move-lom (game-missiles s))
             (move-tank (game-tank s))))


;; ListOfInvaders -> ListOfInvaders
;; move all of the invaders in the list to their new x and y positions
(check-expect (move-loinvader empty) empty)
(check-expect (move-loinvader (list (make-invader 100 100 -1)
                                    (make-invader 200 200 1)))
              (list (make-invader (+ (* -1 INVADER-SPEED) 100) (+ 100 (* 1 INVADER-SPEED)) -1)
                    (make-invader (+ (* 1 INVADER-SPEED) 200) (+ 200 (* 1 INVADER-SPEED)) 1)))
              
;(define (move-loinvader loinvader) empty) ;stub

;<used template from ListOfInvader>

(define (move-loinvader loinvader)
  (cond [(empty? loinvader) empty]
        [else 
         (cons (move-invader (first loinvader))
               (move-loinvader (rest loinvader)))]))


;; Invader -> Invader
;; moves invader at a 45 degree angle (i.e. same number of x and y pixels traversed per tick) according to dx and INVADER-SPEED
;; also makes invaders bounce of left/right border of screen, changing invader dx by inverting sign.
(check-expect (move-invader (make-invader 100 100 -1))
              (make-invader
               (+ 100 (* -1 INVADER-SPEED))
               (+ 100 (* 1 INVADER-SPEED))
               -1))
(check-expect (move-invader (make-invader (- WIDTH INVADER-SPEED) 100 1))
              (make-invader WIDTH (+ 100 INVADER-SPEED) -1))
(check-expect (move-invader (make-invader (- WIDTH 1) 100 1))
              (make-invader WIDTH 101 -1))
(check-expect (move-invader (make-invader INVADER-SPEED 100 -1))
              (make-invader 0 (+ 100 INVADER-SPEED) 1))
(check-expect (move-invader (make-invader 1 100 -1))
              (make-invader 0 101 1))

;(define (move-invader invader) I1) ;stub

;<used template from Invader>

(define (move-invader invader)
  (cond [(>= (+ (invader-x invader)
                (* (invader-dx invader) INVADER-SPEED))
             WIDTH)
         (make-invader WIDTH
                       (+ (- WIDTH (invader-x invader)) (invader-y invader))
                       (* -1 (invader-dx invader)))]
        [(<= (+ (invader-x invader)
                (* (invader-dx invader) INVADER-SPEED))
             0)
         (make-invader 0
                       (+ (invader-x invader) (invader-y invader))
                       (* -1 (invader-dx invader)))]
        [else
         (make-invader (+ (invader-x invader)
                          (* (invader-dx invader) INVADER-SPEED))
                       (+ (invader-y invader)
                          (* 1 INVADER-SPEED))
                       (invader-dx invader))]))


;; ListOfMissile -> ListOfMissile
;; moves missles in the list by updating their y position
(check-expect (move-lom empty) empty)
(check-expect (move-lom (list (make-missile (/ WIDTH 2) (/ HEIGHT 2))
                              (make-missile (/ WIDTH 4) (/ HEIGHT 4))))
              (list (make-missile (/ WIDTH 2) (- (/ HEIGHT 2) MISSILE-SPEED))
                    (make-missile (/ WIDTH 4) (- (/ HEIGHT 4) MISSILE-SPEED))))
                        
;(define (move-lom lom) empty) ;stub

;<used template from ListOfMissile

(define (move-lom lom)
  (cond [(empty? lom) empty]
        [else 
         (cons (move-missile (first lom))
               (move-lom (rest lom)))]))


;; Missile -> Missile
;; moves missile vertically (i.e. changes y position) by MISSILE-SPEED pixels per tick
(check-expect (move-missile (make-missile (/ WIDTH 2) (/ HEIGHT 2)))
              (make-missile (/ WIDTH 2) (- (/ HEIGHT 2) MISSILE-SPEED)))

;(define (move-missile m) (make-missile 0 0)) ;stub

;<used template from Missile>

(define (move-missile m)
  (make-missile (missile-x m)
                (- (missile-y m) MISSILE-SPEED)))


;; Tank -> Tank
;; move tank according to TANK-SPEED (pixels/tick) and current tank dx; tank will stop at screen edges
(check-expect (move-tank (make-tank 100 0)) (make-tank (+ 100 (* TANK-SPEED 0)) 0))
(check-expect (move-tank T1) (make-tank (+ (tank-x T1) (* TANK-SPEED (tank-dir T1))) (tank-dir T1)))
(check-expect (move-tank T2) (make-tank (+ (tank-x T2) (* TANK-SPEED (tank-dir T2))) (tank-dir T2)))
(check-expect (move-tank (make-tank 0 -1)) (make-tank 0 -1))
(check-expect (move-tank (make-tank (+ 0 TANK-SPEED) -1)) (make-tank 0 -1))
(check-expect (move-tank (make-tank WIDTH 1)) (make-tank WIDTH 1))
(check-expect (move-tank (make-tank (- WIDTH TANK-SPEED) 1)) (make-tank WIDTH 1))

;(define (move-tank t) T0) ;stub

;<used template from Tank>

(define (move-tank t)
  (cond [(<= (+ (tank-x t) (* TANK-SPEED (tank-dir t)))
             0)
         (make-tank 0 (tank-dir t))]
        [(>= (+ (tank-x t) (* TANK-SPEED (tank-dir t)))
             WIDTH)
         (make-tank WIDTH (tank-dir t))]
        [else
         (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t)))
                    (tank-dir t))]))


;; Game -> Image
;; render all entities onto background 
(check-expect (render (make-game
                       empty
                       empty
                       (make-tank (/ WIDTH 2) 0)))
              (place-image TANK
                           (/ WIDTH 2)
                           (- HEIGHT TANK-HEIGHT/2)
                           BACKGROUND))
(check-expect (render (make-game
                       (list (make-invader 100 100 1))
                       (list (make-missile 200 200))
                       (make-tank (/ WIDTH 2) 0)))
              (place-image INVADER
                           100
                           100
                           (place-image MISSILE
                                        200
                                        200
                                        (place-image TANK
                                                     (/ WIDTH 2)
                                                     (- HEIGHT TANK-HEIGHT/2)
                                                     BACKGROUND))))

;(define (render s) BACKGROUND) ;stub

;<used template from function composition>

(define (render s)
  (render-invaders (game-invaders s) (render-missiles (game-missiles s) (render-tank (game-tank s)))))

;; Tank -> Image
;; produce image of tank on the background at the appropriate position
(check-expect (render-tank (make-tank 0 1)) (place-image TANK
                                                         0
                                                         (- HEIGHT TANK-HEIGHT/2)
                                                         BACKGROUND))

;(define (render-tank t) BACKGROUND) ;stub

;<used template from Tank>

(define (render-tank t)
  (place-image TANK
               (tank-x t)
               (- HEIGHT TANK-HEIGHT/2)
               BACKGROUND))


;; ListOfMissiles Image -> Image
;; render missiles in given list onto the image produced by render-tank
(check-expect (render-missiles empty BACKGROUND) BACKGROUND)
(check-expect (render-missiles (list (make-missile 100 100)
                                     (make-missile 200 200))
                               BACKGROUND)
              (place-image MISSILE 100 100
                           (place-image MISSILE 200 200 BACKGROUND)))

;(define (render-missiles lom i) BACKGROUND) ;stub

;<used template from ListOfMissiles with extra atomic parameter>

(define (render-missiles lom i)
  (cond [(empty? lom) i]
        [else 
         (place-missile (first lom) (render-missiles (rest lom) i))]))


;; Missile Image -> Image
;; render a single missile onto the background image (background image is background + tank + prior missiles)
(check-expect (place-missile (make-missile 20 50) BACKGROUND)
              (place-image MISSILE 20 50 BACKGROUND))

;(define (place-missile m i) BACKGROUND) ;stub

;<used template from Missile with extra atomic parameter>

(define (place-missile m i)
  (place-image MISSILE (missile-x m) (missile-y m) i))


;; ListOfInvader Image -> Image
;; render invaders in given list onto the image produced by render-tank and render-missiles
(check-expect (render-invaders empty BACKGROUND) BACKGROUND)
(check-expect (render-invaders (list (make-invader 100 100 1)
                                     (make-invader 200 200 1))
                               BACKGROUND)
              (place-image INVADER 100 100
                           (place-image INVADER 200 200 BACKGROUND)))

;(define (render-invaders loinvader i) BACKGROUND) ;stub

;<used template from ListOfInvaders with extra atomic parameter>

(define (render-invaders loinvader i)
  (cond [(empty? loinvader) i]
        [else 
         (place-invader (first loinvader) (render-invaders (rest loinvader) i))]))


;; Invader Image -> Image
;; render a single invader onto the background image (background image is background + tank + missile + prior invaders)
(check-expect (place-invader (make-invader 20 50 1) BACKGROUND)
              (place-image INVADER 20 50 BACKGROUND))

;(define (place-invader invader i) BACKGROUND) ;stub

;<used template from Invader with extra atomic parameter>

(define (place-invader invader i)
  (place-image INVADER (invader-x invader) (invader-y invader) i))


;; Game KeyEvent -> Game
;; when left key is pressed, change tank-dir to -1 to move it left.
;; when right key is pressed, change tank-dir to 1 to move it right.
;; when spacebar is pressed, fire a missile
(check-expect (control-tank (make-game empty empty (make-tank 0 0)) "left")
              (make-game empty empty (make-tank 0 -1)))
(check-expect (control-tank (make-game empty empty (make-tank 0 1)) "left")
              (make-game empty empty (make-tank 0 -1)))
(check-expect (control-tank (make-game empty empty (make-tank 0 0)) "right")
              (make-game empty empty (make-tank 0 1)))
(check-expect (control-tank (make-game empty empty (make-tank 0 -1)) "right")
              (make-game empty empty (make-tank 0 1)))
(check-expect (control-tank (make-game empty empty (make-tank 0 0)) " ")
              (make-game empty (list (make-missile 0 (- HEIGHT (image-height TANK)))) (make-tank 0 0)))
(check-expect (control-tank (make-game empty empty (make-tank 0 0)) "a")
              (make-game empty empty (make-tank 0 0)))

;(define (control-tank s ke) (make-game empty empty (make-tank 0 1))) ;stub

;<used template from KeyEvent>

(define (control-tank s ke)
  (cond [(key=? ke " ")
         (make-game (game-invaders s)
                    (cons (make-missile (tank-x (game-tank s)) (- HEIGHT (image-height TANK))) (game-missiles s))
                    (game-tank s))]
        [(key=? ke "left")
         (make-game (game-invaders s)
                    (game-missiles s)
                    (make-tank (tank-x (game-tank s)) -1))]
        [(key=? ke "right")
         (make-game (game-invaders s)
                    (game-missiles s)
                    (make-tank (tank-x (game-tank s)) 1))]
        [else s]))


;; Game -> Boolean
;; produces true if any invaders in game have landed and signals the game to end
(check-expect (game-over? (make-game empty empty (make-tank 0 0))) false)
(check-expect (game-over? (make-game (list (make-invader 0 0 1)) empty (make-tank 0 0))) false)
(check-expect (game-over? (make-game (list (make-invader 0 HEIGHT 1)) empty (make-tank 0 0))) true)
(check-expect (game-over? (make-game (list (make-invader 0 (+ HEIGHT 1) 1)) empty (make-tank 0 0))) true)
(check-expect (game-over? (make-game (list (make-invader 0 HEIGHT 1) (make-invader 0 0 1)) empty (make-tank 0 0))) true)

;(define (game-over? s) false) ;stub

;<used template from Game>

(define (game-over? s)
  (landed-invaders? (game-invaders s)))


;; ListOfInvaders -> Boolean
;; produces true if any invader in given list has a y >= HEIGHT and has therefore landed
(check-expect (landed-invaders? empty) false)
(check-expect (landed-invaders? (list (make-invader 0 0 1))) false)
(check-expect (landed-invaders? (list (make-invader 0 HEIGHT 1))) true)
(check-expect (landed-invaders? (list (make-invader 0 (+ HEIGHT 1) 1))) true)
(check-expect (landed-invaders? (list (make-invader 0 HEIGHT 1) (make-invader 0 0 1))) true)

;(define (landed-invaders? loinvader) false) ;stub

;<used Template from ListOfInvader>

(define (landed-invaders? loinvader)
  (cond [(empty? loinvader) false]
        [else 
         (or (landed? (first loinvader))
             (landed-invaders? (rest loinvader)))]))


;; Invader -> Boolean
;; produce true if given invader y coordinate is larger than or equal to HEIGHT
(check-expect (landed? (make-invader 0 0 1)) false)
(check-expect (landed? (make-invader 0 HEIGHT 1)) true)
(check-expect (landed? (make-invader 0 (+ HEIGHT 1) 1)) true)
               
;(define (landed? invader) false) ;stub

;<used template from invader>

(define (landed? invader)
  (>=  (invader-y invader) HEIGHT))
