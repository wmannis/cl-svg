;(in-package :cl-svg)

(defun one-of (list)
  (nth (random (length list)) list))

(defvar *groovy-test-colors*
  (list
   "rgb(252, 229, 105)"
   "rgb(228, 174, 60)"
   "rgb(212, 228, 164)"
   "rgb(196, 234, 212)"
   "rgb(124, 218, 156)"
   "rgb(244, 128, 20)"
   "rgb(212, 229, 190)"))

(defvar *sandy-test-colors*
  (list
   "rgb(129, 132, 72)"
   "rgb(201, 197, 117)"
   "rgb(140, 44, 18)"
   "rgb(187, 136, 59)"
   "rgb(50, 68, 56)"
   "rgb(185, 141, 126)"
   "rgb(135, 90, 31)"
   "rgb(235, 228, 134)"
   "rgb(200, 89, 47)"
   "rgb(246, 185, 144)"
   "rgb(147, 164, 153)"))

(let ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 300 :width 300)))
  (draw scene (:rect :x 5 :y 5 :height 30 :width 30))
  (draw scene (:rect :x 40 :y 40 :height 30 :width 30)
        :stroke "blue" :stroke-width 1 :fill "yellow")
  (draw scene (:rect :x 75 :y 75 :height 30 :width 30)
        :fill "purple")
  (with-open-file (s #p"test.svg" :direction :output :if-exists :supersede)
    (stream-out s scene)))


(let ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 500 :width 500)))
  (dotimes (i 80)
    (draw scene (:circle :cx (random 500)
                         :cy (random 500)
                         :r (+ 10 (random 45)))
          :stroke "rgb(232, 229, 148)"
          :fill (one-of *groovy-test-colors*)))
  (with-open-file (s #p"test.svg" :direction :output :if-exists :supersede)
    (stream-out s scene)))

;;; eliptical grooviness
(let ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 500 :width 500)))
  (title scene "Eliptical Grooviness")
  (dotimes (i 310)
    (draw scene (:ellipse :cx (random 500)
                          :cy (random 500)
                          :rx (+ 10 (random 65))
                          :ry (+ 10 (random 65)))
          :stroke "rgb(232, 229, 148)"
          :fill (one-of *groovy-test-colors*)))
  (with-open-file (s #p"test.svg" :direction :output :if-exists :supersede)
    (stream-out s scene)))

;;; rectangular grooviness
(let ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 700 :width 700)))
  (title scene "Rectangular grooviness!")
  (desc scene "This is a scene of unspeakable grooviness!  Just look at the
great colors.  And the random rectangles!  You want this as wallpaper.")
  (dotimes (i 1200)
    (draw scene (:rect :x (- (random 750) 25)
                       :y (- (random 750) 25)
                       :rx 4 :ry 4
                       :height (+ 5 (random 65))
                       :width (+ 5 (random 65)))
          :stroke "rgb(232, 229, 148)"
          :opacity (+ 0.3 (random 0.2))
          :fill (one-of *groovy-test-colors*)))
  (with-open-file (s #p"test.svg" :direction :output :if-exists :supersede)
    (stream-out s scene)))


;;; Now some symbols (in the SVG sense).
(let* ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 700 :width 700))
       ;;; draw a nice picture
       (columns (make-svg-symbol scene (:id :generate :view-box "0 0 20 20")
                  (draw* (:rect :x 5 :y 5 :height 10 :width 3) :opacity 1.0)
                  (draw* (:rect :x 10 :y 5 :height 10 :width 3) :opacity 0.7)
                  (draw* (:rect :x 15 :y 5 :height 10 :width 3) :opacity 0.3))))
  ;;; Next, instantiate like mad.
  (dotimes (i 400)
    (let ((size (+ 5 (random 95))))
      (draw scene (:using :xlink-href (xlink-href columns))
            :x (- (random 750) 25)
            :y (- (random 700) 25)
            :height size :width size
            :stroke "rgb(232, 229, 148)"
            :stroke-width 0.3
            :fill (one-of *sandy-test-colors*))))
  (title scene "SVG test: using symbols")
  (with-open-file (s #p"test.svg" :direction :output :if-exists :supersede)
    (stream-out s scene)))


;;; Gradients
(let*
    ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 700 :width 700))
     (lg1 (make-linear-gradient scene (:id :generate
                                       :x1 "0%" :y1 "0%" :x2 "100%" :y2 "100%")
            (stop :color "black" :offset "0%")
            (stop :color "purple" :offset "50%")
            (stop :color "red" :offset "100%")))
     (rg1 (make-radial-gradient scene (:id :generate
                                       :cx "50%" :cy "50%" :r "50%")
            (stop :color "yellow" :offset "0%")
            (stop :color "orange" :offset "40%")
            (stop :color "red" :offset "70%" :opacity 0.3)
            (stop :color "grey" :offset "100%")))
     (rg2 (make-radial-gradient scene (:id :generate
                                       :cx "50%" :cy "50%" :r "50%"
                                       :fx "50%" :fy "25%")
            (stop :color "black" :offset "0%")
            (stop :color "lime" :offset "30%")
            (stop :color "white" :offset "70%"))))
  (title scene "SVG test: gradients")
  (draw scene (:rect :x 10 :y 10 :height 200 :width 200)
        :fill (xlink-href lg1))
  (draw scene (:rect :x 250 :y 250 :height 200 :width 200)
        :fill (xlink-href rg1))
  (draw scene (:rect :x 490 :y 490 :height 200 :width 200)
        :fill (xlink-href rg2))
  (with-open-file (s #p"test.svg" :direction :output :if-exists :supersede)
    (stream-out s scene)))

;;; Line markers -- locating these correctly is a pain in the rear.
(let* ((scene (make-svg-toplevel 'svg-1.1-toplevel :view-box "0 0 4000 2000"
                                 :height "2in" :width "4in"))
       (marker (make-marker scene (:id :generate :view-box "0 0 10 10"
                                   :ref-x 0 :ref-y 5
                                   :marker-width 4 :marker-height 3
                                   :marker-units "strokeWidth" :orient "auto")
                 (draw* (:path :d "M 0 0 L 10 5 L 0 10 z")))))
  (draw scene (:rect :x 10 :y 10 :width 3980 :height 1980)
               :fill "none" :stroke "blue" :stroke-width 10)
  (draw scene (:path :d "M 1000 750 L 2000 750 L 2500 1250")
               :fill "none" :stroke "black" :stroke-width 100
               :marker-end (xlink-href marker))
  (with-open-file (s #p"test.svg" :direction :output :if-exists :supersede)
    (stream-out s scene)))

;;; Fill patterns.
(let* ((scene (make-svg-toplevel 'svg-1.1-toplevel :height 700 :width 700))
       (pattern (make-pattern scene (:id :generate
                                     :x 0 :y 0 :width 20 :height 20
                                     :view-box "0 0 10 10"
                                     :pattern-units "userSpaceOnUse")
                  (draw* (:rect :x 0 :y 0 :width 5 :height 5 :fill "lightblue"))
                  (draw* (:rect :x 5 :y 5 :width 5 :height 5 :fill "lightblue")))))
  (comment scene "Just a quick fill test.")
  (draw scene (:rect :x 0 :y 0 :height "100%" :width "100%")
              :fill (xlink-href pattern))
  (with-open-file (s #p"test.svg" :direction :output :if-exists :supersede)
    (stream-out s scene)))
