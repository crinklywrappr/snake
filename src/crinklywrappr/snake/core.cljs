(ns crinklywrappr.snake.core
  (:require [goog.object :as gobject]
            [clojure.browser.event :as ev]
            [cljsjs.pixi]))

(def GRID_SIZE 20)

;; amount of time the snake takes to move a block in the grid
(def speed-units
  (letfn [(f [k t]
            (let [start 2.8 target 10]
              (+ 2.8 (/ (- target start) (inc (js/Math.pow js/Math.E (* (- k) (- t 0.5))))))))]
    (atom
     (vec
      (concat [10]
              (->> (range 0 1 (/ 1 73))
                   (mapv (partial f 10))
                   reverse)
              [2.8])))))

(defn current-speed [] (first @speed-units))
(defn increase-speed! [] (when (seq (rest @speed-units)) (swap! speed-units rest)))

(defn kill [app listener]
  (ev/unlisten-by-key listener)
  (.remove (.-view app))
  (.destroy app))

(def app (js/PIXI.Application.))

(.appendChild (.-body js/document) (.-view app))

(defn build-col [x]
  (doto (js/PIXI.Graphics.)
    (.lineStyle 1 0x606060)
    (.moveTo x 0)
    (.lineTo x 600)))

(defn build-row [y]
  (doto (js/PIXI.Graphics.)
    (.lineStyle 1 0x606060)
    (.moveTo 0 y)
    (.lineTo 800 y)))

(def grid
  (concat
   (mapv build-col (range 0 800 GRID_SIZE))
   (mapv build-row (range 0 600 GRID_SIZE))))

(doseq [g grid]
  (.addChild (.-stage app) g))

(defn place-food [g]
  (set! (.-x g) (rand-nth (range 0 800 GRID_SIZE)))
  (set! (.-y g) (rand-nth (range 0 600 GRID_SIZE)))
  (doto g
    (.beginFill 0x563259)
    (.drawRect 5 5 10 10)))

(def food
  [(place-food (js/PIXI.Graphics.))
   (place-food (js/PIXI.Graphics.))
   (place-food (js/PIXI.Graphics.))])

(doseq [f food]
  (.addChild (.-stage app) f))

(def direction (atom :down))
(def current-direction (atom @direction))
(def snake-length (atom 1))
;; start with a turn so we have no special case
(def turns (atom (list {:from :left :to :down :coord [360 -100]})))
(def elapsed (atom 0))

(defn accrue-segments
  [{:keys [points remaining-length new-turns] :as acc}
   {[x y] :coord from :from to :to :as turn}]
  (let [[x' y'] (last points)
        blocks (+ (long (/ (abs (- x' x)) 20))
                  (long (/ (abs (- y' y)) 20)))]
    (if (> blocks (dec remaining-length))
      (let [offset (if (seq new-turns) (* GRID_SIZE (/ @elapsed (current-speed))) 0)]
        (reduced
         (-> acc
             (update :new-turns conj turn)
             (update :points conj
                     (case to
                       :down (let [end-y (- y' (* remaining-length GRID_SIZE))]
                               [x (+ end-y offset) to])
                       :right (let [end-x (- x' (* remaining-length GRID_SIZE))]
                                [(+ end-x offset) y to])
                       :up (let [end-y (+ y' (* remaining-length GRID_SIZE))]
                             [x (- end-y offset) to])
                       :left (let [end-x (+ x' (* remaining-length GRID_SIZE))]
                               [(- end-x offset) y to]))))))
      (-> acc
          (update :remaining-length - blocks)
          (update :new-turns conj turn)
          (update :points conj [x y to])))))

(defn build-snake' [g]
  (let [acc {:points [[(.-x g) (.-y g)]] :new-turns []
             :remaining-length (dec @snake-length)}]
    (reduce accrue-segments acc @turns)))

(defn build-segment [g [x y] [x' y' to']]
  (case to'
    :down (.drawRect g x' y' GRID_SIZE (- y y'))
    :right (.drawRect g x' y' (- x x') GRID_SIZE)
    :up (.drawRect g x (+ y GRID_SIZE) GRID_SIZE (- y' y))
    :left (.drawRect g (+ x GRID_SIZE) y (- x' x) GRID_SIZE)))

(defn build-head [g]
  (cond
    (< (.-x g) 0 (+ (.-x g) GRID_SIZE))
    (do (.drawRect g 0 0 GRID_SIZE GRID_SIZE)
        (.drawRect g (-> app .-view .-width) 0 GRID_SIZE GRID_SIZE))

    (< (.-y g) 0 (+ (.-y g) GRID_SIZE))
    (do (.drawRect g 0 0 GRID_SIZE GRID_SIZE)
        (.drawRect g 0 (-> app .-view .-height) GRID_SIZE GRID_SIZE))

    (< (.-x g) (-> app .-view .-width) (+ (.-x g) GRID_SIZE))
    (do (.drawRect g 0 0 GRID_SIZE GRID_SIZE)
        (.drawRect g (- (-> app .-view .-width)) 0 GRID_SIZE GRID_SIZE))

    (< (.-y g) (-> app .-view .-height) (+ (.-y g) GRID_SIZE))
    (do (.drawRect g 0 0 GRID_SIZE GRID_SIZE)
        (.drawRect g 0 (- (-> app .-view .-height)) GRID_SIZE GRID_SIZE))

    :else (.drawRect g 0 0 GRID_SIZE GRID_SIZE)))

(defn build-snake [g]
  (let [{:keys [points new-turns]} (build-snake' g)
        translate (fn [[x y to]] [(- x (.-x g))  (- y (.-y g)) to])
        points' (mapv translate points)]
    (reset! turns (list* new-turns))
    (.clear g)
    (.beginFill g 0x4daf3b 1)
    (build-head g)
    (doseq [[p1 p2] (partition 2 1 points')]
      (build-segment g p1 p2))
    (.beginFill g 0xff0000 1)
    (case @current-direction
      :up (do (.drawCircle g 0 GRID_SIZE 5)
              (.drawCircle g GRID_SIZE GRID_SIZE 5))
      :left (do (.drawCircle g GRID_SIZE 0 5)
                (.drawCircle g GRID_SIZE GRID_SIZE 5))
      :down (do (.drawCircle g 0 0 5)
                (.drawCircle g GRID_SIZE 0 5))
      :right (do (.drawCircle g 0 0 5)
                 (.drawCircle g 0 GRID_SIZE 5)))
    g))

;; snake grid position
(def pos (atom [360 280]))

(def snake
  (let [g (js/PIXI.Graphics.)]
    (set! (.-x g) (first @pos))
    (set! (.-y g) (second @pos))
    (build-snake g)))

;; sets the grid position of the snake
;; and snaps the snake to the grid
(defn set-pos! [x y]
  (reset! pos [x y])
  (set! (.-x snake) x)
  (set! (.-y snake) y))

;; updates the grid position of the snake
;; and snaps the snake to the grid
(defn update-pos! [delta-x delta-y]
  (swap! pos update 0 + delta-x)
  (swap! pos update 1 + delta-y)
  (set! (.-x snake) (first @pos))
  (set! (.-y snake) (second @pos)))

(.addChild (.-stage app) snake)

(defn change-direction [event]
  (condp = (.-key event)
    "ArrowLeft" (reset! direction :left)
    "ArrowRight" (reset! direction :right)
    "ArrowUp" (reset! direction :up)
    "ArrowDown" (reset! direction :down)))

(def listener (ev/listen (.-body js/document) "keydown" change-direction))

(defn found-food? [snake food]
  (and (< (.-x food) (+ (.-x snake) (/ GRID_SIZE 2)) (+ (.-x food) GRID_SIZE))
       (< (.-y food) (+ (.-y snake) (/ GRID_SIZE 2)) (+ (.-y food) GRID_SIZE))))

(defn eat-food [snake food]
  (when-let [food' (some #(when (found-food? snake %) %) food)]
    (.clear food')
    (place-food food')
    (swap! snake-length inc)
    (increase-speed!)))

(defn calc-offset [] (* GRID_SIZE (/ @elapsed (current-speed))))

(defn left! []
  (let [offset (calc-offset)]
    (when (zero? (first @pos))
      (set-pos! (-> app .-view .-width) (second @pos)))
    (set! (.-x snake) (- (first @pos) offset))))

(defn right! []
  (let [offset (calc-offset)]
    (when (> (+ (first @pos) offset GRID_SIZE) (-> app .-view .-width))
      (set-pos! (- GRID_SIZE) (second @pos)))
    (set! (.-x snake) (+ (first @pos) offset))))

(defn up! []
  (let [offset (calc-offset)]
    (when (zero? (second @pos))
      (set-pos! (first @pos) (-> app .-view .-height)))
    (set! (.-y snake) (- (second @pos) offset))))

(defn down! []
  (let [offset (calc-offset)]
    (when (> (+ (second @pos) offset GRID_SIZE) (-> app .-view .-height))
      (set-pos! (first @pos) (- GRID_SIZE)))
    (set! (.-y snake) (+ (second @pos) offset))))

(defn turn! [from to x y]
  (swap! turns conj {:from from :to to :coord [x y]})
  (reset! current-direction to)
  (reset! elapsed 0)
  (set-pos! x y))

(defn advance-grid! [delta-x delta-y f]
  (swap! elapsed mod (current-speed))
  (update-pos! delta-x delta-y)
  (f))

(.add (.-ticker app)
      (fn render-loop [t]
        (swap! elapsed + t)
        (case @current-direction
          :left
          (if (>= @elapsed (current-speed))
            (let [new-direction @direction]
              (if (and (not= new-direction :left) (not= new-direction :right))
                (turn! :left new-direction (- (first @pos) GRID_SIZE) (second @pos))
                (advance-grid! (- GRID_SIZE) 0 left!)))
            (left!))
          :right
          (if (>= @elapsed (current-speed))
            (let [new-direction @direction]
              (if (and (not= new-direction :right) (not= new-direction :left))
                (turn! :right new-direction (+ (first @pos) GRID_SIZE) (second @pos))
                (advance-grid! GRID_SIZE 0 right!)))
            (right!))
          :up
          (if (>= @elapsed (current-speed))
            (let [new-direction @direction]
              (if (and (not= new-direction :up) (not= new-direction :down))
                (turn! :up new-direction (first @pos) (- (second @pos) GRID_SIZE))
                (advance-grid! 0 (- GRID_SIZE) up!)))
            (up!))
          :down
          (if (>= @elapsed (current-speed))
            (let [new-direction @direction]
              (if (and (not= new-direction :down) (not= new-direction :up))
                (turn! :down new-direction (first @pos) (+ (second @pos) GRID_SIZE))
                (advance-grid! 0 GRID_SIZE down!)))
            (down!)))
        (build-snake snake)
        (eat-food snake food)))
