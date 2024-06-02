(ns crinklywrappr.snake.core
  (:require [goog.object :as gobject]
            [clojure.browser.event :as ev]
            [cljsjs.pixi]))

;; amount of time the snake takes to move a block in the grid
(def SPEED_MILLIS 10)
(def GRID_SIZE 20)

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
(def snake-length (atom 10))
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
      (reduced
       (-> acc
           (update :new-turns conj turn)
           (update :points conj
                   (case to
                     :down (let [end-y (- y' (* remaining-length GRID_SIZE))]
                             [x end-y to])
                     :right (let [end-x (- x' (* remaining-length GRID_SIZE))]
                              [end-x y to])
                     :up (let [end-y (+ y' (* remaining-length GRID_SIZE))]
                           [x end-y to])
                     :left (let [end-x (+ x' (* remaining-length GRID_SIZE))]
                             [end-x y to])))))
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

(defn build-snake [g]
  (let [{:keys [points new-turns]} (build-snake' g)
        translate (fn [[x y to]] [(- x (.-x g))  (- y (.-y g)) to])
        points' (mapv translate points)]
    (reset! turns (list* new-turns))
    (.clear g)
    (.beginFill g 0x4daf3b 1)
    (.drawRect g 0 0 GRID_SIZE GRID_SIZE)
    (doseq [[p1 p2] (partition 2 1 points')]
      (build-segment g p1 p2))
    g))

(def snake
  (let [g (js/PIXI.Graphics.)]
    (set! (.-x g) 360)
    (set! (.-y g) 280)
    (build-snake g)))

(.addChild (.-stage app) snake)

(defn change-direction [event]
  (condp = (.-key event)
    "ArrowLeft" (reset! direction :left)
    "ArrowRight" (reset! direction :right)
    "ArrowUp" (reset! direction :up)
    "ArrowDown" (reset! direction :down)))

(def listener (ev/listen (.-body js/document) "keydown" change-direction))

(.add (.-ticker app)
      (fn render-loop [t]
        (case @current-direction
          :left
          (let [offset (* GRID_SIZE (/ @elapsed SPEED_MILLIS))
                start-x (+ (-> snake .-position .-x) offset)]
            (swap! elapsed + t)
            (if (>= @elapsed SPEED_MILLIS)
              (let [new-direction @direction]
                (when (and (not= new-direction :left) (not= new-direction :right))
                  (swap! turns conj {:from :left :to new-direction
                                     :coord [(- start-x GRID_SIZE) (-> snake .-position .-y)]})
                  (reset! current-direction new-direction))
                (reset! elapsed 0)
                (set! (-> snake .-position .-x) (- start-x GRID_SIZE)))
              (set! (-> snake .-position .-x)
                    (- start-x (* GRID_SIZE (/ @elapsed SPEED_MILLIS))))))
          :right
          (let [offset (* GRID_SIZE (/ @elapsed SPEED_MILLIS))
                start-x (- (-> snake .-position .-x) offset)]
            (swap! elapsed + t)
            (if (>= @elapsed SPEED_MILLIS)
              (let [new-direction @direction]
                (when (and (not= new-direction :right) (not= new-direction :left))
                  (swap! turns conj {:from :right :to new-direction
                                     :coord [(+ start-x GRID_SIZE) (-> snake .-position .-y)]})
                  (reset! current-direction new-direction))
                (reset! elapsed 0)
                (set! (-> snake .-position .-x) (+ start-x GRID_SIZE)))
              (set! (-> snake .-position .-x)
                    (+ start-x (* GRID_SIZE (/ @elapsed SPEED_MILLIS))))))
          :up
          (let [offset (* GRID_SIZE (/ @elapsed SPEED_MILLIS))
                start-y (+ (-> snake .-position .-y) offset)]
            (swap! elapsed + t)
            (if (>= @elapsed SPEED_MILLIS)
              (let [new-direction @direction]
                (when (and (not= new-direction :up) (not= new-direction :down))
                  (swap! turns conj {:from :up :to new-direction
                                     :coord [(-> snake .-position .-x) (- start-y GRID_SIZE)]})
                  (reset! current-direction new-direction))
                (reset! elapsed 0)
                (set! (-> snake .-position .-y) (- start-y GRID_SIZE)))
              (set! (-> snake .-position .-y)
                    (- start-y (* GRID_SIZE (/ @elapsed SPEED_MILLIS))))))
          :down
          (let [offset (* GRID_SIZE (/ @elapsed SPEED_MILLIS))
                start-y (- (-> snake .-position .-y) offset)]
            (swap! elapsed + t)
            (if (>= @elapsed SPEED_MILLIS)
              (let [new-direction @direction]
                (when (and (not= new-direction :down) (not= new-direction :up))
                  (swap! turns conj {:from :down :to new-direction
                                     :coord [(-> snake .-position .-x) (+ start-y GRID_SIZE)]})
                  (reset! current-direction new-direction))
                (reset! elapsed 0)
                (set! (-> snake .-position .-y) (+ start-y GRID_SIZE)))
              (set! (-> snake .-position .-y)
                    (+ start-y (* GRID_SIZE (/ @elapsed SPEED_MILLIS)))))))
        (build-snake snake)))

