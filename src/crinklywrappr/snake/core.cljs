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
(defn increase-speed [] (when (seq @speed-units) (swap! speed-units rest)))

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

(defn found-food? [snake food]
  (and (< (.-x food) (+ (.-x snake) (/ GRID_SIZE 2)) (+ (.-x food) GRID_SIZE))
       (< (.-y food) (+ (.-y snake) (/ GRID_SIZE 2)) (+ (.-y food) GRID_SIZE))))

(defn eat-food [snake food]
  (when-let [food' (some #(when (found-food? snake %) %) food)]
    (.clear food')
    (place-food food')
    (swap! snake-length inc)
    (increase-speed)))

(.add (.-ticker app)
      (fn render-loop [t]
        (let [offset (* GRID_SIZE (/ @elapsed (current-speed)))]
          (swap! elapsed + t)
          (case @current-direction
            :left
            (let [start-x (+ (-> snake .-position .-x) offset)]
              (if (>= @elapsed (current-speed))
                (let [new-direction @direction]
                  (when (and (not= new-direction :left) (not= new-direction :right))
                    (swap! turns conj {:from :left :to new-direction
                                       :coord [(- start-x GRID_SIZE) (-> snake .-position .-y)]})
                    (reset! current-direction new-direction))
                  (reset! elapsed 0)
                  (set! (-> snake .-position .-x) (- start-x GRID_SIZE)))
                (let [new-x (- start-x (* GRID_SIZE (/ @elapsed (current-speed))))]
                  (set! (-> snake .-position .-x) (if (zero? @elapsed) (js/Math.round new-x) new-x)))))
            :right
            (let [start-x (- (-> snake .-position .-x) offset)]
              (if (>= @elapsed (current-speed))
                (let [new-direction @direction]
                  (when (and (not= new-direction :right) (not= new-direction :left))
                    (swap! turns conj {:from :right :to new-direction
                                       :coord [(+ start-x GRID_SIZE) (-> snake .-position .-y)]})
                    (reset! current-direction new-direction))
                  (reset! elapsed 0)
                  (set! (-> snake .-position .-x) (+ start-x GRID_SIZE)))
                (let [new-x (+ start-x (* GRID_SIZE (/ @elapsed (current-speed))))]
                  (set! (-> snake .-position .-x) (if (zero? @elapsed) (js/Math.round new-x) new-x)))))
            :up
            (let [start-y (+ (-> snake .-position .-y) offset)]
              (if (>= @elapsed (current-speed))
                (let [new-direction @direction]
                  (when (and (not= new-direction :up) (not= new-direction :down))
                    (swap! turns conj {:from :up :to new-direction
                                       :coord [(-> snake .-position .-x) (- start-y GRID_SIZE)]})
                    (reset! current-direction new-direction))
                  (reset! elapsed 0)
                  (set! (-> snake .-position .-y) (- start-y GRID_SIZE)))
                (let [new-y (- start-y (* GRID_SIZE (/ @elapsed (current-speed))))]
                  (set! (-> snake .-position .-y) (if (zero? @elapsed) (js/Math.round new-y) new-y)))))
            :down
            (let [start-y (- (-> snake .-position .-y) offset)]
              (if (>= @elapsed (current-speed))
                (let [new-direction @direction]
                  (when (and (not= new-direction :down) (not= new-direction :up))
                    (swap! turns conj {:from :down :to new-direction
                                       :coord [(-> snake .-position .-x) (+ start-y GRID_SIZE)]})
                    (reset! current-direction new-direction))
                  (reset! elapsed 0)
                  (set! (-> snake .-position .-y) (+ start-y GRID_SIZE)))
                (let [new-y (+ start-y (* GRID_SIZE (/ @elapsed (current-speed))))]
                  (set! (-> snake .-position .-y) (if (zero? @elapsed) (js/Math.round new-y) new-y)))))))
        (build-snake snake)
        (eat-food snake food)))
