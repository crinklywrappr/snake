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
(def snake-length (atom (+ GRID_SIZE GRID_SIZE)))
;; start with a turn so we have no special case
(def breaks (atom (list {:type :turn :to :down :coord [360 0]})))
(def elapsed (atom 0))

;; snake grid position
(def pos (atom [360 280]))

(defn create-segment
  [segments
   [x y last-type :as last-point]
   {type :type [x' y'] :coord to :to :as break}]
  (let [dx (abs (- x' x)) dy (abs (- y' y))]
    (->>
     (case [last-type type to]
       ;; normal
       [:turn :turn :left] [(+ x GRID_SIZE) y dx GRID_SIZE]
       [:turn :turn :right] [x' y' dx GRID_SIZE]
       [:turn :turn :up] [x (+ y GRID_SIZE) GRID_SIZE dy]
       [:turn :turn :down] [x' y' GRID_SIZE dy]

       [:head :tail :left] [(+ x GRID_SIZE) y dx GRID_SIZE]
       [:head :tail :right] [x' y' dx GRID_SIZE]
       [:head :tail :up] [x (+ y GRID_SIZE) GRID_SIZE dy]
       [:head :tail :down] [x' y' GRID_SIZE dy]

       [:turn :tail :left] [(+ x GRID_SIZE) y dx GRID_SIZE]
       [:turn :tail :right] [x' y' dx GRID_SIZE]
       [:turn :tail :up] [x (+ y GRID_SIZE) GRID_SIZE dy]
       [:turn :tail :down] [x' y' GRID_SIZE dy]

       [:head :turn :left] [(+ x GRID_SIZE) y dx GRID_SIZE]
       [:head :turn :right] [x' y' dx GRID_SIZE]
       [:head :turn :up] [x (+ y GRID_SIZE) GRID_SIZE dy]
       [:head :turn :down] [x' y' GRID_SIZE dy]

       ;; wrap-around
       [:turn :break :left] [x y dx GRID_SIZE]
       [:turn :break :right] [x' y' dx GRID_SIZE]
       [:turn :break :up] [x y GRID_SIZE dy]
       [:turn :break :down] [x' y' GRID_SIZE dy]

       [:break :break :left] [x y dx GRID_SIZE]
       [:break :break :right] [x' y' dx GRID_SIZE]
       [:break :break :up] [x y GRID_SIZE dy]
       [:break :break :down] [x' y' GRID_SIZE dy]

       [:break :turn :left] [x y (+ dx GRID_SIZE) GRID_SIZE]
       [:break :turn :right] [x' y' dx GRID_SIZE]
       [:break :turn :up] [x y GRID_SIZE (+ dy GRID_SIZE)]
       [:break :turn :down] [x' y' GRID_SIZE dy]

       ;; problem segments
       [:head :break :left] [x y dx GRID_SIZE]
       [:head :break :right] [x' y' dx GRID_SIZE]
       [:head :break :up] [x y GRID_SIZE dy]
       [:head :break :down] [x' y' GRID_SIZE dy]

       [:break :tail :left] [x y dx GRID_SIZE]
       [:break :tail :right] [x' y' dx GRID_SIZE]
       [:break :tail :up] [x y GRID_SIZE dy]
       [:break :tail :down] [x' y' GRID_SIZE dy])
     (conj segments))))

(defn tail-break
  [{:keys [to] :as break} [x y :as last-point] remaining-length]
  (->
   (case to
     :left (assoc-in break [:coord 0] (+ x remaining-length))
     :right (assoc-in break [:coord 0] (- x remaining-length))
     :up (assoc-in break [:coord 1] (+ y remaining-length))
     :down (assoc-in break [:coord 1] (- y remaining-length)))
   (assoc :type :tail)))

(defn calc-distance
  [[x y :as last-point]
   {to :to [x' y'] :coord :as break}]
  (case to
    :left (- x' x)
    :right (- x (max x' 0))
    :up (- y' y)
    :down (- y (max y' 0))))

(defn accrue-segments
  [{:keys [segments remaining-length]
    [x y :as last-point] :last-point :as acc}
   {type :type to :to [x' y'] :coord next-point :last-point :as break}]
  (let [distance (calc-distance last-point break)]
    (if (> distance remaining-length)
      (reduced
       (-> acc
           (update :new-breaks conj break)
           (update :segments create-segment last-point
                   (tail-break break last-point remaining-length))))
      (-> acc
          (update :remaining-length - distance)
          (update :new-breaks conj break)
          (update :segments create-segment last-point break)
          (assoc :last-point (conj next-point type))))))

(defn build-snake' [x y]
  (reduce
   accrue-segments
   {:last-point [x y :head] :segments [] :new-breaks []
    :remaining-length (- @snake-length GRID_SIZE)}
   @breaks))

(defn build-segment [g [x y width height :as segment]]
  (.drawRect g (- x (.-x g)) (- y (.-y g)) width height))

(defn build-eyes [g width height]
  (.beginFill g 0xff0000 1)
  (letfn [(f [x' y']
            ;; center
            (.drawCircle g x' y' 5)
            ;; right
            (.drawCircle g (+ x' width) y' 5)
            ;; down
            (.drawCircle g x' (+ y' height) 5)
            ;; left
            (.drawCircle g (- x' width) y' 5)
            ;; up
            (.drawCircle g x' (- y' height) 5))]
    (case @current-direction
      :up (do (f 0 GRID_SIZE)
              (f GRID_SIZE GRID_SIZE))
      :left (do (f GRID_SIZE 0)
                (f GRID_SIZE GRID_SIZE))
      :down (do (f 0 0)
                (f GRID_SIZE 0))
      :right (do (f 0 0)
                 (f 0 GRID_SIZE)))))

(defn build-head [g]
  (let [width (-> app .-view .-width)
        height (-> app .-view .-height)]
    (build-eyes g width height)
    (.beginFill g 0x27541f #_0x4daf3b 1)
    ;; center
    (.drawRect g 0 0 GRID_SIZE GRID_SIZE)
    ;; right
    (.drawRect g width 0 GRID_SIZE GRID_SIZE)
    ;; down
    (.drawRect g 0 height GRID_SIZE GRID_SIZE)
    ;; left
    (.drawRect g (- width) 0 GRID_SIZE GRID_SIZE)
    ;; up
    (.drawRect g 0 (- height) GRID_SIZE GRID_SIZE)))

(defn build-snake [g]
  (let [{:keys [segments new-breaks]} (build-snake' (.-x g) (.-y g))]
    (reset! breaks (list* new-breaks))
    (.clear g)
    (.beginFill g 0x4daf3b 1)
    (doseq [segment segments]
      (build-segment g segment))
    (build-head g)
    (.beginFill g 0xff0000 1)
    g))

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
    " " (if (.-started (.-ticker app))
          (.stop (.-ticker app))
          (.start (.-ticker app)))
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
    (swap! snake-length + GRID_SIZE)
    (increase-speed!)))

(defn calc-offset [] (* GRID_SIZE (/ @elapsed (current-speed))))

(defn left! []
  (let [offset (calc-offset)]
    (when (zero? (first @pos))
      (swap! breaks conj {:type :break :to :left
                          :coord [(-> app .-view .-width) (second @pos)]
                          :last-point [0 (second @pos)]})
      (set-pos! (-> app .-view .-width) (second @pos)))
    (set! (.-x snake) (- (first @pos) offset))))

(defn right! []
  (let [offset (calc-offset)]
    (when (> (+ (first @pos) offset GRID_SIZE) (-> app .-view .-width))
      (swap! breaks conj {:type :break :to :right
                          :coord [(- GRID_SIZE) (second @pos)]
                          :last-point [(-> app .-view .-width) (second @pos)]})
      (set-pos! (- GRID_SIZE) (second @pos)))
    (set! (.-x snake) (+ (first @pos) offset))))

(defn up! []
  (let [offset (calc-offset)]
    (when (zero? (second @pos))
      (swap! breaks conj {:type :break :to :up
                          :coord [(first @pos) (-> app .-view .-height)]
                          :last-point [(first @pos) 0]})
      (set-pos! (first @pos) (-> app .-view .-height)))
    (set! (.-y snake) (- (second @pos) offset))))

(defn down! []
  (let [offset (calc-offset)]
    (when (> (+ (second @pos) offset GRID_SIZE) (-> app .-view .-height))
      (swap! breaks conj {:type :break :to :down
                          :coord [(first @pos) (- GRID_SIZE)]
                          :last-point [(first @pos) (-> app .-view .-height)]})
      (set-pos! (first @pos) (- GRID_SIZE)))
    (set! (.-y snake) (+ (second @pos) offset))))

(defn turn! [to x y]
  (swap! breaks conj {:type :turn :to to :coord [x y] :last-point [x y]})
  (reset! current-direction to)
  (reset! elapsed 0)
  (set-pos! x y))

(defn advance-grid! [delta-x delta-y f]
  (swap! elapsed mod (current-speed))
  (update-pos! delta-x delta-y)
  (f))

(defn axis [dir]
  (case dir
    :up :vertical
    :down :vertical
    :left :horizontal
    :right :horizontal))

(.add (.-ticker app)
      (fn render-loop [t]
        (swap! elapsed + t)
        (if (>= @elapsed (current-speed))
          (let [new-direction @direction]
            (case [@current-direction (axis new-direction)]
              [:left :vertical] (turn! new-direction (- (first @pos) GRID_SIZE) (second @pos))
              [:right :vertical] (turn! new-direction (+ (first @pos) GRID_SIZE) (second @pos))
              [:up :horizontal] (turn! new-direction (first @pos) (- (second @pos) GRID_SIZE))
              [:down :horizontal] (turn! new-direction (first @pos) (+ (second @pos) GRID_SIZE))

              [:left :horizontal] (advance-grid! (- GRID_SIZE) 0 left!)
              [:right :horizontal] (advance-grid! GRID_SIZE 0 right!)
              [:up :vertical] (advance-grid! 0 (- GRID_SIZE) up!)
              [:down :vertical] (advance-grid! 0 GRID_SIZE down!)))
          (case @current-direction
            :left (left!)
            :right (right!)
            :up (up!)
            :down (down!)))
        (build-snake snake)
        (eat-food snake food)))
