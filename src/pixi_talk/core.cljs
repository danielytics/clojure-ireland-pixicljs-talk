(ns pixi-talk.core
  (:require
    [cljsjs.pixi]))

(enable-console-print!)

(defonce app (new js/PIXI.Application))
(defonce frames (mapv #(.fromImage js/PIXI.Texture (str "assets/" % ".png")) (range 5)))
(defonce sprites (volatile! nil))
(defonce request-id (volatile! nil))

(defn get-time []
  (.now js/Date))

(defn make-world []
  (println "Making world")
  (let [cells [[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
               [0 0 2 3 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
               [0 1 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0]
               [0 0 1 1 1 1 1 1 1 1 0 1 0 0 0 0 1 0 0 1 1 0 0 0 0 0 0]
               [0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1 1 1 0 0 1 0 0 0 0 0]
               [0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1 1 1 1 1 1 0]
               [0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1 1 1 0 0 1 0 0 0 0 0]
               [0 0 1 1 1 1 1 1 1 1 0 1 0 0 0 0 1 0 0 1 1 0 0 0 0 0 0]
               [0 1 0 0 0 0 0 0 0 0 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0]
               [0 0 1 1 1 1 1 1 3 2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
               [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
               [0 2 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0]
               [0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 1 0]
               [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0]
               [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]]]
    {:width (reduce max (map count cells))
     :height (count cells)
     :cells (flatten cells)
     :animation 0}))

(defn make-sprites [old world]
  (println "Creating Sprites")
  (doseq [sprite old]
    (.destroy sprite))
  (map-indexed
    (fn [idx cell]
      (let [y (int (/ idx (:width world)))
            x (int (- idx (* y (:width world))))
            sprite (new js/PIXI.Sprite)]
        (set! (.-x sprite) (* x 28))
        (set! (.-y sprite) (* y 28))
        (set! (.-width sprite) 30)
        (set! (.-height sprite) 30)
        (set! (.-visible sprite) (not= cell 0))
        (.addChild (.-stage app) sprite)
        sprite))
    (:cells world)))

(defn from-idx [world idx]
  (let [y (int (/ idx (:width world)))
        x (int (- idx (* y (:width world))))]
    [x y]))

(defn to-idx [world x y]
  (+ (* y (:width world)) x))

(defn get-adjacent [world idx]
  (let [cells (:cells world)
        [x y] (from-idx world idx)
        adjacent [[-1 -1] [ 0 -1] [1 -1]
                  [-1  0]         [1  0]
                  [-1  1] [ 0 -1] [1  1]]]
    (->> adjacent
         (map (fn [[dx dy]] (nth cells (to-idx world (+ dx x) (+ dy y)))))
         (filter #(= % 3))
        count)))

(defn tick-wireworld [world]
  (update
    world
    :cells
    (partial
      map-indexed
      (fn [idx cell]
        (case cell
          3 2
          2 1
          0 0
          (if (<= 1 (get-adjacent world idx) 2) 3 1))))))

(defn on-tick [dt sprites {:keys [cells animation] :as world}]
  (let [anims [nil [0 0] [2 1] [4 3]]]
    (doseq [[sprite cell] (map vector sprites cells)]
      (set! (.-visible sprite) (not= cell 0))
      (when (not= cell 0)
        (set! (.-texture sprite) (nth frames (get-in anims [cell animation]))))))
  (.render app)
  (update
    (if (= animation 1)
      (tick-wireworld world)
      world)
    :animation #(- 1 %)))



(defn main []
  (.stop app)
  (.removeChildren (.-stage app))
  (when @request-id
    (js/cancelAnimationFrame @request-id))
  (let [element (.getElementById js/document "app")
        counter (volatile! 0)
        world   (volatile! (make-world))
        prev-t  (volatile! (get-time))
        tick-fn (fn this []
                  (let [current-t (get-time)
                        delta   (* (- current-t @prev-t) 0.001)
                        c       (vswap! counter #(+ % delta))]
                    (vreset! prev-t current-t)
                    (when (> c 0.33)
                      (vreset! counter 0)
                      (vswap! world (fn [world] (on-tick #(* delta %) @sprites world)))))
                  (vreset! request-id (js/requestAnimationFrame this)))]
    (vswap! sprites #(make-sprites % @world))
    (set! (.-innerHTML element) "")
    (.appendChild element (.-view app))
    (vreset! request-id (js/requestAnimationFrame tick-fn))))

