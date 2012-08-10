(ns First-Game.core
  (:require [lanterna.screen :as s]))

(def game-state [{:entity-name "player" :display \@ :x 0 :y 0}])

(def wall {:entity-name "wall" :display \#})

; (def floor {:entity-name "floor" :display \space})

(defn handle-input [k game-state]
  (let [player (first (filter #(= "player" (:entity-name %)) game-state))
        new-state (filter #(not (= "player" (:entity-name %))) game-state)]
    (if player
      (cond
       (= k :down)  (cons (assoc player :y (inc (:y player))) new-state)
       (= k :right) (cons (assoc player :x (inc (:x player))) new-state)
       (= k :left)  (cons (assoc player :x (dec (:x player))) new-state)
       (= k :up)    (cons (assoc player :y (dec (:y player))) new-state)
       :else        game-state)
      game-state)))

(defn create-rep [game-state]
  (map (fn [cvec] (apply str cvec))
       (partition 80 (map (fn [entity] (if entity (:display entity) \space))
                          (for [y (range 0 24)
                                x (range 0 80)]
                            (first (filter (fn [entity] (and (= x (:x entity)) (= y (:y entity)))) game-state)))))))

(defn render [screen game]
  (doseq [rownum (range 0 (count game))]
    (s/put-string screen 0 rownum (nth game rownum)))
  (s/redraw screen))

(defn get-input [screen]
  (s/get-key-blocking screen))

(defn init-dungeon-floor [width height]
  (map (fn [rownum] (repeat width :wall)) (range 0 height)))

(defn create-dungeon-room [floor [x y width height]]
  (let [pre  (take y floor)
        post (drop (+ y height) floor)
        mod  (map
              (fn [row] (take (count row) (concat (take x row)
                                (repeat width :floor)
                                (drop (+ x width) row))))
              (take height (drop y floor)))]
   (concat pre mod post)))

(defn create-dungeon-layout [floor room-dimensions]
  (if (empty? room-dimensions)
    floor
    (recur (create-dungeon-room floor (first room-dimensions)) (rest room-dimensions))))

(defn create-dungeon [floor]
  (filter (comp not nil?) (flatten (map-indexed (fn [y row] (map-indexed (fn [x col] (if (= col :wall) (assoc wall :x x :y y))) row)) floor))))

(defn is-valid-room? [room rooms]
  (if (empty? rooms)
    true
    (let [other-room (first rooms)
          in-x (< (+ 1 (nth room 0) (nth room 2)) (nth other-room 0))
          in-y (< (+ 1 (nth room 1) (nth room 3)) (nth other-room 1))
          valid (or in-x in-y)]
      (if valid
        (is-valid-room? room (rest rooms))
        false))))

(defn is-valid-dungeon-layout? [layout]
  (if (> 2 (count layout))
    true
    (and (is-valid-room? (first layout) (rest layout)) (is-valid-dungeon-layout? (rest layout)))))

(defn random-dungeon-layout [minroom maxroom width height minrw maxrw minrh maxrh]
  (let [layout (repeatedly (+ minroom (rand-int (- maxroom minroom)))
                           (fn [] [(rand-int width) (rand-int height) (+ minrw (rand-int (- maxrw minrw))) (+ minrh (rand-int (- maxrh minrh)))]))
        sorted-layout (sort (comp (fn [rooma roomb] (and (> (nth rooma 0) (nth roomb 0)) (> (nth rooma 1) (nth roomb 1))))) layout)]
    (if (is-valid-dungeon-layout? sorted-layout)
      sorted-layout
      (recur  minroom maxroom width height minrw maxrw minrh maxrh))))

(defn do-game
  ([screen]
     (do-game screen (concat game-state
                             (create-dungeon
                              (create-dungeon-layout
                               (init-dungeon-floor 80 24)
                               (random-dungeon-layout 7 12 80 24 2 12 2 12))))))
  ([screen game-state]
  ;   (println game-state)
     (render screen (create-rep game-state))
     (let [key (get-input screen)]
       (if (= \q key)
         nil
         (recur screen (handle-input key game-state))))))

(defn main [screen-type]
  (let [screen (s/get-screen screen-type)]
    (s/in-screen screen (do-game screen))))

(defn -main [& args]
  (let [args (set args)
        screen-type (cond
                     (args ":swing") :swing
                     (args ":text")  :text
                     :else           :auto)]
    (main screen-type)))

