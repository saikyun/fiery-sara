(use freja/flow)

(def render-scale 2)

(var shoot nil)
(var start-fire nil)

### UTIL

(defn noop [& args])

(defn lerp
  [start stop t]
  (+ start (* t (- stop start))))

(defn v-lerp
  [[x y] [x2 y2] t]
  [(lerp x x2 t)
   (lerp y y2 t)])

(defn circle-circle?
  [{:radius r1 :pos p1} {:radius r2 :pos p2}]
  (<= (v/dist p1 p2) (+ r1 r2)))

### ASSETS

(var knight-stand nil)
(var sara-stand nil)
(var fire nil)
(var flame-bullet nil)
(var grass nil)
(var grass-dim [29 22])
(var ground nil)
(var grass-dim [29 22])

### STATE

(def state @{:gos @[]
             :mp @[0 0]})

(varfn new-fire [& args])

(defn fire-update
  [self]
  (when (and (not (self :split))
             (< 10 (self :heat)))
    (put self :split true)
    (update self :heat - 2)
    (def offset (+ 5 (* 10 (math/random))))
    (update-in self [:pos 0] - offset)
    (update-in self [:pos 1] + 2)
    (def nf (new-fire (v/v+ (self :pos) [(* 2 offset) 0])))
    (put nf :heat 8)
    (update-in nf [:pos 1] + 2)

    (update state :gos array/push nf))

  (loop [go :in (state :gos)
         :when (circle-circle? go {:pos (self :pos)
                                   :radius (* (min 3 (max 1 (/ (self :heat) 10)))
                                              (self :radius))})
         :when (go :heat)
         :when (not (go :passthrough))
         :when (not= go self)]
    (update go :heat + (* (self :heat) (get-frame-time)))
    (update self :heat + (* 60 (get-frame-time))))

  (update self :heat - (* 50 (get-frame-time)))
  (when (<= (self :heat) 0)
    (put self :dead true)))

(defn fire-render
  [{:pos pos
    :size size
    :heat h}]
  (let [s (min 1 (/ h 10))]
    (defer (rl-pop-matrix)
      (rl-push-matrix)
      (rl-translatef ;(v/v- pos (v/v* size (* s 0.5))) 0)
      (rl-scalef s s 1)
      (draw-texture-v fire [0 0] :white))))

(varfn new-fire
  [pos]
  (play-sound (get start-fire (math/floor (* (length start-fire) (math/random)))))
  @{:pos pos
    :heat 3
    :size [29 22]
    :passthrough true
    :radius 8
    :update fire-update
    :render fire-render})

(defn flame-bullet-update
  [self]
  (update-in self [:dir 1] + (* 1.5 (get-frame-time)))
  (update self :pos v/v+ (v/v* (self :dir)
                               (* (get-frame-time)
                                  (self :speed))))

  (loop [go :in (state :gos)
         :when (not (go :passthrough))
         :when (circle-circle? go self)
         :when (and (not= go (self :caster))
                    (not= go self))]
    (put self :dead true)
    (update state :gos array/push
            (new-fire (self :pos)))
    (break)))

(defn flame-bullet-render
  [{:pos pos :size size}]
  (draw-texture-v flame-bullet (v/v- pos (v/v* size 0.5)) :white))

(defn new-flame-bullet
  [caster pos dir force]
  @{:caster caster
    :pos pos
    :dir dir
    :size @[21 11]
    :radius 8
    :speed (* force 100)
    :update flame-bullet-update
    :render flame-bullet-render})

(defn grass-update
  [self]
  (when (>= (self :heat) (self :burnt))
    (put self :dead true)))

(defn grass-render
  [{:pos pos :size size}]
  (draw-texture-v grass (v/v- pos (v/v* size 0.5)) :white))

(defn new-grass
  [pos]
  @{:pos pos
    :burnt 40
    :heat 0
    :size [29 22]
    :radius 8
    :update grass-update
    :render grass-render})

(defn ground-render
  [{:pos pos :size size}]
  (draw-texture-v ground (v/v- pos (v/v* size 0.5)) :white))

(defn new-ground
  [pos]
  @{:pos pos
    :ground-offset -20
    :ground true
    :size [32 6]
    :radius 8
    :update noop
    :render ground-render})

(defn sara-update
  [self]
  (update self :damage-timer - (get-frame-time))
  (update self :damage-timer max 0)

  (var on-ground nil)
  (loop [go :in (state :gos)
         :when (go :ground)
         :when (let [[x y] (self :pos)
                     y (+ y (* 0.5 ((self :size) 1)))]
                 (and
                   (>= x (get-in go [:pos 0]))
                   (<= x (+ (get-in go [:size 0])
                            (get-in go [:pos 0])))
                   (>= y (- (get-in go [:pos 1])
                            (* 0.5 (get-in go [:size 1]))))
                   (<= y (+ (get-in go [:pos 1])
                            (* 0.5 (get-in go [:size 1]))))))]
    (set on-ground go)
    (break))

  (update self :jump-timer - (get-frame-time))

  (if-let [dt (and (pos? (self :damage-timer))
                   (self :damage-timer))]
    (update self :pos v/v+ (v/v* [-1 -1.5] (* (max 0.3 dt) 150 (get-frame-time))))
    (do
      (when (pos? (self :jump-timer))
        (self :jump-timer)
        (update-in self [:pos 1] - (* 7 (self :jump-timer)))
        (set on-ground nil))

      (when (key-down? :d)
        (update-in self [:pos 0] + (self :speed)))
      (when (and on-ground (key-down? :w))
        (put self :jump-timer 1))
      (when (key-down? :a)
        (update-in self [:pos 0] - (self :speed)))
      (when (mouse-button-pressed? 0)
        (play-sound (get shoot (math/floor (* (length shoot) (math/random)))))

        (update
          state
          :gos array/push
          (new-flame-bullet
            self
            @[;(self :aim-pos)]
            (self :aim-dir)
            (* 0.2
               (self :aim-force)))))))

  (var on-ground nil)
  (loop [go :in (state :gos)
         :when (go :ground)
         :when (let [[x y] (self :pos)
                     y (+ y (* 0.5 ((self :size) 1)))]
                 (and
                   (>= x (get-in go [:pos 0]))
                   (<= x (+ (get-in go [:size 0])
                            (get-in go [:pos 0])))
                   (>= y (- (get-in go [:pos 1])
                            (* 0.5 (get-in go [:size 1]))))
                   (<= y (+ (get-in go [:pos 1])
                            (* 0.5 (get-in go [:size 1]))))))]
    (set on-ground go)
    (break))

  (if-let [go on-ground]
    (unless (pos? (self :jump-timer))
      (put-in self [:pos 1]
              (+ (* -0.5 ((self :size) 1))
                 (- (get-in go [:pos 1])

                    (* 0.5
                       (get-in go [:size 1]))))))
    (update-in self [:pos 1] + (* 2 80 (get-frame-time)))))

(defn sara-render
  [{:pos pos :size size}]
  (draw-texture-v sara-stand (v/v- pos (v/v* size 0.5)) :white))

(defn sara-damage
  [self dmg]
  (when (<= (self :damage-timer) 0)
    (update self :hp - dmg)
    (put self :damage-timer 1)
    (when (<= (self :hp) 0)
      (put self :dead true))))

(defn new-sara
  [pos]
  @{:pos pos
    :jump-timer 0
    :hp 42
    :size @[32 37]
    :damage-timer 0
    :radius 15
    :speed 3
    :damage sara-damage
    :update sara-update
    :render sara-render})

(def sara (new-sara @[30 153]))

(defn knight-update
  [self]
  (update-in self [:pos 0] - (self :speed))

  (when (<= (self :damage-timer) 0)
    (let [dmg (* (self :heat) (get-frame-time))
          dmg (if (> dmg 0.1)
                dmg
                0)]
      (put self :damage-timer 0.3)
      (update self :hp - dmg)))

  (when (<= (self :hp) 0)
    (put self :dead true))

  (update self :damage-timer - (get-frame-time))

  (when (circle-circle? self sara)
    (:damage sara (self :damage))))

(defn knight-render
  [{:pos pos :size size :heat heat :hp hp}]
  (let [normalized-heat (- 1 (/ heat 300))]
    '(draw-text hp [100 10] :color :red)
    (draw-texture-v knight-stand (v/v- pos (v/v* size 0.5))
                    [1 normalized-heat normalized-heat 1])))

(defn new-knight
  [pos]
  @{:pos pos
    :heat 0
    :hp 10
    :damage-timer 0
    :damage 13
    :size @[34 38]
    :radius 15
    :speed 0.5
    :update knight-update
    :render knight-render})

(defonce inited-audio @{})

(defn init
  []
  (set knight-stand (load-texture "knight.png"))
  (set sara-stand (load-texture "sara.png"))
  (set fire (load-texture "fire.png"))
  (set flame-bullet (load-texture "flame-bullet.png"))
  (set grass (load-texture "grass.png"))
  (set ground (load-texture "ground.png"))

  (unless (inited-audio :did-it)
    (init-audio-device)
    (put inited-audio :did-it true))

  (set-master-volume 0.1)

  (set shoot [(load-sound "shoot1.ogg")
              (load-sound "shoot2.ogg")])

  (set start-fire [(load-sound "start-fire.ogg")
                   (load-sound "start-fire.ogg")
                   (load-sound "start-fire.ogg")
                   (load-sound "start-fire.ogg")
                   (load-sound "start-fire.ogg")
                   (load-sound "start-fire.ogg")
                   (load-sound "start-fire.ogg")
                   (load-sound "start-fire.ogg")
                   (load-sound "start-fire.ogg")

                   (load-sound "start-fire.ogg")
                   (load-sound "start-fire.ogg")
                   (load-sound "start-fire.ogg")
                   (load-sound "start-fire.ogg")
                   (load-sound "start-fire.ogg")])

  (loop [x :range [20 290 (grass-dim 0)]
         y :in [172]]
    (update state :gos array/push (new-ground @[x y])))

  (loop [x :range [20 290 (grass-dim 0)]
         y :in [161]
         :when (< (math/random) 0.6)]
    (update state :gos array/push (new-grass @[x y])))

  (loop [x :range [100 500 (grass-dim 0)]
         y :in [272]]
    (update state :gos array/push (new-ground @[x y])))

  (loop [x :range [100 500 (grass-dim 0)]
         y :in [261]
         :when (< (math/random) 0.6)]
    (update state :gos array/push (new-grass @[x y])))

  (update state :gos array/push sara)

  (update state :gos array/push (new-knight @[300 153]))

  (update state :gos array/push (new-knight @[500 253])))


### RENDER ALL STUFF

(defn render-ui
  [el]
  '(draw-text (sara :hp) [10 10] :color :black)
  (let [nof 10
        cursor-pos (state :mp)
        center-of-sara (v/v+ (sara :pos) [0 -20])]
    (loop [i :range-to [1 nof]
           :let [[x y] (v-lerp center-of-sara
                               cursor-pos
                               (/ i nof))
                 y (+ y (* -10 (math/sin (* math/pi (/ i nof)))))]]
      (when (= i 1)
        (put sara :aim-force (v/dist [x y] center-of-sara))
        (put sara :aim-pos [x y])
        (put sara :aim-dir (-> (v/v- [x y] center-of-sara)
                               v/normalize)))
      (draw-circle (math/floor x)
                   (math/floor y)
                   3 [0.5 0.1 0.1 0.7]))))

(defonce rt @{:texture nil :last-size nil})
(def offset [0 0])
(def game-size [(* render-scale 270) (* render-scale 170)])

(defn render
  [el]
  (def {:width rw :height rh} el)
  (def render-size game-size)

  (def {:gos gos} state)

  (loop [go :in gos]
    (:update go))

  (draw-rectangle 0 0 rw rh :white)

  (loop [go :in gos]
    (:render go))

  (render-ui el)

  (update state :gos (fn [gos] (filter |(not ($ :dead)) gos))))

(defn on-event
  [_ ev]
  (match ev
    {:mouse/move p}
    (put state :mp p)))

(start-game {:render render
             :init init
             :size game-size
             #:scale render-scale
             :border :gray
             :on-event on-event})
