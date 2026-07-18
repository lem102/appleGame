;; an overcooked clone

;; current goal is to be able to recreate the first overcooked level,
;; where onions are chopped to create onion soup.

;; TODO: plates

;; TODO: sink

;; TODO: orders

;; TODO: time limit

;; TODO: score

(local PLAYER_SPEED 200)
(local PLAYER_SIZE 50)
(local PLAYER_GRAB_DISTANCE 100)

(local BOX_SIZE 40)

(var world nil)

(var player nil)

(var things [])

(macro with-colour [r g b ...]
  `(do
     (love.graphics.setColor ,r ,g ,b)
     (do ,...)
     (love.graphics.setColor 1 1 1)))

(lambda player-create []
  "Create the player."
  (let [body (love.physics.newBody world 400 500 "dynamic")
        shape (love.physics.newRectangleShape PLAYER_SIZE PLAYER_SIZE)
        fixture (love.physics.newFixture body shape 1)]
    (body:setFixedRotation true)
    (fixture:setCategory 1)
    (fixture:setMask)
    {:type "player"
     : body
     : shape
     : fixture
     :reticle-x 0
     :reticle-y 0
     :placed-on nil
     :alive true}))

(lambda player-update [player]
  (let [vx (if (love.keyboard.isDown "a")
               (- PLAYER_SPEED)
               (love.keyboard.isDown "d")
               PLAYER_SPEED
               0)
        vy (if (love.keyboard.isDown "w")
               (- PLAYER_SPEED)
               (love.keyboard.isDown "s")
               PLAYER_SPEED
               0)]
    (player.body:setLinearVelocity vx vy)
    (when (not (and (= 0 vx)
                    (= 0 vy)))
      (set player.reticle-x (+ (player.body:getX) vx))
      (set player.reticle-y (+ (player.body:getY) vy)))))

(fn apple-draw [apple x y]
  (let [r (if apple.prepared 0 1)
        g (if apple.prepared 1 0)
        b (if apple.prepared 1 0)]
    (with-colour r g b
      (love.graphics.circle "fill"
                            (or x (apple.body:getX))
                            (or y (apple.body:getY))
                            (apple.shape:getRadius)))))

(fn pot-calculate-cooking-time [pot]
  "Return the amount of time it takes to cook the contents of POT in seconds."
  (* 10 pot.held))

(fn pot-draw [pot x y]
  (let [pot-x (or x (pot.body:getX))
        pot-y (or y (pot.body:getY))
        radius (pot.shape:getRadius)]
    (with-colour 0.1 0.1 0.1
      (love.graphics.circle "fill" pot-x pot-y radius))
    (when (> pot.held 0)
      (with-colour 1 0 0
        (love.graphics.circle "fill"
                              pot-x
                              pot-y
                              (* radius
                                 (if (= pot.held 1)
                                     0.2
                                     (= pot.held 2)
                                     0.5
                                     (>= pot.held 3)
                                     0.8))))
      (with-colour 1 1 1
        (love.graphics.rectangle "fill"
                                 (- pot-x radius)
                                 (+ pot-y (* 1.2 radius))
                                 (* 2 radius)
                                 10))
      (with-colour (if pot.spoilt 1 0) (if pot.spoilt 0 1) 0
        (love.graphics.rectangle "fill"
                                 (- pot-x radius)
                                 (+ pot-y (* 1.2 radius))
                                 (/ (* 2 radius) (math.max (/ (pot-calculate-cooking-time pot) pot.cooking-time)
                                                           1))
                                 10)))))

(fn plate-draw [plate x y]
  "Draw a plate."
  (let [plate-x (or x (plate.body:getX))
        plate-y (or y (plate.body:getY))
        radius (plate.shape:getRadius)]
    (with-colour 1 1 1
      (love.graphics.circle "fill" plate-x plate-y radius))
    (when plate.is-filled
      (with-colour 1 0 0
        (love.graphics.circle "fill" plate-x plate-y (- radius 10))))))

(lambda distance [x1 y1 x2 y2]
  "Find the distance between two points in 2d space."
  (math.sqrt (+ (^ (- x2 x1) 2) (^ (- y2 y1) 2))))

(lambda player-what-grab [player things]
  "Return the thing that PLAYER can grab.

Return nil if PLAYER cannot grab anything."
  (let [thing-distances
        (icollect [_ thing (ipairs things)]
          (let [distance (distance player.reticle-x player.reticle-y
                                   (thing.body:getX) (thing.body:getY))]
            (when (<= distance PLAYER_GRAB_DISTANCE)
              {: thing
               : distance})))]

    (table.sort thing-distances
                (lambda [a b]
                  (< a.distance b.distance)))

    (when (> (length thing-distances) 0)
      (. (. thing-distances 1) "thing"))))

(lambda player-handle-grab [player thing]
  "Handle PLAYER grabbing THING."
  (set player.placed-on thing)
  (set thing.alive false))

(lambda apple-p [thing]
  "Return non-nil if THING is an apple."
  (= thing.type "apple"))

(lambda apple-create [x y]
  "Create the apple."
  (let [body (love.physics.newBody world x y "dynamic")
        shape (love.physics.newCircleShape 10)
        fixture (love.physics.newFixture body shape 1)]
    (body:setLinearDamping 1)
    (fixture:setRestitution 0.9)
    (fixture:setCategory 1)
    (fixture:setMask)
    {:type "apple"
     : body
     : shape
     : fixture
     :alive true
     :chopped false}))

(fn pot-update [pot deltatime]
  "Update time based properties of POT."
  (when (and (not pot.spoilt) (> pot.held 0))
    (let [new-time (+ pot.cooking-time deltatime)]
      (set pot.cooking-time new-time)
      (when (> pot.cooking-time (* 1.5 (pot-calculate-cooking-time pot)))
        (set pot.spoilt true)))))

(fn pot-p [thing]
  "Return non-nil if THING is a pot."
  (and thing (= thing.type "pot")))

(fn plate-p [thing]
  "Return non-nil if THING is a plate."
  (and thing (= thing.type "plate")))

(lambda pot-create [x y]
  "Create the pot."
  (let [body (love.physics.newBody world x y "dynamic")
        shape (love.physics.newCircleShape 15)
        fixture (love.physics.newFixture body shape 1)]
    (body:setLinearDamping 1)
    (fixture:setRestitution 0.9)
    (fixture:setCategory 1)
    (fixture:setMask)
    {:type "pot"
     : body
     : shape
     : fixture
     :alive true
     :held 0
     :cooking-time 0
     :spoilt false}))

(lambda plate-create [x y]
  "Create a plate."
  (let [body (love.physics.newBody world x y "dynamic")
        shape (love.physics.newCircleShape 15)
        fixture (love.physics.newFixture body shape 1)]
    (body:setLinearDamping 1)
    (fixture:setRestitution 0.9)
    (fixture:setCategory 1)
    (fixture:setMask)
    {:type "plate"
     : body
     : shape
     : fixture
     :alive true
     :is-filled false}))

(fn counter-update [counter deltatime]
  "Update COUNTER."
  (when (and (= "hob" counter.station)
             counter.placed-on
             (pot-p counter.placed-on))
    (pot-update counter.placed-on deltatime)))

(fn counter-p [thing]
  "Return t if THING is a counter."
  (and thing
       (= "counter" thing.type)))

(fn player-grab [player selected-thing]
  "As PLAYER, grab SELECTED-THING."
  (when selected-thing
    (when (apple-p selected-thing)
      (player-handle-grab player selected-thing))
    (when (pot-p selected-thing)
      (player-handle-grab player selected-thing))
    (when (plate-p selected-thing)
      (player-handle-grab player selected-thing))
    (when (counter-p selected-thing)
      (if selected-thing.placed-on
          (do
            (player-handle-grab player selected-thing.placed-on)
            (set selected-thing.placed-on nil))
          (= selected-thing.station "box")
          (let [apple (apple-create 0 0)]
            (table.insert things apple)
            (player-handle-grab player apple))))))

(fn bin-p [thing]
  "Return t if THING is a bin."
  (and thing
       (= "bin" thing.type)))

(fn player-drop [player selected-thing]
  "As PLAYER, drop the currently held thing."
  (let [thing player.placed-on]
    (if (bin-p selected-thing) (let [bin selected-thing]
                                 (if
                                  ;; bin the contents of the pot instead of the pot itself
                                  (= player.placed-on.type "pot")
                                  (let [pot player.placed-on]
                                    (set pot.held 0)
                                    (set pot.cooking-time 0)
                                    (set pot.spoilt false))
                                  ;; bin what the player is holding
                                  (set player.placed-on nil)))
        (counter-p selected-thing) (let [counter selected-thing]
                                     (if
                                      ;; place prepared food in pot on counter
                                      (and counter.placed-on
                                           (= counter.placed-on.type "pot"))
                                      (let [pot counter.placed-on]
                                        (when (and player.placed-on.prepared
                                                   (not pot.spoilt)) ; TODO: resolve repitition
                                          (set pot.held
                                               (+ pot.held 1))
                                          (set player.placed-on nil)))
                                      (and (not counter.placed-on)
                                           ;; prevent non-pots from being placed on a hob
                                           (not (and (= counter.station "hob")
                                                     (not (= player.placed-on.type "pot")))))
                                      (do
                                        (set counter.placed-on player.placed-on)
                                        (set player.placed-on nil))))
        (pot-p selected-thing) (let [pot selected-thing]
                                 (when (and player.placed-on.prepared
                                            (not pot.spoilt)) ; TODO: resolve repitition
                                   (set pot.held
                                        (+ pot.held 1))
                                   (set player.placed-on nil)))
        (do
          (thing.fixture:setMask)
          (set player.placed-on nil)
          (thing.body:setX player.reticle-x)
          (thing.body:setY player.reticle-y)
          (set thing.alive true)
          (table.insert things thing)))))

(lambda player-grab-or-drop [player things]
  "As PLAYER, grab or drop an apple."
  (let [selected-thing (player-what-grab player things)]
    (if player.placed-on
        (player-drop player selected-thing)
        (player-grab player selected-thing))))

(fn player-context-action [player things]
  "Perform the context sensitive action.

For example, this could be to chop an apple. To perform a context
sensitive action, the player should not be placed-on anything."
  (when (not player.placed-on)
    (let [selected (player-what-grab player things)]
      (when (and selected
                 (= selected.type "counter")
                 (= selected.station "chop"))
        (set selected.placed-on.prepared true)))))

(lambda bin-create [x y]
  "Create a bin."
  (let [body (love.physics.newBody world x y "static")
        shape (love.physics.newRectangleShape BOX_SIZE BOX_SIZE)
        fixture (love.physics.newFixture body shape 1)]
    (fixture:setCategory 1)
    (fixture:setMask)
    {:type "bin"
     :alive true
     : body
     : shape
     : fixture}))

(fn counter-create [x y station]
  "Create a counter."
  (let [body (love.physics.newBody world x y "static")
        shape (love.physics.newRectangleShape BOX_SIZE BOX_SIZE)
        fixture (love.physics.newFixture body shape 1)]
    (fixture:setCategory 1)
    (fixture:setMask)
    {:type "counter"
     :alive true
     :placed-on nil
     : station
     : body
     : shape
     : fixture}))

(lambda bin-draw [bin]
  "Draw a bin"
  (with-colour 1 0 1
    (love.graphics.polygon "fill"
                           (bin.body:getWorldPoints (bin.shape:getPoints)))))

(fn love.load []
  (love.window.setMode 1280 720 {:resizable true :vsync true})
  (love.physics.setMeter 64)
  (set world (love.physics.newWorld 0 0 true))
  (set player (player-create))
  (table.insert things player)
  (table.insert things (apple-create 500 100))
  (table.insert things (apple-create 100 500))
  (table.insert things (bin-create 900 100))
  (table.insert things (counter-create 1200 100))
  (table.insert things (counter-create 1200 500 "chop"))
  (table.insert things (counter-create 200 500 "hob"))
  (table.insert things (counter-create 600 600 "box"))
  (table.insert things (pot-create 600 500))
  (table.insert things (plate-create 300 300)))

(fn player-p [thing]
  "Return non-nil if THING is a player."
  (and thing (= thing.type "player")))

(fn thing-update [thing deltatime]
  "Update THING."
  (if (player-p thing) (player-update thing)
      ;; (pot-p thing) (pot-update thing deltatime)
      (counter-p thing) (counter-update thing deltatime)))

(fn love.update [deltatime]
  (world:update deltatime)
  (set things (icollect [_ thing (ipairs things)]
                (when thing.alive
                  (thing-update thing deltatime)
                  thing))))

(fn counter-draw [counter]
  "Draw a counter"
  (with-colour 0 1 1
    (love.graphics.polygon
     "fill"
     (counter.body:getWorldPoints
      (counter.shape:getPoints))))
  (if (= counter.station "chop")
      (with-colour 0.4 0.4 0.4
        (love.graphics.polygon "fill"
                               (counter.body:getWorldPoints (counter.shape:getPoints))))
      (= counter.station "hob")
      (with-colour 0.6 0.6 0.6
        (love.graphics.polygon "fill"
                               (counter.body:getWorldPoints (counter.shape:getPoints))))
      (= counter.station "box")
      (with-colour 0 0 1
        (love.graphics.polygon "fill"
                               (counter.body:getWorldPoints (counter.shape:getPoints))))))

(lambda player-draw [player]
  (with-colour 0 1 0
    (love.graphics.polygon "fill"
                           (player.body:getWorldPoints (player.shape:getPoints)))
    (love.graphics.circle "fill"
                          player.reticle-x
                          player.reticle-y
                          5)))

(fn thing-draw [thing x y]
  "Draw THING."
  (if (player-p thing)
      (player-draw thing)
      (apple-p thing)
      (apple-draw thing x y)
      (bin-p thing)
      (bin-draw thing)
      (counter-p thing)
      (counter-draw thing)
      (pot-p thing)
      (pot-draw thing x y)
      (plate-p thing)
      (plate-draw thing x y))
  (if thing.placed-on
      (thing-draw thing.placed-on (thing.body:getX) (thing.body:getY))))

(fn love.draw []
  (love.graphics.setColor 1 1 1)
  (each [_ thing (ipairs things)]
    (thing-draw thing)))

(fn love.keypressed [key _scancode _repeat]
  (when (= key "j")
    (player-grab-or-drop player things))
  (when (= key "k")
    (player-context-action player things))

  (when (love.keyboard.isDown "escape")
    (love.event.quit)))
