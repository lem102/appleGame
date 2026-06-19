;; an overcooked clone

;; current goal is to be able to recreate the first overcooked level,
;; where onions are chopped to create onion soup.

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

(fn pot-draw [pot x y]
  (with-colour 0.1 0.1 0.1
    (love.graphics.circle "fill"
                          (or x (pot.body:getX))
                          (or y (pot.body:getY))
                          (pot.shape:getRadius))))

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

(fn pot-p [thing]
  "Return non-nil if THING is an pot."
  (and thing
       (= thing.type "pot")))

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
     :held 0}))

(lambda box-p [thing]
  "Return non-nil if THING is an box."
  (= thing.type "box"))

(fn counter-p [thing]
  "Return t if THING is a counter."
  (and thing
       (= "counter" thing.type)))

(lambda player-grab [player ?selected-thing]
  "As PLAYER, grab SELECTED-THING."
  (when ?selected-thing
    (when (apple-p ?selected-thing)
      (player-handle-grab player ?selected-thing))
    (when (pot-p ?selected-thing)
      (player-handle-grab player ?selected-thing))
    (when (box-p ?selected-thing)
      (let [apple (apple-create 0 0)]
        (table.insert things apple)
        (player-handle-grab player apple)))
    (when (counter-p ?selected-thing)
      (when ?selected-thing.placed-on
        (player-handle-grab player ?selected-thing.placed-on)
        (set ?selected-thing.placed-on nil)))))

(fn bin-p [thing]
  "Return t if THING is a bin."
  (and thing
       (= "bin" thing.type)))

(fn player-drop [player selected-thing]
  "As PLAYER, drop the currently held thing."
  ;; TODO: handle dropping on box
  (let [thing player.placed-on]
    (if (bin-p selected-thing) (set player.placed-on nil)
        (counter-p selected-thing) (when (not selected-thing.placed-on)
                                     (set selected-thing.placed-on player.placed-on)
                                     (set player.placed-on nil))
        (pot-p selected-thing) (when player.placed-on.prepared
                                 (set selected-thing.held
                                      (+ selected-thing.held 1))
                                 (set player.placed-on nil))
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

(lambda box-create [x y]
  "Create a box."
  (let [body (love.physics.newBody world x y "static")
        shape (love.physics.newRectangleShape BOX_SIZE BOX_SIZE)
        fixture (love.physics.newFixture body shape 1)]
    (fixture:setCategory 1)
    (fixture:setMask)
    {:type "box"
     :alive true
     : body
     : shape
     : fixture}))

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
     ;; TODO: property to contain tool on the counter (e.g. chopping board)
     : station
     : body
     : shape
     : fixture}))

(lambda box-draw [box]
  "Draw a box"
  (with-colour 0 0 1
    (love.graphics.polygon "fill"
                           (box.body:getWorldPoints (box.shape:getPoints)))))

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
  (table.insert things (box-create 600 600))
  (table.insert things (bin-create 900 100))
  (table.insert things (counter-create 1200 100))
  (table.insert things (counter-create 1200 500 "chop"))
  (table.insert things (pot-create 600 500)))

(fn love.update [deltatime]
  (world:update deltatime)
  (player-update player)
  (set things (icollect [_ thing (ipairs things)]
                (if thing.alive
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
                               (counter.body:getWorldPoints (counter.shape:getPoints))))))

(fn player-p [thing]
  "Return true if THING is a player."
  (= thing.type "player"))

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
      (box-p thing)
      (box-draw thing)
      (bin-p thing)
      (bin-draw thing)
      (counter-p thing)
      (counter-draw thing)
      (pot-p thing)
      (pot-draw thing x y))
  (if thing.placed-on
      ;; instead of doing this whole placed on thing with the apple
      ;; and the pot, trying to drop the apple on the pot should
      ;; change some state within the pot, and the apple should be
      ;; deleted.
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
