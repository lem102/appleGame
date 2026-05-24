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
     :carrying nil}))

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

(lambda apple-draw [apple ?x ?y]
  (with-colour 1 0 0
    (love.graphics.circle "fill"
                          (if ?x
                              ?x
                              (apple.body:getX))
                          (if ?y
                              ?y
                              (apple.body:getY))
                          (apple.shape:getRadius))))

(lambda player-draw [player]
  (with-colour 0 1 0
    (love.graphics.polygon "fill"
                           (player.body:getWorldPoints (player.shape:getPoints)))
    (love.graphics.circle "fill"
                          player.reticle-x
                          player.reticle-y
                          5))
  (if player.carrying
      (apple-draw player.carrying (player.body:getX) (player.body:getY))))

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

(lambda player-handle-grab [player apple]
  "Handle PLAYER grabbing APPLE."
  (set player.carrying apple)
  (set apple.alive false))

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
     :alive true}))

(lambda box-p [thing]
  "Return non-nil if THING is an box."
  (= thing.type "box"))

(lambda counter-p [thing]
  "Return t if THING is a counter."
  (= "counter" thing.type))

(lambda player-grab [player selected-thing]
  "As PLAYER, try to grab one of THINGS."
  (when selected-thing
    (when (apple-p selected-thing)
      (player-handle-grab player selected-thing))
    (when (box-p selected-thing)
      (let [apple (apple-create 0 0)]
        (table.insert things apple)
        (player-handle-grab player apple)))
    (when (counter-p selected-thing)
      (when selected-thing.placed-on
        (player-handle-grab player selected-thing.placed-on)
        (set selected-thing.placed-on nil)))))

(lambda bin-p [thing]
  "Return t if THING is a bin."
  (= "bin" thing.type))

(lambda player-drop [player selected-thing]
  "As PLAYER, drop the currently held thing."
  (let [thing player.carrying]
    (if (= nil selected-thing) (do
                                 (thing.fixture:setMask)
                                 (set player.carrying nil)
                                 (thing.body:setX player.reticle-x)
                                 (thing.body:setY player.reticle-y)
                                 (set thing.alive true)
                                 (table.insert things thing))
        (bin-p selected-thing) (set player.carrying nil)
        (counter-p selected-thing) (when (not selected-thing.placed-on)
                                     (set selected-thing.placed-on player.carrying)
                                     (set player.carrying nil)))))

(lambda player-grab-or-drop [player things]
  "As PLAYER, grab or drop an apple."
  (let [selected-thing (player-what-grab player things)]
    (if player.carrying
        (player-drop player selected-thing)
        (player-grab player selected-thing))))

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

(lambda counter-create [x y]
  "Create a counter."
  (let [body (love.physics.newBody world x y "static")
        shape (love.physics.newRectangleShape BOX_SIZE BOX_SIZE)
        fixture (love.physics.newFixture body shape 1)]
    (fixture:setCategory 1)
    (fixture:setMask)
    {:type "counter"
     :alive true
     :placed-on nil
     : body
     : shape
     : fixture}))

(lambda box-draw [box]
  "Draw a box"
  (with-colour 0 0 1
    (love.graphics.polygon "fill"
                           (box.body:getWorldPoints (box.shape:getPoints)))))

(lambda counter-draw [counter]
  "Draw a counter"
  (with-colour 0 1 1
    (love.graphics.polygon "fill"
                           (counter.body:getWorldPoints (counter.shape:getPoints))))
  (if counter.placed-on
      (apple-draw counter.placed-on (counter.body:getX) (counter.body:getY))))

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
  (table.insert things (apple-create 500 100))
  (table.insert things (apple-create 100 500))
  (table.insert things (box-create 600 600))
  (table.insert things (bin-create 900 100))
  (table.insert things (counter-create 1200 100)))

(lambda thing-update [thing]
  "Update THING."
  )

(fn love.update [deltatime]
  (world:update deltatime)
  (player-update player)
  (set things (icollect [_ thing (ipairs things)]
                (if thing.alive
                    thing)))
  (each [_ thing (ipairs things)]
    (thing-update thing)))

(lambda thing-draw [thing]
  "Draw THING."
  (if (apple-p thing)
      (apple-draw thing)
      (box-p thing)
      (box-draw thing)
      (bin-p thing)
      (bin-draw thing)
      (counter-p thing)
      (counter-draw thing)))

(fn love.draw []
  (love.graphics.setColor 1 1 1)
  (player-draw player)
  (each [_ thing (ipairs things)]
    (thing-draw thing)))

(fn love.keypressed [key _scancode _repeat]
  (when (= key "space")
    (player-grab-or-drop player things))

  (when (love.keyboard.isDown "escape")
    (love.event.quit)))
