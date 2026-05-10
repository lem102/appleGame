(local PLAYER_SPEED 200)
(local PLAYER_SIZE 50)

(local BOX_SIZE 40)

(local PLAYER_GRAB_DISTANCE 100)

(var world nil)

(local (box-x box-y) (values 600 600))

(var player nil)

(local apples [])

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
    {: body
     : shape
     : fixture
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
    (player.body:setLinearVelocity vx vy)))

(lambda apple-draw [apple]
  (with-colour 1 0 0
    (love.graphics.circle "fill"
                          (apple.body:getX)
                          (apple.body:getY)
                          (apple.shape:getRadius))))

(lambda player-draw [player]
  (with-colour 0 1 0
    (love.graphics.polygon "fill"
                           (player.body:getWorldPoints (player.shape:getPoints)))))

(lambda distance [x1 y1 x2 y2]
  "Find the distance between two points in 2d space."
  (math.sqrt (+ (^ (- x2 x1) 2) (^ (- y2 y1) 2))))

(lambda player-what-grab [player apples]
  "Return the thing that PLAYER can grab.

Return nil if PLAYER cannot grab anything."
  (local apple-distances
         (icollect [_ apple (ipairs apples)]
           (let [distance (distance (player.body:getX) (player.body:getY)
                                    (apple.body:getX) (apple.body:getY))]
             (when (<= distance PLAYER_GRAB_DISTANCE)
               {: apple
                : distance}))))

  (table.sort apple-distances
              (lambda [a b]
                (< a.distance b.distance)))

  (if (= 0 (length apple-distances))
      nil
      (do
        (local smallest-distance (. apple-distances 1))
        smallest-distance.apple)))

(lambda player-handle-grab [player apple]
  "Handle PLAYER grabbing APPLE."
  (set player.carrying apple))

(lambda apple-handle-grabbed [apple]
  "Handle APPLE being grabbed."
  (set apple.is-carried true)
  (apple.fixture:setMask 1))

(lambda player-grab [player apples]
  "As PLAYER, try to grab an apple from APPLES."
  (let [apple (player-what-grab player apples)]
    (when apple
      (player-handle-grab player apple)
      (apple-handle-grabbed apple))))

(lambda player-drop [player]
  "As PLAYER, drop the currently held apple."
  (let [apple player.carrying]
    (apple.fixture:setMask)
    (set player.carrying nil)
    (set apple.is-carried false)))

(lambda player-grab-or-drop [player apples]
  "As PLAYER, grab or drop an apple."
  (if player.carrying
      (player-drop player)
      (player-grab player apples)))

(lambda apple-create [x y]
  "Create the apple."
  (let [body (love.physics.newBody world x y "dynamic")
        shape (love.physics.newCircleShape 10)
        fixture (love.physics.newFixture body shape 1)]
    (body:setLinearDamping 1)
    (fixture:setRestitution 0.9)
    (fixture:setCategory 1)
    (fixture:setMask)
    {: body
     : shape
     : fixture
     :is-carried false}))

(lambda apple-update [apple]
  (when apple.is-carried
    (apple.body:setX (player.body:getX))
    (apple.body:setY (player.body:getY))))

(fn love.load []
  (love.window.setMode 1280 720 {:resizable true :vsync true})
  (love.physics.setMeter 64)
  (set world (love.physics.newWorld 0 0 true))
  (set player (player-create))
  (table.insert apples (apple-create 500 100))
  (table.insert apples (apple-create 100 500)))

(fn love.update [deltatime]
  (world:update deltatime)
  (player-update player)
  (each [_ apple (ipairs apples)]
    (apple-update apple)))

(fn draw-box []
  (with-colour 0 0 1
    (love.graphics.rectangle "fill" box-x box-y BOX_SIZE BOX_SIZE)))

(fn love.draw []
  (love.graphics.setColor 1 1 1)
  (draw-box)
  (player-draw player)
  (each [_ apple (ipairs apples)]
    (apple-draw apple)))

(fn love.keypressed [key _scancode _repeat]
  (when (= key "space")
    (player-grab-or-drop player apples))

  (when (love.keyboard.isDown "escape")
    (love.event.quit)))
