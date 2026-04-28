(local PLAYER_SPEED 200)
(local PLAYER_SIZE 50)

(local BOX_SIZE 40)

(var world nil)

(var (box-x box-y) (values 600 600))

(var player nil)

(var objects [])

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
     :carrying nil
     :update (lambda []
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
                 (body:setLinearVelocity vx vy)))}))

(lambda apple-draw [apple]
  (with-colour 1 0 0
    (love.graphics.circle "fill"
                          (apple.body:getX)
                          (apple.body:getY)
                          (apple.shape:getRadius))))

(lambda player-draw [player]
  (with-colour 0 1 0
    (love.graphics.polygon "fill" (player.body:getWorldPoints (player.shape:getPoints)))
    (when player.carrying
      (apple-draw player.carrying))))

(lambda player-pick-or-drop [player ?apple]
  (if (and (not player.carrying)
           ?apple
           (<= (math.abs (- (player.body:getX) (?apple.body:getX))) 50)
           (<= (math.abs (- (player.body:getY) (?apple.body:getY))) 50))
      (set player.carrying ?apple)
      player.carrying
      (table.remove objects 2)))

(lambda apple-create []
  "Create the apple."
  (let [body (love.physics.newBody world 500 500 "dynamic")
        shape (love.physics.newCircleShape 10)
        fixture (love.physics.newFixture body shape 1)]
    (body:setLinearDamping 1)
    (fixture:setRestitution 0.9)
    (fixture:setCategory 1)
    (fixture:setMask)
    {: body
     : shape
     : fixture}))



(fn love.load []
  (love.window.setMode 1280 720 {:resizable true :vsync true})
  (love.physics.setMeter 64)
  (set world (love.physics.newWorld 0 0 true))
  (let [new-player (player-create)]
    (table.insert objects new-player)
    (set player new-player))
  (let [new-apple (apple-create)]
    (table.insert objects new-apple)))

(fn love.update [deltatime]
  (world:update deltatime)

  (each [key value (pairs objects)]
    (when value.update
      (value:update))))

(fn draw-box []
  (with-colour 0 0 1
    (love.graphics.rectangle "fill" box-x box-y BOX_SIZE BOX_SIZE)))

(fn love.draw []
  (love.graphics.setColor 1 1 1)
  (love.graphics.print "Hello from Fennel!\nPress any key to quit" 10 10)
  (draw-box)
  (player-draw player)
  (when (. objects 2)
    (apple-draw (. objects 2))))

(fn love.keypressed [key _scancode _repeat]
  (when (= key "space")
    (player-pick-or-drop player (. objects 2)))

  (when (love.keyboard.isDown "escape")
    (love.event.quit)))
