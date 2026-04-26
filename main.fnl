(var picked-up false)
(local PLAYER_SPEED 200)
(local PLAYER_SIZE 50)

(local BIN_SIZE 40)

(var world nil)

(var (bin-x bin-y) (values 600 600))

(var objects [])

(fn love.load []
  (love.window.setMode 1280 720 {:resizable true :vsync true})
  (love.physics.setMeter 64)
  (set world (love.physics.newWorld 0 0 true))
  
  (set objects.ball {})
  (set objects.ball.body
       (love.physics.newBody world 500 500 "dynamic"))
  (set objects.ball.shape
       (love.physics.newCircleShape 10))
  (set objects.ball.fixture
       (love.physics.newFixture objects.ball.body objects.ball.shape 1))
  (objects.ball.fixture:setRestitution 0.9)
  (objects.ball.body:setLinearDamping 1)
  (objects.ball.fixture:setCategory 1)
  (objects.ball.fixture:setMask)

  (set objects.player {})
  (set objects.player.body
       (love.physics.newBody world 400 500 "dynamic"))
  (objects.player.body:setFixedRotation true)
  (set objects.player.shape
       (love.physics.newRectangleShape PLAYER_SIZE PLAYER_SIZE))
  (set objects.player.fixture
       (love.physics.newFixture objects.player.body objects.player.shape 1))
  (objects.player.fixture:setCategory 1)
  (objects.player.fixture:setMask))

(fn love.update [deltatime]
  (world:update deltatime)

  (when picked-up
    (objects.ball.body:setLinearVelocity 0 0)
    (objects.ball.body:setX (objects.player.body:getX))
    (objects.ball.body:setY (objects.player.body:getY)))

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
    (objects.player.body:setLinearVelocity vx vy)))

(macro with-colour [r g b ...]
  `(do
     (love.graphics.setColor ,r ,g ,b)
     (do ,...)
     (love.graphics.setColor 1 1 1)))

(macro unless [condition ...]
  `(when (not ,condition)
     ,...))

(fn draw-apple [x y]
  (with-colour 1 0 0
    (love.graphics.circle "fill"
                          (objects.ball.body:getX)
                          (objects.ball.body:getY)
                          (objects.ball.shape:getRadius))))

(fn draw-bin []
  (with-colour 0 0 1
    (love.graphics.rectangle "fill" bin-x bin-y BIN_SIZE BIN_SIZE)))

(fn draw-player []
  (with-colour 0 1 0
    (let [offset (/ PLAYER_SIZE 2)]
      (love.graphics.polygon "fill"
                         (objects.player.body:getWorldPoints
                          (objects.player.shape:getPoints))))))

(fn love.draw []
  (love.graphics.setColor 1 1 1)
  (love.graphics.print "Hello from Fennel!\nPress any key to quit" 10 10)
  (draw-bin)
  (draw-player)
  (if picked-up
      (draw-apple (objects.player.body:getX) (objects.player.body:getY))
      (draw-apple (objects.ball.body:getX) (objects.ball.body:getY))))

(fn can-pick-up []
  (and (<= (math.abs (- (objects.player.body:getX) (objects.ball.body:getX))) 50)
       (<= (math.abs (- (objects.player.body:getY) (objects.ball.body:getY))) 50)))

(fn love.keypressed [key _scancode _repeat]
  (when (= key "space")
    (if (and (can-pick-up) (not picked-up))
        (do
          (set picked-up true)
          (objects.player.fixture:setMask 1))
        picked-up
        (do
          (set picked-up false)
          (objects.player.fixture:setMask)
          (objects.ball.body:setX (objects.player.body:getX))
          (objects.ball.body:setY (objects.player.body:getY)))))

  (when (love.keyboard.isDown "escape")
    (love.event.quit)))
