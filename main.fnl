(var (x y) (values 100 100))
(var picked-up false)
(local PLAYER_SPEED 200)
(local PLAYER_SIZE 50)

(local BIN_SIZE 40)

(var world nil)

(var (apple-x apple-y) (values 200 200))

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
       (love.physics.newCircleShape 20))
  (set objects.ball.fixture
       (love.physics.newFixture objects.ball.body objects.ball.shape 1))
  (objects.ball.fixture:setRestitution 0.9)

  (set objects.player {})
  (set objects.player.body
       (love.physics.newBody world 400 500 "dynamic"))
  (set objects.player.shape
       (love.physics.newRectangleShape 20 20))
  (set objects.player.fixture
       (love.physics.newFixture objects.player.body objects.player.shape 1)))

(fn love.update [deltatime]
  (world:update deltatime)
  
  (when (love.keyboard.isDown "d")
    (set x (+ x (* PLAYER_SPEED deltatime))))

  (when (love.keyboard.isDown "a")
    (set x (- x (* PLAYER_SPEED deltatime))))

  (when (love.keyboard.isDown "s")
    (set y (+ y (* PLAYER_SPEED deltatime))))

  (when (love.keyboard.isDown "w")
    (set y (- y (* PLAYER_SPEED deltatime))))

  (let [vx (if (love.keyboard.isDown "d")
               PLAYER_SPEED
               0)]
    (when (love.keyboard.isDown "d")
      (set y (- y (* PLAYER_SPEED deltatime)))
      (objects.player.body:setLinearVelocity vx 0)))
  

  )

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
    (love.graphics.circle "fill" x y 10)))

(fn draw-bin []
  (with-colour 0 0 1
    (love.graphics.rectangle "fill" bin-x bin-y BIN_SIZE BIN_SIZE)))

(fn draw-player [x y]
  (with-colour 0 1 0
    (let [offset (/ PLAYER_SIZE 2)]
      (love.graphics.rectangle "fill" (- x offset) (- y offset) PLAYER_SIZE PLAYER_SIZE))))

(fn love.draw []
  (love.graphics.circle "fill"
                        (objects.ball.body:getX)
                        (objects.ball.body:getY)
                        (objects.ball.shape:getRadius))
  (love.graphics.polygon "fill"
                         (objects.player.body:getWorldPoints
                          (objects.player.shape:getPoints)))
  (love.graphics.setColor 1 1 1)
  (love.graphics.print "Hello from Fennel!\nPress any key to quit" 10 10)
  (draw-bin)
  (draw-player x y)
  (if picked-up
      (draw-apple x y)
      (draw-apple apple-x apple-y)))

(fn can-pick-up []
  (and (<= (math.abs (- x apple-x)) 30)
       (<= (math.abs (- y apple-y)) 30)))

(fn love.keypressed [key _scancode _repeat]
  (when (= key "space")
    (if (and (can-pick-up) (not picked-up))
        (set picked-up true)
        picked-up
        (do
          (set picked-up false)
          (set apple-x x)
          (set apple-y y))))

  (when (love.keyboard.isDown "escape")
    (love.event.quit)))
