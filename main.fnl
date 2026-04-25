(var (x y) (values 100 100))
(var picked-up false)
(local PLAYER_SPEED 200)
(local PLAYER_SIZE 50)

(local BIN_SIZE 40)

(var (apple-x apple-y) (values 200 200))

(var (bin-x bin-y) (values 600 600))

(fn love.load []
  (love.window.setMode 1280 720 {:resizable true :vsync true}))

(fn love.update [deltatime]
  (when (love.keyboard.isDown "d") 
    (set x (+ x (* PLAYER_SPEED deltatime))))

  (when (love.keyboard.isDown "a")
    (set x (- x (* PLAYER_SPEED deltatime))))

  (when (love.keyboard.isDown "s")
    (set y (+ y (* PLAYER_SPEED deltatime))))

  (when (love.keyboard.isDown "w")
    (set y (- y (* PLAYER_SPEED deltatime)))))

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

