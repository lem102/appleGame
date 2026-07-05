;; an overcooked clone

;; current goal is to be able to recreate the first overcooked level,
;; where onions are chopped to create onion soup.

;; TODO: prepared food in pot should be cookable on a hob
;; - how to control the state of something being cooked?
;; the pot could calculate the amount of time required to cook based on its contents.
;; - how to render the cooking bar based on that state?
;; we can render a white bar that slowly fills to green as the contents of the pot approach being cooked.

;; it makes sense for the drawing part to be tackled first, as then we
;; have a good visual indication of what is happening inside the
;; object.

;; plan

;; 1. render a white bar underneath the pot when it has something in
;; it. if the pot is empty, no bar should display.

;; 2. render part of the bar as green. the amount of the bar that is
;; rendered as green should depend on a value stored in the pot.

;; 3. create a mechanism to track the time the pot has spent on the
;; hob with ingredients inside.

;; 4. create a mechanism to calculate the difference between the time
;; to cook the pot's contents and the current cooking duration.

;; 5. alter the rendering so that the amount of the bar that is green
;; is determined by the mechanism in step 4.


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
                                 10)))))

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
                                  (set player.placed-on.held 0)
                                  ;; bin what the player is holding
                                  (set player.placed-on nil)))
        (counter-p selected-thing) (let [counter selected-thing]
                                     (if
                                      ;; place prepared food in pot on counter
                                      (and counter.placed-on
                                           (= counter.placed-on.type "pot"))
                                      (let [pot counter.placed-on]
                                        (when player.placed-on.prepared ; TODO: resolve repitition
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
        (pot-p selected-thing) (when player.placed-on.prepared ; TODO: resolve repitition
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
                               (counter.body:getWorldPoints (counter.shape:getPoints))))
      (= counter.station "hob")
      (with-colour 0.6 0.6 0.6
        (love.graphics.polygon "fill"
                               (counter.body:getWorldPoints (counter.shape:getPoints))))
      (= counter.station "box")
      (with-colour 0 0 1
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
