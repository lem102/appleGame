;; an overcooked clone

;; current goal is to be able to recreate the first overcooked level,
;; where onions are chopped to create onion soup.

;; TODO: refactor to duck typing:
;; all are one, there are only containers

;; TODO: delivery point

;; TODO: dirty plates

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
  (* 10 (length pot.contents)))

(fn pot-draw [pot x y]
  (let [pot-x (or x (pot.body:getX))
        pot-y (or y (pot.body:getY))
        radius (pot.shape:getRadius)]
    (with-colour 0.1 0.1 0.1
      (love.graphics.circle "fill" pot-x pot-y radius))
    (let [held-amount (length pot.contents)]
      (when (> held-amount 0)
        (with-colour 1 0 0
          (love.graphics.circle "fill"
                                pot-x
                                pot-y
                                (* radius
                                   (if (= held-amount 1)
                                       0.2
                                       (= held-amount 2)
                                       0.5
                                       (>= held-amount 3)
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
                                   10))))))

(fn plate-draw [plate x y]
  "Draw a plate."
  (let [plate-x (or x (plate.body:getX))
        plate-y (or y (plate.body:getY))
        radius (plate.shape:getRadius)]
    (with-colour 1 1 1
      (love.graphics.circle "fill" plate-x plate-y radius))
    (when (< 0 (length plate.contents))
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
  (when (and (not pot.spoilt) (> (length pot.contents) 0))
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

(fn container? [thing]
  "Return non-nil if THING is a container."
  (and thing
       thing.contain))

(fn pot-empty [pot]
  "Empty POT."
  (set pot.contents [])
  (set pot.cooking-time 0)
  (set pot.spoilt false))

(fn container-empty? [container]
  "Return t if CONTAINER is empty."
  (= (length container.contents) 0))

(fn container-transfer [a b]
  "Transfer contents of container A to B."
  (table.move a.contents
              1
              (length a.contents)
              1
              b.contents)
  (set a.contents []))

(fn standard-contain-function [container player]
  "Attempts to put what PLAYER is holding in CONTAINER."
  (let [player-holding player.placed-on]
    ;; 18/08/26 plan

    ;; 1. alter this function to switch on the container types first and then perform filtering
    ;; 2. identify which branch requires existing filtering
    ;; 3. reintroduce existing filtering on that branch with a more appropriate name perhaps incoming or outgoing
    ;; 4. introduce new kind of filtering on the other container related branch

    (when (< (length container.contents) container.content-limit)
      (if (and (container? player-holding)
               (container-empty? player-holding)
               (not (container-empty? container))
               (player-holding.content-filter player-holding container))
          (do
            (print "move content from target to player")
            (container-transfer container player-holding)
            ;; TODO: think about how to flexibly empty containers
            (when (pot-p container)
              (pot-empty container)))
          (and (container? player-holding)
               (not (container-empty? player-holding))
               (container-empty? container)
	           (container.content-filter container player-holding))
          ;; the player is holding a container so the contents of
          ;; the held container want to be transferred to the
          ;; selected container
          (do
            (print "move content from player to target")
            (container-transfer player-holding container)
            ;; TODO: think about how to flexibly empty containers
            (when (pot-p player-holding)
              (pot-empty player-holding)))
          ;; otherwise, add what the player is holding to the
          ;; contents of the container
          (and (not (container? player-holding))
               (container.content-filter container player-holding))
          (do
            (print "move item from player to target")
            (table.insert container.contents player-holding)
            (set player.placed-on nil))))))

(fn counter-contain-function [counter player]
  "Attempts to put what PLAYER is holding in COUNTER. More specialised
for counters."
  (if
   (container? counter.placed-on) (let [counter-container counter.placed-on]
                                    (counter-container:contain player))
   (and (not counter.placed-on)
        ;; prevent non-pots from being placed on a hob
        (not (and (= counter.station "hob")
                  (not (= player.placed-on.type "pot"))))) (do (set counter.placed-on player.placed-on)
                                                               (set player.placed-on nil))))

;; FIXME: it doesn't make sense for the pot to hold the "cooked state" as this is lost when the food is transferred to another container. i.e. if you take cooked soup out of a pot, put it in a plate, then put it back to the pot it appears as if the soup has uncooked itself.

;; instead what we can do is store the cooked information in the individual ingredients.

;; each item in a pot can have a cooking time, when all the items have done their cooking time the overall meal is cooked. if all the ingredients in the pot have completed their cooking time, the cooking time continues to rise. if all the ingredients pass the burn/spoilt threshold, the overall meal is considered burnt/spoilt

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

     :contents []
     :content-limit math.huge
     :content-filter (fn [pot thing]
                       (print "pot content filter")
                       (or (container? thing)
                           (and thing.prepared
                                (not pot.spoilt))))

     :contain standard-contain-function

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


     :contents []
     :content-limit math.huge
     :content-filter (fn [plate thing]
                       (when (= thing.type "pot")
                         (let [pot thing]
                           (and (= (length pot.contents) 3)
                                (> pot.cooking-time (pot-calculate-cooking-time pot))
                                (not pot.spoilt)))))

     :contain standard-contain-function

     :alive true}))

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

(fn delivery-point-p [thing]
  "Return t if THING is a delivery-point."
  (and thing
       (= "delivery-point" thing.type)))

(fn plate-return-update [plate-return deltatime]
  "Update plate return timer and spawn plate."
  (when plate-return.delivered
    (set plate-return.timer (+ plate-return.timer deltatime))
    (when (>= plate-return.timer 5) ;; 5 seconds to return
      (set plate-return.timer 0)
      (set plate-return.delivered false)
      (table.insert things (plate-create (plate-return.body:getX) (plate-return.body:getY))))))

(fn plate-return-p [thing]
  "Return t if THING is a plate return station."
  (and thing
       (= "plate-return" thing.type)))

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

(fn plate-empty [plate]
  "Empty PLATE."
  (set plate.contents []))

(fn player-drop [player selected-thing]
  "As PLAYER, drop the currently held thing."
  (let [thing player.placed-on]
    (if (container? selected-thing) (selected-thing:contain player)
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

     :contain (fn [bin player]
                (if
                 ;; bin the contents of the pot instead of the pot itself
                 (= player.placed-on.type "pot")
                 (let [pot player.placed-on]
                   (pot-empty pot))
                 ;; bin the contents of the plate instead of the plate itself
                 (= player.placed-on.type "plate")
                 (let [plate player.placed-on]
                   (plate-empty plate))
                 ;; bin what the player is holding
                 (set player.placed-on nil)))
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
     : fixture

     :contain counter-contain-function

     }))

(fn plate-return-draw [station]
  "Draw a plate return station."
  (with-colour 0.5 0.5 0.5
    (love.graphics.polygon "fill" (station.body:getWorldPoints (station.shape:getPoints)))))

(fn delivery-point-create [x y]
  "Create a delivery point."
  (let [body (love.physics.newBody world x y "static")
        shape (love.physics.newRectangleShape BOX_SIZE BOX_SIZE)
        fixture (love.physics.newFixture body shape 1)]
    (fixture:setCategory 1)
    (fixture:setMask)
    {:type "delivery-point"
     :alive true

     :contain (fn [delivery-point player]
                (when (and (= player.placed-on.type "plate")
                           (< 0 (length player.placed-on.contents)))
                  (set player.placed-on.alive false)
                  (set player.placed-on nil)
                  (each [_ thing (ipairs things)]
                    (when (= thing.type "plate-return")
                      (set thing.delivered true)))))

     : body
     : shape
     : fixture}))

(lambda plate-return-create [x y]
  "Create a plate return station."
  (let [body (love.physics.newBody world x y "static")
        shape (love.physics.newRectangleShape BOX_SIZE BOX_SIZE)
        fixture (love.physics.newFixture body shape 1)]
    (fixture:setCategory 1)
    (fixture:setMask)
    {:type "plate-return"
     :alive true
     :delivered false
     :timer 0
     :placed-on nil
     :body body
     :shape shape
     :fixture fixture}))

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
  (table.insert things (plate-return-create 100 100))
  (table.insert things (apple-create 100 500))
  (table.insert things (bin-create 900 100))
  (table.insert things (counter-create 1200 100))
  (table.insert things (counter-create 1200 500 "chop"))
  (table.insert things (counter-create 200 500 "hob"))
  (table.insert things (counter-create 600 600 "box"))
  (table.insert things (delivery-point-create 800 600))
  (table.insert things (pot-create 600 500))
  (table.insert things (plate-create 300 300)))

(fn player-p [thing]
  "Return non-nil if THING is a player."
  (and thing (= thing.type "player")))

(fn thing-update [thing deltatime]
  "Update THING."
  (if (player-p thing) (player-update thing)
      ;; (pot-p thing) (pot-update thing deltatime)
      (counter-p thing) (counter-update thing deltatime)
      (plate-return-p thing) (plate-return-update thing deltatime)))

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

(fn delivery-point-draw [delivery-point]
  "Draw a delivery point."
  (with-colour 1 1 0
    (love.graphics.polygon "fill"
                           (delivery-point.body:getWorldPoints (delivery-point.shape:getPoints)))))

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
      (plate-return-p thing)
      (plate-return-draw thing)
      (delivery-point-p thing)
      (delivery-point-draw thing)
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
