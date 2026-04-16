;; title:  FNAC
;; author: N/A (Manchot Analfabete)
;; desc:   Un peu tout
;; script: fennel

(var battery 100)
(var activated 0)
(var power_out false)
(var power_out_timer 0)
(var gameover false)
(var gameover_timer 0)
(var gameover_msg "")
(var t 0)
(var couleur-texte 5)
(var couleur-fond 0)
(var menu 0)

;; menu 0 -> main screen
;; menu 1 -> office sans lumiere
;; menu 2 -> office avec lumiere
;; menu 3 -> vent
;; menu 4 -> porte droite
;; menu 5 -> generateur

(var nights_unlocked 5)
(var nights_completed 0)
(var difficulty 20)
(var previous_left false)

;; =========================
;; MENU TITRE
;; =========================

(fn nightSelection [x y]
  (when (and (< x 50) (> y 22) (< y 40) (>= nights_unlocked 1))
    (set menu 1) (tset STATE :difficulty 4)  (music 1) (sfx 30 "D#4" -1 3 7 4))
  (when (and (< x 50) (> y 43) (< y 60) (>= nights_unlocked 2))
    (set menu 1) (tset STATE :difficulty 8)  (music 2) (sfx 30 "D#4" -1 3 7 4))
  (when (and (< x 50) (> y 63) (< y 80) (>= nights_unlocked 3))
    (set menu 1) (tset STATE :difficulty 12) (music 3) (sfx 30 "D#4" -1 3 7 4))
  (when (and (< x 50) (> y 83) (< y 100) (>= nights_unlocked 4))
    (set menu 1) (tset STATE :difficulty 16) (music 4) (sfx 30 "D#4" -1 3 7 4))
  (when (and (< x 50) (> y 103) (< y 120) (>= nights_unlocked 5))
    (set menu 1) (tset STATE :difficulty 20) (music 5) (sfx 30 "D#4" -1 3 7 4)))

(fn displayMenu []
  (cls couleur-fond)
  (print "Five Nights at CERI's" 60 0 couleur-texte)
  (for [i 1 5]
    (let [night (.. "Night " (tostring i))
          color (if (<= i nights_unlocked) couleur-texte 15)]
      (print night 10 (* i 22) color))))

;; =========================
;; GRAPH
;; =========================
(global graph
  {:nodes           [[:couloir-ouest-1 100]]
   :couloir-ouest-1 [[:foyer           80]
                     [:couloir-ouest-2 20]]
   :foyer           [[:couloir-ouest-2 100]]
   :dm              [[:sas-amphi       100]]
   :sas-amphi       [[:couloir-sud     100]]
   :couloir-sud     [[:couloir-ouest-2 90]
                     [:main-room       10]]
   :couloir-ouest-2 [[:main-room       100]]
   :main-room       []})

(global cameras
  [{:name "Amphi Blaise"     :room :dm}
   {:name "SAS Amphi Blaise" :room :sas-amphi}
   {:name "Couloir Ouest"    :room :couloir-ouest-1}
   {:name "Nodes"            :room :nodes}])

;; =========================
;; STATE
;; =========================
(global STATE
  {:difficulty  4
   :log         []
   :enervement  0
   :enrv-timer  0
   :debug       false
   :view        :office
   :cursor      :normal
   :prev-mouse  false
   :lever       0
   :light       0
   :cam         0
   :cam-index   0

   :nodes-flashes    0
   :nodes-prev-light 0
   :dm-hold-timer    0
   :dm-hold-required 0
   :counter-msg      ""
   :counter-timer    0

   :qte-cursor    0
   :qte-dir       1
   :qte-red-pos   50
   :qte-hits      0
   :qte-flash     0
   :qte-flash-ok  false

   :shark-stage        0
   :shark-timer        0
   :shark-attack-timer 0
   :shark-prev-click   false

   :enemies
    [{:name "NODES" :room :nodes :timer 0 :color 2 :just-moved false :attack-timer 0 :sprite 418 :sw 4 :sh 4}
     {:name "DM"    :room :dm    :timer 0 :color 8 :just-moved false :attack-timer 0 :sprite 354 :sw 2 :sh 2}]

   :T
    {:room       :tv-spawn
     :timer      0
     :energie    100
     :just-moved false}})

;; =========================
;; POSITIONS DEBUG MAP
;; =========================
(global room-pos
  {:nodes           [30  20]
   :couloir-ouest-1 [30  40]
   :foyer           [30  60]
   :couloir-ouest-2 [80  80]
   :main-room       [80  100]
   :dm              [180 20]
   :sas-amphi       [180 40]
   :couloir-sud     [180 60]
   :tv-spawn        [130 15]
   :tv-bout         [130 38]
   :tv-milieu       [130 61]
   :tv-porte        [130 84]})

;; =========================
;; ZONES UI
;; =========================
(global hover-zones
  [;;{:x 10  :y 2   :w 220 :h 18 :target :vent  :label "VENT"}
   {:x 10  :y 116 :w 160 :h 18 :target :gen   :label "GEN"}])
   ;;{:x 220 :y 30  :w 18  :h 76 :target :porte :label "PORTE"})

;; =========================
;; HELPERS
;; =========================
(fn in-zone? [mx my z]
  (and (>= mx z.x) (<= mx (+ z.x z.w))
       (>= my z.y) (<= my (+ z.y z.h))))

(fn add-log [msg]
  (table.insert STATE.log 1 msg)
  (when (> (length STATE.log) 4) (table.remove STATE.log)))

(fn move-frames []
  (math.max 60
    (- 300
       (* STATE.difficulty 8)
       (* STATE.enervement 1.5))))

(fn weighted-pick [transitions]
  (let [total (accumulate [s 0 _ [_ w] (ipairs transitions)] (+ s w))
        roll  (math.random 1 total)]
    (var acc 0)
    (var result nil)
    (each [_ [dest w] (ipairs transitions) &until result]
      (set acc (+ acc w))
      (when (>= acc roll) (set result dest)))
    result))

(fn should-move? []
  (<= (math.random 1 20) STATE.difficulty))

(fn get-enemy [name]
  (var result nil)
  (each [_ e (ipairs STATE.enemies) &until result]
    (when (= e.name name) (set result e)))
  result)

;; =========================
;; TOGGLES  (avant click-zones)
;; =========================
(fn toggle-light []
  (when (not power_out)
    (if (= STATE.light 0)
      (do (set activated (+ activated 1)) (tset STATE :light 1)
      (sfx 29 "C#5" -1 3 15 4))
      
      (do (set activated (- activated 1)) (tset STATE :light 0)
      (sfx 29 "C#4" -1 3 15 4)))))

(fn toggle-lever []
  (when (not power_out)
    (if (= STATE.lever 0)
      (do (set activated (+ activated 1)) (tset STATE :lever 1) 
      (sfx 17 "C-3" -1 3 15 5)
      )
      (do (set activated (- activated 1)) (tset STATE :lever 0) 
      (sfx 16 "C-4" -1 3 15 5)
      ))))

(fn toggle-cam []
  (when (not power_out)
    (if (= STATE.cam 0)
      (do (set activated (+ activated 1)) (tset STATE :cam 1) (tset STATE :view :cam) (sfx 18 "C-6" -1 3 15 5))
      (do (set activated (- activated 1)) (tset STATE :cam 0) (tset STATE :view :office) (set menu 1) (sfx 18 "C-6" -1 3 15 5)))))

;; =========================
;; CLICK-ZONES  (apres les toggles)
;; =========================
(global click-zones
  [{:x 2   :y 55  :w 18 :h 12 :action toggle-light :label "Lumiere"}
   {:x 2   :y 72  :w 18 :h 12 :action toggle-lever :label "Lever"}
   {:x 98 :y 69 :w 32 :h 32 :action toggle-cam   :label "CAM"}])

;; =========================
;; UI LOGIC
;; =========================
(fn update-ui []
  (let [(mx my mb) (mouse)
        clicking   (and mb (not STATE.prev-mouse))]
    (tset STATE :prev-mouse mb)
    (tset STATE :cursor :normal)

    (when (= STATE.view :office)
      (each [_ z (ipairs hover-zones)]
        (when (in-zone? mx my z)
          (tset STATE :view z.target))))

    (each [_ z (ipairs click-zones)]
      (when (in-zone? mx my z)
        (tset STATE :cursor :pointer)
        (when clicking
          (if z.action
            (z.action)
            (do
              (tset STATE :view z.target)
              (add-log (.. ">" z.label)))))))
    ;; cam nav: left/right buttons
    (when (and (= menu 6) clicking (>= my 3) (<= my 17))
      (when (and (>= mx 4) (<= mx 18))
        (tset STATE :cam-index (% (+ STATE.cam-index (- (length cameras) 1)) (length cameras)))
        (sfx 30 "D#4" -1 3 7 4) )
      (when (and (>= mx 222) (<= mx 236))
        (tset STATE :cam-index (% (+ STATE.cam-index 1) (length cameras)))
        (sfx 30 "D#4" -1 3 7 4)))))

;; =========================
;; ENEMIES
;; =========================
(fn update-enemies []
  (each [_ e (ipairs STATE.enemies)]
    (set e.just-moved false)
    (set e.timer (+ e.timer 1))
    (when (>= e.timer (move-frames))
      (set e.timer 0)
      (when (should-move?)
        (let [transitions (. graph e.room)]
          (when (and transitions (> (length transitions) 0))
            (let [next (weighted-pick transitions)]
              (set e.just-moved true)
              (add-log (.. e.name "->" (tostring next)))
              (set e.room next))))))))

(global tv-stages [:tv-spawn :tv-bout :tv-milieu :tv-porte])

(fn tv-stage-index []
  (var idx 1)
  (each [i s (ipairs tv-stages) &until (= s STATE.T.room)]
    (set idx i))
  idx)

(fn update-T []
  (let [t STATE.T]
    (set t.just-moved false)
    (set t.timer (+ t.timer 1))
    (when (>= t.timer (move-frames))
      (set t.timer 0)
      (when (should-move?)
        (let [idx (tv-stage-index)]
          (when (< idx 4)
            (set t.just-moved true)
            (set t.room (. tv-stages (+ idx 1)))
            (add-log (.. "T->" (tostring t.room)))
            ))))))

(fn update-enervement []
  (tset STATE :enrv-timer (+ STATE.enrv-timer 1))
  (when (>= STATE.enrv-timer 600)
    (tset STATE :enrv-timer 0)
    (tset STATE :enervement (math.min 100 (+ STATE.enervement 1)))))

(fn update-counters []
  (let [nodes (get-enemy "NODES")
        dm    (get-enemy "DM")]

    ;; NODES: flash light 3 times (count 0->1 edges)
    (let [edge (and (= STATE.light 1) (= STATE.nodes-prev-light 0))]
      (tset STATE :nodes-prev-light STATE.light)
      (if (= nodes.room :main-room)
        (when edge
          (tset STATE :nodes-flashes (+ STATE.nodes-flashes 1))
          (when (>= STATE.nodes-flashes 3)
            (set nodes.room :nodes)
            (set nodes.timer 0)
            (tset STATE :nodes-flashes 0)
            (tset STATE :counter-msg "NODES repousse !")
            (tset STATE :counter-timer 120)
            (add-log "NODES<OUT>")))
        (tset STATE :nodes-flashes 0)))

    ;; DM: hold lever for a random time (3-7 seconds)
    (if (= dm.room :main-room)
      (do
        (when (= STATE.dm-hold-required 0)
          (tset STATE :dm-hold-required (math.random 180 420)))
        (if (= STATE.lever 1)
          (do
            (tset STATE :dm-hold-timer (+ STATE.dm-hold-timer 1))
            (when (>= STATE.dm-hold-timer STATE.dm-hold-required)
              (set dm.room :dm)
              (set dm.timer 0)
              (tset STATE :dm-hold-timer 0)
              (tset STATE :dm-hold-required 0)
              (tset STATE :counter-msg "Donald repousse !")
              (tset STATE :counter-timer 120)
              (add-log "Donald<OUT>")))
          (tset STATE :dm-hold-timer 0)))
      (do
        (tset STATE :dm-hold-timer 0)
        (tset STATE :dm-hold-required 0)))

    ;; tick down counter message
    (when (> STATE.counter-timer 0)
      (tset STATE :counter-timer (- STATE.counter-timer 1)))
    ;; attack timers: game over if enemy stays in main-room for 5s
    ;; DM timer pauses while the lever is held
    (each [_ e (ipairs STATE.enemies)]
      (if (= e.room :main-room)
        (do
          (let [paused (and (= e.name "DM") (= STATE.lever 1))]
            (when (not paused)
              (set e.attack-timer (+ e.attack-timer 1)))
            (when (and (>= e.attack-timer 300) (not gameover))
              (set gameover true)
              (set gameover_timer 0)
              (set gameover_msg (.. e.name " t'a attrape !")))))
        (set e.attack-timer 0)))))

(fn update-qte []
  (when (= menu 5)
    (tset STATE :qte-cursor (+ STATE.qte-cursor (* STATE.qte-dir 1.5)))
    (when (>= STATE.qte-cursor 160)
      (tset STATE :qte-dir -1) (tset STATE :qte-cursor 160))
    (when (<= STATE.qte-cursor 0)
      (tset STATE :qte-dir 1) (tset STATE :qte-cursor 0))
    (when (> STATE.qte-flash 0)
      (tset STATE :qte-flash (- STATE.qte-flash 1)))
    (when (keyp 48)
      (let [cx (math.floor STATE.qte-cursor)]
        (if (and (>= cx STATE.qte-red-pos)
                 (<= cx (+ STATE.qte-red-pos 25)))
          (do
            (tset STATE :qte-hits (+ STATE.qte-hits 1))
            (tset STATE :qte-red-pos (math.random 5 130))
            (tset STATE :qte-flash 40)
            (tset STATE :qte-flash-ok true)
            (when (>= STATE.qte-hits 5)
              (set battery 100)
              (tset STATE :qte-hits 0)
              (tset STATE :qte-flash 90)))
          (do
            (tset STATE :qte-flash 40)
            (tset STATE :qte-flash-ok false)))))))

(fn update-shark []
  (let [(mx my mb) (mouse)
        clicking (and mb (not STATE.shark-prev-click))]
    (tset STATE :shark-prev-click mb)
    ;; stage progression: 0->1->2->3, one step every 7s
    (when (< STATE.shark-stage 3)
      (tset STATE :shark-timer (+ STATE.shark-timer 1))
      (when (>= STATE.shark-timer 420)
        (tset STATE :shark-stage (+ STATE.shark-stage 1))
        (tset STATE :shark-timer 0)))
    ;; stage 3: click center to push back to stg1, else -50 battery after 5s
    (when (= STATE.shark-stage 3)
      (tset STATE :shark-attack-timer (+ STATE.shark-attack-timer 1))
      (when (and clicking
                 (>= mx 160) (<= mx 192)
                 (>= my 60) (<= my 92)
                 (or (= menu 1) (= menu 2)))
        (tset STATE :shark-stage 1)
        (tset STATE :shark-timer 0)
        (tset STATE :shark-attack-timer 0)
        (sfx 24 "A-6" -1 3 15 5))
      (when (>= STATE.shark-attack-timer 300)
        (set battery (math.max 0 (- battery 50)))
        (add-log "SHARK-50bat")
        (tset STATE :shark-stage 0)
        (tset STATE :shark-timer 0)
        (tset STATE :shark-attack-timer 0)
        (sfx 19 "E-1" -1 1 15 6)
        ))))

;; =========================
;; DRAW VIEWS
;; =========================
(fn draw-office []
  (cls 13)
  ;;(rect 10  2   220 18 5)
  ;;(rect 10  116 160 18 5)
  ;;(rect 2   55  18  12 (if (= STATE.light 1) 7 6))
  ;;(rect 2   72  18  12 (if (= STATE.lever 1) 7 6))
  ;;(rect 220 30  18  76 6)
  ;;(print "VENT"  100 8   0)
  ;;(print "GEN"   80  122 0)
  ;;(print "A"     7   59  0)
  ;;(print "B"     7   76  0)
  ;;(print "P" 224 65 0)
  ;;(print "O" 224 72 0)
  ;;(print "R" 224 79 0)
  ;;(print "T" 224 86 0)
  ;;(print "E" 224 93 0)
  (if (= STATE.light 1) 
  (do (map 30 0 240 136 0 0 -1 1)
  (if (= STATE.shark-stage 2)
      (spr 388 60 57 11 2 0 0 2 2))
    (if (= STATE.shark-stage 3)
      (spr 384 160 57 11 2 0 0 2 2))))
  (if (= STATE.light 0) (map 150 0 240 136 0 0 -1 1))

  
  (if (= STATE.lever 0) (spr 268 7 72 11 1 0 0 2 2)
  (do
  (spr 262 28 0 11 2 0 0 2 2)
  (spr 264 60 0 11 2 0 0 2 2)
  (spr 264 92 0 11 2 0 0 2 2)
  (spr 264 124 0 11 2 0 0 2 2)
  (spr 264 156 0 11 2 0 0 2 2)
  (spr 262 179 0 11 2 1 0 2 2)

  (spr 294 28 32 11 2 0 0 2 2)
  (spr 296 60 32 11 2 0 0 2 2)
  (spr 296 92 32 11 2 0 0 2 2)
  (spr 296 124 32 11 2 0 0 2 2)
  (spr 296 156 32 11 2 0 0 2 2)
  (spr 294 179 32 11 2 1 0 2 2)

  (spr 294 28 64 11 2 0 0 2 2)
  (spr 296 60 64 11 2 0 0 2 2)
  (spr 296 92 64 11 2 0 0 2 2)
  (spr 296 124 64 11 2 0 0 2 2)
  (spr 296 156 64 11 2 0 0 2 2)
  (spr 294 179 64 11 2 1 0 2 2)))
  (if (= STATE.lever 1) (spr 270 7 72 11 1 0 0 2 2))
  (if (= STATE.light 1) (spr 300 7 55 11 1 0 0 2 2))
  (if (= STATE.light 0) (spr 302 7 55 11 1 0 0 2 2))
  (spr 292 98 69 11 2 0 0 2 2)

  
  ;;(rect 0 0 240 10 0)
  ;;(print (.. "DIFF:" STATE.difficulty " ENRV:" STATE.enervement) 2 2 7)
  ;; counter prompts
  (let [nodes (get-enemy "NODES")
        dm    (get-enemy "DM")]
    (when (= nodes.room :main-room)
      (spr 418 120 42 11 1 0 0 4 4))
    (when (= dm.room :main-room)
      (spr 354 165 54 11 1 0 0 2 2)
      (when (> STATE.dm-hold-required 0)
        (let [prog (math.floor (* 100 (/ STATE.dm-hold-timer STATE.dm-hold-required)))]
          (rect 70 45 prog 5 8)
          ))))
  ;; shark
  (when (> STATE.shark-stage 0)
    
    (when (= STATE.shark-stage 3)
      ;;(rectb 96 58 48 28 8)
      (let [secs (math.max 1 (math.ceil (/ (- 300 STATE.shark-attack-timer) 60)))]
        (print (.. secs "s") 116 88 8))))
  (let [bw (math.floor (* 2.4 battery))
        bc  (if (> battery 50) 11 (if (> battery 25) 4 8))]
    (rect 0 130 bw 4 bc)
    (rectb 0 130 240 4 7)))

(fn draw-enlighted []
  (draw-office))

(fn draw-vent []
  (cls 1)
  (print "-- SYSTEME VENTILATION --" 40 20 7)
  (print (.. "T room: " (tostring STATE.T.room)) 30 55 4)
  (print "F=retour office" 70 120 5))

(fn draw-gen []
  (cls 0)
  (spr 256 60 20 11 4 0 0 4 2)
  (print "-- GENERATEUR --" 10 10 5)
  ;; 5 progress slots
  (for [i 1 5]
    (let [bx (+ 68 (* (- i 1) 22))]
      (rect bx 95 18 8 (if (<= i STATE.qte-hits) 11 0))
      (rectb bx 95 18 8 7)))
  ;; gauge
  (let [gx 40  gy 105  gw 160  gh 12
        cx (math.floor STATE.qte-cursor)]
    (rect gx gy gw gh 0)
    (rect (+ gx STATE.qte-red-pos) gy 25 gh 8)
    (rect (+ gx cx) gy 3 gh 7)
    (rectb gx gy gw gh 7))
  ;; feedback
  (when (> STATE.qte-flash 0)
    (if (not STATE.qte-flash-ok)
      (do
        (print "RATE !"             96 75 8)
        (sfx 22 "C-2" -1 3 10 5))
      (if (> STATE.qte-flash 60)
        (print "BATTERIE PLEINE !" 57 75 11)
        (do
          (spr 260 124 20 11 4 0 0 2 2)
          (print "SUCCES !"          88 75 11)
          (sfx 23 "C-2" -1 3 10 5)))))
  (print "[ESPACE] pour recharger" 40 90 0)
  (print "F=retour office" 70 120 0))

(fn draw-power-out []
  (cls 0)
  (print "PANNE DE COURANT" 55 50 8)
  (let [secs (math.max 0 (- 5 (math.floor (/ power_out_timer 60))))]
    (print (.. "Retour dans " (tostring secs) "s") 70 70 7))
  (print "Nuit echouee..." 65 90 6))

(fn draw-gameover []
  (cls 0)
  (print "GAME OVER" 85 45 8)
  (print gameover_msg 55 62 7)
  (let [secs (math.max 0 (- 5 (math.floor (/ gameover_timer 60))))]
    (print (.. "Retour dans " (tostring secs) "s") 70 80 6))
  (music 0))

(fn draw-cam []
  (cls 0)
  (let [cam (. cameras (+ STATE.cam-index 1))
        nx  (math.floor (/ (- 240 (* (length cam.name) 6)) 2))]
    ;; top bar
    (if (= STATE.cam-index 0) (map 90 0 240 136 0 0 -1 1))
    (if (= STATE.cam-index 1) (map 60 0 240 136 0 0 -1 1))
    (if (= STATE.cam-index 3) 
    (do
    (map 120 0 240 136 0 0 -1 1) 
    (spr 418 120 78 11 1 0 0 4 4)))
    (if (= STATE.cam-index 2) (map 0 0 240 136 0 0 -1 1))
    (rect 0 0 240 20 0)
    ;; left button
    (spr  356 4 3 11 1 1 0 2 2)
    ;; right button
    (spr  356 222 3 11 1 0 0 2 2)
    (print cam.name nx 7 7)
    
    )
    ;; camera name centered
  


  
  
    (line 0 20 239 20 7)
    ;; enemies present in this room
    (let [cam (. cameras (+ STATE.cam-index 1))]
      (each [_ e (ipairs STATE.enemies)]
        (when (= e.room cam.room)
          (spr e.sprite
               (math.floor (/ (- 240 (* e.sw 8)) 2))
               (math.floor (/ (- 116 (* e.sh 8)) 2))
               11 1 0 0 e.sw e.sh)))))

(fn draw-porte []
  (cls 6)
  (print "-- PORTE --" 80 20 0)
  (print "[ fermer ]" 80 60 2)
  (print "F=retour office" 70 120 0))

;; =========================
;; CURSEUR
;; =========================
(fn draw-cursor []
  (let [(mx my) (mouse)]
    (if (= STATE.cursor :pointer)
      (do
        (line mx (- my 4) mx (+ my 4) 7)
        (line (- mx 4) my (+ mx 4) my 7)
        (circb mx my 3 7))
      (do
        (line mx my (+ mx 5) (+ my 3) 12)
        (line mx my (+ mx 2) (+ my 5) 12)
        (line (+ mx 2) (+ my 5) (+ mx 5) (+ my 3) 12)))))

;; =========================
;; BATTERY
;; =========================
(fn battery-update []
  (let [drain (+ 0.2 (* activated 1.5))]
    (set battery (math.max 0 (- battery drain))))
  (when (and (<= battery 0) (not power_out))
    (set power_out true)
    (set activated 0)
    (tset STATE :light 0)
    (tset STATE :lever 0)
    (tset STATE :cam 0)))

;; =========================
;; DEBUG
;; =========================
(fn draw-debug []
  (when STATE.debug
    (let [(mx my) (mouse)]
      (print (.. "MX:" mx " MY:" my " V:" (tostring STATE.view)) 2 126 12)
      (each [_ z (ipairs hover-zones)]
        (rectb z.x z.y z.w z.h 10))
      (each [_ z (ipairs click-zones)]
        (rectb z.x z.y z.w z.h 2)))))

;; =========================
;; INPUT
;; =========================
(fn handle-input []
  (when (keyp 6)
    (when (= STATE.cam 1)
      (set activated (- activated 1))
      (tset STATE :cam 0))
    (tset STATE :view :office)
    (set menu 1))
  (when (keyp 2)
    (tset STATE :debug (not STATE.debug)))
  (when (keyp 54)
    (tset STATE :difficulty (math.min 20 (+ STATE.difficulty 1))))
  (when (keyp 55)
    (tset STATE :difficulty (math.max 1 (- STATE.difficulty 1)))))

;; =========================
;; TIC
;; =========================
(math.randomseed 42)
(music 0)

(fn _G.TIC []
  (var (x y left) (mouse))

  ;; -- MENU TITRE --
  (when (= menu 0)
    (displayMenu)
    (when (and (= previous_left false) (= left true))
      (let [prev menu]
        (nightSelection x y)
        (when (> menu prev)
          ;; full reset on new night
          (set battery 100) (set activated 0) (set t 0)
          (set power_out false) (set power_out_timer 0)
          (set gameover false) (set gameover_timer 0)
          (tset STATE :light 0) (tset STATE :lever 0) (tset STATE :cam 0)
          (tset STATE :view :office)
          (tset STATE :enervement 0) (tset STATE :enrv-timer 0)
          (tset STATE :nodes-flashes 0)
          (tset STATE :dm-hold-timer 0) (tset STATE :dm-hold-required 0)
          (tset STATE :qte-hits 0) (tset STATE :qte-cursor 0) (tset STATE :qte-dir 1)
          (tset STATE :shark-stage 0) (tset STATE :shark-timer 0) (tset STATE :shark-attack-timer 0)
          (tset STATE :cam-index 0)
          (each [_ e (ipairs STATE.enemies)]
            (set e.room (if (= e.name "NODES") :nodes :dm))
            (set e.timer 0) (set e.attack-timer 0) (set e.just-moved false))
          (let [T STATE.T]
            (set T.room :tv-spawn) (set T.timer 0) (set T.just-moved false))))))

  ;; -- JEU --
  (when (>= menu 1)
    (if gameover
      (do
        (set gameover_timer (+ gameover_timer 1))
        (draw-gameover)
        (when (>= gameover_timer 300)
          (set gameover false)
          (set gameover_timer 0)
          (set menu 0)
          (music 0)))
      power_out
      (do
        (set power_out_timer (+ power_out_timer 1))
        (draw-power-out)
        (when (>= power_out_timer 300)
          (set power_out false)
          (set power_out_timer 0)
          (set activated 0)
          (set menu 0)))
      (do
        (handle-input)
        (update-ui)
        (update-enervement)
        (update-enemies)
        (update-T)
        (update-counters)
        (update-qte)
        (update-shark)
        (when (= (% t 60) 0) (battery-update))

        (when (= menu 1)
          (case STATE.view
            :vent   (set menu 3)
            :gen    (set menu 5)
            :light  (set menu 2)
            :porte  (set menu 4)
            :cam    (set menu 6)))

        (when (= menu 1) (draw-office))
        (when (= menu 2) (draw-enlighted))
        (when (= menu 3) (draw-vent))
        (when (= menu 4) (draw-porte))
        (when (= menu 5) (draw-gen))
        (when (= menu 6) (draw-cam)))))

  (set t (+ t 1))

  (draw-debug)
  (draw-cursor)

  (set previous_left left))