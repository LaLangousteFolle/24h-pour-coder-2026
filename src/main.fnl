;; title:  Comportement Ennemis + UI FNAF
;; author: Noe SEILER
;; desc:   Pathfinding + UI overlay interactive
;; script: fennel

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
   :prev-mouse  false   ;; pour detecter leading edge du clic

   :enemies
    [{:name "NODES" :room :nodes :timer 0 :color 2 :just-moved false}
     {:name "DM"    :room :dm    :timer 0 :color 8 :just-moved false}]

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
;; (coordonnees sur ecran 240x136)
;;
;; HOVER : entrer dans la zone change la vue
;; CLICK : clic dans la zone change la vue
;;         + change le curseur au survol
;; =========================

;; Zones hover invisibles (sprites par dessus)
(global hover-zones
  [{:x 10 :y 2  :w 220 :h 18 :target :vent  :label "VENT"}
   {:x 10 :y 116 :w 160 :h 18 :target :gen   :label "GEN"}])

;; Zones cliquables (sprites par dessus)
(global click-zones
  [{:x 2  :y 55 :w 18 :h 12 :target :cam-a  :label "CAM A"}
   {:x 2  :y 72 :w 18 :h 12 :target :cam-b  :label "CAM B"}
   {:x 220 :y 30 :w 18 :h 76 :target :porte  :label "PORTE"}])

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

;; =========================
;; UI LOGIC
;; =========================

(fn update-ui []
  (let [(mx my mb) (mouse)
        clicking   (and (= mb 1) (not STATE.prev-mouse))]
    (tset STATE :prev-mouse (= mb 1))
    (tset STATE :cursor :normal)

    ;; Si on est en vue office : hover zones actives
    (when (= STATE.view :office)
      (each [_ z (ipairs hover-zones)]
        (when (in-zone? mx my z)
          (tset STATE :view z.target))))

    ;; Click zones actives depuis n'importe quelle vue office-like
    (each [_ z (ipairs click-zones)]
      (when (in-zone? mx my z)
        (tset STATE :cursor :pointer)
        (when clicking
          (tset STATE :view z.target)
          (add-log (.. ">" z.label)))))

    ;; Retour office : bouton A ou sortir de zone hover par le bas/haut
    ;; (gere dans handle-input)
    ))

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
              (sfx 0 -1 -1 0)
              (set e.room next))))))))

(local tv-stages [:tv-spawn :tv-bout :tv-milieu :tv-porte])

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
            (sfx 1 -1 -1 0)))))))

(fn update-enervement []
  (tset STATE :enrv-timer (+ STATE.enrv-timer 1))
  (when (>= STATE.enrv-timer 600)
    (tset STATE :enrv-timer 0)
    (tset STATE :enervement (math.min 100 (+ STATE.enervement 1)))))

;; =========================
;; DRAW VIEWS
;; =========================

(fn draw-map-debug []
  (each [from transitions (pairs graph)]
    (let [fp (. room-pos from)]
      (when fp
        (each [_ [to _] (ipairs transitions)]
          (let [tp (. room-pos to)]
            (when tp (line (. fp 1) (. fp 2) (. tp 1) (. tp 2) 6)))))))
  (each [id pos (pairs room-pos)]
    (let [[x y] pos]
      (rect  (- x 18) (- y 5) 36 11 0)
      (rectb (- x 18) (- y 5) 36 11 6)
      (print (tostring id) (- x 17) (- y 3) 7 false 1 true)))
  (each [i e (ipairs STATE.enemies)]
    (let [pos (. room-pos e.room)]
      (when pos
        (let [[x y] pos ox (* (- i 1) 8)]
          (circ (+ x ox) (- y 10) 3 e.color))))))

(fn draw-office []
  ;; TODO: remplacer par le sprite du bureau
  (cls 13)
  (rect 10 2  220 18 5)   ;; placeholder VENT
  (rect 10 116 160 18 5)  ;; placeholder GEN
  (rect 2  55  18  12 6)  ;; placeholder CAM A
  (rect 2  72  18  12 6)  ;; placeholder CAM B
  (rect 220 30 18  76 6)  ;; placeholder PORTE
  (print "VENT"  100 8  0)
  (print "GEN"   80  122 0)
  (print "A"     7   59  0)
  (print "B"     7   76  0)
  (print "P" 224 65 0)
  (print "O" 224 72 0)
  (print "R" 224 79 0)
  (print "T" 224 86 0)
  (print "E" 224 93 0)
  ;; status bar
  (rect 0 0 240 10 0)
  (print (.. "DIFF:" STATE.difficulty " ENRV:" STATE.enervement) 2 2 7))

(fn draw-vent []
  (cls 1)
  (print "-- SYSTEME VENTILATION --" 40 20 7)
  (print "T est ici si dans tv-stages" 30 40 6)
  (print (.. "T room: " (tostring STATE.T.room)) 30 55 4)
  (print "A=retour office" 70 120 5))

(fn draw-gen []
  (cls 4)
  (print "-- GENERATEUR --" 60 20 0)
  (print "A=retour office" 70 120 0))

(fn draw-cam [label]
  (cls 0)
  (print (.. "-- CAMERA " label " --") 70 20 7)
  ;; debug : ennemis dans les salles visibles par cette cam
  (each [i e (ipairs STATE.enemies)]
    (print (.. e.name ": " (tostring e.room)) 30 (+ 40 (* i 10)) e.color))
  (print (.. "T: " (tostring STATE.T.room)) 30 70 4)
  (print "A=retour office" 70 120 6))

(fn draw-porte []
  (cls 6)
  (print "-- PORTE --" 80 20 0)
  (print "[ fermer ]" 80 60 2)
  (print "A=retour office" 70 120 0))

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
;; DEBUG OVERLAY
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
  (when (btnp 4) (tset STATE :view :office))
  (when (btnp 2) (tset STATE :debug (not STATE.debug)))
  (when (btnp 0) (tset STATE :difficulty (math.min 20 (+ STATE.difficulty 1))))
  (when (btnp 1) (tset STATE :difficulty (math.max 1  (- STATE.difficulty 1)))))

;; =========================
;; TIC
;; =========================

(math.randomseed 42)

(fn _G.TIC []
  (handle-input)
  (update-ui)
  (update-enervement)
  (update-enemies)
  (update-T)

  (case STATE.view
    :office (draw-office)
    :vent   (draw-vent)
    :gen    (draw-gen)
    :cam-a  (draw-cam "A")
    :cam-b  (draw-cam "B")
    :porte  (draw-porte))

  (draw-debug)
  (draw-cursor))