;; title:  Template de base
;; author: Quentin
;; script: fennel
;; input: mouse

(var couleur-texte 6)
(var couleur-fond 0)

;; Variable pour l'animation
(var t 0)

(var menu 0) ;; ID of the screen that needs to be shown
;; menu 0 -> main screen
;; menu 1 -> main point of view without light
;; menu 2 -> main point of view with light
;; menu 3 -> Vent point of view
;; menu 4 -> Right door point of view
;; menu 5 -> generator point of  view

(var nights_unlocked 1) ;; Number of nights unlocked
(var nights_completed 0) ;; Number of nights completed
(var difficulty 4) ;;Difficulty of the game
(var previous_left false) ;;used to make sure that it's a simple click

(lambda displayMenu []

;;Display the main title
(cls couleur-fond)
(print "Five Nights at CERI's" 60 0 couleur-texte)

;;Display the different nights
(var night "Night ")
(for [i 1 5]
(set night (.. "Night " (tostring i)))
(if (<= i nights_unlocked)
 ;;Nights unlocked so far
 (print night 10 (* i 22) couleur-texte) ;
 (> i nights_unlocked)
    ;;Nights locked so far
    (print night 10 (* i 22) 10)
)))

(lambda nightSelection [x y]

(if (and ( and (< x 100 ) ( and (> y 22) (< y 40) ) ) (>= nights_unlocked 1))
  (set menu 1)
  (set difficulty 4)
)

(if (and ( and (< x 100 ) ( and (> y 43) (< y 60) ) ) (>= nights_unlocked 2))
  (set menu 1)
  (set difficulty 8)
)

(if (and ( and (< x 100 ) ( and (> y 63) (< y 80) ) ) (>= nights_unlocked 3))
  (set menu 1)
  (set difficulty 12)
)

(if (and ( and (< x 100 ) ( and (> y 83) (< y 100) ) ) (>= nights_unlocked 4))
  (set menu 1)
  (set difficulty 16)
)

(if (and ( and (< x 100 ) ( and (> y 22) (< y 40) ) ) (>= nights_unlocked 1))
  (set menu 1)
  (set difficulty 20)
)
)




;; Boucle principale exécutée à 60 FPS
(fn _G.TIC []
  ;; 1 Black screen (clean-up)
  (cls couleur-fond)
  (var (x y left) (mouse))
  (var pressed (and (= previous_left false) (= left true)))
  (if (= menu 0)
  (do
  (displayMenu)
  (print pressed 100 100 couleur-texte)
  (print left 100 110 couleur-texte)
  (print previous_left 100 120 couleur-texte)
  (if ( and (= previous_left false) (= left true))
  (nightSelection x y))
  )
  )
  
  


  
  ;; 4. Fait avancer le temps
  (set t (+ t 0.1)))