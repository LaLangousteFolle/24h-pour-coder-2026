;; title:  Template de base
;; author: Quentin
;; desc:   Template de base pour le 24h pour coder 2026
;; script: fennel

(var couleur-texte 0)  ; 6 = vert. Essaie 11 (bleu clair)
(var couleur-fond 12)  ; 12 = Blanc. Essaie 0 (Noir)

;; Variable pour l'animation
(var t 0)

;; Boucle principale exécutée à 60 FPS
(fn _G.TIC []
  ;; 1. Nettoie l'écran
  (cls couleur-fond)

  ;; 2. Calcule un petit mouvement de vague
  (var decalage-y (* (math.sin t) 5))
  
  ;; 3. Affiche le texte au centre avec l'effet de vague
  (print "WORKFLOW OPERATIONNEL !" 45 (+ 64 decalage-y) couleur-texte)
  
  ;; 4. Fait avancer le temps
  (set t (+ t 0.1)))