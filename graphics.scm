(define-module (graphics)
  #:use-module (chickadee graphics font)
  #:use-module (chickadee graphics texture)

  #:export (load-textures
            player-ship-sprite
            fireball-texture
            enemy-fireball-texture

            upgrade-texture
            
            explosion-atlas
            large-explosion-atlas
            background-map
            rocket-texture
            enemy-ships-textures-atlas
            
            game-font))
             

(define (load-textures)
  (set! enemy-ships-textures-atlas (load-tileset "graphics/enemy-ships.png" 16 16))
  (set! player-ship-sprite (load-image "graphics/ship.png"))
  (set! fireball-texture (load-image "graphics/fire.png"))
  (set! enemy-fireball-texture (load-image "graphics/enemy-fire.png"))
  
  (set! rocket-texture (load-image "graphics/rocket.png"))
  
  (set! background-map (load-image "graphics/space_dn.png"))
  (set! explosion-atlas (load-tileset "graphics/explosion.png" 128 128))
  (set! large-explosion-atlas (load-tileset "graphics/explosion-large.png" 256 256))

  (set! upgrade-texture (load-image "graphics/upgrade.png"))
  
  (set! game-font (load-font "font.otf" 32)))
  
(define enemy-ships-textures-atlas #f)
(define player-ship-sprite #f)
(define fireball-texture #f)
(define enemy-fireball-texture #f)
(define upgrade-texture #f)
(define explosion-atlas #f)

(define game-font)

(define large-explosion-atlas #f)

(define background-map #f)

(define rocket-texture #f)
