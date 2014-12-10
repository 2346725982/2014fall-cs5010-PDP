#lang racket

(require 2htdp/image)

overlay/xy

(overlay/xy
 (rectangle 20 80 'outline' black)
 30 -50
 (overlay/xy
  (above (circle 25 'outline' black) (rectangle 50 80 'outline' black) (overlay/xy (rectangle 20 80 'outline' black) 30 0 (rectangle 20 80 'outline' black)))
  60 50
  (rectangle 20 80 'outline' black)))