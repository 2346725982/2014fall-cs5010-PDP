#lang racket
(require 2htdp/image)

(define my-image (bitmap "/Users/wangzoltan/GitHub/folder/test_picture.jpg"))
(define my-image2 (bitmap "/Users/wangzoltan/GitHub/folder/test_picture2.jpeg"))

(above my-image my-image2)
(beside my-image my-image2)