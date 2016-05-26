#lang s-exp cfdg/language

(set background "black")
(set luminance 80)
(set rotation 0)
(set saturation 90)
(set nesting 6)
(set hue 180)
(set size 100)
(set alpha 70)

(rule circles
      (square)
      (four-circles))

(rule four-circles
      (circle 'x 5 's 0.75)
      (circle 'x -5 's 0.75)
      (circle 'y 5 's 0.75)
      (circle 'y -5 's 0.75)
      (four-circles 'x 5 'c 10 's 0.5 'b .9 't .8)
      (four-circles 'x -5 'c -10 's 0.5 'b .1 't .8)
      (four-circles 'y 5 'c 10 's 0.5 'b .9 't .8)
      (four-circles 'y -5 'c -10 's 0.5 'b .1 't .8))

(start-rule four-circles)
