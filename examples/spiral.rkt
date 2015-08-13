#lang s-exp cfdg/language

(set background "DimGray")
(set width 1000)
(set height 1000)
(set size 50)
(set luminance 220)
(set rotation 0)
(set saturation 200)
(set nesting 100)
(set alpha 0)

(rule brick
      (square)
      (if (> .2 (random))
          (brick 'r -13 'a 0.3 'x 1.7 's 0.95 'b .2)
          (brick 'r -20 'a 0.2 'x 1.3 's 0.9 'b .25)))

(rule first
      (brick)
      (brick 'r 72 'c 51)
      (brick 'r 144 'c 102)
      (brick 'r 216 'c 153)
      (brick 'r 288 'c 204))

(start-rule first 'b 220 'r 0 'c 0 't 200 'a 0)
