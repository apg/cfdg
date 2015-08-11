# cfdg

An implementation of the original CFDG (or at least as defined
[here](http://korsh.com/cfdg)) in Racket.

## Example

Right now, things are pretty bogus, and there's only an S-expression
based language to boot. However, you can put pixels on a canvas --
maybe not as you intended them, but on a canvas nevertheless.

```racket
#lang s-exp cfdg/language

(rule circles
      (square)
      (four-circles))

(rule four-circles
      (circle 'x 10.5 's 0.7)
      (circle 'x -10.5 's 0.7)
      (circle 'y 10.5 's 0.7)
      (circle 'y -10.5 's 0.7)
      (four-circles 'x 10.5 's 0.5)
      (four-circles 'x -10.5 's 0.5)
      (four-circles 'y 10.5 's 0.5)
      (four-circles 'y -10.5 's 0.5))

(start-rule four-circles)
```

Run it with `racket four-circles.rkt`, and you should get `out.png` in
your current directory.

You can also utilize `#lang cfdg`, but that will eventually be
replaced with a non s-expression based language more closely
resembling the original CFDG.

## Copyright

CFDG was originally &copy; [Chris Coyne](https://chriscoyne.com/)

This implementation is &copy; Andrew Gwozdziewycz, 2015, and licensed
under the LGPL. See `LICENSE.txt` for more details.
