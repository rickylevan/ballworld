# BallWorld

Re-implementation of ballworld in Clojure

This is a college project for a class where we originally wrote this in Java. This fun little physics sim was rewritten by me into Clojure. Did it for the love of Lambda.

To run, make sure you have Java, Clojure, and Leiningen installed. With all this, I come back in 2019 to see this still works on my Mac with a simple `lein run`. Alas, we get things like `WARNING: Illegal reflective access by clojure.lang.Reflector (file:/Users/rick/.m2/repository/org/clojure/clojure/1.5.1/clojure-1.5.1.jar) to method sun.java2d.SunGraphics2D.setColor(java.awt.Color)` in the logs, but y'know:

![Be like it do](https://github.com/rickylevan/ballworld/blob/master/be_like_it_do.png =250x)
