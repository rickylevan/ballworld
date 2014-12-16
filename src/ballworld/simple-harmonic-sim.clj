(ns ballworld.simple-harmonic-sim
  (:reqiure [clojure.core.matrix :as m])
  (:require [incanter [core :as ic] [charts :as ch]]))

;; state transition matrix, where X is total system state
(def dt 0.01)
(def s 10) ;; milliseconds
(def X0 [1 0])
(def STM [[1 dt] [(- dt) 1]])
;;(def trajectory (atom {0 X0}))

(def stop-boolean (atom true))

(def states-times
(loop [t 0 states [X0] times [0]] 
          (if (< t 70)
                   (recur (+ t dt)
                          (conj states (m/inner-product STM (last states)))
                          (conj times t))
            [states times]
            )))

(def states (first states-times))
(def times (second states-times))

;; ** Incanter plotting stuff **
(doto (ch/xy-plot times (m/get-column states 0)
:title "Numerical Decay"
:legend true)
(ch/add-lines times (map #(Math/cos %) times))
ic/view)


