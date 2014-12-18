(ns ballworld.core
  (:require [clojure.core.matrix :as m])
  (:import  [javax.swing JButton JFrame JPanel])
  (:import  java.awt.event.ActionListener)
  (:import  java.awt.Color))

;; http://stackoverflow.com/questions/3636364/can-i-clean-the-repl
;; trying to purge existing state for a fresh run with a new -main call
;; (map #(ns-unmap *ns* %) (keys (ns-interns *ns*))) 

;; structs
(defrecord Point [x y])
(defrecord Ball [pos vel rad color])

;; time primitives
(def timeflow-bool (atom false))
(defn time-flowing? [] @timeflow-bool)
(defn resume-timeflow! [] (swap! timeflow-bool (fn [_] true)))
(defn stop-timeflow! [] (swap! timeflow-bool (fn [_] false)))
(defn start-timeflow! [] (resume-timeflow!))
(defn switch-timeflow! [] (swap! timeflow-bool (fn [b] (not b))))

;; helpers for color shifting
(def time-period 3)
(def time-counter (atom 0))

(defn flip-sign [x] (* -1 x))
(defn add-points [p1 p2] 
  (Point. (+ (:x p1) (:x p2)) (+ (:y p1) (:y p2))))
(def default-radius 30)

(defn rand-color [] 
  (let [rand-val (fn [] (int (* 256 (Math/random))))]
    (Color. (rand-val) (rand-val) (rand-val))))
(defn rand-vel []
  (- 12 (* 24 (Math/random))))

(def ball1 (atom (Ball. (Point. 80 80) (Point. 8.0 3.7) default-radius Color/red)))
(def ball2 (atom (Ball. (Point. 120 70) (Point. 7.0 4.7) default-radius Color/blue)))
(def ball3 (atom (Ball. (Point. 90 60) (Point. 10.0 7.5) default-radius Color/green)))
(def balls (atom [ball1 ball2 ball3]))

;; store special motions for the balls to execute
(def special-motions (atom #{}))

;; toggle whether the balls obey some special motion
(defn swap-in-special [f!]
  (if (contains? @special-motions f!)
    (swap! special-motions disj f!)
    (swap! special-motions conj f!)))

(defn add-ball! []
  (swap! balls conj
         (atom (Ball. (Point. 80 80) (Point. (rand-vel) (rand-vel)) 
                  default-radius (rand-color)))))

(defn clear-balls! []
  (swap! balls empty))

;; Move as instructed by Newton's first
(defn move-straight! [b] (swap! b assoc :pos (add-points (:pos @b) (:vel @b))))

;; Move in a curved path
(defn move-curved! [b]
  (let [theta (/ Math/PI 16)]
    (swap! b assoc :vel
      (Point. 
        (- (* (:x (:vel @b)) (Math/cos theta))
           (* (:y (:vel @b)) (Math/sin theta)))
        (+ (* (:y (:vel @b)) (Math/cos theta))
           (* (:x (:vel @b)) (Math/sin theta)))))))

;; Change color of ball, at some period n * thread sleep time
(defn move-color-shift! [b]
  (if (= 0 @time-counter)
    (swap! b assoc :color (rand-color))))

;; buttons
(def add-ball-button
  (doto (JButton. "Add")
    (.addActionListener
      (proxy [ActionListener] []
        (actionPerformed [e]
          (add-ball!))))))
(def clear-balls-button
  (doto (JButton. "Clear")
    (.addActionListener
      (proxy [ActionListener] []
        (actionPerformed [e]
          (clear-balls!))))))
(def pause-button
  (doto (JButton. "Pause")
    (.addActionListener
      (proxy [ActionListener] []
        (actionPerformed [e]
          (switch-timeflow!))))))
(def curve-balls-button
  (doto (JButton. "Curve")
    (.addActionListener
      (proxy [ActionListener] []
        (actionPerformed [e]
          (swap-in-special move-curved!))))))
(def color-shift-button
  (doto (JButton. "Colorshift")
    (.addActionListener
      (proxy [ActionListener] []
        (actionPerformed [e]
          (swap-in-special move-color-shift!))))))

;; plug to stop odd bug of a ball wiggling against the frame's edge
(def bump 5) 

;; so it's defined for the .getBounds methods below -- of course changes later
(def main-panel)

;; Bounce transitions. There should be a way to generalize these
;; very similar code blocks
(defn no-bounce! [ball])
(defn right-bounce! [ball]
  (do
    (let [old-vel (:vel @ball)]
      (swap! ball assoc :vel (Point. (flip-sign (:x old-vel)) (:y old-vel))))
    (let [dx (Math/abs (+ (:x (:pos @ball)) (:rad @ball) (- (.width (.getBounds main-panel)))))]
      (swap! ball assoc :pos (Point. (- (:x (:pos @ball)) (* 2 dx) bump) (:y (:pos @ball)))))))
(defn bottom-bounce! [ball]
  (do
    (let [old-vel (:vel @ball)]
      (swap! ball assoc :vel (Point. (:x old-vel) (flip-sign (:y old-vel)))))
    (let [dy (Math/abs (+ (:y (:pos @ball)) (:rad @ball) (- (.height (.getBounds main-panel)))))]
      (swap! ball assoc :pos (Point.  (:x (:pos @ball)) (- (:y (:pos @ball)) (* 2 dy) bump))))))
(defn left-bounce! [ball]
  (do
    (let [old-vel (:vel @ball)]
      (swap! ball assoc :vel (Point. (flip-sign (:x old-vel)) (:y old-vel))))
    (let [dx (Math/abs (- (:x (:pos @ball)) (:rad @ball)))]
      (swap! ball assoc :pos (Point. (+ (:x (:pos @ball)) (* 2 dx) bump) (:y (:pos @ball)))))))
(defn top-bounce! [ball]
  (do
    (let [old-vel (:vel @ball)]
      (swap! ball assoc :vel (Point. (:x old-vel) (flip-sign (:y old-vel)))))
    (let [dy (Math/abs (- (:y (:pos @ball)) (:rad @ball)))]
      (swap! ball assoc :pos (Point. (:x (:pos @ball)) (+ (:y (:pos @ball) (* 2 dy)) bump))))))

;; Find if part (or all) of the ball is outside of the panel. Return the function
;; corresponding to the side (possibly none) that it has fallen off
(defn get-bounce-fun [ball]
  (let [shrunk-bounds
        {:x      (:rad @ball)
         :y      (:rad @ball)
         :width  (- (.width (.getBounds main-panel))  (* 2 (:rad @ball)))
         :height (- (.height (.getBounds main-panel)) (* 2 (:rad @ball)))}]
    (cond 
      (> (:x (:pos @ball)) (+ (:x shrunk-bounds) (:width shrunk-bounds)))     right-bounce!
      (< (:x (:pos @ball)) (:x shrunk-bounds))                                left-bounce!
      (< (:y (:pos @ball)) (:y shrunk-bounds))                                top-bounce!
      (> (:y (:pos @ball)) (+ (:y shrunk-bounds) (:height shrunk-bounds)))    bottom-bounce!
      :else                                                                   no-bounce!)))

(defn bounce! [ball]
  ((get-bounce-fun ball) ball))

;; core ball behavior through time
(defn update! [ball]
  (dosync 
    (move-straight! ball)
    (doseq [speci-m! @special-motions]
      (speci-m! ball))
    (bounce! ball)))

;; painting and panels
(defn paint-ball [ball g] 
  (.fillOval g (- (:x (:pos @ball)) (:rad @ball)) 
               (- (:y (:pos @ball)) (:rad @ball))
               (* 2 (:rad @ball)) 
               (* 2 (:rad @ball))))

(def main-panel (proxy [JPanel] []
         (paintComponent [g]
           (proxy-super paintComponent g)
           (doseq [ball @balls]
             (.setColor g (:color @ball))
             (paint-ball ball g)))))

(def control-panel
  (doto (JPanel.)
    (.setBackground Color/lightGray)
    (.add add-ball-button)
    (.add clear-balls-button)
    (.add curve-balls-button)
    (.add color-shift-button)
    (.add pause-button)))

(def main-frame (JFrame. "Ballworld"))
(doto main-frame
  (.add control-panel java.awt.BorderLayout/NORTH)
  (.add main-panel java.awt.BorderLayout/CENTER)
  (.setSize 600 600))

;; main loops
(defn refresh [] (javax.swing.SwingUtilities/invokeLater #(.repaint main-panel)))
(defn start-gui-refresh-loop [] (loop [] (refresh) (Thread/sleep 30) (recur)))
(defn start-action-loop [] 
  (loop [] (if (time-flowing?)
    (do
      (doseq [ball @balls] (update! ball))
      ;; loop time steps modulo time-period (to slow color flashing)
      (swap! time-counter #(mod (inc %) time-period))))
    (Thread/sleep 15)
    (recur)))


(defn -main []
  (.show main-frame)
  (start-timeflow!)
  (future (start-gui-refresh-loop))
  (future (start-action-loop)))



;; defining collision primitives

(defn get-delta-vel [this-ball that-ball]
  (Point. 
    (- (:x (:vel @that-ball)) (:x (:vel @this-ball)))
    (- (:y (:vel @that-ball)) (:y (:vel @this-ball)))))

(defn get-delta-pos [this-ball that-ball]
  (Point.
    (- (:x (:pos @that-ball)) (:x (:pos @this-ball)))
    (- (:y (:pos @that-ball)) (:y (:pos @this-ball)))))

;; real collision time. Negative, relative to detection time t = 0 
(defn get-collision-time [this-ball that-ball]
  (let [dvel (get-delta-vel this-ball that-ball)
        dpos (get-delta-pos this-ball that-ball)
        R      (+ (:rad @this-ball) (:rad @that-ball))]
    ;; magic from the math
    (let [physics-factor
          (- (Math/pow (m/dot dvel dpos) 2)
             (* (m/dot dvel dvel)
                (- (m/dot dpos dpos) (Math/pow R 2))))]
      )))







