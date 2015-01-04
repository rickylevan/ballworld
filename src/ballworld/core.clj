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
(defn rand-radius []
  (+ 15 (* 25 (Math/random))))

(def ball1 (atom (Ball. (Point. 80 80) (Point. 8.0 3.7) default-radius Color/red)))
(def ball2 (atom (Ball. (Point. 120 70) (Point. 7.0 4.7) default-radius Color/blue)))
(def ball3 (atom (Ball. (Point. 90 60) (Point. 10.0 7.5) default-radius Color/green)))
(def balls (atom #{ball1 ball2 ball3}))

;; store special behaviors for the balls to execute
(def special-behaviors (atom #{}))

;; toggle whether the balls obey some special behavior 
(defn swap-in-special [f!]
  (if (contains? @special-behaviors f!)
    (swap! special-behaviors disj f!)
    (swap! special-behaviors conj f!)))

(defn add-ball! []
  (swap! balls conj
         (atom (Ball. (Point. 80 80) (Point. (rand-vel) (rand-vel)) 
                  (rand-radius) (rand-color)))))

(defn clear-balls! []
  (swap! balls empty))

;; defining collision primitives -- these helper functions are all pure.
;; 'this-ball' and 'that-ball' here are the underlying immutable values.
(defn get-delta-vel [this-ball that-ball]
    [(- (:x (:vel that-ball)) (:x (:vel this-ball)))
     (- (:y (:vel that-ball)) (:y (:vel this-ball)))])

(defn get-delta-pos [this-ball that-ball]
    [(- (:x (:pos that-ball)) (:x (:pos this-ball)))
     (- (:y (:pos that-ball)) (:y (:pos this-ball)))])

;; real collision time. Negative, relative to detection time t = 0 
(defn get-collision-time [this-ball that-ball]
  (let [dvel (get-delta-vel this-ball that-ball)
        dpos (get-delta-pos this-ball that-ball)
        R      (+ (:rad this-ball) (:rad that-ball))]
    ;; magic from the math
    (let [physics-factor
          (- (Math/pow (m/dot dvel dpos) 2)
             (* (m/dot dvel dvel)
                (- (m/dot dpos dpos) (Math/pow R 2))))]
      (/ (+ (flip-sign (m/dot dvel dpos))
            (flip-sign (Math/sqrt physics-factor)))
         (m/dot dvel dvel)))))

(defn get-mass [ball] (Math/pow (:rad ball) 2))

(defn get-distance [this-ball that-ball]
  (let [this-pos (:pos this-ball) that-pos (:pos that-ball)]
    (m/distance [(:x this-pos) (:y this-pos)]
                [(:x that-pos) (:y that-pos)])))

(defn overlapping? [this-ball that-ball]
  (< (get-distance this-ball that-ball)
     (+ (:rad this-ball) (:rad that-ball))))  

(defn get-reduced-mass [this-ball that-ball]
  (let [this-mass (Math/pow (:rad this-ball) 2)
        that-mass (Math/pow (:rad that-ball) 2)]
    (/ (* this-mass that-mass) (+ this-mass that-mass))))

(defn get-impulse [this-ball that-ball]
  (let [nx (/ (- (:x (:pos that-ball)) (:x (:pos this-ball)))
              (get-distance this-ball that-ball))
        ny (/ (- (:y (:pos that-ball)) (:y (:pos this-ball)))
              (get-distance this-ball that-ball))]
    (let [dvn (+ (* nx (- (:x (:vel that-ball)) (:x (:vel this-ball))))
                 (* ny (- (:y (:vel that-ball)) (:y (:vel this-ball)))))
          rm (get-reduced-mass this-ball that-ball)]
      (Point. (* 2 rm dvn nx) (* 2 rm dvn ny)))))
  
(def nudge 1.1)

;; collision logic --- now the input balls are the mutable atoms 
(defn collide! 
  ([ball]
   (let [this-ball ball]
     (doseq [that-ball (disj @balls this-ball)]
       (if (overlapping? @this-ball @that-ball)
         (collide! this-ball that-ball)))))
  ([this-ball that-ball]
   ;; compute new velocities from impulse physics
   (let [this-new-vel 
         (Point. (+ (:x (:vel @this-ball))
                    (/ (:x (get-impulse @this-ball @that-ball)) (get-mass @this-ball)))
                 (+ (:y (:vel @this-ball))
                    (/ (:y (get-impulse @this-ball @that-ball)) (get-mass @this-ball))))
         that-new-vel
         (Point. (+ (:x (:vel @that-ball))
                    (/ (:x (get-impulse @that-ball @this-ball)) (get-mass @that-ball)))
                 (+ (:y (:vel @that-ball))
                    (/ (:y (get-impulse @that-ball @this-ball)) (get-mass @that-ball))))
         tstar (get-collision-time @this-ball @that-ball)
         ;; rewind time at old velocity to negative time tstar, and redo time at new velocity
         this-new-pos
         (Point. (+ (:x (:pos @this-ball)) 
                    (* tstar (:x (:vel @this-ball)))
                    (flip-sign (* nudge tstar (:x this-new-vel))))
                 (+ (:y (:pos @this-ball)) 
                    (* tstar (:y (:vel @this-ball)))
                    (flip-sign (* nudge tstar (:y this-new-vel)))))
         that-new-pos
         (Point. (+ (:x (:pos @that-ball)) 
                    (* tstar (:x (:vel @that-ball)))
                    (flip-sign (* nudge tstar (:x that-new-vel))))
                 (+ (:y (:pos @that-ball)) 
                    (* tstar (:y (:vel @that-ball)))
                    (flip-sign (* nudge tstar (:y that-new-vel)))))]
     (doseq []
       (swap! this-ball assoc :vel this-new-vel)
       (swap! that-ball assoc :vel that-new-vel)
       (swap! this-ball assoc :pos this-new-pos)
       (swap! that-ball assoc :pos that-new-pos)))))


;; Move as instructed by Newton's first
(defn move-straight! [b] (swap! b assoc :pos (add-points (:pos @b) (:vel @b))))

;; Move in a curved path
(defn curve! [b]
  (let [theta (/ Math/PI 16)]
    (swap! b assoc :vel
      (Point. 
        (- (* (:x (:vel @b)) (Math/cos theta)) (* (:y (:vel @b)) (Math/sin theta)))
        (+ (* (:y (:vel @b)) (Math/cos theta)) (* (:x (:vel @b)) (Math/sin theta)))))))

;; Change color of ball, at some period n * thread sleep time
(defn color-shift! [b]
  (if (= 0 @time-counter)
    (swap! b assoc :color (rand-color))))

;; buttons
(def add-ball-button
  (doto (JButton. "Add")
    (.addActionListener (proxy [ActionListener] []
      (actionPerformed [e] (add-ball!))))))
(def clear-balls-button
  (doto (JButton. "Clear")
    (.addActionListener (proxy [ActionListener] []
      (actionPerformed [e] (clear-balls!))))))
(def pause-button
  (doto (JButton. "Pause")
    (.addActionListener (proxy [ActionListener] []
      (actionPerformed [e] (switch-timeflow!))))))
(def curve-balls-button
  (doto (JButton. "Curve")
    (.addActionListener (proxy [ActionListener] []
      (actionPerformed [e] (swap-in-special curve!))))))
(def color-shift-button
  (doto (JButton. "Colorshift")
    (.addActionListener (proxy [ActionListener] []
      (actionPerformed [e] (swap-in-special color-shift!))))))
(def collide-balls-button
  (doto (JButton. "Collide")
    (.addActionListener (proxy [ActionListener] []
      (actionPerformed [e] (swap-in-special collide!))))))


;; plug to stop odd bug of a ball wiggling against the frame's edge
(def bump 5) 

(declare main-panel)

;; Bounce transitions. There should be a way to generalize these very similar code blocks
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
;; Impure in its dependence on the bounds
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
    (doseq [special-behavior! @special-behaviors]
      (special-behavior! ball))
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
    (.add collide-balls-button)
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
  (loop [] 
    (if (time-flowing?)
      (do
        (doseq [ball @balls] (update! ball))
        ;; count time steps modulo time-period (used to slow color flashing)
        (swap! time-counter #(mod (inc %) time-period))))
    (Thread/sleep 15)
    (recur)))


(defn -main []
  (.show main-frame)
  (start-timeflow!)
  (future (start-gui-refresh-loop))
  (future (start-action-loop)))


