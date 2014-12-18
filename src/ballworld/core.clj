(ns ballworld.core
  (:require [seesaw [keymap :as skm] [keystroke :as sks] [core :refer :all]])
  (:require [clojure.core.matrix :as m])
  (:require [clojure.java [browse :as b] [javadoc :as j]])
  (:import  [javax.swing JButton JFrame JOptionPane JPanel])
  (:import  [java.awt.event ActionListener KeyListener KeyEvent])
  (:import  [java.awt.Color]))



(defrecord Point [x y])
(defrecord Ball [pos vel rad color])
(defn add-points [p1 p2] 
  (Point. (+ (:x p1) (:x p2)) (+ (:y p1) (:y p2))))

;; playing with time primitives. These will be useful later
;; in adding control features
(def timeflow-bool (atom false))
(defn time-flowing? [] @timeflow-bool)
(defn resume-timeflow! [] (swap! timeflow-bool (fn [_] true)))
(defn stop-timeflow! [] (swap! timeflow-bool (fn [_] false)))
(defn start-timeflow! [] (resume-timeflow!))
(defn switch-timeflow! [] (swap! timeflow-bool (fn [b] (not b))))

(defn flip-sign [x] (* -1 x))
(def default-radius 30)


(defn paint-ball [ball g] 
  (.fillOval g (- (:x (:pos @ball)) (:rad @ball)) 
               (- (:y (:pos @ball)) (:rad @ball))
               (* 2 (:rad @ball)) 
               (* 2 (:rad @ball))))

(defn rand-color [] 
  (let [rand-val (fn [] (int (* 256 (Math/random))))]
    (Color. (rand-val) (rand-val) (rand-val))))

(defn rand-vel []
  ;; right now numbers are heuristic. No units
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


(def main-panel (proxy [JPanel] []
         (paintComponent [g]
           (proxy-super paintComponent g)
           (doseq [ball @balls]
             (.setColor g (:color @ball))
             (paint-ball ball g)))))


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

(def control-panel
  (doto (JPanel.)
    (.setBackground java.awt.Color/lightGray)
    (.add add-ball-button)
    (.add clear-balls-button)
    (.add curve-balls-button)
    (.add pause-button)))
  

;; trying to stop this odd bug of a ball wiggling against the edge
(def bump 5) 

;; There should be a way of generalizing these *-bounce! commands. Very similar ideas
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

;; Find if part (or all) of the ball is outside of the panel. Return the side
;; it has fallen off, or nil if it is fully inside the panel
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

(defn update! [ball]
  (dosync 
    (move-straight! ball)
    (doseq [sm! @special-motions]
      (sm! ball))
    (bounce! ball)))


(defn -main []

  ;; http://stackoverflow.com/questions/3636364/can-i-clean-the-repl
  ;; trying to purge existing state for a fresh run with a new -main call
  ;; (map #(ns-unmap *ns* %) (keys (ns-interns *ns*))) 

  (def main-frame (JFrame.))
  (doto main-frame
    (.add control-panel java.awt.BorderLayout/NORTH)
    (.add main-panel java.awt.BorderLayout/CENTER)
    (.setSize 400 400)
    (.show))
  (defn refresh [] (javax.swing.SwingUtilities/invokeLater #(.repaint main-panel)))
  (start-timeflow!)

  ;; refresh loop
  (future (loop [] (refresh) (Thread/sleep 30) (recur)))
  ;; action loop
  (future (loop [] (if (time-flowing?)
                     (doseq [ball @balls] (update! ball)))
                     ;;(do
                      ;; (update! ball1)
                       ;;(update! ball2)
                       ;;(update! ball3)))
                   (Thread/sleep 15)
                   (recur)))
)




