(ns ballworld.core
  (:require [seesaw [keymap :as skm] [keystroke :as sks] [core :refer :all]])
  (:require [clojure.core.matrix :as m])
  (:require [incanter [core :as ic] [charts :as ch]])
  (:require [clojure.java [browse :as b] [javadoc :as j]])
  (:import  [javax.swing JButton JFrame JOptionPane JPanel])
  (:import  [java.awt.event ActionListener KeyListener KeyEvent]))

(defrecord Point [x y])
(defrecord Ball [pos vel rad])
(defn add-points [p1 p2] 
  (Point. (+ (:x p1) (:x p2)) (+ (:y p1) (:y p2))))

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



(def ball1 (atom (Ball. (Point. 80 80) (Point. 6.0 2.7) default-radius)))
(def ball2 (atom (Ball. (Point. 120 70) (Point. 5.0 3.7) default-radius)))
(def ball3 (atom (Ball. (Point. 90 60) (Point. 7.3 4.5) default-radius)))
(def balls [ball1 ball2 ball3])

(def main-panel (proxy [JPanel] []
         (paintComponent [g]
           (proxy-super paintComponent g)
           (.setColor g java.awt.Color/red)
           (paint-ball ball1 g)
           (.setColor g java.awt.Color/blue)
           (paint-ball ball2 g)
           (.setColor g java.awt.Color/green)
           (paint-ball ball3 g))))
  


;; Do Newton's first
(defn move-straight [b] (swap! b assoc :pos (add-points (:pos @b) (:vel @b))))

;; Find if part (or all) of the ball is outside of the panel. Return the side
;; it has fallen off, or nil if it is fully inside the panel
(defn get-bounce-state [ball]
  (let [shrunk-bounds
        {:x      (:rad @ball)
         :y      (:rad @ball)
         :width  (- (.width (.getBounds main-panel))  (* 2 (:rad @ball)))
         :height (- (.height (.getBounds main-panel)) (* 2 (:rad @ball)))}]
    (cond 
      (> (:x (:pos @ball)) (+ (:x shrunk-bounds) (:width shrunk-bounds)))     :right-bounce
      (< (:x (:pos @ball)) (:x shrunk-bounds))                                :left-bounce
      (< (:y (:pos @ball)) (:y shrunk-bounds))                                :top-bounce
      (> (:y (:pos @ball)) (+ (:y shrunk-bounds) (:height shrunk-bounds)))    :bottom-bounce
      :else                                                                   :no-bounce)))

     
(defn no-bounce! [ball])
(defn right-bounce! [ball]
  (do
    (let [old-vel (:vel @ball)]
      (swap! ball assoc :vel (Point. (flip-sign (:x old-vel)) (:y old-vel))))
    (let [dx (Math/abs (+ (:x (:pos @ball)) (:rad @ball) (- (.width (.getBounds main-panel)))))]
      (swap! ball assoc :pos (Point. (- (:x (:pos @ball)) (* 2 dx)) (:y (:pos @ball)))))))
    ;;(prn "right bounce function called!!!!!")))

(defn bottom-bounce! [ball]
  (do
    (let [old-vel (:vel @ball)]
      (swap! ball assoc :vel (Point. (:x old-vel) (flip-sign (:y old-vel)))))
    (let [dy (Math/abs (+ (:y (:pos @ball)) (:rad @ball) (- (.height (.getBounds main-panel)))))]
      (swap! ball assoc :pos (Point.  (:x (:pos @ball)) (- (:y (:pos @ball)) (* 2 dy)))))))
    ;;(prn "bottom bounce function called!!!!!")))



(defn left-bounce! [ball]
  (do
    (let [old-vel (:vel @ball)]
      (swap! ball assoc :vel (Point. (flip-sign (:x old-vel)) (:y old-vel))))
    (let [dx (Math/abs (- (:x (:pos @ball)) (:rad @ball)))]
      (swap! ball assoc :pos (Point. (+ (:x (:pos @ball)) (* 2 dx)) (:y (:pos @ball)))))))
    ;;(prn "left bounce function called!!!!!")))

(defn top-bounce! [ball]
  (do
    (let [old-vel (:vel @ball)]
      (swap! ball assoc :vel (Point. (:x old-vel) (flip-sign (:y old-vel)))))
    (let [dy (Math/abs (- (:y (:pos @ball)) (:rad @ball)))]
      (swap! ball assoc :pos (Point. (:x (:pos @ball)) (+ (:y (:pos @ball) (* 2 dy))))))))
    ;;(prn "top bounce function called!!!!!")))


(def bounce-fun-map
  {:right-bounce right-bounce!, :left-bounce left-bounce!, :top-bounce top-bounce!,
   :bottom-bounce bottom-bounce!, :no-bounce no-bounce!})

(defn update! [ball]
  (dosync 
    (move-straight ball)
    ((bounce-fun-map (get-bounce-state ball)) ball)))


(defn -main []

  ;; re-defining to try to get fresh balls with each -main run
  ;;(def ball1 (atom (Ball. (Point. 80 80) (Point. 6.0 2.7) default-radius)))
  ;;(def ball2 (atom (Ball. (Point. 120 70) (Point. 5.0 3.7) default-radius))) 
  ;;(def ball3 (atom (Ball. (Point. 90 60) (Point. 7.3 4.5) default-radius))) 
  ;;(def balls [ball1 ball2 ball3])


  (def main-frame (JFrame.))
  (doto main-frame
    (.setContentPane main-panel)
    (.setSize 400 400)
    (.show))

  (defn refresh [] (javax.swing.SwingUtilities/invokeLater #(.repaint main-panel)))
  (start-timeflow!)

  ;; refresh loop
  (future (loop [] (refresh) (Thread/sleep 20) (recur)))
  ;; action loop
  (future (loop [] (if (time-flowing?)
                     ;;(for [ball balls] (do (update! ball))))
                     (do
                       (update! ball1)
                       (update! ball2)
                       (update! ball3)))
                   (Thread/sleep 15)
                   (recur)))
)




