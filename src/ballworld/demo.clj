(let [click (ref nil)
      panel (proxy [javax.swing.JPanel] []
              (paintComponent [g]
                (proxy-super paintComponent g)
                (.drawImage g (.getImage
                            (javax.swing.ImageIcon. "play.png"))
                            0 0 (.getWidth this) (.getHeight this) nil)
                (if @click
                  (.fillRect g (:x @click) (:y @click) 10 10))))]
  (.addMouseListener panel
      (proxy [java.awt.event.MouseAdapter] []
        (mouseClicked [e]
          (let [p (.getPoint e)]
            (dosync (ref-set click {:x (.x p), :y (.y p)})))
          (javax.swing.SwingUtilities/invokeLater #(.repaint panel)))))
  (doto (javax.swing.JFrame.)
    (.setContentPane panel)
    (.setSize 200 200)
    (.show)))


;; separate thing here:
(import 'java.awt.event.MouseAdapter)
(defn add-mousepressed-listener
  [component f & args]
  (let [listener (proxy [MouseAdapter] []
                     (mousePressed [event]
                                   (apply f event args)))]
    (.addMouseListener component listener)
    listener))

