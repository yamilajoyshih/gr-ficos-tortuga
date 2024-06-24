(ns tp2.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn tortuga-inicial [angulo]
  {:posicion    {:x 0.0 :y 0.0}
   :orientacion -90.0
   :pluma       {:color "negro" :ancho 1 :abajo true}
   :angulo angulo
   })

(defn adelantar
  "segun la orientacion de la tortuga, se avanza para donde este orientada, devolviendo las coordenadas previas al avance y las nuevas coordenadas"
  [estado-tortuga]
  (let [x (:x (:posicion estado-tortuga))
        y (:y (:posicion estado-tortuga))
        orientacion (:orientacion estado-tortuga)
        rad (Math/toRadians orientacion)
        nuevo-x (+ x (* (Math/cos rad) -10))
        nuevo-y (+ y (* (Math/sin rad) 10))]
    {:x1 x :y1 y :x2 nuevo-x :y2 nuevo-y}))

(defn cambiar-pluma [tortuga estado]
  (assoc-in tortuga [:pluma :abajo] estado))

(defn girar [tortuga angulo]
  (assoc tortuga :orientacion (+ (:orientacion tortuga) angulo)))

(defn escribir-svg
  "se realizan las operaciones necesarias para acomodar el viewbox a la imagen, luego itera sobre el map coordenadas para asi escribir las lineas en el svg"
  [arch-svg coordenadas]
  (with-open [writer (io/writer arch-svg)]
    (let [min-x (apply min (map first coordenadas))
          min-y (apply min (map second coordenadas))
          max-x (apply max (map (fn [c] (nth c 2)) coordenadas))
          max-y (apply max (map (fn [c] (nth c 3)) coordenadas))
          width-vb (- max-x min-x)
          height-vb (- max-y min-y)
          margin-width (* 0.125 width-vb)
          margin-height (* 0.125 height-vb)]
      (.write writer "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n")
      (.write writer (str "<svg viewBox=\"" (- min-x margin-width) " " (- min-y margin-height) " " (+ width-vb (* 2 margin-width)) " " (+ height-vb (* 2 margin-height)) "\" xmlns=\"http://www.w3.org/2000/svg\">\n"))
      (doseq [[x1 y1 x2 y2] coordenadas]
        (.write writer (str "<line x1=\"" x1 "\" y1=\"" y1 "\" x2=\"" x2 "\" y2=\"" y2 "\" stroke-width=\"1\" stroke=\"black\" />\n")))
      (.write writer "</svg>"))))

(defn procesar-instruccion-c
  "se lee el caracter c y, segun su valor, se devuelve valores-map con sus respectivos cambios"
  [c tortuga-actual pila-tortuga coordenadas]
  (let [valores-map {:new-estado tortuga-actual :nueva-pila pila-tortuga :coordenadas coordenadas}]
    (case c
      (\F \G) (let [nueva-posicion (adelantar tortuga-actual)
                    coordenadas-nuevas (if nueva-posicion (conj coordenadas [(:x1 nueva-posicion) (:y1 nueva-posicion) (:x2 nueva-posicion) (:y2 nueva-posicion)]) coordenadas)
                    tortuga-movida (assoc tortuga-actual :posicion {:x (:x2 nueva-posicion) :y (:y2 nueva-posicion)})]
                (assoc valores-map :new-estado tortuga-movida :coordenadas coordenadas-nuevas))

      (\f \g) (let [tortuga-sin-pluma (cambiar-pluma tortuga-actual false)
                    nueva-posicion (adelantar tortuga-sin-pluma)
                    tortuga-con-pluma (cambiar-pluma (assoc tortuga-sin-pluma :posicion {:x (:x2 nueva-posicion) :y (:y2 nueva-posicion)}) true)]
                (assoc valores-map :new-estado tortuga-con-pluma))

      \+ (assoc valores-map :new-estado (girar tortuga-actual (- (:angulo tortuga-actual))))
      \- (assoc valores-map :new-estado (girar tortuga-actual (:angulo tortuga-actual)))
      \| (assoc valores-map :new-estado (girar tortuga-actual 180))
      \[ (assoc valores-map :nueva-pila (cons tortuga-actual pila-tortuga))
      \] (assoc valores-map :new-estado (first (rest pila-tortuga)) :nueva-pila (rest pila-tortuga))
      valores-map)))

(defn movimientos-tortuga
  "se realizan todos los procesamientos de la informacion recibida en la cadena, para poder dibujar la imagen en el svg"
  [estado-tortuga cadena]
  (loop [pila-tortuga (list estado-tortuga)
         cs (seq cadena)
         coordenadas '()]
    (if (empty? cs)
      coordenadas
      (let [tortuga-actual (first pila-tortuga)
            c (first cs)
            valores-map (procesar-instruccion-c c tortuga-actual pila-tortuga coordenadas)]
        (recur (cons (:new-estado valores-map) (rest (:nueva-pila valores-map))) (rest cs) (:coordenadas valores-map))))))

(defn aplicar-reglas
  "Se aplican las reglas de iteración sobre el axioma"
  [axioma regla1 regla2]
  (let [regla1-char (first regla1)
        regla2-char (first regla2)
        reemplazar (fn [c] (cond
                             (= c regla1-char) (apply str(drop 2 regla1))
                             (= c regla2-char) (apply str(drop 2 regla2))
                             :else (str c)))]
    (apply str (map reemplazar axioma))))

(defn crear-cadena
  "se itera sobre el sistema-L, obteniendo la cadena la cual se debe procesar para obtener la imagen"
  [sistema-L iteraciones]
  (let [{axioma :axioma
         regla1 :regla1
         regla2 :regla2} sistema-L]
    (loop [cadena-resultado axioma i 0]
      (if (< i iteraciones)
        (recur (aplicar-reglas cadena-resultado regla1 regla2) (inc i))
        cadena-resultado))))

(defn leer-sistema-L
  "se lee el archivo de la ruta, enviando un mensaje de error en caso de que no sea compatible o falle. Devuelve un mapa con el angulo, el axioma y las reglas del sistema-L"
  [arch_text]
  (let [contenido (slurp arch_text)
        lineas (str/split-lines contenido)
        angulo (Double/parseDouble (first lineas))
        axioma (second lineas)
        reglas (drop 2 lineas)]
    {:angulo angulo
     :axioma axioma
     :regla1 (first reglas)
     :regla2 (second reglas)}))

(defn -main [ruta-sl n ruta-svg]
  (if (or (nil? ruta-sl) (nil? n) (nil? ruta-svg))
    (println "Comandos incorrectos, ingrese: lein run <archivo_txt> <iteraciones> <archivo_csv>")
    (let [sistema-L (leer-sistema-L ruta-sl)
          angulo (:angulo sistema-L)
          cadena (crear-cadena sistema-L (Integer/parseInt n))
          tortuga (tortuga-inicial angulo)]
      (escribir-svg ruta-svg (movimientos-tortuga tortuga cadena)))))