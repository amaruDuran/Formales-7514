;; Ejercicios resueltos En este archivo:

;;1 - 5 - 7 - 9 - 10 - 11 - 12 - 13 - 18 - 19 - 27 - 28 - 30 - 35a

;; OBS: en la guía están resaltados estos ejercicios junto con el 21 y 22 que están en otro archivo.

;; Recomiendo usar vscode + plugin calva. Les va a servir luego para el TP.

;;Ejercicio 1
(defn tercer-angulo [alpha beta]
  (cond
    (>= alpha 0) "Error"
    (>= beta 0) "Error"
    :else (- 180 (+ alpha beta))
  )
)

(println(tercer-angulo 0 40))



;; Ejercicio 5
(defn capicua? [x]
  (cond
    (> x 99999) "error, numero mayor a 4 digitos."
    :else (= (str x) (apply str (reverse (str x))))  
    )
  )

(capicua? 343)

;; Ejercicio 7
(defn invertir [x]
  (cond
    (not (number? x)) "No es un número"
    (<= x 0) "Numero no es entero positivo"
    :else (Integer/parseInt (apply str (reverse (str x))))
    )
  )

(invertir 1234)

;; Ejercicio 9

(defn cant-dig [x]
  (cond
    (not (number? x)) "No es Número"
    :else (count (str x))
    )
  )

(cant-dig 1312)

;; Ejercicio 10
(defn pot? [a b]
  (cond
    (or (<= a 0) (<= b 0)) "Los números deben ser positivos"
    (== a b) true
    (< a b) false 
    :else (pot? (float (/ a b)) b)
    )
  )

(pot? 10000 10)

;; Ejercicio 11
(defn parse_int [a]
  (Integer/parseInt a))

(defn digs [x]
  (map (comp parse_int str) (str x))) 

(digs 1234) 

;; Opción II: Sin uso de funciones java.
;; (defn digs [x]
;;   (map {
;;         \0 0
;;         \1 1
;;         \2 2
;;         \3 3
;;         \4 4
;;         \5 5
;;         \6 6
;;         \7 7
;;         \8 8
;;         \9 9} (str x)
;;        )
;;   )

;; (digs 1234)

;; Ejercicio 12
(defn escribir_frase [a]
  (str "Uno para " a ", uno para mi")
  )

(defn repartir 
  ([] "Uno para vos, uno para mí") 
  ([& more] (map (partial escribir_frase) more))
  )

(repartir "Gallanto" "Bianchi")

;; Ejercicio 13

(defn f_lista_con_posiciones [l]
  (map-indexed list l)
  )

(defn es_pos_par? [[p elem]]
  (even? p)
  )

(defn filtrar_posisiones_pares [l]
  (filter es_pos_par? l)
  )

(defn listas_sin_posiciones [l]
  (nth l 1)
  )

(defn unir_listas_pos_pares [l1 l2]
  (map listas_sin_posiciones (concat (filtrar_posisiones_pares (f_lista_con_posiciones l1))
                    (filtrar_posisiones_pares (f_lista_con_posiciones l2))
                    )
       ) 
  ) 

(filtrar_posisiones_pares '((0 1) (1 2) (2 3)))

(unir_listas_pos_pares '(1 2 3) '(4 2 5 6 7))


;; Ejercicio 18
(defn eliminar_repetidos [lis]
  (into '() (reduce conj #{} lis))
  ) 

(eliminar_repetidos '(1 2 3 3 4))


;; Ejercicio 19

(defn ordenar_por_longitud_desc [l]
  (sort-by count > l)
  )

;; Por defecto ordena ascendentemente.
(defn ordenar_por_longitud_asc [l]
  (sort-by count l))



(ordenar_por_longitud_desc '((1 2 3) (3 2 1) (2 3) (4 5 6 7)))

(ordenar_por_longitud_asc '((1 2 3) (3 2 1) (2 3) (4 5 6 7))) 

;; Ejercicio 27

(defn letra_repetida? [[value set] letra]
  (cond
    (= value true) (list true set)
    (= (contains? set letra) true) (list true set)
    :else (list false (conj set letra))
    )
  )

(defn tiene_repetidas? [pal]
  (nth (reduce letra_repetida? '(false #{}) pal) 0)
  ) 

(tiene_repetidas? "abccd")

;; Ejercicio 28
(defn aplicar_a [x fi]
  (fi x)
  )

(defn b [f x]
  (map (partial aplicar_a x) f)
  )

(b [list vec] "abcdario") 

;; Ejercicio 30

(defn _slice [l_letras]
  (apply str l_letras)
  )

(defn slice [s n]
  (map _slice (partition n 1 s))
  ) 

(slice "abcdefghi" 9)

;; Ejercicio 35a

(defn cantidad_de_V [l]
  (cond
    (number? (get (frequencies l) 'V)) (get (frequencies l) 'V)
    :else 0
    )
  )

(defn calc_max_V [m]
  (reduce max 0 (map cantidad_de_V m))
  )

(defn tiene_maxima_V? [n [idx l]]
  (= n (cantidad_de_V l))
  )

(defn nth_mio [tuple]
  (nth tuple 0) 
  )

;; Ojo: El partial cuando le pasas un parámetro fijo, 
;;    aplica la función dentro del partial de izquierda a derecha.
(defn filas_max_V [m] 
  (map inc (map (partial nth_mio) (filter (partial tiene_maxima_V? (calc_max_V m)) (map-indexed list m)))) 
  )

;; Opción II : más legible, pero no dejan hacerlo con macros.
;; TODO
;; (defn filas_max_V [m]
;;   (-> m
;;       (map-indexed list)
;;       (filter (partial tiene_maxima_V? (calc_max_V m)))
;;       )
;;   )
 
(tiene_maxima_V? 3 '(0 '(V V V))) 

(map cantidad_de_V '((V F V V) (V V V) (F F))) 

(filas_max_V '((V F V V) (V V V) (F F)))

