(ns c64-basic-interpreter.core-test
  (:require [clojure.test :refer :all]
            [c64-basic-interpreter.core :refer :all]))

(deftest palabra-reservada-test
  
  (testing "palabra-reservada? de Símbolo REM es true"
    (is (= true (palabra-reservada? 'REM))))
  (testing "palabra-reservada? de Símbolo SPACE es false"
    (is (= false (palabra-reservada? 'SPACE)))))


(deftest operador?-test
  
  (testing "+ - * / son operadores"
    (is (= true (operador? (symbol "+"))))
    (is (= true (operador? (symbol "-"))))
    (is (= true (operador? (symbol "*"))))
    (is (= true (operador? (symbol "/"))))
    (is (= true (operador? '+)))
    (is (= true (operador? '-)))
    (is (= true (operador? '*)))
    (is (= true (operador? '/))))
  (testing "AND OR son operadores"
    (is (= true (operador? (symbol "AND"))))
    (is (= true (operador? (symbol "OR"))))
    (is (= true (operador? 'AND)))
    (is (= true (operador? 'OR))))
  (testing "<> < <= > >= son operadores"
    (is (= true (operador? (symbol "="))))
    (is (= true (operador? (symbol "<>"))))
    (is (= true (operador? (symbol "<"))))
    (is (= true (operador? (symbol "<="))))
    (is (= true (operador? (symbol ">"))))
    (is (= true (operador? (symbol ">="))))
    (is (= true (operador? '=)))
    (is (= true (operador? '<>)))
    (is (= true (operador? '<)))
    (is (= true (operador? '<=)))
    (is (= true (operador? '>)))
    (is (= true (operador? '>=))))
  (testing "el resto no son operadores"
    (is (= false (operador? (symbol "THEN"))))
    (is (= false (operador? (symbol "ENV"))))
    (is (= false (operador? (symbol "INPUT"))))
    (is (= false (operador? (symbol "END"))))
    (is (= false (operador? (symbol "IF"))))
    (is (= false (operador? (symbol "INT"))))
    (is (= false (operador? (symbol "CHR$"))))))


(deftest variable-float?-test
  
  (testing "variable-float? de simbolo X es true"
    (is (= true (variable-float? 'X))))
  (testing "variable-float? de simbolo X% es false"
    (is (= false (variable-float? 'X%))))
  (testing "variable-float? de simbolo X$ es false"
    (is (= false (variable-float? 'X$)))))
  

(deftest variable-integer?-test
  
  (testing "variable-integer? de Símbolo X% es true"
    (is (= true (variable-integer? 'X%))))
  (testing "variable-integer? de Símbolo X es false"
    (is (= false (variable-integer? 'X))))
  (testing "variable-integer? de Símbolo X$ es false"
    (is (= false (variable-integer? 'X$))))) 


(deftest variable-string?-test
  
  (testing "variable-string? de Símbolo X$ es true"
    (is (= true (variable-string? 'X$))))
  (testing "variable-string? de Símbolo X es false"
    (is (= false (variable-string? 'X))))
  (testing "variable-string? de Símbolo X% es false"
    (is (= false (variable-string? 'X%)))))
 

(deftest dar-error-test
  
  (testing "dar-error devuelve nil"
    (is (= nil (dar-error 16 [:ejecucion-inmediata 4])))))





(deftest continuar-linea-test
  
  (testing "continuar-linea sin valores de retorno en (gosub-return-stack)
            devuelve mensaje de error y detiene la ejecución de programa."
    (is (= (continuar-linea [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}])
           [nil [(list (list 10 (list 'PRINT 'X)) (list 15 (list 'X '= 'X '+ 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}]]))
    (is (= (with-out-str (continuar-linea [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}]))
           "\n?RETURN WITHOUT GOSUB  ERROR IN 20")))
  (testing "continuar-linea con valores de retorno en (gosub-return-stack)
                devuelve una tupla (un vector) con un resultado."
    (is (= (continuar-linea [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [[15 2]] [] [] 0 {}])
           [:omitir-restante [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {}]])))) 


(deftest extraer-data-test
  
  (testing "extraer-data de representación intermedia vacía es lista de listas de vacio."
    (is (= (extraer-data '(())) (list))))
  (testing "extraer-data de representación intermedia 
            con una linea con REM anula sentencias DATA posteriores.
            extraer-data con lineas con sentencias DATA con un valor/es asociado/s
            que pueden estar separados por comas, se devuelven con su tipo correspondiente
            en una lista"
    (is (= (extraer-data (list '(10 (PRINT X) (REM ESTE NO) (DATA 30)) '(20 (DATA HOLA)) (list 100 (list 'DATA 'MUNDO (symbol ",") 10 (symbol ",") 20)))) 
           (list "HOLA" "MUNDO" 10 20))))
  ) 


(deftest ejecutar-asignacion-test
  
  (testing "ejecutar-asignación de valor en variable sin asignar es su nuevo valor"
    (is (= (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 {}])
           [(list (list 10 (list 'PRINT 'X))) [10 1] [] [] [] 0 {'X 5}])))
  (testing "ejecutar-asignación de valor en variable asignada reemplaza su valor"
    (is (= (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])
           [(list (list 10 (list 'PRINT 'X))) [10 1] [] [] [] 0 {'X 5}])))
  (testing "ejecutar-asignación de una variable con expresión, evalua la expresión y asigna el resultado"
    (is (= (ejecutar-asignacion '(X = X + 1) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])
           [(list (list 10 (list 'PRINT 'X))) [10 1] [] [] [] 0 {'X 3}])))
  (testing "ejecutar-asignación de X$ con expresión, evalua la expresión, asigna el resultado en X$"
    (is (= (ejecutar-asignacion '(X$ = X$ + " MUNDO") ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}])
           [(list (list 10 (list 'PRINT 'X))) [10 1] [] [] [] 0 {'X$ "HOLA MUNDO"}])))) 


(deftest preprocesar-expresion-test
  
  (testing "preprocesar-expresion X$ se reemplaza por su valor. Z$ no está seteada previamente, entonces es cadena vacía"
    (is (= (preprocesar-expresion '(X$ + " MUNDO" + Z$) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}])
           (list "HOLA" '+ " MUNDO" '+ ""))))
  (testing "preprocesar-expresion X e Y se reemplaza por su valor. El simbolo . se reemplaza por 0. Z sin asignar se reemplaza por 0."
    (is (= (preprocesar-expresion '(X + . / Y% * Z) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 5 Y% 2}])
           (list 5 '+ 0 '/ 2 '* 0)))))
   

(deftest desambiguar-test
  
  (testing "desambiguar - unario es -u"
    (is (= (desambiguar (list '- 2 '* (symbol "(")'- 3 '+ 5 '- (symbol "(")'+ 2 '/ 7 (symbol ")")(symbol ")"))) 
           (list '-u 2 '* (symbol "(") '-u 3 '+ 5 '- (symbol "(") 2 '/ 7 (symbol ")") (symbol ")")))))
  (testing "desambiguar MID$ binario es MID$"
    (is (= (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ")"))) 
           (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ")")))))
  (testing "desambiguar MID$ ternario es MID3$"
    (is (= (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ",") 3 (symbol ")"))) 
           (list 'MID3$ (symbol "(") 1 (symbol ",") 2 (symbol ",") 3 (symbol ")")))))
  (testing "desambiguar MID$ y - unario"
    (is (= (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") '- 2 '+ 'K (symbol ",") 3 (symbol ")"))) 
           (list 'MID3$ (symbol "(") 1 (symbol ",") '-u 2 '+ 'K (symbol ",") 3 (symbol ")"))))))
  

(deftest precedencia-test
  
  (testing "precedencia de OR es 1"
    (is (= (precedencia 'OR) 1)))
  (testing "precedencia de AND es 2"
    (is (= (precedencia 'AND) 2)))
  (testing "precedencia de * es 6"
    (is (= (precedencia '*) 6)))
  (testing "precedencia de -u es 7"
    (is (= (precedencia '-u) 7)))
  (testing "precedencia de MID$ es 8"
    (is (= (precedencia 'MID$) 8))))


(deftest aridad-test
  
  (testing "aridad de THEN es 0"
    (is (= (aridad 'THEN) 0)))
  (testing "aridad de SIN es 1"
    (is (= (aridad 'SIN) 1)))
  (testing "aridad de * es 2"
    (is (= (aridad '*) 2)))
  (testing "aridad de MID$ es 2"
        (is (= (aridad 'MID$) 2)))
  (testing "aridad de MID3$ es 3"
    (is (= (aridad 'MID3$) 3))))
  

(deftest eliminar-cero-decimal-test
  (testing "eliminar-cero-decimal de 1.5 es 1.5"
    (is (= 1.5 (eliminar-cero-decimal 1.5))))
  (testing "eliminar-cero-decimal de 1.50 es 1.5"
    (is (= 1.5 (eliminar-cero-decimal 1.50))))
  (testing "eliminar-cero-decimal de 1.0 es 1"
    (is (= 1 (eliminar-cero-decimal 1.0))))
  (testing "eliminar-cero-decimal del símbolo A es A"
    (is (= 'A (eliminar-cero-decimal (symbol "A"))))))

(deftest eliminar-cero-entero-test
  (testing "eliminar-cero-entero de nil es nil"
    (is (= nil (eliminar-cero-entero nil))))
  (testing "eliminar-cero-entero de simbolo A es \"A\" "
    (is (= "A" (eliminar-cero-entero 'A))))
  (testing "eliminar-cero-entero de entero 0 es \" 0\" "
    (is (= " 0" (eliminar-cero-entero 0))))
  (testing "eliminar-cero-entero de entero 1 es \" 1\" "
    (is (= " 1" (eliminar-cero-entero 1))))
  (testing "eliminar-cero-entero de entero -1 es \"-1\""
    (is (= "-1" (eliminar-cero-entero -1))))
  (testing "eliminar-cero-entero de float 1.5 es \" 1.5\" "
    (is (= " 1.5" (eliminar-cero-entero 1.5))))
  (testing "eliminar-cero-entero de float -1.5 es \"-1.5\" "
    (is (= "-1.5" (eliminar-cero-entero -1.5))))
  (testing "eliminar-cero-entero de float 0.5 es \" .5\" "
    (is (= " .5" (eliminar-cero-entero 0.5))))
  (testing "eliminar-cero-entero de float -0.5 es \"-.5\" "
    (is (= "-.5" (eliminar-cero-entero -0.5)))))
