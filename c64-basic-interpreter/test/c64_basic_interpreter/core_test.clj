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
    (is (= false (variable-float? 'X$))))
  )

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
    (is (= false (variable-string? 'X%))))
  ) 

(deftest dar-error-test
  (testing "dar-error devuelve nil"
    (is (= nil (dar-error 16 [:ejecucion-inmediata 4])))))

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
