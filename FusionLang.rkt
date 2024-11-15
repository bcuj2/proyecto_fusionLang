#lang eopl
#|----------------EQUIPO DE TRABAJO - PROYECTO FINAL----------------------------- 

» Ervin Caravali Ibarra
ervin.caravali@correounivalle.edu.co
Código: 1925648

»  Brayan Camilo Urrea Jurado
urrea.brayan@correounivalle.edu.co
Código: 2410023

Enlace al repositorio:
---------------------------------------------------------

--------------------INTERPRETADOR FUSIONLANG--------------------------

Definición de la gramática BNF para las expresiones del lenguaje:



|#

;******************************************************************************************
;Especificación léxica

;******************************************************************************************

(define scanner-spec-simple-interpreter
  '((white-sp ;Espacios en blanco
     (whitespace) skip) 
    (comentario ;Comentarios
     ("#" (arbno (not #\newline))) skip);
    (texto
     ((or letter "_") (arbno (or letter digit "_" ":"))) string); ;Regla para cadenas de texto
    (identifier ;Identificador
     ("@" letter (arbno (or letter digit))) symbol)
    (number ;Número entero positivo
     (digit (arbno digit)) number)
    (number ;Número entero negativo
     ("-" digit (arbno digit)) number)
    (number ;Número flotante positivo
     (digit (arbno digit) "." digit (arbno digit)) number) 
    (number ;Número flotante negativo
     ("-" digit (arbno digit) "." digit (arbno digit)) number)))

;Especificación Sintáctica (Gramática)
(define grammar-simple-interpreter
  '(
     (program (globals "PROGRAM" "{" (arbno proc-decl ";") "}") program-exp)
     ;Expresión de un número
     (expression (number) number-lit)

     ;Expresión de un texto
     (expression ("\"" texto "\"") texto-lit)
  
     ;BLOQUE GLOBALS
     (globals ("GLOBALS" "{" (arbno var-decl ";") "}") globals-exp)

     ;Declaración de variables
     (var-decl (type-exp identifier "=" expression) var-exp)                 ;Mutable
     (var-decl ("const" type-exp identifier "=" expression) const-exp)       ;Inmutable
     
     ;Declaración de listas para el bloque GLOBALS 
     (expr-lista ("list" "<" type-exp ">" identifier "=" "(" (arbno expression ",") ")") simple-exp-lista) 
     (var-decl (expr-lista) lista-exp)

     ;Operaciones sobre listas
     (unary-primitive-list ("empty?") is-null-primitive-list)
     (unary-primitive-list ("empty") null-primitive-list)
     (expression ("make-list" "<"type-exp">" "(" (separated-list expression ",") ")") make-list-expr) 
     (unary-primitive-list ("list?") is-list-primitive)
     (unary-primitive-list ("head") car-primitive-list)
     (unary-primitive-list ("tail") cdr-primitive-list)
     (list-primitive ("append") append-primitive)

     ;Declaración de vectores para el bloque GLOBALS
     (expr-vector ("vector" "<" type-exp ">" identifier "=" "[" (arbno expression ",") "]") simple-expr-vector) 
     (var-decl (expr-vector) vector-exp)

     ;Operaciones sobre vectores
     (unary-primitive-vector ("vector?") is-vector-primitive)
     (expression ("make-vector" "(" expression "," expression ")") make-vector-expr) 
     (vector-primitive ("ref-vector") ref-vector-primitive) 
     (vector-primitive ("set-vector") set-vector-primitive)
     (vector-primitive ("append-vector") append-vector-exp) 
     (vector-primitive ("delete-val-vector") delete-val-vector-exp) 

     ;Declaración de diccionarios para el bloque GLOBALS
     (expr-dict ("dict" "<" type-exp "," type-exp ">" identifier "=" "{" (arbno expression ":" expression ",") "}") simple-expr-dict)
     (var-decl (expr-dict) dict-exp)

     ;Operaciones sobre diccionarios
     (unary-primitive-dict ("dict?") is-dict-primitive) 
     (expression ("make-dict" "{" (separated-list expression ":" expression ",") "}") make-dict-expr) 
     (dict-primitive ("ref-dict") ref-dict-primitive-dic) 
     (dict-primitive ("set-dict") set-dict-primitive-dic)  
     (dict-primitive ("append-dict") append-dict-exp)  
     (dict-primitive ("keys-dict") keys-dict-exp) 
     (dict-primitive ("values-dict") values-dict-exp) 

     ;Expresiones aritméticas
     (primitive ("+") prim-suma)
     (primitive ("-") prim-resta)
     (primitive ("*") prim-multi)
     (primitive ("/") prim-div)
     (primitive ("concat") prim-concat)
     (primitive ("length") primitiva-longitud)

     ;Operaciones lógicas
     (primitive ("<") menor-exp)
     (primitive (">") mayor-exp)
     (primitive ("<=") menorIgual-exp)
     (primitive (">=") mayorIgual-exp)
     (primitive ("==") igual-exp)
     (primitive ("!=") diferente-exp)

     ;Operación de asignación
     (primitive ("->") asignar-valor)
     (expresion (identifier "->" expression) asignar-exp)

     ;Condicional - if
     (expression ("if" expression "then" expression "else" expression)if-exp)
     
     ;Ciclos - while/for
     (expression ("while" "(" expression ")" "{" (arbno expression ";") "}") while-exp)
     (expression ("for" "(" identifier "->" expression ";" expression ";" identifier "->" expression ")" "{" (arbno expression ";") "}") for-exp)

     ;Definición de funciones (procedimientos)
     (proc-decl ("proc" identifier "=" "function" "(" (separated-list param-decl ",") ")" "{" (arbno expression ";") "}") proc-exp)

     ;Declaración de parámetros para un procedimiento
     (param-decl (type-exp identifier) param-exp)

     ;Tipos de datos
     (type-exp ("int") int-type-exp)
     (type-exp ("float") float-type-exp)
     (type-exp ("bool") bool-type-exp)
     (type-exp ("string") string-type-exp)
     (type-exp ("proc") proc-type-exp)

     ;Secuenciación - BLOCK
     (expression ("BLOCK" "{" (arbno expression ";") "}") block-exp)

     ;Estructura de control switch
     (expression ("switch" "(" expression ")" "{" (arbno "case" expression ":" (arbno expression ";") ";") "default" ":" (arbno expression ";") "}") switch-exp)

     ;Expresiones de operación sobre funciones
     (expression (identifier "(" (separated-list expression ",") ")") func-call-exp)
     ))

;Construyendo datos automáticamente
(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

;Definición de la función show-the-datatypes
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

