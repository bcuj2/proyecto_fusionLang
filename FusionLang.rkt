#lang eopl 

#|----------------EQUIPO DE TRABAJO - PROYECTO FINAL----------------------------- 

» Ervin Caravali Ibarra
ervin.caravali@correounivalle.edu.co
Código: 1925648

» Brayan Camilo Urrea Jurado
urrea.brayan@correounivalle.edu.co
Código: 2410023

Enlace al repositorio: https://github.com/bcuj2/proyecto_fusionLang.git
---------------------------------------------------------

--------------------INTERPRETADOR FUSIONLANG--------------------------

; Definición de la gramática BNF para las expresiones del lenguaje:

|#

;******************************************************************************************
; Especificación léxica

(define scanner-spec-simple-interpreter
  '((white-sp ; Espacios en blanco
     (whitespace) skip) 
    (comentario ; Comentarios
     ("#" (arbno (not #\newline))) skip);
    (texto
     ((or letter "_") (arbno (or letter digit "_" ":"))) string); ; Reconoce y maneja cadenas de texto
    (identifier ; Identificador
     ("@" letter (arbno (or letter digit))) symbol)
    (number ; Número entero positivo
     (digit (arbno digit)) number)
    (number ; Número entero negativo
     ("-" digit (arbno digit)) number)
    (number
     (digit (arbno digit) "." digit (arbno digit)) number) ; Número flotante positivo
    (number ; Número flotante negativo
     ("-" digit (arbno digit) "." digit (arbno digit)) number)))


; Especificación Sintáctica (Gramática)

(define grammar-simple-interpreter
  '((program (expression) a-program)

     ; Expresión de un número
     (expression (number) number-lit)

     ; Expresión de un texto
     (expression ("\"" texto "\"") texto-lit)
  
     ; BLOQUE GLOBALS
     (globals ("GLOBALS" "{" (arbno var-decl ";") "}") globals-exp)

     ; BLOQUE PROGRAM
     (globals ("PROGRAM" "{" (arbno proc-decl ";") "}") program-exp)

     ; Declaración de parámetros para un procedimiento
     (param-decl (type-exp identifier) param-exp)

     ; Definición de funciones (procedimientos)
     (proc-decl ("proc" identifier "=" "function" "(" (separated-list param-decl ",") ")" "{" (arbno expression ";") "}") proc-exp)

     ; Expresiones de operación sobre funciones
     (expression (identifier "(" (separated-list expression ",") ")") func-call-exp)

     ; Declaración de variables para el bloque GLOBALS
     (var-decl (type-exp identifier "=" expression) var-exp)                 ; Mutable
     (var-decl ("const" type-exp identifier "=" expression) const-exp)       ; Inmutable
     
     ; Declaración de listas para el bloque GLOBALS 
     (expr-lista ("list" "<" type-exp ">" identifier "=" "(" (arbno expression ",") ")") simple-exp-lista) 
     (var-decl (expr-lista) lista-exp)

     ; Operaciones sobre listas
     (unary-primitive-list ("empty?") is-null-primitive-list)
     (unary-primitive-list ("empty") null-primitive-list)
     (expression ("make-list" "<" type-exp ">" "(" (separated-list expression ",") ")") make-list-expr) 
     (unary-primitive-list ("list?") is-list-primitive)
     (unary-primitive-list ("head") car-primitive-list)
     (unary-primitive-list ("tail") cdr-primitive-list)
     (list-primitive ("append") append-primitive)

     ; Expresiones de operación sobre listas
     (expression (unary-primitive-list "(" expression ")") unary-primitive-list-exp) 
     (expression (list-primitive "(" identifier "," (separated-list expression ",") ")") list-primitive-exp)

     ; Declaración de vectores para el bloque GLOBALS
     (expr-vector ("vector" "<" type-exp ">" identifier "=" "[" (arbno expression ",") "]") simple-expr-vector) 
     (var-decl (expr-vector) vector-exp)

     ; Operaciones sobre vectores
     (unary-primitive-vector ("vector?") is-vector-primitive)
     (expression ("make-vector" "(" expression "," expression ")") make-vector-expr) 
     (vector-primitive ("ref-vector") ref-vector-primitive) 
     (vector-primitive ("set-vector") set-vector-primitive)
     (vector-primitive ("append-vector") append-vector-exp) 
     (vector-primitive ("delete-val-vector") delete-val-vector-exp) 

     ; Expresiones de operación sobre vectores
     (expression (unary-primitive-vector "(" expression ")") unary-primitive-vector-exp)
     (expression (vector-primitive "(" identifier "," (separated-list expression ",") ")") vector-primitive-exp)

     ; Declaración de diccionarios para el bloque GLOBALS
     (expr-dict ("dict" "<" type-exp "," type-exp ">" identifier "=" "{" (arbno expression ":" expression ",") "}") simple-expr-dict)
     (var-decl (expr-dict) dict-exp)

     ; Operaciones sobre diccionarios
     (unary-primitive-dict ("dict?") is-dict-primitive) 
     (expression ("make-dict" "{" (separated-list expression ":" expression ",") "}") make-dict-expr) 
     (dict-primitive ("ref-dict") ref-dict-primitive-dic) 
     (dict-primitive ("set-dict") set-dict-primitive-dic)  
     (dict-primitive ("append-dict") append-dict-exp)  
     (dict-primitive ("keys-dict") keys-dict-exp) 
     (dict-primitive ("values-dict") values-dict-exp) 

     ; Expresión de operación sobre diccionarios
     (expression (unary-primitive-dict "(" expression ")") unary-primitive-dict-exp)  
     (expression (dict-primitive "(" identifier "," (separated-list expression ":" expression ",") ")") dict-primitive-exp) 

     ; Operaciones aritméticas
     (primitive ("+") prim-suma)
     (primitive ("-") prim-resta)
     (primitive ("*") prim-multi)
     (primitive ("/") prim-div)
     (primitive ("concat") prim-concat)
     (primitive ("length") primitiva-longitud)

     ; Operaciones lógicas
     (primitive ("<") menor-exp)
     (primitive (">") mayor-exp)
     (primitive ("<=") menorIgual-exp)
     (primitive (">=") mayorIgual-exp)
     (primitive ("==") igual-exp)
     (primitive ("!=") diferente-exp)

     ; Operación de asignación
     (primitive ("->") asignar-valor)
     (expresion (identifier "->" expression) asignar-exp)

     ; Condicional - if
     (expression ("if" expression "then" expression "else" expression) if-exp)
     
     ; Ciclos - for/while
     (expression ("while" "(" expression ")" "{" (arbno expression ";") "}") while-exp)
     (expression ("for" "(" identifier "->" expression ";" expression ";" identifier "->" expression ")" "{" (arbno expression ";") "}") for-exp)
     
     ; Estructura de control switch
     (expression ("switch" "(" expression ")" "{" (arbno "case" expression ":" (arbno expression ";") ";") "default" ":" (arbno expression ";") "}") switch-exp)

     ; Secuenciación - BLOCK
     (expression ("BLOCK" "{" (arbno expression ";") "}") block-exp)

     ; Definición para la secuencia LOCALS
     (expression ("LOCALS" "{" (arbno var-decl ";") "}" "{" (arbno expression ";") "}") locals-exp)
     
     ; Tipos de datos
     (type-exp ("int") int-type-exp)
     (type-exp ("float") float-type-exp)
     (type-exp ("bool") bool-type-exp)
     (type-exp ("string") string-type-exp)
     (type-exp ("proc") proc-type-exp)
)) 

; Construyendo datos automáticamente
(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

; Definición de la función show-the-datatypes
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

; El Analizador Léxico (Scanner)
(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))


(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))


;*************************************INTERPRETE*****************************************************

;Función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)
(define eval-program
  (lambda (pgm)
    (cases program pgm
     (a-program (body)
                (eval-expression body (init-env))))))


;Ambiente inicial
(define init-env
  (lambda ()
    (extend-env
     '(@xn @yn @zn @d @e)
     '("an" "bn" "cn" 4 "FLP")
     (empty-env))))


;; Función de evaluación general para expresiones
(define eval-expression1
  (lambda (exp env)
    (cond
      ((boolean? exp) exp)  ; Si la expresión es un booleano, simplemente devuelve su valor
      ((number? exp) exp)   ; Si la expresión es un número, simplemente devuelve su valor
      (else (eopl:error "Expresión no reconocida")))))  ; Maneja otros tipos de expresiones


(define eval-if
  (lambda (cond-exp then-exp else-exp env)
    (let ((cond-val (eval-expression1 cond-exp env)))
      (if (not (boolean? cond-val))
          (eopl:error "La condición de if debe ser un booleano.")
          (if cond-val
              (eval-expression1 then-exp env)
              (eval-expression1 else-exp env))))))


;; Función que evalúa el ciclo while con chequeo de tipo
(define eval-while
  (lambda (test-exp body env)
    (let ((test-val (eval-expression1 test-exp env)))
      (if (not (boolean? test-val))
          (eopl:error "La condición en while debe ser un booleano.")
          (if test-val
              (begin
                (eval-expression1 body env)
                (eval-while test-exp body env))  ; Recursión de cola
              'done)))))


;; Función que evalúa el ciclo for con chequeo de tipo
(define eval-for
  (lambda (init-exp cond-exp update-exp body-exp env)
    ;; Evaluar la inicialización (identifier -> expression)
    (eval-expression1 init-exp env)
    
    ;; Evaluar la condición de la expresión (expresión booleana)
    (let loop ((test-val (eval-expression1 cond-exp env)))
      (if (not (boolean? test-val))
          (eopl:error "La condición en el ciclo for debe ser un booleano.")
          (if test-val
              (begin
                ;; Evaluar el cuerpo del ciclo (se evalúan todas las expresiones del cuerpo)
                (for-each (lambda (exp) (eval-expression1 exp env)) body-exp)
                
                ;; Evaluar la actualización
                (eval-expression1 update-exp env)
                
                ;; Recursión para continuar el ciclo
                (loop (eval-expression1 cond-exp env)))
              'done)))))




(define eval-switch
  (lambda (exp env cases default-exp)
    (define test-val (eval-expression1 exp env))
    
    (define eval-case
      (lambda (case-val case-exp)
        (if (equal? test-val case-val)
            (eval-expression1 case-exp env)
            #f)))
    
    (define case-results
      (map (lambda (case)
             (eval-case (car case) (cdr case))) cases))
    
    (if (memv #f case-results)
        (eval-expression1 default-exp env)
        'done)))


(define (make-list-exp type elements)
  (if (not (list? elements))
      (eopl:error "Se esperaba una lista como argumento para make-list.")
      (list 'make-list type elements)))


;; Verifica si una lista está vacía
(define eval-empty? 
  (lambda (list-exp env)
    (let ((list-val (eval-expression1 list-exp env)))
      (if (not (list? list-val))
          (eopl:error "Se esperaba una lista.")
          (null? list-val)))))

;; Devuelve el primer elemento de la lista
(define eval-head
  (lambda (list-exp env)
    (let ((list-val (eval-expression1 list-exp env)))
      (if (not (list? list-val))
          (eopl:error "Se esperaba una lista.")
          (if (null? list-val)
              (eopl:error "La lista está vacía.")
              (car list-val))))))


;; Devuelve el resto de la lista (todos los elementos excepto el primero)
(define eval-tail
  (lambda (list-exp env)
    (let ((list-val (eval-expression1 list-exp env)))
      (if (not (list? list-val))
          (eopl:error "Se esperaba una lista.")
          (if (null? list-val)
              (eopl:error "La lista está vacía.")
              (cdr list-val))))))

;; Crea una lista con los elementos dados
(define eval-make-list
  (lambda (type-exp list-exp env)
    (let ((list-val (eval-expression1 list-exp env)))
      (if (not (list? list-val))
          (eopl:error "Se esperaba una lista.")
          (list 'make-list type-exp list-val)))))

;; Operación append de listas
(define eval-append-list
  (lambda (list1-exp list2-exp env)
    (let ((list1-val (eval-expression1 list1-exp env))
          (list2-val (eval-expression1 list2-exp env)))
      (if (not (list? list1-val)) 
          (eopl:error "El primer argumento no es una lista.")
          (if (not (list? list2-val)) 
              (eopl:error "El segundo argumento no es una lista.")
              (append list1-val list2-val))))))


;; Refere a un elemento del vector en el índice dado
(define eval-ref-vector
  (lambda (vector-exp index-exp env)
    (let ((vector-val (eval-expression1 vector-exp env))
          (index-val (eval-expression1 index-exp env)))
      (if (not (vector? vector-val))
          (eopl:error "Se esperaba un vector.")
          (if (not (integer? index-val))
              (eopl:error "El índice debe ser un número entero.")
              (vector-ref vector-val index-val))))))


;; Modifica el valor en el índice dado
(define eval-set-vector
  (lambda (vector-exp index-exp value-exp env)
    (let ((vector-val (eval-expression1 vector-exp env))
          (index-val (eval-expression1 index-exp env))
          (value-val (eval-expression1 value-exp env)))
      (if (not (vector? vector-val))
          (eopl:error "Se esperaba un vector.")
          (if (not (integer? index-val))
              (eopl:error "El índice debe ser un número entero.")
              (begin
                (vector-set! vector-val index-val value-val)
                vector-val))))))

;; Crea un vector con los elementos dados
(define eval-make-vector
  (lambda (size-exp value-exp env)
    (let ((size-val (eval-expression1 size-exp env))
          (value-val (eval-expression1 value-exp env)))
      (if (not (integer? size-val))
          (eopl:error "El tamaño debe ser un número entero.")
          (make-vector size-val value-val)))))




;; Evaluar operaciones aritméticas
(define eval-prim-arithmetic
  (lambda (operator exp1 exp2 env)
    (let ((val1 (eval-expression1 exp1 env))
          (val2 (eval-expression1 exp2 env)))
      (cond
        ((eq? operator '+) (+ val1 val2))
        ((eq? operator '-) (- val1 val2))
        ((eq? operator '*) (* val1 val2))
        ((eq? operator '/) (/ val1 val2))
        (else (eopl:error "Operador aritmético no reconocido"))))))

;; Evaluar operaciones lógicas
(define eval-prim-logical
  (lambda (operator exp1 exp2 env)
    (let ((val1 (eval-expression1 exp1 env))
          (val2 (eval-expression1 exp2 env)))
      (cond
        ((eq? operator '==) (if (= val1 val2) #t #f))
        ((eq? operator '!=) (if (not (= val1 val2)) #t #f))
        ((eq? operator '<) (if (< val1 val2) #t #f))
        ((eq? operator '> ) (if (> val1 val2) #t #f))
        ((eq? operator '<=) (if (<= val1 val2) #t #f))
        ((eq? operator '>=) (if (>= val1 val2) #t #f))
        (else (eopl:error "Operador lógico no reconocido"))))))



; Definición del entorno para variables y funciones
(define empty-env '())  ; El entorno vacío
(define extend-env (lambda (var val env) (cons (cons var val) env)))  ; Extiende el entorno
(define lookup-env (lambda (var env) 
                     (cond ((null? env) (eopl:error "Variable no encontrada"))
                           ((eq? var (car (car env))) (cdr (car env)))
                           (else (lookup-env var (cdr env))))))

; Función para mostrar el entorno (útil para depuración)
(define show-env
  (lambda (env)
    (if (null? env)
        '()
        (cons (car (car env)) (show-env (cdr env))))))

; Evaluación general para expresiones
(define eval-expression
  (lambda (exp env)
    (cond
      ((boolean? exp) exp)
      ((number? exp) exp)
      ((string? exp) exp)
      ((symbol? exp) (lookup-env exp env))
      ((pair? exp) (eval-special-form exp env))
      (else (eopl:error "Expresión no reconocida")))))

; Evaluación de expresiones especiales (condicionales, ciclos, funciones, etc.)

(define eval-special-form
  (lambda (exp env)
    (cond
      ((eq? (car exp) 'if) (eval-if (cadr exp) (caddr exp) (cadddr exp) env))
      ((eq? (car exp) 'while) (eval-while (cadr exp) (caddr exp) env))
      ((eq? (car exp) 'for)
 (eval-for (cadr exp) (caddr exp) (cadddr exp) (cadddr (cdr (cdr (cdr exp)))) env))  ; Corrige esto
  ; Corrige esto
      ((eq? (car exp) 'proc) (eval-proc-decl (cadr exp) (caddr exp) (cadddr exp) env))
      ((eq? (car exp) 'call) (eval-proc-call (cadr exp) (cdr exp) env))
      ((eq? (car exp) 'make-list) (eval-make-list (cadr exp) (caddr exp) env))
      ((eq? (car exp) 'make-vector) (eval-make-vector (cadr exp) (caddr exp) env))
      ((eq? (car exp) 'make-dict) (eval-make-dict (cadr exp) (caddr exp) env))
      (else (eopl:error "Forma especial no reconocida")))))


; Evaluación de la declaración de procedimiento
(define eval-proc-decl
  (lambda (name params body exp env)
    (define proc
      (lambda args
        (let ((new-env (extend-env params args env)))
          (eval-expression body new-env))))
    (extend-env name proc env)))  ; Aquí estaba el problema, ya está separado


; Evaluación de llamada a procedimiento
(define eval-proc-call
  (lambda (name args exp env)
    (define proc (lookup-env name env))
    (let ((arg-values (map (lambda (arg) (eval-expression arg env)) args)))
      (apply proc arg-values))))

; Evaluación de la creación de diccionarios
(define make-dict
  (lambda ()
    '()))  ; Devuelve una lista vacía como diccionario

(define add-to-dict
  (lambda (dict key value)
    (cons (cons key value) dict)))  ; Añade un par clave-valor al diccionario

(define lookup-in-dict
  (lambda (dict key)
    (define lookup-helper
      (lambda (lst)
        (cond ((null? lst) (eopl:error "Clave no encontrada"))
              ((= (car (car lst)) key) (cdr (car lst)))
              (else (lookup-helper (cdr lst))))))
    (lookup-helper dict)))  ; Busca un valor dado una clave

(define eval-make-dict
  (lambda (key-exp value-exp env)
    (let ((key-val (eval-expression key-exp env))
          (value-val (eval-expression value-exp env)))
      (if (not (symbol? key-val))
          (eopl:error "Las claves deben ser símbolos.")
          (add-to-dict (make-dict) key-val value-val)))))  ; Crea el diccionario y añade el par clave-valor

; Función para manejar el flujo de control de break y continue
(define eval-control-flow
  (lambda (exp env)
    (cond ((eq? (car exp) 'break) 'break)
          ((eq? (car exp) 'continue) 'continue)
          (else (eval-expression exp env)))))


(define foldl
  (lambda (f base lst)
    (if (null? lst)
        base
        (foldl f (f (car lst) base) (cdr lst)))))  ; Recursión sobre la lista


; Ejecuta una lista de expresiones en secuencia
(define eval-block
  (lambda (exp-list env)
    (foldl (lambda (exp acc)
             (if (eq? acc 'break)
                 'break
                 (let ((result (eval-expression exp env)))
                   (if (eq? result 'continue) result 'done))))
           'done exp-list)))


; Implementación de grafos dirigidos y primitivas adicionales para FusionLang

; Primitivas para manejo de grafos dirigidos
(define make-graph
  (lambda (vertices edges)
    (list 'graph vertices edges)))

(define graph-vertices
  (lambda (graph)
    (if (eq? (car graph) 'graph)
        (cadr graph)
        (eopl:error "Se esperaba un grafo"))))

(define graph-edges
  (lambda (graph)
    (if (eq? (car graph) 'graph)
        (caddr graph)
        (eopl:error "Se esperaba un grafo"))))

(define graph-add-edge
  (lambda (graph edge)
    (if (eq? (car graph) 'graph)
        (let* ((vertices (cadr graph))
               (edges (caddr graph))
               (new-vertices (foldl (lambda (v acc)
                                      (if (member v vertices)
                                          acc
                                          (cons v acc)))
                                    vertices edge)))
          (list 'graph new-vertices (cons edge edges)))
        (eopl:error "Se esperaba un grafo"))))


(define filter
  (lambda (pred lst)
    (if (null? lst)
        '()
        (if (pred (car lst))
            (cons (car lst) (filter pred (cdr lst)))
            (filter pred (cdr lst))))))


; Primitivas de operación de vecinos
(define graph-neighbors-out
  (lambda (graph vertex)
    (if (eq? (car graph) 'graph)
        (let ((edges (caddr graph)))
          (filter (lambda (e) (eq? (car e) vertex)) edges))
        (eopl:error "Se esperaba un grafo"))))

(define graph-neighbors-in
  (lambda (graph vertex)
    (if (eq? (car graph) 'graph)
        (let ((edges (caddr graph)))
          (filter (lambda (e) (eq? (cadr e) vertex)) edges))
        (eopl:error "Se esperaba un grafo"))))

; Primitivas adicionales para diccionarios
(define eval-keys-dict
  (lambda (dict env)
    (if (list? dict)
        (map car dict)
        (eopl:error "Se esperaba un diccionario"))))

(define eval-values-dict
  (lambda (dict env)
    (if (list? dict)
        (map cdr dict)
        (eopl:error "Se esperaba un diccionario"))))

(define eval-append-dict
  (lambda (dict key value env)
    (if (list? dict)
        (if (lookup-in-dict dict key)
            (eopl:error "La clave ya existe en el diccionario")
            (cons (cons key value) dict))
        (eopl:error "Se esperaba un diccionario"))))
;(interpretador)