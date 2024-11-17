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
     (program ("PROGRAM" "{" (arbno proc-decl ";") "}") program-exp)
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
     ;(primitive ("->") asignar-valor)
     ;(expresion (identifier "->" expression) asignar-exp)
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
     ; Imprimir
     (expression ("imprimir" "(" expression ")") imprimir-exp)
     
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


;Funciones auxiliares--------------------------------------------------------------------------------------

;Evaluar una lista de expresiones(operandos) en un ambiente
(define eval-rands
  (lambda (exps env)
    (map
      (lambda (exp) (eval-expression exp env))
      exps)))

;Evaluar primitivas
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      ; Operaciones aritméticas
      (prim-suma () (+ (car args) (cadr args)))
      (prim-resta () (- (car args) (cadr args)))
      (prim-multi () (* (car args) (cadr args)))
      (prim-div () (/ (car args) (cadr args)))
      (prim-concat () (string-append (car args) (cadr args)))
      (primitiva-longitud () (string-length (car args)))
      ; Operaciones lógicas
      (menor-exp () (if (< (car args) (cadr args)) 1 0))
      (mayor-exp () (if (> (car args) (cadr args)) 1 0))
      (menorIgual-exp () (if (<= (car args) (cadr args)) 1 0))
      (mayorIgual-exp () (if (>= (car args) (cadr args)) 1 0))
      (igual-exp () (if (= (car args) (cadr args)) 1 0))
      (diferente-exp () (if (not (= (car args) (cadr args))) 1 0))
      ;Falta operación de asignación
      )))

;Aplicar operadores primitivos sobre listas
(define apply-unary-primitive-list
  (lambda (un-prim arg)
    (cases unary-primitive-list un-prim
      ;; Caso: el operador primitivo unario es empty?
      (is-null-primitive-list ()
        (cases lista arg
          ;; Si el argumento es una lista vacía, devuelve #t, de lo contrario, devuelve #f
          (lista-vacia () #t)
          (else #f)
        )
      )
      ;; Caso: el operador primitivo unario es empty (lista vacía) (null-primitive-list)
      (null-primitive-list () lista-vacia)
      ;; Caso: el operador primitivo unario es list? (is-lista-primitive)
      (is-list-primitive () (list? arg))
      ;; Caso: el operador primitivo unario es car (head)
      (car-primitive-list () 
        (cases lista arg
          ;; Si la lista es vacía, se genera un error de índice fuera de rango
          (lista-vacia () (eopl:error 'apply-unary-primitive-list
                            "List index out of range"))
          ;; Si la lista es extendida, devuelve el primer elemento
          (lista-extendida (vals) (vector-ref vals 0))
        )
      )
      ;; Caso: el operador primitivo unario es cdr (tail)
      (cdr-primitive-list () 
        (cases lista arg
          ;; Si la lista es vacía, se genera un error de índice fuera de rango
          (lista-vacia () (eopl:error 'apply-unary-primitive-list
                            "List index out of range"))
          ;; Si la lista es extendida, devuelve la lista sin el primer elemento
          (lista-extendida (vals) 
            (letrec ((vals-l (vector->list vals))
                     (cdr-vals-l (cdr vals-l)))
              (if (null? cdr-vals-l)
                  lista-vacia
                  (lista-extendida (list->vector cdr-vals-l))))))))))

(define apply-list-primitive
  (lambda (l-prim list-ref rands)
    ; Obtener la lista y el valor del entorno
    (let ((l (deref list-ref))
          (val (car rands)))
      ; Realizar el patrón de casos para la primitiva de concatenación de listas (append)
      (cases list-primitive l-prim
        ;; Caso para la primitiva de concatenación de listas (append)
        (append-primitive ()
          (let ((new-list 
                 (cases lista l
                   ;; Si la lista es vacía, crea una nueva lista con el valor
                   (lista-vacia () (lista-extendida (vector val)))
                   ;; Si la lista es extendida, concatena el valor al final de la lista
                   (lista-extendida (vals) 
                     (letrec ((vals-l (vector->list vals))
                              (new-vals (append vals-l (list val))))
                       (lista-extendida (list->vector new-vals)))))))
            ;; Actualizar la referencia de la lista con la nueva lista concatenada
            (setref! list-ref new-list)))))))

;Aplicar operadores primitivos sobre vectores
(define apply-unary-primitive-vector
  (lambda (un-prim arg)
    (cases unary-primitive-vector un-prim
      ;; Caso: el operador primitivo unario es vector? (comprobar si es un vector)
      (is-vector-primitive ()
        (vector? arg)))))  ;; Devuelve #t si el argumento es un vector, #f si no lo es

(define apply-vector-primitive
  (lambda (v-prim vec-ref rands)
    ; Obtener el vector y el valor del entorno
    (let ((v (deref vec-ref))
          (val (car rands)))  ;; Tomar el primer valor de los operandos (rands)
      (cond
        ;; Caso: ref-vector (acceso a un elemento en el vector)
        [(equal? v-prim 'ref-vector) 
         (vector-ref v val)]  ;; Devuelve el valor en la posición 'val' del vector

        ;; Caso: set-vector (modificar un elemento en el vector)
        [(equal? v-prim 'set-vector) 
         (vector-set! v val (car (cdr rands)))  ;; Modifica el elemento en la posición 'val'
         v]  ;; Retorna el vector actualizado

        ;; Caso: append-vector (agregar un valor al final del vector)
        [(equal? v-prim 'append-vector) 
         (let ((new-v (list->vector (append (vector->list v) (list (car rands))))))  ;; Convierte el vector a lista, agrega el valor, y convierte de vuelta a vector
           new-v)]  ;; Devuelve el nuevo vector con el valor agregado

        ;; Caso: delete-val-vector (eliminar un valor específico del vector) FALTA ESTE CASO
        ))))

;Aplicar operadores primitivos sobre diccionarios
(define apply-unary-primitive-dict
  (lambda (un-prim arg)
    (cases unary-primitive-dict un-prim
      ;; Caso: el operador primitivo unario es dict? (comprobar si es un diccionario)
      (is-dict-primitive ()
        (dict? arg)))))  ;; Devuelve #t si el argumento es un diccionario, #f si no lo es

;; Funciones auxiliares para el manejo de diccionarios

;; Busca un valor en el diccionario usando la clave
(define (dict-ref dict key)
  (cond
    [(null? dict) #f]  ;; Si el diccionario está vacío, retorna #f
    [(equal? (car (car dict)) key) (cdr (car dict))]  ;; Si la clave coincide, retorna el valor
    [else (dict-ref (cdr dict) key)]))  ;; Busca en el resto del diccionario

;; Modifica o agrega una nueva entrada en el diccionario
(define (dict-set! dict key value)
  (cond
    [(null? dict) (list (list key value))]  ;; Si el diccionario está vacío, crea una nueva entrada
    [(equal? (car (car dict)) key) (list (list key value) (cdr dict))]  ;; Si la clave ya existe, modifica el valor
    [else (cons (car dict) (dict-set! (cdr dict) key value))]))  ;; Si no, sigue buscando y agrega la clave-valor al final

;; Agrega una nueva pareja clave-valor al diccionario
(define (dict-pair dict key value)
  (cons (list key value) dict))  ;; Agrega la nueva pareja al diccionario

;; Devuelve una lista con todas las claves del diccionario
(define (dict-keys dict)
  (map car dict))  ;; Devuelve una lista de todas las claves

;; Devuelve una lista con todos los valores del diccionario
(define (dict-values dict)
  (map cadr dict))  ;; Devuelve una lista de todos los valores

;; Función principal para aplicar las primitivas del diccionario
(define apply-dict-primitive
  (lambda (d-prim dict-ref rands)
    ; Obtener el diccionario y los valores del entorno
    (let ((d (deref dict-ref))
          (key (car rands))  ;; Tomar el primer valor de los operandos (clave)
          (val (car (cdr rands))))  ;; Tomar el valor asociado para algunos casos
      (cond
        ;; Caso: ref-dict (acceso a un valor en el diccionario)
        [(equal? d-prim 'ref-dict) 
         (dict-ref d key)]  ;; Devuelve el valor asociado con la clave 'key' del diccionario

        ;; Caso: set-dict (modificar un valor en el diccionario)
        [(equal? d-prim 'set-dict) 
         (dict-set! d key val)  ;; Modifica el valor para la clave 'key'
         d]  ;; Retorna el diccionario actualizado

        ;; Caso: append-dict (agregar una nueva pareja clave-valor)
        [(equal? d-prim 'append-dict) 
         (dict-pair d key val)]  ;; Agrega la nueva pareja clave-valor al diccionario

        ;; Caso: keys-dict (obtener todas las claves del diccionario)
        [(equal? d-prim 'keys-dict) 
         (dict-keys d)]  ;; Devuelve todas las claves del diccionario

        ;; Caso: values-dict (obtener todos los valores del diccionario)
        [(equal? d-prim 'values-dict) 
         (dict-values d)]  ;; Devuelve todos los valores del diccionario
      ))))












;Referencias --------------------------------------------------------------------------------------------
(define-datatype reference reference?
  ;; Definición de la variante a-ref que representa una referencia
  (a-ref
    (position integer?)   ; Campo position debe ser un entero
    (vec vector?))         ; Campo vec debe ser un vector

  ;; Definición de la variante a-const que representa una constante
  (a-const
    (position integer?)   ; Campo position debe ser un entero
    (vec vector?)))        ; Campo vec debe ser un vector


(define deref
  (lambda (ref)
    ; Utiliza el formulario 'cases' para realizar coincidencias de patrones en el tipo de referencia.
    (cases reference ref
      ; Si es una referencia a un elemento en un vector ('a-ref'),
      ; accede al vector en la posición 'pos' y devuelve el elemento en esa posición.
      (a-ref (pos vec)
             (vector-ref vec pos))
      ; Si es una referencia a una constante en un vector ('a-const'),
      ; accede al vector en la posición 'pos' y devuelve la constante en esa posición.
      (a-const (pos vec)
               (vector-ref vec pos)))))

(define setref! 
  (lambda (ref val)
    ; Casos para el patrón de referencia
    (cases reference ref
      ; Si es una referencia a un elemento mutable (a-ref)
      (a-ref (pos vec)
        ; Modificar el vector en la posición especificada con el nuevo valor
        (vector-set! vec pos val))
      ; Si es una referencia a una constante (a-const)
      (a-const (pos vec)
        ; Generar un error, ya que no se puede cambiar el valor de una constante
        (eopl:error 'setref! "No es posible cambiar el valor de una constante")))
    ; Devuelve 1 (puede ser un valor arbitrario, dependiendo del contexto)
    1))


;El interpretador ------------------------------------------------------------------------------------------

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
       ;AÑADIR TODOS L
      )))


;Datatypes ---------------------------------------------------------------------------------------------
;; Definición del tipo de datos lista
(define-datatype lista lista?
  ;; Constructor: lista-vacia
  (lista-vacia)
  
  ;; Constructor: lista-extendida
  (lista-extendida
    ;; Argumento del constructor: vals (un vector)
    (vals vector?)
  )
)

(define-datatype dic dict?
  ;; Constructor: lista-vacia
  (dict-vacio)
)


;Ambientes --------------------------------------------------------------------------------------------

(define extend-env
  (lambda (syms vals env)
    ; Se utiliza extended-env-record para construir el nuevo entorno.
    ; extended-env-record debe ser definido en otra parte del código.
    (extended-env-record syms (list->vector vals) env)))

;(interpretador)