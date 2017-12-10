(DEFUN prefijo (L X)
	(COND 
		(
			(OR (NOT (ZEROP (mod X 1)))	(> X (largo L)) (< X 0)) '(Ingrese un indice valido)
		)
		(
			T (prefijo_aux L X)
		)

	)

)


(DEFUN prefijo_aux (L X) 
	(COND 
		(
			(> X 1) (APPEND (LIST(CAR L)) (prefijo (CDR L) (- X 1)))
		)

		(
			(EQUAL X 1) (LIST (CAR L))
		)
					
	)
)

(DEFUN sublist (L X Y)

	(COND
		(
			(OR (< X 0) (> Y (largo L)) (> X Y) (NOT (ZEROP (mod X 1))) (NOT (ZEROP (mod Y 1)))) '(Ingrese indices validos)
		)
		(
			T (sublist_aux L X Y)
		)

	)

)

(DEFUN sublist_aux (L X Y) 

		(COND	
			(
				(> X 1) (sublist (CDR L) (- X 1) (- Y 1))
			)
			(
				(EQUAL X 1) (prefijo L Y)
			)
		)	

)


(DEFUN largo (L)
			(COND
					(
						(NULL L) 0
					)
					(
						T (+ 1 (largo (CDR L)))
					)
			)
)

(DEFUN rec-reverse (L)

		(COND 
			(
				(NULL L) NIL
			)
			(
				(ATOM (CAR L)) (APPEND (rec-reverse (CDR L)) (LIST(CAR L))) 
			)

			(
				T (APPEND (rec-reverse (CDR L)) (LIST (rec-reverse (CAR L)))) 
			) 
				

		)
)



(DEFUN eliminar (L X Y Elimine)

	(COND 
			(
				(NULL L) NIL
			)
			(;Esta parte del código es para cuando ya eliminó de la lista que esta recorriendo pero sigue recorriendola para eliminar de las sublistas
				(EQUAL X 1) (COND
								((ZEROP Elimine) (eliminar (CDR L) 1 Y 1))
								((ATOM(CAR L)) (APPEND (LIST (CAR L)) (eliminar (CDR L) 1 Y 1)))
								(T (APPEND (LIST (eliminar (CAR L) Y Y 0)) (eliminar (CDR L) 1 Y 1)))
							)
			)
			;Si todavia no elimino, se ejecutara esta porcion de codigo
			(  
				(ATOM (CAR L)) (APPEND (LIST (CAR L)) (eliminar (CDR L) (- X 1) Y 0))
			
			)
			(
				T (APPEND (LIST (eliminar (CAR L) Y Y 0)) (eliminar (CDR L) (- X 1) Y 0))
			)
	)

)

(DEFUN elim-n-rec (L N)

	(COND
		(  	;Valida las entradas	
			(OR (> 0 N) (NOT (ZEROP (mod N 1)))) '(Ingrese un indice valido)
		)
		(   ;Llama al metodo auxiliar
			T (eliminar L N N 0)
		)
	)
	
)
