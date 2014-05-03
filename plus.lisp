
;Applique ou non la regle
(defun appliquer_regle_plus (r)
	(cond
		((is_appliquee r) 
			nil)
		((is_vraie_lp_plus (get_partie_gauche_regle_plus r))
			(format t "# Application de la regle ~S~%" (get_nom_regle r))
			(set_arg_plus (cadar (get_partie_droite_regle_plus r)) (caddar(get_partie_droite_regle_plus r)))
			(set_appliquee r)
			(set_vraie (get_nom_regle r))
			t
		)
		(t
			nil)
	)
)
(defun chainer_avant_plus()
	(setq rc (somme 'appliquer_regle_plus regles))
	(cond
		((null rc)
			())
		(t 
			(chainer_avant_plus))
	)
)

(defun compile_regles_plus()
	(mapcar 'compile_regle_plus regles)
)

;Initialise les differentes variables
(defun compile_regle_plus(r)
	(compte_premisses_regle r)
	(mapcar
		#'(lambda(p)
			(set_arg_plus (cadr p) 'default)
		)
		(get_partie_droite_regle_plus r)
	)
	(mapcar
		#'(lambda(p)
			(set_arg_plus (cadr p) '())
		)
		(get_partie_gauche_regle r)
	)
)

(defun get_partie_droite_regle_plus (r)
	(cddddr r)
)
(defun get_partie_gauche_regle_plus (r)
	(caddr r)
)

;Test si l'ensemble des conditions de la sont verifiees
(defun is_vraie_lp_plus (liste_de_prop)
	(not (member nil (mapcar #'(lambda(p) (eval_arg_plus (car p) (get_arg_plus (cadr p)) (caddr p)) ) liste_de_prop)))
)

;Definit un arguement 'name' avec la valeur 'value'
(defun set_arg_plus (name value)
	(putprop name value 'value)
)

;Recupere la valeur de l'argument 'name'
(defun get_arg_plus (name)
	(get name 'value)
)

;Evalue la condition
(defun eval_arg_plus (name condi value)
	(if (null name)
		nil
		(progn
			(cond 
				((numberp condi)
					(if (eval (list name condi value))
						t
						nil)
				)
				((eq condi nil)
					(if (eval (list name condi (string value)))
						t
						nil)
				)
				(t 
					(if (eval (list name (string condi) (string value)))
						t
						nil)
				)
			)
		)
	)
)