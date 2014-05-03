(load "base.lisp")
(load "menuFunc.lisp")

;Buffer pour la stack trace
(setq buffer_inference (make-string-output-stream))

(defun putprop (atome valeur propriete)
	(setf (get atome propriete) valeur)
)
; details regle
(defun get_partie_gauche_regle (r)
	(caddr r)
)
(defun get_partie_droite_regle (r)
	(cadr(cdddr r))
)
(defun get_nom_regle (r)
	(car r)
)

(defun get_nombre_de_premisses (r)
	(get (get_nom_regle r) 'nbPremisses)
)

; infos sur propriete
(defun get_regles_concl_sur_prop (p)
	(get p 'concl)
)
(defun is_terminal (p)
	(null (get_regles_concl_sur_prop p))
)

; appliquee
(defun is_appliquee (r)
	(get (get_nom_regle r) 'appliquee)
)
(defun set_appliquee (r)
	(putprop (get_nom_regle r) t 'appliquee)
)

; demandable
(defun is_demandable (p)
	(get p 'demandable)
)
(defun set_demandable (p)  
	(putprop p t 'demandable)
)

; recherchee
(defun is_recherchee (p)
	(get p 'recherchee)
)
(defun set_recherchee (p)
	(putprop p t 'recherchee)
)

; vraie
(defun is_vraie (p)
	(get p 'vraie)
)

(defun is_vraie_lp (liste_de_prop)
	(not (member nil (mapcar 'is_vraie liste_de_prop)))
)
(defun set_vraie (p)
	(putprop p t 'vraie)
)

(defun is_recherchee_and_notvraie (p)
	(  and (is_recherchee p) ( not (is_vraie p) )  )
)

; presentee
(defun is_presentee (p)
	(get p 'presentee)
)
(defun set_presentee (p)
	(putprop p t 'presentee)
)

(defun somme (pred liste)
	(cond
		((null liste) nil)
		((apply pred (list(car liste)))(car liste))
		(t (somme pred (cdr liste)))
	)
)

(defun compte_premisses_regle (r)
	(putprop (get_nom_regle r) (length (get_partie_gauche_regle r)) 'nbPremisses)
)
; compilation
(defun compile_regles()
	(setq propositions nil)
	(mapcar 'compile_regle regles)
)
(defun compile_regle(r)
	(compte_premisses_regle r)
	(mapcar
		#'(lambda(p)
			(if (member p propositions) propositions 
				(setq propositions (cons p propositions))) ; else
			(putprop p (cons r(get p 'concl)) 'concl)
		)
		(get_partie_droite_regle r)
	)
	(mapcar
		#'(lambda(p)
			(if (member p propositions) propositions 
				(setq propositions (cons p propositions))) ; else
		)
		(get_partie_gauche_regle r)
	)
)

; rafraichir le moteur d'inférence
(defun rafraichir_moteur_inference()
  (mapcar 'rafraichir_prop propositions)
  (mapcar 'rafraichir_regle regles)
)

(defun rafraichir_prop (p)
	(remprop p 'recherchee)
	(remprop p 'vraie)
	(remprop p 'presentee)
)
(defun rafraichir_regle (r)
	(remprop (get_nom_regle r) 'appliquee)
)

;chainage arrière
(defun chainer_arriere (p)
	(rafraichir_moteur_inference)
	(etablir_prop p)
	(presenter_resultats)
)

; établir une propostion (procédé différent selon ses propriétés)
(defun etablir_prop(p)
	(cond
		((is_recherchee p)
			(if(is_vraie p) t 
				(nil))) ; else
		((is_terminal p)
			(set_recherchee p)
			(questionner_et_conclure_prop p))
		((is_demandable p)
			( cond
				((questionner_et_conclure_prop p) 
					t)
				((equal (questionner_et_conclure_prop p) '?)
					(inferer_prop p)) )) 
		(t
			(set_recherchee p)
			(inferer_prop p))
	)
)

; demander une proposition
(defun questionner_et_conclure_prop (p)
	(setq terminer 0)
	(setq curseurON 0)

	(loop while (/= terminer 1) do
		(afficher-header)
		(format t "#~%# La proposition ~S est-elle vraie ? ~[> Oui    Non    Ne sais pas~;  Oui  > Non    Ne sais pas~;  Oui    Non  > Ne sais pas~]" p curseurON)
		(setq touche (lire-clavier))

		(cond
			((equal touche :LEFT)
				(setq curseurON (mod (- curseurON 1) 3))
			)
			((equal touche :RIGHT)
				(setq curseurON (mod (+ curseurON 1) 3))
			)
			((equal touche #\Return)
				(setq terminer 1)
				(cond
					((equal curseurON 0)
						(format buffer_inference "# La proposition ~S est-elle vraie ? Oui~%" p)
						
						(set_vraie p)
						(return-from questionner_et_conclure_prop t)
					)
					((equal curseurON 1)
						(format buffer_inference "# La proposition ~S est-elle vraie ? Non~%" p)

						(return-from questionner_et_conclure_prop nil)
					)
					((equal curseurON 2)
						(format buffer_inference "# La proposition ~S est-elle vraie ? Je ne sais pas~%" p)

						(return-from questionner_et_conclure_prop '?)
					)
				)
			)
		)
	)
)

; inférer une proposition
(defun inferer_prop(p)
	(let
		((liste_de_regles (get_regles_concl_sur_prop p)))
		(inferer1 p liste_de_regles)
	)
)
(defun inferer1 (p lisreg1)
	(cond
		((null lisreg1)
			nil)
		(t (essaye_regle (car lisreg1)) 
			(if (is_vraie p) t
				(inferer1 p (cdr lisreg1)) ; else
			)
		)
	)
)

; essayer une certaine règle en chainage arrière
(defun essaye_regle (r)
	(format buffer_inference "# Le moteur d'inférance essaye la regle ~S~%" r)

	(cond
		((is_appliquee r)
			nil)
		(t
			(let ((v (etablir_partie_gauche_pg (get_partie_gauche_regle r))))
				(cond
					((null v)
						nil)
					(t
						(applique_partie_droite_pd (get_partie_droite_regle r)))
				)
				(set_appliquee r)
			)
		)
	)	
)

; traitements sur partie gauche
(defun etablir_partie_gauche_pg (liste_de_prop)
	(cond
		((somme 'is_recherchee_and_notvraie liste_de_prop)
			nil)
		(t
			(etablir_pg1_pg liste_de_prop))
	)
)
(defun etablir_pg1_pg (liste_de_prop)
	(cond
		((null liste_de_prop) 
			t)
		((null (etablir_prop (car liste_de_prop))) 
			nil)
		(t 
			(etablir_pg1_pg (cdr liste_de_prop)))
	)
)

; traitements sur partie droite
(defun applique_partie_droite_pd (lp)
	(mapcar 'set_vraie lp)
	(mapcar 'montre_deduction_prop lp)
)
(defun montre_deduction_prop (p)
	(format buffer_inference "# Le moteur d'inférance déduit le fait ~S~%" p)
)

; chainage avant
(defun chainer_avant(liste_de_prop)
	(rafraichir_moteur_inference)
	(mapcar 'set_vraie liste_de_prop)
	(chainer_avant1)
	(presenter_resultats)
)

(defun chainer_avant1 ()
	(setq rc (somme 'appliquer_regle regles))
	(cond
		((null rc)
			())
		(t 
			(chainer_avant1))
	)
)

; appliquer une certaine règle en chainage avant
(defun appliquer_regle (r)
	(cond
		((is_appliquee r) 
			nil)
		((is_vraie_lp (get_partie_gauche_regle r))
			(set_appliquee r)

			(format buffer_inference "# Le moteur d'inférance applique la regle ~S~%" r)

			(applique_partie_droite_pd (get_partie_droite_regle r)) 
			t)
	)
)

; réponse final du moteur d'inférance
(defun presenter_resultats ()
	(format t "~%#~%#~%###############################################################~%" buffer_inference)
	(format t "#~%# -- Etapes de deduction du moteur d'inference : ~%#~%")
	(format t (get-output-stream-string buffer_inference))
	(format t "#~%#~%###############################################################~%")
	(format t "#~%# -- La conclusion du moteur d'inférance est~%#")
	(mapcar 'presenter_prop propositions)
)
(defun presenter_prop (p)
	(cond
		((is_presentee p) 
			nil)
		(t 
			(set_presentee p)
			(cond
				((is_vraie p)
					(format t "~%# ~S est vraie" p))
				((is_recherchee_and_notvraie p)
					(format t "~%# ~S est faux" p))
			)
		)
	)
)

; donner la priorité aux règles ayant le moins ou le plus de prémisses 
(defun prioriteMoinsDePremisses (regles)
	(sort regles 
    	(lambda (a b) 
    		(< (get_nombre_de_premisses a) (get_nombre_de_premisses b))
    	)
    )
)
(defun prioritePlusDePremisses (regles)
	(sort regles 
    	(lambda (a b) 
    		(> (get_nombre_de_premisses a) (get_nombre_de_premisses b))
    	)
    )
)