(load "projet.lisp")
(load "plus.lisp")

(setq texteChoixRegle '("Base de Winston" "Base personnelle - Ordre 0" "Base personnelle - Ordre 0+"))
(setq texteChoixChainage '("Chainage avant" "Chainage arriere"))
(setq texteChoixHeuristique '("La premiere regle valide" "La regle avec le moins de premisse valide" "La regle avec le plus de premisse valide"))
(setq texteChoix texteChoixRegle)
(setq message "Veuillez choisir une base de regle")
(setq basesRegle (list regles_winston regles_cantine regles_achat_voiture ))
(setq continuer 1)
(setq curseur 0)
(setq ordre+ 0)

;step=0 : Choix de la base de regle
;step=1 : Choix du type de chainage
;step=2 : Choix de l'heuristique (chainage avant)
;step=3 : Chainage avant (choix faits)
;step=4 : Chainage arriere (choix fait)
;step=5 : Message de fin
(setq step 0)

(setq modeSaisie 0)

;affichage du menu
(afficher-header)
(afficher-message-header message)
(afficher-menu curseur texteChoix)

(loop while ( /= continuer 0) do
	(setq toucheClavier (lire-clavier))

	;Test quelle touche du clavier a ete pressee
	(cond
		((equal toucheClavier #\Space)
			;Si la liste des choix possibles est repartie sur plusieurs colonnes 
			(if (> modeSaisie 0)
				;Si l'option n'est pas selectionnee on la coche sinon on la decoche
				(if (equal (car (nth curseur listeFaits)) 0)
					(setq listeFaits (remplacer-element listeFaits curseur 1))
					(setq listeFaits (remplacer-element listeFaits curseur 0))
				)
			)
		)
		((equal toucheClavier :UP)
			;Si la liste des choix possibles est repartie sur une colonne
			(if (equal modeSaisie 0)
				(setq curseur (mod (- curseur 1) (length texteChoix)))
				(setq curseur (mod (- curseur 3) (length listeFaits)))
			)
		)
		((equal toucheClavier :DOWN)
			;Si la liste des choix possibles est repartie sur une colonne
			(if (equal modeSaisie 0)
				(setq curseur (mod (+ curseur 1) (length texteChoix)))
				(setq curseur (mod (+ curseur 3) (length listeFaits)))
			)
		)
		((equal toucheClavier :LEFT)
			(if (equal modeSaisie 1)
				(setq curseur (mod (- curseur 1) (length listeFaits)))
			)
		)
		((equal toucheClavier :RIGHT)
			(if (equal modeSaisie 1)
				(setq curseur (mod (+ curseur 1) (length listeFaits)))
			)
		)
		((equal toucheClavier #\Escape)
			(setq continuer 0)
		)
		((equal toucheClavier #\Return)
			(cond
				((equal step 0);Choix de la base de regle
					(setq regles (nth curseur basesRegle))

					(if (< curseur 2)
						(compile_regles)
						(progn
							(compile_regles_plus)
							(mapcar #'(lambda(r)
								(mapcar
									#'(lambda(p)
										(set (cadr p) '())
									)
									(get_partie_droite_regle_plus r)
								)
								(mapcar
									#'(lambda(p)
										(set (cadr p) '())
									)
									(get_partie_gauche_regle r)
								)
								) regles
							)
							(setq ordre+ 1)
						)
					)

					(setq step 1)
				)
				((equal step 1);Choix du chainage
					;Si cursuer = 0 -> Chainage avant donc choix d'heuristique, sinon choix direct du fait pour le chainage arriere
					(if (equal curseur 0)
						(setq step 2)
						(setq step 4)
					)

					(if (not (equal ordre+ 1))
						(setq listeFaits (get_liste_faits_formatee))
					)
					
				)
				((equal step 2);Choix de l'heuristique
					(cond 
						((equal curseur 1)(prioriteMoinsDePremisses regles))
						((equal curseur 2)(prioritePlusDePremisses regles))
					)
					(setq step 3)
				)
				((equal step 3);Choix des faits pour le chainage avant
					(if (equal ordre+ 1)
						(progn
							;(setq compagnon 'VRAI)
							;(setq enfants 0)
							;(setq revenus 500000)
							(set_arg_plus 'compagnon 'VRAI)
							(set_arg_plus 'enfants 0)
							(set_arg_plus 'revenus 500000)

							(chainer_avant_plus)
							(format t "#~%#~%# Conclusion : MA VOITURE = ~S" (get_arg_plus 'voiture))
							(setq continuer 0)
							(setq step 5)
						)
						(progn
							;Si au moins un fait a ete selectionne
							(if (> (length (extraire_faits_selectionnes listeFaits)) 0)
								(progn 
									(afficher-header)
									(chainer_avant (extraire_faits_selectionnes listeFaits))
									(setq continuer 0)
									(setq step 5)
								)
							)
						)
					)

				)
				((equal step 4);Choix du fait pour le chainage arriere
					;Si UN SEUL fait a ete selectionne
					(if (equal (length (extraire_faits_selectionnes listeFaits)) 1)
						(progn 
							(afficher-header)
							(chainer_arriere (car (extraire_faits_selectionnes listeFaits)))
							(setq continuer 0)
							(setq step 5)
						)
					)
				)
			)
			(setq curseur 0);Reset du curseur a la position 0
		)
	)

	(cond
		((< step 3)
			(afficher-header)
			(cond
				((equal step 0)
					(setq texteChoix texteChoixRegle)
					(setq message "Veuillez choisir une base de regle")
				)
				((equal step 1)
					(setq texteChoix texteChoixChainage)
					(setq message "Veuillez choisir un type de chainage")
				)
				((equal step 2)
					(setq texteChoix texteChoixHeuristique)
					(setq message "Veuillez choisir une heuristique")
				)
			)

			(afficher-message-header message)
			(afficher-menu curseur texteChoix)
		)
		((< step 5)

			(if (equal ordre+ 0)
				(progn
					(afficher-header)
					(setq modeSaisie 1)
					(afficher-menu-choix curseur listeFaits)
				)
			)
		)
		((equal step 5)
			(setq continuer 0)
			(afficher-footer)
		)
	)
)