;retourne la touche appuyée
(defun lire-clavier ()
	(ext:with-keyboard 
		(setq input (read-char ext:*keyboard-input*))
		(setq key (OR (ext:char-key input) (character input)))
	)
	(return-from lire-clavier key)
)

;buffer utilisé pour afficher les déductions du moteur d'inférence
(setq buffer_inference '())

(defun afficher-header ()
	(format t "~50%###############################################################~%")
	(format t "#      ,     ,~%")
	(format t "#     (\\____\/)~%")
	(format t "#      (_oo_)~Vd~%" 20 "Bonjour humain")
	(format t "#        (O)~%")
	(format t "#      __||__    \\)~%")
	(format t "#   []/______\\[] \/~Vd~%" 36 "[Echap]  : Quitter")
	(format t "#   \/ \\______/ \\\/~Vd~%" 42 "[Espace] : Selectionner")
	(format t "#  \/    \/__\\~Vd~%" 51 "[Entrée] : Valider le choix")
	(format t "# (\\   \/____\\~Vd~%" 49 "[Haut] et [Bas] : Naviguer")
	(format t "#~%")
	(format t "###############################################################~%")
)

(defun afficher-footer ()
	(format t "~%#~%###############################################################~%")
	(format t "#      ,     ,~%")
	(format t "#     (\\____\/)~%")
	(format t "#      (_oo_)~Vd~%" 20 "Aurevoir")
	(format t "#        (O)~%")
	(format t "#      __||__    \\)~%")
	(format t "#   []/______\\[] \/~%")
	(format t "#   \/ \\______/ \\\/~%")
	(format t "#  \/    \/__\\~%")
	(format t "# (\\   \/____\\~%")
	(format t "#~%")
	(format t "###############################################################~%")
)

(defun afficher-message-header (msg)
	(format t "# >> ~A~%#~%#~%#~%" msg)
)

(defun afficher-menu (curseur liste)
	(loop for i from 0 to (- (length liste) 1) do
		(format t "# ~:[ ~;>~] ~A~%" (eq curseur i) (nth i liste))
	)
	(format t "#~%###############################################################~%")
)

;Affiche les différents éléments de la liste sous forme de liste sélectionnable
(defun afficher-menu-choix (curseur liste)
	(setq cases '((-) (*)))
	(setq elemParLigne 3)
	(setq nbLignes (ceiling (length liste) elemParLigne))
	(setq nbElemDerniereLigne (rem (length liste) elemParLigne))

	(format t "#~%")
	(loop for i from 0 to (- nbLignes 2) do
		(format t "#")
		(loop for j from 0 to (- elemParLigne 1) do
			(format t "~:[ ~;>~] ~S ~25,S" (equal curseur (+ (* i elemParLigne) j)) (nth (car (nth (+ (* i elemParLigne) j) liste)) cases) (cdr (nth (+ (* i elemParLigne) j) liste)))
		)
		(format t "~%")
	)

	(format t "#")
	(loop for k from (- (length liste) nbElemDerniereLigne) to (- (length liste) 1) do
		(format t "~:[ ~;>~] ~S ~25,S" (equal curseur k) (nth (car (nth k liste)) cases) (cdr (nth k liste)))
	)

	(format t "~%")
)

;Remplace l'element situé à la position 'num' de 'liste' par 'elem'
(defun remplacer-element (liste num elem)
	(cond 
		((null liste)
			()
		)
		((eq num 0)
			(cons (cons elem (cdar liste)) (cdr liste))
		)
		(t
			(cons (car liste) (remplacer-element (cdr liste) (- num 1) elem))
		)
	)
)

;Génère une liste de la même longueur que 'liste' avec comme valeurs 'valeurParDefaut'
(defun generer-cases (liste valeurParDefaut)
	(setq listeCases '())

	(loop for i from 0 to (- (length liste) 1) do
		(setq listeCases (cons valeurParDefaut listeCases))
	)

	(return-from generer-cases listeCases)
)

(defun get_liste_faits_formatee ()
	(return-from get_liste_faits_formatee (mapcar #'(lambda(x)(cons 0 (car (list x)))) propositions))
)

(defun extraire_faits_selectionnes (liste)
	(setq newListe '())

	(cond 
		((null liste) () )
		((equal (caar liste) 1)
			(cons (cdar liste) (extraire_faits_selectionnes (cdr liste)))
		)
		(t
			(extraire_faits_selectionnes (cdr liste))
		)
	)
)