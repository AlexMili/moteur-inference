(setq regles_winston '( 
	    (R1 si (a-des-poils) 
	        alors (est-un-mammifere)) 
	    (R2 si (donne-du-lait) 
	        alors (est-un-mammifere)) 
	    (R3 si (a-des-plumes) 
	        alors (est-un-oiseau)) 
	    (R4 si (vole 
	            donne-des-oeufs) 
	        alors (est-un-oiseau)) 
	    (R5 si (mange-viande) 
	        alors (est-un-carnivore)) 
	    (R6 si (a-des-dent-pointues 
	            a-des-griffes 
	            a-des-yeux-frontaux) 
	        alors (est-un-carnivore)) 
	    (R7 si (est-un-mammifere 
	            a-des-sabots) 
	        alors (est-ongule)) 
	    (R8 si (est-un-mammifere 
	            est-un-ruminant) 
	        alors (est-ongule)) 
	    (R9 si (est-un-mammifere 
	            a-une-couleur-fauve 
	            est-un-carnivore) 
	        alors (est-un-guepard)) 
	    (R10 si (est-un-mammifere 
	             a-une-couleur-fauve 
	             est-un-carnivore 
	             a-des-rayures-noires) 
	        alors (est-un-tigre)) 
	    (R11 si (a-des-taches-noires 
	             a-de-longues-pattes 
	             est-ongule 
	             a-un-long-cou) 
	         alors (est-une-girafe)) 
	    (R12 si (a-des-rayures-noires 
	             est-ongule) 
	         alors (est-un-zebre)) 
	    (R13 si (a-un-long-cou 
	             est-un-oiseau 
	             ne-vole-pas 
	             est-noir-et-blanc) 
	         alors (est-une-autruche)) 
	    (R14 si (ne-vole-pas 
	             est-un-oiseau 
	             est-noir-et-blanc 
	             nage) 
	         alors (est-une-pingouin)) 
	    (R15 si (est-un-oiseau 
	             vole-bien) 
	         alors (est-un-albatros)) 
    ) 
 ) 

(SETQ regles_cantine '(
		(R1 si (aime-les-desserts)
			alors (est-gourmand))
		(R2 si (est-gourmand 
				il-reste-des-desserts)
			alors (prend-dessert))
		(R3 si (a-tres-faim
				il-y-a-une-place-assise
				a-le-temps)
			alors (prend-plat-du-jour))
		(R4 si (a-tres-faim
				il-y-a-une-place-assise)
			alors (mange-assis))
		(R5 si (a-soif)
			alors (prend-boisson))
		(R6 si (prend-boisson
				a-tres-faim
				a-le-temps)
			alors (prend-menu))
		(R7 si (a-soif
				n-a-pas-bcp-de-temps)
			alors (prend-sandwich prend-boisson))
		(R8 si (prend-menu
				menu-contient-des-frites)
			alors (recoit-ketchup recoit-mayo))
		(R9 si (prend-plat-du-jour)
			alors (recoit-sel recoit-poivre))
		(R10 si (dessert-est-yaourt)
			alors (recoit-cuillere))
		(R11 si (dessert-est-fruit
				prend-plat-du-jour
				boisson-est-eau)
			alors (mange-equilibre))
		(R12 si (a-une-pause-de-2h)
			alors (a-le-temps))
 	) 
 ) 


(SETQ regles_achat_voiture '(
		(R1 si ((equal compagnon VRAI))
			alors (= celibataire FAUX))
		(R2 si ((equal compagnon FAUX))
			alors (= celibataire VRAI))
		(R3 si ((equal enfants 0))
			alors (= famille sans_enfants))
		(R4 si ((equal enfants 1))
			alors (= famille enfant_unique))
		(R5 si ((> enfants 1))
			alors (= famille famille_nombreuse))
		(R6 si ((> revenus 100000))
			alors (= fortune riche))
		(R7 si ((< revenus 100000))
			alors (= fortune pauvre))
		(R8 si ((equal fortune riche))
			alors (= voiture COUPE))
		(R9 si ((equal celibataire VRAI) 
				(equal fortune riche))
			alors (= voiture COUPE))
		(R10 si ((equal celibataire FAUX) 
				(equal famille sans_enfants) 
				(equal fortune riche))
			alors (= voiture COUPE))
		(R11 si ((equal celibataire VRAI) 
				(equal famille sans_enfants) 
				(equal fortune riche))
			alors (= voiture COUPE))
		(R12 si ((equal celibataire VRAI) 
				(equal famille famille_nombreuse) 
				(equal fortune riche))
			alors (= voiture TOUT_TERRAIN))
		(R13 si ((equal celibataire VRAI) 
				(equal famille enfant_unique) 
				(equal fortune riche))
			alors (= voiture TOUT_TERRAIN))
		(R14 si ((equal celibataire FAUX) 
				(equal famille famille_nombreuse)
				(equal fortune pauvre))
			alors (= voiture FAMILIALE))
		(R15 si ((equal celibataire VRAI) 
				(equal famille enfant_unique) 
				(equal fortune pauvre))
			alors (= voiture CITADINE))
		(R16 si ((equal celibataire VRAI) 
				(equal famille sans_enfants) 
				(equal fortune pauvre))
			alors (= voiture CITADINE))
))