with Farm ;
with Gada.Text_IO ;

procedure Mission6 is
   
   package F renames Farm ;
   package Txt renames GAda.Text_IO ;
   
   --- Affiche les informations basiques du joueur
   procedure Affiche_Base (Jeu : F.T_Jeu) is
   begin
      Txt.Put_Line("Bienvenue dans Top-Farmer !") ;
      Txt.New_Line ;
      Txt.Put_Line("  Score  : " & Integer'Image(Jeu.Score)) ;
      Txt.Put_Line("  Argent : " & Integer'Image(Jeu.Argent) & " chbufs." ) ;
      Txt.New_Line ;
      
      if Jeu.Nombre_Enclos = 0 then
	 Txt.Put_Line("  Aucun enclos.") ;
      else
	 Txt.Put_Line("  La ferme est composée de " & Integer'Image(Jeu.Nombre_Enclos) & " enclos.") ;
      end if ;
   end Affiche_Base ;
      
   --- Affiche une grille représentant le plan.
   procedure Affiche_Plan (Plan : F.T_Plan) is
      -- Symbole que l'on affiche pour représenter un enclos.
      Symbole : Character ;
      Enclos : F.T_Enclos ;
   begin
      for Rangee in Plan'Range(1) loop
	 for Col in Plan'Range(2) loop
	    
	    -- Accède à l'enclos situé sur cette rangée et sur cette colonne.
	    Enclos := Plan(Rangee, Col) ;
	    
	    -- Cas particulier : l'enclos est vide.
	    if Enclos.Bebetes = 0 then Symbole := '.' ;
	    else	    
	       -- Sinon, le symbole cela dépend du type d'animaux.
	       case Plan(Rangee, Col).Animal is
		  when F.Canard => Symbole := 'C' ;
		  when F.Vache => Symbole := 'V' ;
		  when F.Poule => Symbole := 'P' ;
		  when F.Mouton => Symbole := 'M' ;
	       end case ;
	       
	    end if ;
	    
	    Txt.Put(Symbole) ;
	    
	 end loop ;
	 Txt.New_Line ;
      end loop ;
   end Affiche_Plan ;
   
   procedure Affiche (Jeu : F.T_Jeu) is
   begin
      Affiche_Base(Jeu) ;
      Affiche_Plan(Jeu.Plan) ;
   end Affiche ;
   
   --- Cout des enclos
   function Cout_Enclos (Aire : Float ; Animal : F.T_Animal) return Integer is
      Cout_Par_M2 : Float ;
   begin
      case Animal is
	 when F.Canard => Cout_Par_M2 := 2.0 ;
	 when F.Vache => Cout_Par_M2 := 6.0 ;
	 when F.Poule => Cout_Par_M2 := 1.5 ;
	 when F.Mouton => Cout_Par_M2 := 4.0 ;
      end case ;
      
      return Integer(Cout_Par_M2 * Aire) ;
   end Cout_Enclos ;
   
   --- Ajoute un enclos si le joueur a assez d'argent.
   procedure Ajoute_Enclos (Aire : Float ; Animal : F.T_Animal ; Jeu : in out F.T_Jeu ; Ligne : Integer ; Colonne : Integer) is
      Cout : Integer ;
   begin
      Cout := Cout_Enclos(Aire, Animal) ;
      
      if Jeu.Argent >= Cout then
	 Jeu.Argent := Jeu.Argent - Cout ;
	 Jeu.Nombre_Enclos := Jeu.Nombre_Enclos + 1 ;
	 Jeu.Plan(Ligne, Colonne) := ( Superficie => Aire, Animal => Animal, Bebetes => 8) ;
	   
	 Txt.Put_Line("Enclos ajouté. Coût = " & Integer'Image(Cout) & " chbufs") ;
      else
	 Txt.Put_Line("Pas assez d'argent. Coût = " & Integer'Image(Cout) & " chbufs") ;
      end if ;
   end Ajoute_Enclos ;
   
   --- Calcul du bonus : 100 points par animal.
   procedure Bonus (Jeu : in out F.T_Jeu) is
   begin
      for Rangee in Jeu.Plan'Range(1) loop
	 for Col in Jeu.Plan'Range(2) loop
	    Jeu.Score := Jeu.Score + 100 * Jeu.Plan(Rangee,Col).Bebetes ;
	 end loop ;
      end loop ;
   end Bonus ;

   
   --- Remplace les canards par des poules.
   procedure Plus_De_Carnard (Jeu : in out F.T_Jeu) is
      Enclos : F.T_Enclos ;
   begin
      -- Pour chaque enclos,
      for Ligne in Jeu.Plan'Range(1) loop
	 for Colonne in Jeu.Plan'Range(2) loop
	    
	    -- Quel enclos à ces coordonnées ?
	    Enclos := Jeu.Plan(Ligne, Colonne) ;
	    
	    -- Est-ce un enclos à canards ?
	    case Enclos.Animal is
	       when F.Canard => 
		  Enclos.Animal := F.Poule ;
		  Enclos.Bebetes := 1 ;
		  
		  -- On replace l'enclos modifié dans la grille.
		  Jeu.Plan(Ligne, Colonne) := Enclos ;
		  
	       when others => null ;
	    end case ;
	    
	 end loop ;
      end loop ;
   end Plus_De_Carnard ;
   
   --- Les extra-terrestres clonent les animaux
   procedure Reproduction (Jeu : in out F.T_Jeu) is
      Enclos : F.T_Enclos ;
      Pcent : Integer ;
   begin
       for Ligne in Jeu.Plan'Range(1) loop
	  for Colonne in Jeu.Plan'Range(2) loop
	     
	     -- Pour chaque enclos peuplé, ...
	     Enclos := Jeu.Plan(Ligne, Colonne) ;
	     if Enclos.Bebetes > 0 then 
		
		-- Faire croître le nombre de bebêtes.
		case Enclos.Animal is
		   when F.Poule => Pcent := 100 ;
		   when F.Canard => Pcent := 80 ;
		   when F.Vache => Pcent := 40 ;
		   when F.Mouton => Pcent := 60 ;
		end case ;
		
		Enclos.Bebetes := (Enclos.Bebetes * (100 + Pcent)) / 100 ;
		
		-- On remet bien l'enclos dans le plan.
		Jeu.Plan(Ligne, Colonne) := Enclos ;
	     end if ;
	 end loop ;
       end loop ;
   end Reproduction ;
   
      
   --- Fabrique une liste des enclos.
   function Lister(Jeu : F.T_Jeu) return F.T_Liste_Enclos is
      Resultat : F.T_Liste_Enclos(1..Jeu.Nombre_Enclos) ;
      
      -- Cette variable indique combien d'enclos ont déjà été mis dans la liste.
      Compte : Integer := 0 ;
   begin
      -- Parcours du plan
      for Ligne in Jeu.Plan'Range(1) loop
	 for Colonne in Jeu.Plan'Range(2) loop
	    
	    if Jeu.Plan(Ligne, Colonne).Bebetes > 0 then
	       -- Un enclos non vide
	       Compte := Compte + 1 ;
	       Resultat(Compte) := Jeu.Plan(Ligne, Colonne) ;
	    end if ;
	    
	 end loop ;
      end loop ;
      
      return Resultat ;
   end Lister ;
   
   --- Afficher les enclos de la liste
   procedure Afficher_Liste (Liste : F.T_Liste_Enclos) is
      Enclos : F.T_Enclos ;
   begin
      for Index in Liste'Range loop
	 Enclos := Liste(Index) ;
	 Txt.Put_Line( "Enclos " & Integer'Image(Index) & " : " & Integer'Image(Enclos.Bebetes) 
			 & " " & F.T_Animal'Image(Enclos.Animal) & "(s)") ;
      end loop ;
   end Afficher_Liste ;
   
   --- Enclos peuplé avec le moins d'animaux.
   function Min_Enclos (Liste : F.T_Liste_Enclos) return Integer is
      Enclos : F.T_Enclos ;
      Min_Animaux : Integer := Integer'Last ;
      Indice_Min : Integer ;
      
   begin
      for Index in Liste'Range loop
	 Enclos := Liste(Index) ;
	 if Enclos.Bebetes < Min_Animaux then
	    Min_Animaux := Enclos.Bebetes ;
	    Indice_Min := Index ;
	 end if ;
      end loop ;
      
      return Indice_Min ;
   end Min_Enclos ;
   
   --- Trouver une rangée vide
   function Rangee_Vide (Jeu : F.T_Jeu) return Integer is
      Trouve : Boolean := False ;
      No_Rangee : Integer := Jeu.Plan'First(1) ;
      Resultat : Integer ;
   begin
      -- Algorithme de recherche : trouver une rangée vide.
      while (not Trouve) and No_Rangee <= Jeu.Plan'Last(1) loop
	 
	 -- Si la première case (enclos) est vide, la rangée est vide.
	 if Jeu.Plan(No_Rangee, 1).Bebetes = 0 then
	    Resultat := No_Rangee ;
	    Trouve := True ;
	 else 
	    No_Rangee := No_Rangee + 1 ;
	 end if ;
      end loop ;
      
      if not Trouve then 
	 Resultat := -1 ;
      end if ;
      
      return Resultat ;
   end Rangee_Vide ;
   
   --- 
   ---  Calcul du bonus assorti (en plusieurs étapes)
   ---
   
   -- Détecte si une rangée est assortie.
   function Assortie (Plan : F.T_Plan ; Rangee : Integer) return Boolean is
      Resultat : Boolean ;
      Total : Integer := 0 ;
   begin
      -- On vérifie d'abord que la rangée n'a pas d'enclos vide.
      -- (Il suffit de vérifier que le dernier enclos n'est pas vide)
      Resultat := ( Plan(Rangee, Plan'Last(2)).Bebetes > 0 ) ;
      
      if Resultat then
	 -- Pour vérifier si tous les animaux sont présents, on ajoute des points par type d'animaux
	 -- 1 pour les poules, 10 pour les moutons, 100 pour les vaches, 1000 pour les canards.
	 -- Tous les animaux sont présents si le total est égal à 1111.
	 for Col in Plan'Range(2) loop
	    case Plan(Rangee, Col).Animal is
	       when F.Poule => Total := Total + 1 ;
	       when F.Mouton => Total := Total + 10 ;
	       when F.Vache => Total := Total + 100 ;
	       when F.Canard => Total := Total + 1000 ;
	    end case ;
	 end loop ;
	 
	 Resultat := (Total = 1111) ;
      end if ;
      
      return Resultat ;
   end Assortie ;
   
   -- Compte le nombre de rangées assorties
   function Nb_Assorties (Jeu : F.T_Jeu) return Integer is
      Compte : Integer := 0 ;
   begin
      for Rangee in Jeu.Plan'Range(1) loop
	 if Assortie(Jeu.Plan, Rangee) then
	    Compte := Compte + 1 ;
	 end if ;
      end loop ;
      
      return Compte ;
   end Nb_Assorties ;
   
   -- Ajoute le bonus des rangées assorties
   procedure Bonus_Assorti (Jeu : in out F.T_Jeu) is
      Bonus : Integer ;
   begin
      case Nb_Assorties(Jeu) is
	 when 0 => Bonus :=       0 ;
	 when 1 => Bonus :=   5_000 ;
	 when 2 => Bonus :=  20_000 ;
	 when 3 => Bonus :=  30_000 ;
	 when 4 => Bonus :=  50_000 ;
	 when 5 => Bonus :=  80_000 ;
	 when 6 => Bonus := 100_000 ;
	 when others => Bonus := 0 ; --- ce cas ne devrait pas arriver
      end case ;
      
      Jeu.Score := Jeu.Score + Bonus ;
   end Bonus_Assorti ;
   
   
   --- Trois variables pour tester
   Enclos_Vide : F.T_Enclos := ( Superficie => 100.0, 
				 Animal => F.Poule,
				 Bebetes => 0 ) ;
   
   Plan_Initial : F.T_Plan(1..6, 1..4) := (others => (others => Enclos_Vide)) ;
   
   Jeu : F.T_Jeu := ( Score => 0,
		      Argent => 8000,
		      Nombre_Enclos => 0,
		      Plan => Plan_Initial ) ;
   
begin
   
   -- Jeu initial
   Affiche_Base(Jeu) ;
   Affiche_Plan(Jeu.Plan) ;
   
   -- On ajoute quelques enclos
   Ajoute_Enclos(100.0, F.Poule, Jeu, 1, 1) ;
   Ajoute_Enclos(100.0, F.Poule, Jeu, 1, 2) ;
   Ajoute_Enclos(200.0, F.Canard, Jeu, 2, 1) ;
   Ajoute_Enclos(200.0, F.Vache, Jeu, 4, 1) ;
   Ajoute_Enclos(100.0, F.Mouton, Jeu, 4, 2) ;
   Ajoute_Enclos(100.0, F.Poule, Jeu, 4, 3) ;
   Ajoute_Enclos(100.0, F.Canard, Jeu, 6, 1) ;
   Ajoute_Enclos(100.0, F.Mouton, Jeu, 6, 2) ;
   Ajoute_Enclos(5000.0, F.Mouton, Jeu, 6, 4) ;
   
   Bonus(Jeu) ;
   Affiche(Jeu) ;
   
   -- Canard => Poule
   Plus_De_Carnard(Jeu) ;
   Affiche(Jeu) ;
   
   -- Multiplication des bebetes
   Reproduction(Jeu) ;
   Bonus(Jeu) ;
   Affiche(Jeu) ;
   
   Afficher_Liste( Lister(Jeu) ) ;
   
   -- L'enclos le moins peuplé
   Txt.Put_Line("Enclos le moins peuplé : " & Integer'Image(Min_Enclos( Lister(Jeu) ))) ;
   
   -- Rangee vide
   Txt.Put_Line("Une rangée vide : " & Integer'Image( Rangee_Vide(Jeu))) ;
   
   -- Test des rangées assorties
   Ajoute_Enclos(100.0, F.Canard, Jeu, 4,4) ;
   
   -- Une rangée assortie
   Bonus_Assorti(Jeu) ;
   Affiche(Jeu) ;
   
   -- Et une deuxième
   Ajoute_Enclos(100.0, F.Canard, Jeu, 6,3) ;
   Ajoute_Enclos(100.0, F.Vache, Jeu, 6,4) ;
   
   Bonus_Assorti(Jeu) ;
   Affiche(Jeu) ;
   
end Mission6 ;
