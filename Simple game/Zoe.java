final public class Zoe extends Personnage {

    // Sert a savoir si on peut sortir du niveau, false par defaut. Utile pour changer de niveau
    // devient vrai si zoe est dans la sortie et a le morceau de l'hexaForce
    private boolean peutSortir = false;
    // Garde en memoire le symbol que zoe pietinne (@, X ou ' '). De base, elle pietine le vide.
    private char symbolPietiner = ' ';
    private int nbHexaforce;

    /* Fonction qui affiche les vies et le nombre d'hexaforce de Zoe
     *
     * @param pVActuel (point de vie actuel)
     */
    public String affichageZoe(int pVActuel) {
        String resultat = "Zoe : ";
        // Boucle qui met les vies
        for (int i=0; i<pVActuel; i++) {
            resultat += "♥ " ;
        }
        // Boucle qui met les "_" apres les vies manquantes
        for (int i=0; i<5-pVActuel; i++) {
            resultat += "_ ";
        }
        resultat += "|";
        // Boucle qui met les hexaforces
        for (int i=0; i<nbHexaforce; i++) {
            resultat += "Δ ";
        }
        // Boucle qui met les "_".
        for (int i=0; i<6-nbHexaforce; i++) {
            resultat += "_ ";
        }
        return resultat;
    }

    /* Fonction qui enleve un symbol dans les 9 cases avoisinantes de Zoe (incluant celle de Zoe)
     * et le remplace par un symbol qu'on veut
     *
     * @param symbol
     * @param remplacement
     * @see (Niveau) #setSymbol(int, int, char)
     */
    public void remplacerSymbolVoisinage(char symbol, char remplacement) {

        String[] objetsVoisin = voisinage(symbol);
        for (int i=0; i<objetsVoisin.length; i++) {
            if (objetsVoisin[i] != null) {
                String[] individu = objetsVoisin[i].split(":");
                int yIndividu = Integer.parseInt(individu[0]);
                int xIndividu = Integer.parseInt(individu[1]);
                niveau.setSymbol(yIndividu, xIndividu , remplacement);
            }
        }
    }

    /* Fonction qui consomme l'item en parametre. Affecte l'attribut associe
     *
     * @param item
     * @see (Item) #getNom()
     */
    public void consommerItem(Item item) {
        String nomItem = item.getNom();
        if (nomItem.equals("un coeur !")) {
            System.out.println("Vous trouver " + nomItem);
            if (viesActuels != 5) {
                viesActuels++;
            }
        } else if (nomItem.equals("une potion de vie !")) {
            System.out.println("Vous trouver " + nomItem);
            viesActuels = 5;
        } else {
            System.out.println("Vous trouver " + nomItem);
            nbHexaforce++;
        }
    }

    /* Fonction qui se fait appeler quand Zoe meurt.
     */
    public void mort() {
        System.out.println("Zoe meurt !");
        LegendOfZoe.defaite();
        System.exit(0);
    }

    public void setPeutSortir(boolean valeur) {
        peutSortir = valeur;
    }

    public boolean getPeutSortir() {
        return peutSortir;
    }

    /* Sert a avoir une reference vers le niveau actuel pour acceder aux attributs du niveau
       TRES IMPORTANT.
    */
    public void setNiveau(Niveau niveau) {
        this.niveau = niveau;
    }


    // ---------------- SECTION DES ACTIONS DE ZOE ------------------------------------

    /* Fonction qui contient tous les actions possible que Zoe peut faire(deplacement, creuser, attaquer et ouvrir
     * un tresor)
     *
     * @param entreeClavier
     * @see #deplacer(int, int)
     * @see #creuser()
     * @see #attaquer()
     * @see #ouvrirTresor()
     */
    public void actions(String entreeClavier) {
        String[] mouvement = entreeClavier.split("");
        for(int i=0; i<mouvement.length; i++) {
            switch (mouvement[i]) {
                // Notez que c'est setSymbol(y, x, symbol)
                case "w" :  deplacer(posY - 1, posX); break;
                case "a" :  deplacer(posY, posX - 1); break;
                case "s" :  deplacer(posY + 1, posX); break;
                case "d" :  deplacer(posY, posX + 1); break;
                case "c" : creuser(); break;
                case "x" : attaquer(); break;
                case "o" : ouvrirTresor(); break;
                case "q" : System.exit(0); break;
                default : break;
            }
        }
    }

    /* Fonction qui fait deplacer Zoe
     *
     * @param yDesiree
     * @param xDesiree
     * @inherited
     * @see (Niveau) #setSymbol(int, int, char)
     */
    public void deplacer(int yDesiree, int xDesiree) {

        // Si son deplacement reste dans la grille
        if (estDedansGrille(yDesiree, xDesiree)) {
            char symbol = niveau.getCarte()[yDesiree][xDesiree];
            // Si son deplacement est valide
            if (symbol == ' ' || symbol == '@' || symbol == 'x') {
                // On met l'ancien symbol avant le deplacement de Zoe
                niveau.setSymbol(posY, posX, symbolPietiner);
                symbolPietiner = symbol;
                posY = yDesiree;
                posX = xDesiree;
                // Nouvelle position de Zoe
                niveau.setSymbol(posY, posX, '&');
            }
        }
        /* Calcule si on est a coter de la sortie pour changer de niveau. Si on a l'hexaforce et qu'on est dans le
           voisinage de la sortie, on changera de niveau.
         */
        String[] presSortie = voisinage('E');
        // On prend [0], car il n'y a qu'un E par niveau et il sera affecter dans la premier case
        if (presSortie[0] != null && nbHexaforce == niveauActuel) {
            peutSortir = true;
        }
    }

    // Fonction qui permet de creuser seulement les murs
    public void creuser() {
        remplacerSymbolVoisinage('#', ' ');
    }

    /* Fonction qui attaque un monstre. L'idee est de chercher, case par case, tous les monstres avoisinantes de Zoe
     * et les attaquers. On trouvera le bon monstre par ses coordonnees.
     *
     * @see (Niveau) #getMonstre()
     * @see (Monstre) #mort()
     * @see #consommerItem(Item)
     * @see (Niveau) #setSymbol(int, int, char)
     */
    public void attaquer() {

        Monstre[] monstres = niveau.getMonstres();
        int voisinY = posY - 1;
        // Double boucle qui attaque tous les monstres avoisinantes de Zoe.
        for (int j=0; j<3; j++) {
            int voisinX = posX - 1;
            for (int i=0; i<3; i++) {
                // Cherche quel monstre on attaque
                for (int k=0; k<monstres.length; k++) {
                    int xMonstre = monstres[k].posX;
                    int yMonstre = monstres[k].posY;
                    // Condition pour trouver notre monstre
                    if (xMonstre == voisinX && yMonstre == voisinY) {
                        System.out.println("Zoe attaque " + monstres[k].nom);
                        monstres[k].viesActuels--;
                        // Si le monstre meurt, on lui met son symbol de mort et on le retire de la liste
                        if (monstres[k].viesActuels == 0) {
                            Item drop = monstres[k].mort();
                            consommerItem(drop);
                            // Si Zoe est sur la meme case qu'un monstre (elle a pietiner un monstre vivant)
                            if (symbolPietiner == '@') {
                                symbolPietiner = 'x';
                                niveau.setSymbol(monstres[k].posY, monstres[k].posX, 'x');
                            }
                        }
                    }
                }
                voisinX++;
            }
            voisinY++;
        }
    }

    /* Fonction qui trouve le bon tresor (par ses coordonnes) et l'ouvre. L'idee est de trouver le
     * tresors dans le voisinage de Zoe et comparer sa position avec le tableau de tous les tresors du
     * niveau. Cette etape est cruciale pour enlever un tresor du niveau.
     *
     * @see (Personnage) #voisinage(char)
     * @see (Niveau) #setSymbol
     * @see (Niveau) #getTresors()
     * @see #consommerItem(Item)
     * @see (Niveau) #retirerTresors(int)
     */
    public void ouvrirTresor() {
        // Tableau de String de nos coordonnes de tous les tresors du niveau
        String[] tresorFerme = voisinage('$');
        if (tresorFerme != null)
        // Boucle qui traverse tous les tresors voisins (sera souvent 1 fois)
        for (int i=0; i<tresorFerme.length; i++) {
            if (tresorFerme[i] != null) {
                String[] tresorI = tresorFerme[i].split(":");
                int yTresor = Integer.parseInt(tresorI[0]);
                int xTresor = Integer.parseInt(tresorI[1]);
                niveau.setSymbol(yTresor, xTresor, '_');
                // Tous les tresors du niveau
                Item[] tresorsNiveau = niveau.getTresors();
                // Boucle qui cherche nos tresors voisins
                for (int j=0; j<tresorsNiveau.length; j++) {
                    int xTresorNiveau = tresorsNiveau[j].getPosX();
                    int yTresorNiveau = tresorsNiveau[j].getPosY();
                    if (xTresor == xTresorNiveau && yTresor == yTresorNiveau) {
                        consommerItem(tresorsNiveau[j]);
                        niveau.retirerTresors(j);
                    }
                }
            }
        }
    }
    // -------------- FIN DE LA SECTION DES ACTIONS DE ZOE ------------------------------
}
