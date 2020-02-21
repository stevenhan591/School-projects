final public class Niveau {
/* IMPORTANT : TOUS LES CLASSES QUI MANIPULE DES TABLEAUX 2D SERONT SOUS FORME [y,x] ET NON [x,y]. Etant donne le coin
   en haut a gauche est (0,0), on s'etait dit qu'il serait plus facile de lire sous forme [y,x] que [x,y].
   On s'etait imaginer que la grille du niveau est de cette forme : [[40 elements],[40 elements],...,[40 elements]]
   Et la longueur de ce tableau sera 14.

   Structure du niveau :

   Chaque niveau aura une reference vers un tableau de tous les monstres du niveau ainsi que tous les tresors
   du niveau, mais pas de reference vers Zoe.
 */

    // Numero du niveau
    private int niveauActuel;
    // Carte du niveau contenant tous les symbols [y,x]
    private char[][] carte = new char[14][40];
    // Tableau des monstres, sert a avoir une reference vers les monstres
    private Monstre[] monstres = new Monstre[0];
    // Tableau de tresors, sert a avoir une reference vers les tresors
    private Item[] tresors = new Item[0];
    // Position de Zoe. On la met ici pour pas recalculer deux fois la meme chose.
    private int xZoe;
    private int yZoe;

    /* Constructeur du niveau.
     *
     * @param niveauActuel
     * @see #metMurDansLaCarte(boolean[][])
     * @see #metObjetDansLaCarte(String[])
     */
    public Niveau (int niveauActuel) {
        this.niveauActuel = niveauActuel;
        /* Variable qui contient une paire de donnee : la premiere est les murs et la
           deuxieme sont tous les autres objets du niveau
        */
        Paire<boolean[][], String[]> donneDuNiveau = LevelGenerator.generateLevel(this.niveauActuel);
        String[] valeurObjets = donneDuNiveau.getValue();
        boolean[][] mur = donneDuNiveau.getKey();
        metMurDansLaCarte(mur);
        metObjetDansLaCarte(valeurObjets);
    }

    /* Fonction qui met les symbols des murs dans la variable carte.
     * Un mur correspond a true et un false correspond a un vide.
     *
     * @param mur
     */
    public void metMurDansLaCarte(boolean[][] mur) {
        for (int j=0; j<14; j++) {
            for (int i=0; i<40; i++) {
                if (mur[j][i]) {
                    carte[j][i] = '#';
                } else carte[j][i] = ' ';
            }
        }
    }

    /* Fonction qui met les symbols dans la variable carte et cree, au bon moment, le tableau de monstres
     * et le tableau d'item du niveau. On affectera les variables monstres et tresors
     *
     * @param valeurObjets
     * @see (Item) #setNom(String)
     * @see (Monstre) #setNom(String)
     * @see #ajouterMonstre(Monstre)
     */
    public void metObjetDansLaCarte(String[] valeurObjets) {
        for (int i=0; i<valeurObjets.length; i++) {
            // Separe chaque element du tableau valeursObjets en tableau manipulable
            String[] objet = valeurObjets[i].split(":");
            // Nom de l'objet
            switch (objet[0]) {
                case "tresor" : {
                    // ParseInt pour transformer nos string en chiffre
                    int x = Integer.parseInt(objet[2]);
                    int y = Integer.parseInt(objet[3]);
                    Item itemTresor = new Item(objet[1], y, x);
                    itemTresor.setNom(objet[1]);
                    tresors = ajouterTresors(itemTresor);
                    carte[y][x] = '$';
                    break;
                }
                case "monstre" : {
                    int x = Integer.parseInt(objet[2]);
                    int y = Integer.parseInt(objet[3]);
                    carte[y][x] = '@';
                    Item objetMonstre = new Item(objet[1], y, x);
                    objetMonstre.setNom(objet[1]);
                    Monstre nouveau = new Monstre(niveauActuel,"Mechant monstre" + i, y, x, objetMonstre);
                    monstres = ajouterMonstre(nouveau);
                    break;
                }
                case "sortie" : {
                    int x = Integer.parseInt(objet[1]);
                    int y = Integer.parseInt(objet[2]);
                    carte[y][x] = 'E';
                    break;
                }
                case "zoe" : {
                    int x = Integer.parseInt(objet[1]);
                    int y = Integer.parseInt(objet[2]);
                    carte[y][x] = '&';
                    xZoe = x;
                    yZoe = y;
                    break;
                }
            }
        }
    }

    /* Ajoute un monstre dans notre tableau de monstre. Ressemble beaucoup au push de javascript.
     *
     * @param Monstre
     * @return resultat (nouveau tableau de Monstre)
     */
    public Monstre[] ajouterMonstre(Monstre monstre) {
        int longueurAncienTab = monstres.length;
        Monstre[] resultat = new Monstre[longueurAncienTab + 1];
        for (int i=0; i<resultat.length; i++) {
            // Met notre monstre a la fin du tableau
            if (i == longueurAncienTab) {
                resultat[i] = monstre;
            } else resultat[i] = monstres[i];
        }
        return resultat;
    }

    /* Ajoute un item dans notre tableau de tresor. Ressemble beaucoup au push de javascript.
     *
     * @param Item
     * @return resultat (nouveau tableau d'item)
     */
    public Item[] ajouterTresors(Item item) {
        int longueurAncienTab = tresors.length;
        Item[] resultat = new Item[longueurAncienTab + 1];
        for (int i=0; i<resultat.length; i++) {
            // Met notre tresor a la fin du tableau
            if (i == longueurAncienTab) {
                resultat[i] = item;
            } else resultat[i] = tresors[i];
        }
        return resultat;
    }

    /* Retire un monstre de la liste des monstres. Affecte le tableau de monstres actuel.
     * Utile quand un monstre meurt.
     *
     * @param indexMonstre
     */
    public void retirerMonstre(int indexMonstre) {
        int longueurAncienTab = monstres.length;
        Monstre [] resultat = new Monstre[longueurAncienTab-1];
        // Indice pour parcourir notre nouvelle liste.
        int k = 0;
        for(int i=0; i<longueurAncienTab; i++){
            if(i != indexMonstre) {
                resultat[k] = monstres[i];
                k++;
            }
        }
        monstres = resultat;
    }

    /* Retire un item de la liste des tresors a l'index i et affecte la variable tresors
     *
     * @param indexItem
     */
    public void retirerTresors(int indexItem) {
        int longueurAncienTab = tresors.length;
        Item[] resultat = new Item[longueurAncienTab - 1];
        // Sert d'indice pour le tableau resultat
        int k = 0;
        for (int i=0; i<longueurAncienTab; i++) {
            if (i != indexItem) {
                resultat[k] = tresors[i];
                k++;
            }
        }
        tresors = resultat;
    }


    /* Affiche la map du donjon avec tous les symbols. On les combines sur un String pour l'afficher une fois
       au lieu de 1 par 1 (question de performance)
     */
    public String afficherCarte() {
        String resultat = "";
        for (int j=0; j<14; j++) {
            for (int i=0; i<40; i++) {
                resultat += carte[j][i];
            }
            resultat += "\n";
        }
        return resultat;
    }

    // -------------------------------- Accesseur -----------------------
    public char[][] getCarte() {
        return carte;
    }

    public Monstre[] getMonstres() {
        return this.monstres;
    }

    public Item[] getTresors () {
        return this.tresors;
    }

    public int getXZoe() {
        return xZoe;
    }

    public int getYZoe() {
        return yZoe;
    }

    public int getNumNiveau() {
        return niveauActuel;
    }

    // ---------------------------------- Mutateur --------------------
    public void setSymbol(int y, int x, char symbol) {
        carte[y][x] = symbol;
    }

}
