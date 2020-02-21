final public class Monstre extends Personnage {
    // Force du monstre
    private int force;
    // Item obtenu lorsque Zoe tue l'ennemi
    private Item drop;

    /* Constructeur de monstre.
     *
     * @param niveuActuel
     * @param nom
     * @param yMonstre
     * @param xMonstre
     * @param drop
     */
    public Monstre (int niveauActuel, String nom, int yMonstre, int xMonstre, Item drop) {
        force = (int) Math.max(0.4 * niveauActuel, 1);
        viesActuels = ((int) Math.max(0.6 * niveauActuel, 1));
        this.nom = nom;
        posY = yMonstre;
        posX = xMonstre;
        this.drop = drop;
    }

    /* Fonction qui execute un tour du monstre. Peut soit avancer, attaquer ou rien faire. Prend Zoe en parametre
     * pour savoir quel decision prendre.
     *
     * @param Zoe
     * @see #attaquer(Zoe)
     * @see deplacer(int, int)
     */
    public void actions(Zoe Zoe) {
        int yZoe = Zoe.posY;
        int xZoe = Zoe.posX;
        // Si le monstre est en vie
        if (viesActuels != 0) {
            // Si Zoe est dans le voisinage du monstre
            if(Math.abs(yZoe-posY)<=1 && Math.abs(xZoe-posX)<=1){
                attaquer(Zoe);
            } else {
                if (bonus) {
                    deplacementBonus(yZoe, xZoe);
                } else deplacer(yZoe, xZoe);
            }
        }
    }

    /* Fonction qui deplacera le monstre. C'est la version simpliste sans le bonus.
     *
     * @param yZoe
     * @param xZoe
     * @see (Niveau) #setSymbol(int, int, char)
     */
    public void deplacer(int yZoe, int xZoe) {
        int [] deplacementOptimale = deplacementOptimale(yZoe, xZoe);
        int nouvellePosY = deplacementOptimale[0];
        int nouvellePosX = deplacementOptimale[1];
        char symbolPos = niveau.getCarte()[nouvellePosY][nouvellePosX];
        // S'il y a un vide, le monstre peut se deplacer
        if(symbolPos == ' ' || symbolPos == '@' || symbolPos == 'x'){
            niveau.setSymbol(posY, posX, ' ');
            // Bouge le monstre et remplace l'espace qu'il occupait
            posY = nouvellePosY;
            posX = nouvellePosX;
            niveau.setSymbol(nouvellePosY, nouvellePosX,'@');
        }
    }

    /* Fonction qui attaque Zoe si elle est dans le voisinage. Affiche aussi le message.
     *
     * @param Zoe
     * @see (Zoe) #mort()
     */
    public void attaquer(Zoe Zoe) {
        System.out.println(nom + " attaque Zoe !");
        int viesActuelsZoe = Zoe.viesActuels;
        int vieZoeApresCoup = viesActuelsZoe - force;
        Zoe.viesActuels = vieZoeApresCoup;
        // Verifie si Zoe meurt.
        if (Zoe.viesActuels <= 0) {
            Zoe.mort();
        }
    }

    /* Fonction qui calcule le deplacement optimale des monstres en fonction de la position de Zoe
     * Retourne un tableau de int sous forme [y,x]
     *
     * @param yZoe
     * @param xZoe
     * @return resultat
     */
    public int[] deplacementOptimale(int yZoe, int xZoe) {
        int[] resultat = new int[2];
        int differenceX = posX - xZoe;
        int differenceY = posY - yZoe;

        // Si Zoe est en bas du monstre
        if (differenceY < 0) {
            resultat[0] = posY + 1;
        } else if (differenceY == 0) resultat[0] = posY;
        else resultat[0] = posY - 1;

        // Si Zoe est a droite du monstre
        if (differenceX < 0) {
            resultat[1] = posX + 1;
        } else if (differenceX == 0) resultat[1] = posX;
        else resultat[1] = posX - 1;
        return resultat;
    }

    /* Fonction qui retourne l'item que le monstre possede et s'enleve du tableau de monstres du niveau.
     *
     * @see (Niveau) #setSymbol(int, int, char)
     * @see (Niveau) #retirerMonstre(int)
     * @return recouvert
     */
    public Item mort(){
        Item recouvert = this.drop;
        Monstre[] monsNiveau = niveau.getMonstres();
        // Cherche quel monstre est mort et l'enleve du jeu
        for (int i=0; i<monsNiveau.length; i++) {
            // Marche seulement si tous les monstres ont des noms unique
            if (nom.equals(monsNiveau[i].nom)) {
                System.out.println(nom + " meurt !");
                niveau.setSymbol(posY, posX, 'x');
                niveau.retirerMonstre(i);
            }
        }
        return recouvert;
    }

    /* Mutateur du niveau. @param niveau
     *
     * @param niveau
     */
    public void setNiveau(Niveau niveau) {
        this.niveau = niveau;
    }


    //----------------------------------------------- SECTION BONUS ----------------------------------------

    // Variable qui dira si le jeu prend la version avec le bonus
    private boolean bonus;
    // Variable qui permettra au monstre de ne pas revenir sur ses pas.
    private boolean[][] visite = new boolean[14][40];
    // Fonction qui calcule la distance entre deux coordonnes donnees en faisant un pythagore.

    /* Fonction qui deplacera le monstre de facon plus intelligente. Affecte la variable visite
     * L'idee est explique dans BONUS.txt.
     *
     * @param yZoe
     * @param xZoe
     * @see #deplacer(int, int)
     * @see #calculDistance(int, int, int, int)
     * @see (Niveau) #setSymbol(int, int, char)
     */
    public void deplacementBonus(int yZoe, int xZoe) {
        int positionYAvant = posY;
        int positionXAvant = posX;
        deplacer(yZoe, xZoe);
        /* Si le monstre n'a pas pu se deplacer, car il y avait un obstacle (position reste inchangee).
           L'algorithme du bonus se manifeste.
        */
        if (positionYAvant == posY && positionXAvant == posX) {
            int meilleurFCost = 0;
            int meilleurPosY = posY;
            int meilleurPosX = posX;
            int voisinY = posY - 1;
            for (int j=0; j<3; j++) {
                int voisinX = posX - 1;
                for (int i=0; i<3; i++) {
                    if (estDedansGrille(voisinY, voisinX)) {
                        char symbolVoisinage = niveau.getCarte()[voisinY][voisinX];
                        boolean casePietinable = symbolVoisinage == ' '
                            || symbolVoisinage == '@'
                            || symbolVoisinage == 'x';
                        // Si le monstre peut piler sur la case et qu'il ne la jamais visite
                        if (casePietinable && !visite[voisinY][voisinX]) {
                            int gCost = calculDistance(voisinX, voisinY, posX, posY);
                            int hCost = calculDistance(xZoe, yZoe, posX, posY);
                            // Si on a trouve une meilleur case avec un plus petit fCost
                            if (meilleurFCost < gCost + hCost) {
                                meilleurFCost = gCost + hCost;
                                meilleurPosY = voisinY;
                                meilleurPosX = voisinX;
                            }
                        }
                        voisinX++;
                    }
                }
                voisinY++;
            }
            visite[meilleurPosY][meilleurPosX] = true;
            niveau.setSymbol(posY, posX, ' ');
            posX = meilleurPosX;
            posY = meilleurPosY;
            niveau.setSymbol(posY, posX, '@');
        }
    }

    /* Fonction qui calcule la distance en 2D entre deux coordonnees
     *
     * @param x1
     * @param y1
     * @param x2
     * @param y2
     * @return distance
     */
    public int calculDistance(int x1, int y1, int x2, int y2) {
        int deltaX = x2 - x1;
        int deltaY = y2 - y1;
        int deltaYCaree = deltaY * deltaY;
        int deltaXCaree = deltaX * deltaX;
        int distance = (int) Math.sqrt(deltaYCaree + deltaXCaree);
        return distance;
    }

    public void setBonus(boolean bonus) {
        this.bonus = bonus;
    }
}
