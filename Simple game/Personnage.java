abstract public class Personnage {
    /* Ces attributs sont protected, car on veut que nos enfants heritent de ces attributs
       sans pour avoir a mettre des getters et setters partout et de les redefinir dans
       les classes enfants.
     */
    protected int viesActuels;
    protected int posX;
    protected int posY;
    protected int niveauActuel;
    // Chaque personnage a besoin d'une reference vers le niveau pour changer les attributs du niveau
    protected Niveau niveau;
    protected String nom;


    /* Fonction qui trouve tous les objets voisins du personnage incluant sa position en regardant le symbol.
     * N.B : Le voisinage d'un personnage est les 8 cases voisines de sa position.
     *
     * @param symbol
     * @see (Niveau) #getCarte()
     * @return coordObjets
     */
    public String[] voisinage(char symbol) {
        /* Il peut y avoir jusqu'a 9 objet autour de Zoe, on les gardera en memoire
           similairement aux valeurs de la paire ([y1:x1,y2:x2,...])
        */
        String[] coordObjets = new String[9];
        // indice pour parcourir notre tableau de coordobjets
        int k = 0;
        // -1 car on commence une case en haut du personnage
        int voisinY = this.posY - 1;
        // Traverse les y. On commence en haut a gauche
        for (int i=0; i<3; i++) {
            int voisinX = this.posX - 1;
            // Traverse les x. On commence de une case a gauche du personnage
            for (int j=0; j<3; j++) {
                // Si le voisinage est dans la grille de jeu
                if (estDedansGrille(voisinY, voisinX)) {
                    if(niveau.getCarte()[voisinY][voisinX] == symbol) {
                        coordObjets[k] = voisinY + ":" + voisinX;
                        k++;
                    }
                }
                voisinX++;
            }
            voisinY++;
        }
        return coordObjets;
    }

    /* Fonction qui determine si la coordonne [y,x] est dans la grille.
     *
     * @param y
     * @param x
     * @return boolean
     */
    public boolean estDedansGrille(int y, int x) {
        if ((y >= 0 && y <= 13) && (x >= 0 && x <= 39)) {
            return true;
        }
        return false;
    }

    public abstract void deplacer(int y, int x);
}
