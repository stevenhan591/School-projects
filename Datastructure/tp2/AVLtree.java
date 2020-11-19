import java.util.ArrayList;
import java.util.Date;

public class AVLtree {

    private Node racine;
    // Liste qui servira pour imprimer les medicaments
    private ArrayList<String> sortie;

    // Fonction qui calcule la hauteur du noeud
    public int hauteur(Node noeud) {
        if (noeud == null) {
            return 0;
        }
        return noeud.getHauteur();
    }

    /* Fonction qui fait une simple rotation gauche
     * @param noeud : noeud a faire tourner
     */
    public Node rotationGauche(Node noeud) {
        Node noeud1 = noeud.getDroite();
        Node noeud2 = noeud1.getGauche();

        // Performe la rotation
        noeud1.setGauche(noeud);
        noeud.setDroite(noeud2);

        noeud.setHauteur(Math.max(hauteur(noeud.getGauche()), hauteur(noeud.getDroite())) + 1);
        noeud1.setHauteur(Math.max(hauteur(noeud1.getGauche()), hauteur(noeud1.getDroite())) + 1);

        return noeud1;
    }

    /* Fonction qui fait une simple rotation droite
     * @param noeud : noeud a faire tourner
     */
    public Node rotationDroite(Node noeud) {
        Node noeud1 = noeud.getGauche();
        Node noeud2 = noeud1.getDroite();

        // Fait la rotation
        noeud1.setDroite(noeud);
        noeud.setGauche(noeud2);

        noeud.setHauteur(Math.max(hauteur(noeud.getGauche()), hauteur(noeud.getDroite())) + 1);
        noeud1.setHauteur(Math.max(hauteur(noeud1.getGauche()), hauteur(noeud1.getDroite())) + 1);

        return noeud1;
    }

    // Fonction qui sert a trouver la valeur de la balance du noeud
    public int balance(Node noeud) {
        if (noeud == null) {
            return 0;
        }
        return hauteur(noeud.getGauche()) - hauteur(noeud.getDroite());
    }

    /* Fonction qui insert un noeud avec le medicament au bon endroit dans l'arbre
     * @param noeud : noeud a inserer
     * @param identificateur : identificateur du noeud
     * @param medicament : medicament a ajouter dans le noeud
     * @return Node : retourne un noeud pour pouvoir faire un set.racine a l'arbre (affecter l'arbre)
     */
    public Node checkInsert(Node noeud, int identificateur, Medicament medicament) {
        // Si l'arbre ne contient pas le noeud
        if (noeud == null) {
            Node noeud1 = new Node(identificateur);
            noeud1.setIdentificateur(identificateur);
            noeud1.addMedicament(medicament);
            return noeud1;
        }

        // Notre noeud se trouve FORCEMENT a gauche du parametre noeud
        if (identificateur < noeud.getIdentificateur()) {
            noeud.setGauche(checkInsert(noeud.getGauche(), identificateur, medicament));
        }
        // Notre noeud se trouve FORCEMENT a droite du parametre noeud
        else if (identificateur > noeud.getIdentificateur()) {
            noeud.setDroite(checkInsert(noeud.getDroite(), identificateur, medicament));
        }
        // Si deux medicaments sont egaux
        else {
            noeud.addMedicament(medicament);
        }

        // Les etapes ci-dessous vont rebalancer l'arbre si necessaire. Il y a 4 cas possibles.
        noeud.setHauteur(Math.max(hauteur(noeud.getGauche()), hauteur(noeud.getDroite())) + 1);

        int balance = balance(noeud);

        // Rotation gauche gauche
        if (balance > 1 && identificateur < noeud.getGauche().getIdentificateur()) {
            return rotationDroite(noeud);
        }

        // Rotation droite droite
        if (balance < -1 && identificateur > noeud.getDroite().getIdentificateur()) {
            return rotationGauche(noeud);
        }

        // Rotation gauche-droite
        if (balance > 1 && identificateur > noeud.getGauche().getIdentificateur()) {
            noeud.setGauche(rotationGauche(noeud.getGauche()));
            return rotationDroite(noeud);
        }

        // Rotation droite-gauche
        if (balance < -1 && identificateur < noeud.getDroite().getIdentificateur()){
            noeud.setDroite(rotationDroite(noeud.getDroite()));
            return rotationGauche(noeud);
        }

        return noeud;
    }

    /* Fonction qui se fait appeler par tp2.prescription. Il cherchera le noeud dans notre arbre et retournera un
     * boolean indiquant s'il est valide pour la prescription ou pas. On utilisera la recursivite pour parcourir les
     * noeuds.
     * @param identificateur : identificateur pour trouver le noeud
     * @param noeud : noeud actuel ou on est, sert a chercher les informations de chaque noeud qu'on parcours
     * @param dateActuelle : date a laquelle les prescriptions ont ete demandees
     * @see tp2.prescription
     * @return boolean : indique si la prescription demandee est valide ou pas
     */
    public boolean rechercher(int identificateur, Node noeud, int quantiteMed, Date dateActuelle) {
        // S'il n'est pas dans l'arbre
        if (noeud == null) {
            return false;
        } else {
            // Si on a trouve le noeud correspondant la prescription
            if (identificateur == noeud.getIdentificateur()) {
                ArrayList<Medicament> listeMed = noeud.getListeMedicament();
                // On parcours tous les medicaments du noeud. Il sera souvent un element
                for (Medicament medicamentActuel : listeMed) {
                    Date dateMed = tp2.convertirStringDate(medicamentActuel.getDate());
                    long millisEn1Jour = 1000 * 60 * 60 * 24;
                    // Nouvelle date "d'expiration" si on prend le dosage.
                    long sommeJour = dateActuelle.getTime() + (quantiteMed * millisEn1Jour);
                    Date nouvelleDateExp = new Date(sommeJour);
                    boolean memeDate = !nouvelleDateExp.before(dateMed) && !nouvelleDateExp.after(dateMed);
                    // Si le medicament en a assez et que le medicament n'expire pas avant la totalite de l'usage
                    if (quantiteMed <= medicamentActuel.getQuantite() && (nouvelleDateExp.before(dateMed) || memeDate)) {
                        // On affecte le stocke de medicament
                        medicamentActuel.setQuantite(medicamentActuel.getQuantite() - quantiteMed);
                        return true;
                    }
                }
                return false;
            }
            // Le noeud est FORCEMENT a droite du parametre noeud
            else if (identificateur > noeud.getIdentificateur()) {
                return rechercher(identificateur, noeud.getDroite(), quantiteMed, dateActuelle);
            }
            // Le noeud est FORCEMENT a gauche du parametre noeud
            else {
                return rechercher(identificateur, noeud.getGauche(), quantiteMed, dateActuelle);
            }
        }
    }

    /* Fonction qui affecter la liste de sortie en parcourant chaque noeud et en la mettant dans la liste dans un
     * preOrdre. Il est important de garder une variable globale, car avec la recursion, la variable se reinitialiserais
     * a chaque noeud visitee
     *
     * @param node : noeud que l'on parcours
     */
    public void affecterSortieArbre(Node noeud) {
        if (noeud != null) {
            for (Medicament listeMedPareil : noeud.getListeMedicament()) {
                sortie.add(listeMedPareil.getNom() + " "
                        + listeMedPareil.getQuantite() + " "
                        + listeMedPareil.getDate());
            }
            affecterSortieArbre(noeud.getGauche());
            affecterSortieArbre(noeud.getDroite());
        }
    }

    // --------------------------------- ACCESSEURS ET MUTATEURS -----------------------------------
    public Node getRacine() {
        return racine;
    }

    public void setRacine(Node racine) {
        this.racine = racine;
    }

    public ArrayList<String> getSortie() {
        return sortie;
    }

    public void setSortie(ArrayList<String> sortie) {
        this.sortie = sortie;
    }
}

