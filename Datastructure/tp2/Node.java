import java.util.ArrayList;

class Node {
    private Node gauche, droite;
    private ArrayList<Medicament> listeMedicament = new ArrayList<>();
    private int identificateur, hauteur;

    public void addMedicament(Medicament med){
        listeMedicament.add(med);
    }

    public Node(int d) {
        identificateur = d;
        hauteur = 1;
    }

    public ArrayList<Medicament> getListeMedicament() {
        return listeMedicament;
    }

    public int getIdentificateur() {
        return identificateur;
    }

    public Node getDroite() {
        return droite;
    }

    public Node getGauche() {
        return gauche;
    }

    public int getHauteur() {
        return hauteur;
    }

    public void setHauteur(int hauteur) {
        this.hauteur = hauteur;
    }

    public void setDroite(Node droite) {
        this.droite = droite;
    }

    public void setGauche(Node gauche) {
        this.gauche = gauche;
    }

    public void setIdentificateur(int identificateur) {
        this.identificateur = identificateur;
    }

}
