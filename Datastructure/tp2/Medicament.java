public class Medicament {

    private String nom;
    // La dose * la repetition
    private int quantite;
    private String date;

    public Medicament(String nom, int quantite, String date){
        this.nom = nom;
        this.date = date;
        this.quantite = quantite;
    }

    public int getQuantite() {
        return quantite;
    }

    public String getDate() {
        return date;
    }

    public String getNom() {
        return nom;
    }

    public void setDate(String date) {
        this.date = date;
    }

    public void setQuantite(int quantite) {
        this.quantite = quantite;
    }
}
