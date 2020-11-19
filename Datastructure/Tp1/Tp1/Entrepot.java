public class Entrepot {

    private double longitude;
    private double latitude;
    private int nbBoite;

    public Entrepot(int nbBoite, double latitude, double longitude){
        this.longitude = longitude;
        this.nbBoite = nbBoite;
        this.latitude = latitude;
    }

    public double getLatitude() {
        return latitude;
    }

    public double getLongitude() {
        return longitude;
    }

    public int getNbBoite() {
        return nbBoite;
    }

    public void setNbBoite(int nbBoite) {
        this.nbBoite = nbBoite;
    }
}
