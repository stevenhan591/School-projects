package MVC.Modele;

import javafx.scene.image.Image;
import javafx.scene.paint.Color;

public class Fantome extends ObjetJeu{

    private int vitX = 120;
    private int gravite = 500;
    private double vitY = 0;
    private int score;

    /* Constructeur.
     *
     * @param x : position en x du fantome
     * @param y : position en y du fantome
     * @param image : image du fantome
     */
    public Fantome(double x, double y, Image image) {
        super(30, x, y, image);
        score = 0;
        setColor(Color.web("#000000"));
    }

    /* Fonction qui retournera un boolean disant si on a depasse un obstacle. L'idee est que l'extreme gauche du fantome
     * (position en X - rayon) doit depasser l'extreme droite de l'obstacle (position X + son rayon).
     *
     * @param obstacle : un obstacle quelconque du jeu
     */
    public boolean depasse(ObjetJeu obstacle) {
        if (x - 30 > obstacle.getX() + obstacle.getRayon()) {
            return true;
        } else return false;
    }

    /* Fonction qui borne notre fantome sur l'ecran. Si on touche les bords, il rebondira.
     * Gardez en tete que le fantome est un cercle de rayon 30. Donc les conditions de verification sera y +- rayon.
     *
     * @param nouveauY : nouveau position en Y a adapter dans l'ecran
     */
    public void resterDansEcran(double nouveauY) {
        // Si notre fantome sors du haut de l'ecran
        if (nouveauY - 30 < 0) {
            // 30, car c'est la hauteur maximale que le fantome peut etre
            nouveauY = 30;
            // On reinitialise la vitesse pour ne pas qu'il accumule une vitesse
            vitY = 0;
        }
        // Si notre frantome sors du bas de l'ecran.
        else if (nouveauY + 30 > 400) {
            // 370, car c'est la hauteur minimale que le fantome peut etre
            nouveauY = 370;
            vitY = -vitY;
        }
        y = nouveauY;
    }

    // ------------- GETTERS -----------
    public int getVitX() { return vitX; }

    public double getVitY() { return vitY; }

    public int getScore() { return score; }

    public int getGravite() { return gravite; }

    // --------------- SETTERS ---------
    public void setVitX(int vitX) { this.vitX = vitX; }

    public void setVitY(double vitY) { this.vitY = vitY; }

    public void setScore(int score) { this.score = score; }

    public void setGravite(int gravite) { this.gravite = gravite; }

}
