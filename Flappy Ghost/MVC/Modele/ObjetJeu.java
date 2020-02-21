package MVC.Modele;

import MVC.Controleur;
import javafx.scene.image.Image;
import javafx.scene.paint.Color;

abstract public class ObjetJeu {
    protected int rayon;
    protected double x;
    protected double y;
    protected Image image;
    // Reference vers le controleur
    protected Controleur controleur;
    private Color color;

    public ObjetJeu(int rayon, double x, double y, Image image) {
        this.rayon = rayon;
        this.x = x;
        this.y = y;
        this.image = image;
    }

    // On doit definir des Getters/Setters, car les attributs se font appeler par d'autre package
    // ---------- GETTERS --------
    public double getX() {
        return x;
    }

    public double getY() {
        return y;
    }

    public Image getImage() {
        return image;
    }

    public int getRayon() {
        return rayon;
    }

    public Color getColor() { return this.color; }

    // ----------- SETTERS ---------------
    public void setX(double x) {
        this.x = x;
    }

    public void setControleur(Controleur controleur) {
        this.controleur = controleur;
    }

    public void setColor(Color color) { this.color = color; }

}
