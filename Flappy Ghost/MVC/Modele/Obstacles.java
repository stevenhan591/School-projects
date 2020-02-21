package MVC.Modele;

import javafx.scene.image.Image;
import javafx.scene.paint.Color;

public class Obstacles extends ObjetJeu {

    // Position en y de l'obstacle lors de sa creation. Utile pour les obstacles sinus et quantique
    private double positionYDepart;
    // Boolean qui indique si un obstacle s'est fait depasser par un fantome. Peut se faire depasser une seule fois
    private boolean seFaitDepasser = false;
    // Indiquera quel obstacle il s'agira au lieu de faire des classes inutilements pour chaque obstacles
    private int numeroType;
    // Servira pour notre obstacle sinus
    private double angle;
    private Fantome fantome;

    // ---- Attributs pour le bonus ----
    // Servira a aller plus vite que l'arriere-plan en X
    private double deplacementX = 0;
    // Position en Y du fantome lors de la creation de l'obstacle lineaire. Doit attendre que fantome a ete cree.
    private double yDepartFantome;
    // Indique si on a deja calcule la position en Y du fantome.
    private boolean dejaCalculerYDepart;
    // Distance la plus courte entre l'objet et le fantome
    private double hypothenuse;

    /* Fonction constructeur de tous obstacles du jeu.
     *
     * @param rayon : rayon de l'obstacle
     * @param x : position en x de l'obstacle
     * @param y : position en y de l'obstacle
     * @param image : image de l'obstacle
     */
    public Obstacles(int rayon, double x, double y, Image image) {
        super(rayon, x, y, image);
        positionYDepart = y;
        setColor(Color.web("#FFFF00"));
    }

    /* Fonction qui fera bouger chaque obstacle selon son type
     *
     * @param tempsJeu : temps du jeu actuel en nanoseconde
     */
    public void bougeSelonType(double tempsJeu) {
        switch(numeroType) {
            // Obstacle sinus
            case 1 : {
                // Amplitude est proportionnelle a son rayon
                int amplitude = rayon;
                angle += Math.PI / 30;
                double ySinus = amplitude * Math.sin(angle);
                y = positionYDepart + ySinus;
                break;
            }
            // Obstacle quantique
            case 2 : {
                // Convertit le temps en un centieme de secondes(sweet spot sinon ca barre en couille)
                int Temps = (int) (tempsJeu * 1e-7);
                // S'execute seulement si chaque 0.2 secondes
                if(Temps % 20 == 0){
                    double depX = Math.random() * 61 - 30;
                    double depY = Math.random() * 61 - 30;
                    x += depX;
                    y = positionYDepart + depY;
                }
                break;
            }
            /* Obstacle lineaire (bonus). Le but est que lorsque que l'obstacle est cree, elle se dirige vers la
             * position du fantome (comme des projectiles). Chaque obstacle gardera son trajet puisque la condition
             * repose sur les positions en Y du fantome et de l'obstacle lors de la creation de l'obstacle.
             */
            case 3 : {
                // Si l'objet vient d'etre cree
                if (!dejaCalculerYDepart) {
                    yDepartFantome = fantome.getY();
                    hypothenuse = Math.sqrt(300 * 300 + yDepartFantome * yDepartFantome);
                    // Vitesse a laquelle on avancera
                    hypothenuse = hypothenuse / 100;
                    dejaCalculerYDepart = true;
                } else {
                    // Si le fantome est en haut de l'obstacle au moment de sa creation
                    if (yDepartFantome < positionYDepart) {
                        // Relation de pythagore
                        y -= Math.sqrt(hypothenuse * hypothenuse - (300 * 300)/10000.0);
                    }
                    // Si le fantome est en bas de l'obstacle ou a la meme hauteur au moment de sa creation
                    else {
                        y += Math.sqrt(hypothenuse * hypothenuse - (300 * 300)/10000.0);
                    }
                    // On lui donne une vitesse en X plus grand, pour cree l'effet de projectile
                    x -= deplacementX;
                }
            }
        }
    }

    /* Fonction qui dit si un obstacle touche le fantome. On le met ici, car il y a deja une boucle qui traverse
     * tous les obstacles du jeu dans le controleur. Plus simple de le mettre ici que dans la classe fantome.
     * Pour savoir si l'obstacle touche le fantome, il faut la distance entre le fantome et l'obstacle soit plus
     * petit que la somme de leurs rayons. Retournera un boolean pour nous indiquer s'il y a touchement.
     *
     * @param fantome : reference vers le fantome du jeu
     */
    public boolean touche(Fantome fantome) {
        double xFantome = fantome.getX();
        double yFantome = fantome.getY();
        double xObstacle = getX();
        double yObstacle = getY();
        double xdiff = xFantome - xObstacle;
        double ydiff = yFantome - yObstacle;
        double distanceCentre = Math.sqrt(xdiff * xdiff + ydiff * ydiff);
        double sommeRadius = fantome.getRayon() + rayon;
        if(distanceCentre < sommeRadius){
            return true;
        }
        return false;
    }

    // Fonction qui traitera le cas de collision, s'il y en a.
    public void collision() {
        boolean touche = touche(this.fantome);
        // S'il y a collision
        if(touche) {
            // Si on est en mode debug
            if(controleur.getModedebug()){
                setColor(Color.web("#FF0000"));
            } else {
                controleur.alerterRecommencement();
            }
        } else {
            setColor(Color.web("#FFFF00"));
        }
    }

    // ----------- SETTERS ---------------------
    public void setDeplacementX(double deplacementX) {
        this.deplacementX = deplacementX;
    }

    public void setSeFaitDepasser(boolean seFaitDepasser) {
        this.seFaitDepasser = seFaitDepasser;
    }

    public void setNumeroType(int numeroType) {
        this.numeroType = numeroType;
    }

    public void setFantome(Fantome fantome) {
        this.fantome = fantome;
    }

    // ------------- GETTER -------------------
    public boolean getSeFaitDepasser() {
        return seFaitDepasser;
    }
}
