package MVC;

import MVC.Modele.*;
import javafx.application.Platform;
import javafx.scene.image.Image;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.concurrent.ThreadLocalRandom;

public class Controleur {

    private Vue vue;
    private Fantome fantome;
    // Tableau qui contiendra tous nos obstacles visible a l'utilisateur (on en effacera au fur et a mesure)
    private ArrayList<Obstacles> obstacles = new ArrayList<>();
    private int nbObstacleDepasse = 0;
    // Position en X des obstacles et de l'arriere-plan
    private double updateX = 0;

    public Controleur(Vue vue) { this.vue = vue; }

    // ---------------------- SECTION DE L'AFFICHAGE/ACTUALISATION DE LA VUE ---------------------------

    /* Fonction qui affiche le fantome et tous les obstacles
     *
     * @see afficherObjetJeu(double x, double y, double rayon, Image image, boolean modeDebug, Color color)
     * @see afficherTousLesObstacles(ArrayList<Obstacles> listeObstacles)
     */
    public void afficherFantomeEtObstacles() {
        vue.afficherObjetJeu(fantome.getX(), fantome.getY(), 30, fantome.getImage(), vue.getModeDebug(),
                fantome.getColor());
        afficherTousLesObstacles(obstacles);
    }

    /* Fonction qui affichera tous les obstacles.
     *
     * @param listeObstacles : tableau qui contient tous les obstacles du jeu
     */
    public void afficherTousLesObstacles(ArrayList<Obstacles> listeObstacles) {
        Iterator<Obstacles> it = listeObstacles.iterator();
        while(it.hasNext()) {
            Obstacles objetActuel = it.next();
            // Si l'objet sors de l'ecran du cote gauche, on l'enleve
            if (objetActuel.getX() <= 0) {
                it.remove();
            } else {
                vue.afficherObjetJeu(objetActuel.getX(), objetActuel.getY(),
                        objetActuel.getRayon(), objetActuel.getImage(), vue.getModeDebug(), objetActuel.getColor());
            }
        }
    }

    // ------------------- SECTION DE L'ACTUALISATION DU MODELE (TOUTE LA PHYSIQUE) -------------------------

    /* Fonction qui actualisera la position en X des objets autre que le fantome. Sera appelee par la vue. Comme la
     * position en X de l'arriere-plan et des obstacles dependent de la vitesse du fantome, on le met ici pour enlever
     * toute dependance entre la vue et le modele.
     *
     * @param deltaTemps : temps entre deux appel de la fonction handle dans animationTimer
     */
    public double updateX(double deltaTemps) {
        updateX += deltaTemps * fantome.getVitX();
        /* Il ne faut pas que x devient trop grand, sinon l'arriere plan sera impossible a gerer
         * x doit etre compris entre 0 et 640 pixels.
         */
        if (updateX > 640 || updateX < 0) {
            updateX = 0;
        }
        return updateX;
    }

    /* Fonction qui actualisera la position en y du fantome en tenant compte de son ancienne vitesse et la gravite.
     * On actualise seulement le Y, car le X reste fixe et tous les objets defilent vers la gauche, ce qui cree l'illu-
     * sion que notre fantome "avance" vers la droite.
     */
    public void updatePosYFantome() {
        double deplacementY = fantome.getY();
        deplacementY += fantome.getVitY() * vue.getDeltaTemps();
        // Controle le fantome pour ne pas qu'il sort de l'ecran
        fantome.resterDansEcran(deplacementY);
    }

    /* Fonction qui calcule la vitesse en y du fantome selon le temps donne et son ancienne vitesse
     *
     * @param deltaTime : temps entre deux appels de la fonction handle dans animation timer
     */
    public void updateVitYFantome(double deltaTime) {
        double vyFantome = fantome.getVitY();
        double nouvelleVit = fantome.getGravite() * deltaTime;
        // On additione les vitesse a cause de l'acceleration
        vyFantome += nouvelleVit;
        // Si le fantome exede sa vitesse maximale permise
        if (vyFantome > 300) {
            vyFantome = 300;
        }
        fantome.setVitY(vyFantome);
    }

    /* Met a jour la position en x de l'obstacle, chaque obstacle a un temps different compare a l'arriere-plan.
     *
     * @param obstacle : Un obstacle du jeu
     */
    public void updatePosXObstacle(Obstacles obstacle) {
        // Utile pour l'obstacle lineaire (bonus)
        obstacle.setDeplacementX(vue.getDeltaTemps() * fantome.getVitX());
        // nouveauX = ancienX - deplacementX (- car il va vers la gauche)
        obstacle.setX(obstacle.getX() - vue.getDeltaTemps() * fantome.getVitX());
    }

    // Fonction qui actualise chaque obstacle du jeu (la position en X, son mouvement, et s'il a depasser le fantome)
    public void updateChaqueObstacle() {
        Iterator<Obstacles> it = obstacles.iterator();
        // Va dans la liste de tous les obstacles du jeu
        while(it.hasNext()) {
            Obstacles objetActuel = it.next();
            // Si l'objet sors de l'ecran du cote gauche, on l'enleve
            if (objetActuel.getX() <= 0) {
                it.remove();
            } else {
                updatePosXObstacle(objetActuel);
                bougerSelonType(objetActuel);
                // Fait la collision avec le fantome, s'il y en a
                objetActuel.collision();
                // Si le fantome depasse un objet qui n'a jamais ete depasse
                if (fantome.depasse(objetActuel) && !objetActuel.getSeFaitDepasser()) {
                    fantome.setScore(fantome.getScore() + 5);
                    // Appel la vue pour actualiser le montant du score
                    vue.updateScore(fantome.getScore());
                    nbObstacleDepasse++;
                    objetActuel.setSeFaitDepasser(true);
                    if (nbObstacleDepasse == 2) {
                        fantome.setGravite(fantome.getGravite() + 15);
                        fantome.setVitX(fantome.getVitX() + 15);
                        nbObstacleDepasse = 0;
                    }
                }
            }
        }
    }

    // Fonction qui actualisera tous les objets du jeu (fantome et obstacles)
    public void update() {
        updateVitYFantome(vue.getDeltaTemps());
        updatePosYFantome();
        updateChaqueObstacle();
    }

    /* Fait bouger l'obstacle selon le type
     *
     * @param obstacle : un obstacle particulier du jeu
     */
    public void bougerSelonType(Obstacles obstacle) {
        obstacle.bougeSelonType(vue.getTempsJeu());
    }

    // ---------------------- SECTION DES OUTILS POUR LE FANTOME ET LES OBSTACLES ------------------------------

    /* Fonction qui cree un nouveau fantome. Sera appeler a chaque debut de jeu. 320 et 200, car c'est le millieu 
     * de la fenetre.
     */
    public void creeNouveauFantome() {
        Image imageFantome = new Image("/Images/ghost.png");
        fantome = new Fantome(320, 200, imageFantome);
    }

    /* Fonction qui ajoute a notre arrayList d'obstacles un obstacle.
     * @param obstacle : un obstacle du jeu
     */
    public void ajouterObstacles(Obstacles obstacle) {
        obstacles.add(obstacle);
    }

    /* Fonction qui retourne un chiffre au hasard de min a max inclusivement. Permet de choisir une image au hasard pour
     * les obstacles
     * @param min : valeur minimale du chiffre desiree
     * @param max : valeur maximale du chiffre desiree
     */
    public int chiffreHasard(int min, int max) {
        // +1 pour inclure les bornes
        int chiffreAuHasard = ThreadLocalRandom.current().nextInt(min, max + 1);
        return chiffreAuHasard;
    }

    // Fonction qui cree un obstacle et affecte la variable locale
    public void creeObstacle() {
        int randomRayon = chiffreHasard(10, 45);
        // Genere une position y au hasard. On le divise par un 1 double pour avoir une valeur double
        double randomY = chiffreHasard(0, 400)/ (double) 1;
        int numeroImage = chiffreHasard(0, 26);
        Image imageObstacle = new Image("/Images/" + numeroImage + ".png");
        // 640 + rayon pour qu'il sort de l'ecran
        Obstacles obstacle = new Obstacles(randomRayon, 640 + randomRayon, randomY, imageObstacle);
        obstacle.setFantome(fantome);
        obstacle.setControleur(this);
        /* Genere un obstacle au hasard.
           0 sera un obstacle simple
           1 sera un obstacle sinus
           2 sera un obstacle quantique
           3 sera un obstacle lineaire (bonus)
         */
        int numeroObstacle = chiffreHasard(0, 3);
        obstacle.setNumeroType(numeroObstacle);
        ajouterObstacles(obstacle);
    }

    // ------------- PETITS FONCTIONS UTILES ---------------------
    // Quand l'utilisateur pese sur la touche espace
    public void peserEspace() {
        fantome.setVitY(-300);
    }

    // Quand l'utilisateur pese sur la touche escape
    public void peserEscape() {
        Platform.exit();
    }

    // Fonction qui alerte la vue qu'il y a eu collision
    public void alerterRecommencement() {
        vue.recommencerAffichage();
    }

    // Getter
    public Boolean getModedebug() {
        return vue.getModeDebug();
    }
}
