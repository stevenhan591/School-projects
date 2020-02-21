package MVC;

import javafx.animation.AnimationTimer;
import javafx.application.Application;
import javafx.application.Platform;
import javafx.geometry.Orientation;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Separator;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.*;
import javafx.scene.paint.Color;
import javafx.scene.shape.Circle;
import javafx.scene.text.Text;
import javafx.stage.Stage;


public class Vue extends Application {

    // -------------- ATTRIBUTS D'AFFICHAGE ------------------------------
    // Le pane contiendra notre jeu : l'arriere-plan, le fantome et les opbstacles
    private Pane pane = new Pane();
    // BorderPane sera l'affichage du bas
    private BorderPane root = new BorderPane();
    private Image imageFantome;
    // Garde en memoire pour ne pas refaire des nouveaux
    private HBox basScene;
    // Ce qui suit sert a updater les affichages
    private Text textScore;
    private Button boutonPause;
    private CheckBox checkBoxDebug;
    private Stage primaryStage;

    // --------------- ATTRIBUTS DE MANIPULATION DU TEMPS DE L'ANIMATION TIMER -------------
    // Temps presque constant. Servira pour nos calcules de physique
    private double deltaTemps;
    // Temps qui marque le DEBUT de notre pause
    private long tempsDebutPause = 0;
    // Temps qui calcule combien de temps on est en pause
    private long tempsPause = 0;
    // Temps qui s'est ecroule depuis le TOUT DEBUT de notre jeu
    private long tempsJeu = 0;
    // Pour pouvoir l'arreter ou le recommencer
    private AnimationTimer timer;

    // ---------------- ATTRIBUTS AUTRES ------------------------------------------------------------------
    // Reference vers le controleur pour pouvoir l'avertir des evenements qui se produit
    private Controleur controleur;
    private boolean modeDebug = false;
    // Position en X de l'arriere-plan. Sera compris entre -640 et 0, car l'arriere plan va vers la gauche
    private double xArrierePlan = 0;
    // Indique si on est en pause ou pas
    private boolean pause = false;


    // ------------------ SECTION D'AFFICHAGE D'OBJETS -------------------------------

    /* Fonction qui affiche l'arriere-plan. L'idee est que chaque morceau de l'arriere-plan qui sors de l'ecran de
     * la gauche, la meme grandeur est affichee a droite de la fin de la premiere image. Cela cree l'effet de la
     * continuite. Notez aussi que bg.png a une auteur de 400px.
     */
    public void afficherArrierePlan() {
        pane.getChildren().clear();
        Image bg = new Image("/Images/bg.png");
        ImageView imageView = new ImageView(bg);
        // N'oubliez pas que xArrierePlan est entre -640 et 0.
        imageView.setX(xArrierePlan);
        pane.getChildren().add(imageView);
        Image bg2 = new Image("/Images/bg.png");
        ImageView imageView2 = new ImageView(bg2);
        imageView2.setX(640 + xArrierePlan);
        pane.getChildren().add(imageView2);
    }

    /* Fonction qui prend le pane et affiche par dessus ce qu'il a un objet du jeu (fantome ou obstacle).
     *
     * @param x : position en x de l'objet
     * @param y : position en y de l'objet
     * @param rayon : rayon de l'objet
     * @param image : image de l'objet
     * @param modeDebug : indique si l'utilisateur a coche le modeDebug
     * @param color : Couleur de l'objet, sera utilisee pour le modeDebug
     */
    public void afficherObjetJeu(double x, double y, double rayon, Image image, boolean modeDebug, Color color){
        // Si l'utilisateur coche le modeDebug
        if (modeDebug) {
            Circle cercle = new Circle();
            // Les valeurs de x et y ont le rayon ajoute car image view les considere comme un rectangle
            cercle.setCenterX(x);
            cercle.setCenterY(y);
            cercle.setRadius(rayon);
            cercle.setFill(color);
            pane.getChildren().add(cercle);
        } else {
            ImageView imageView = new ImageView(image);
            // La largeur et la hauteur correspondra aux diametres du rayon
            imageView.setFitWidth(2 * rayon);
            imageView.setFitHeight(2 * rayon);
            // On fait - rayon, car on veut que le X correspond au centre des objets
            imageView.setX(x - rayon);
            imageView.setY(y - rayon);
            pane.getChildren().add(imageView);
        }
    }

    /* Fonction qui affiche le bas de l'interface au complet. Elle est utilise une fois a chaque debut de
     * jeu. Ensuite, chaque partie (pause et score) s'actualiseront eux-memes.
     */
    public void afficherBasSceneDebut() {
        basScene = new HBox();
        // On doit instancier chaque separateur, sinon il y aura une erreur
        Separator separateurVertical1 = new Separator();
        Separator separateurVertical2 = new Separator();
        separateurVertical1.setOrientation(Orientation.VERTICAL);
        separateurVertical2.setOrientation(Orientation.VERTICAL);
        // Ajoute le bouton pause
        boutonPause = new Button("Pause");
        basScene.getChildren().add(boutonPause);
        basScene.getChildren().add(separateurVertical1);
        // Ajoute le mode debug
        checkBoxDebug = new CheckBox("Mode debug");
        basScene.getChildren().add(checkBoxDebug);
        basScene.getChildren().add(separateurVertical2);
        // Ajoute le score
        textScore = new Text("Score : 0");
        basScene.getChildren().add(textScore);
        basScene.setAlignment(Pos.CENTER);
        basScene.setPrefHeight(40);
        // Affiche un fond blanc pour ne pas qu'un objet s'affiche par dessus la barre de l'interface
        basScene.setStyle("-fx-background-color: #FFFFFF;");
        root.setBottom(basScene);
    }

    // ----------------------- SECTION DES ACTUALISATIONS DE L'AFFICHAGE DE PAUSE ET DU SCORE ------------------------
    /* Fonction qui actualisera l'affichage du score. Il enlevera l'ancien et ajoutera un nouveau.
     * @param score : score actuel du fantome
     */
    public void updateScore(int score) {
        basScene.getChildren().remove(textScore);
        textScore = new Text("Score : " + score);
        basScene.getChildren().add(textScore);
        root.setBottom(basScene);
    }

    /* Fonction qui actualisera l'affichage du button pause ou resume. Notez que l'indice 0 correspond au premier
     * element de basScene qui est le bouton pause.
     */
    public void updatePause() {
        pauseOuResume();
        basScene.getChildren().remove(0);
        basScene.getChildren().add(0, boutonPause);
        root.setBottom(basScene);
        evenementPause(boutonPause);
    }

    // Fonction qui affecte le bouton pause ou resume selon si on est en pause ou non
    public void pauseOuResume() {
        // Si on est pas en pause, le bouton sera pause et vice-versa avec resume
        if (!pause) {
            boutonPause = new Button("Pause");
        } else {
            boutonPause = new Button("Resume");
        }
    }

    // ---------------------- SECTION DES EVENEMENTS DE PAUSE ET MODE DEBUG -----------------
    /* Fonction qui traitera si l'utilisateur pese sur le bouton pause/resume
     *
     * @param boutonPause : le bouton pause actuel
     */
    public void evenementPause(Button boutonPause) {
        boutonPause.setOnAction((action) -> {
            // Si on clique sur "resume"
            if (pause) {
                timer.start();
            } else {
                timer.stop();
                // Garde en memoire le temps ou l'utilisateur a fait pause
                tempsDebutPause = tempsJeu;
            }
            // On inverse l'etat qu'on est, peut importe si on est en pause ou non, car on a cliquer sur le bouton
            pause = !pause;
            updatePause();
        });
    }

    /* Fonction qui traitera si l'utilisateur coche le checkBox Mode debug
     *
     * @param checkDebug : checkBox du Mode debug actuel
     */
    public void evenementDebug(CheckBox checkDebug) {
        // Si on click sur "debug"
        checkDebug.setOnAction((action) -> {
            modeDebug = !modeDebug;
            // On reaffiche le jeu, pour pouvoir changer d'affichage lorsqu'on est en pause
            afficherArrierePlan();
            controleur.afficherFantomeEtObstacles();
        });
    }

    public void evenementSautEtEscape(Scene scene) {
        // Après l’exécution de la fonction, le focus va automatiquement au pane
        Platform.runLater(() -> {
            root.requestFocus();
        });
        // Lorsqu’on clique ailleurs sur la scène, le focus retourne sur le pane
        scene.setOnMouseClicked((event) -> {
            root.requestFocus();
        });
        // Ajoute un ecouteur pour savoir les actions de l'utilisateur
        scene.addEventFilter(KeyEvent.KEY_PRESSED, k -> {
            if (k.getCode() == KeyCode.SPACE){
                // Averti le controleur que l'utilisateur a peser espace. Il ne gere pas l'evenement
                controleur.peserEspace();
            } else if (k.getCode() == KeyCode.ESCAPE) {
                // Averti le controleur que la touche escape a ete pese
                controleur.peserEscape();
            }
        });
    }

    // ------------------------ SECTION DU COMMENCEMENT DU JEU ----------------------------------
    @Override
    public void start(Stage primaryStage) throws Exception {
        this.primaryStage = primaryStage;
        // Stage du jeu
        primaryStage.setTitle("Flappy Ghost");
        primaryStage.setResizable(false);
        imageFantome = new Image("/Images/ghost.png");
        primaryStage.getIcons().add(imageFantome);

        controleur = new Controleur(this);
        controleur.creeNouveauFantome();
        root.setCenter(pane);
        Scene scene = new Scene(root, 640, 440);
        afficherJeu();
        afficherBasSceneDebut();
        primaryStage.setScene(scene);
        evenementSautEtEscape(scene);
        evenementDebug(checkBoxDebug);
        evenementPause(boutonPause);
        primaryStage.show();
    }

    /* Fonction qui affichera le jeu (fantome et obstacles) avec un timer. C'est lui qui met en vie le jeu et rend les
     * deplacement possible. N'oubliez pas que si l'utilisateur pese le bouton pause, le timer s'arrete aussi.
     */
    public void afficherJeu() {
        // On affecte la variable timer pour pouvoir le manipuler (l'arreter, le recommencer ou l'effacer)
        timer = new AnimationTimer() {
            // Temps qui servira a cree des obstacles
            private long tempCreeObstacle = 0;
            // Boolean qui servira a detecter si on est au debut du jeu
            private boolean debutJeu = true;

            @Override
            /* @param now : now est calcule par java et indique le temps qui s'est ecroule depuis le TOUT DEBUT
             * de l'animation. Si on pause, l'animation se pausera, mais quand on retournera a la normale, now aura
             * une valeur comme si la pause n'a jamais eu lieu. On s'ajutera en consequent si l'utilisateur clique pause
             */
            public void handle(long now){
                // Skip le premier appel pour éviter un "jump" du carré.
                if (debutJeu) {
                    tempCreeObstacle = now;
                    tempsJeu = now;
                    debutJeu = false;
                    return;
                }
                // Si l'utilisateur a clique sur pause a un moment du passe.
                if (tempsDebutPause != 0) {
                    // Calcule combien de temps l'utilisateur a ete en pause
                    tempsPause = now - tempsDebutPause;
                    tempsDebutPause = 0;
                }
                now -= tempsPause;
                /* La soustraction sert a avoir un temps constant d'une machine a l'autre. On calculera la
                 * difference du temps entre le moment present et la derniere fois que java a fait appel a now.
                 * Le temps est exprime en nanoseconde, donc on doit faire *1e-9 pour avoir en seconde.
                 */
                deltaTemps = (now - tempsJeu) * 1e-9;
                xArrierePlan = -controleur.updateX(deltaTemps);
                afficherArrierePlan();
                // Detecte si on a un intervalle de temps de 3 secondes, elle va seulement avertir le controleur
                if (Math.floor(now * 1e-9) - Math.floor(tempCreeObstacle * 1e-9) == 3) {
                    controleur.creeObstacle();
                    tempCreeObstacle = now;
                }
                controleur.afficherFantomeEtObstacles();
                controleur.update();
                tempsJeu = now;
            }
        };
        timer.start();
    }

    // ---------------- SECTION DU RECOMMENCEMENT DE L'AFFICHAGE -----------------------
    // Fonction qui recommence l'affichage lorsque le fantome touche un obstacle
    public void recommencerAffichage() {
        effacerTout();
        // Recommence un nouveau jeu. Recommencera aussi un nouveau animation timer
        try {
            start(primaryStage);
        } catch (Exception e) {
            Platform.exit();
        }
    }

    // Fonction qui efface tous l'ancien affichage ainsi que les variables
    public void effacerTout() {
        root = new BorderPane();
        pane = new Pane();
        // Arrete l'ancien animation timer a tout jamais
        timer.stop();
        tempsPause = 0;
    }

    // --------------- GETTERS ------------------
    public double getDeltaTemps() { return deltaTemps; }

    public Boolean getModeDebug() { return modeDebug; }

    public double getTempsJeu() { return tempsJeu;}
}
