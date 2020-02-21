import java.util.Scanner;

/**
 * Classe principale du programme.
 *
 * NOTEZ : VOUS NE DEVEZ PAS RENOMMER CETTE CLASSE
 */
public class LegendOfZoe {
/* @author : Steven Han (20129462)
 *
 * Ce programme est inspire de The Legend of Zelda et des jeux Roguelike. La structure generale du code est le voici :
 * On doit instancier Zoe une seule fois au debut du code pour garder ces attributs entre chaque niveau. A chaque
 * niveau on instancie un niveau (pour manipuler les donnes du niveau, l'affichage, etc.) et  les monstres (pour les
 * faire bouger et attaquer). De plus, on affecte la position [x,y] de Zoe (sa position change de niveau en niveau) a
 * chaque debut de niveau.
 */
    public static void main(String[] args) {
        boolean bonus = false;
        // Ces deux if representent le bonus.
        if (args.length != 0) {
            // Activation du bonus
            if (args[0].equals("--bonus")) {
                bonus = true;
            }
        }
        afficherIntro();
        Zoe Zoe = new Zoe();
        Zoe.viesActuels = 5;
        // Boucle qui traverse tous les niveaux
        for (int i=1; i<7; i++) {
            System.out.println("Vous entrez dans le niveau " + i);
            // Genere le niveau
            Niveau niveau = new Niveau(i);

            // Affectation des attributs de Zoe
            Zoe.posX = niveau.getXZoe();
            Zoe.posY = niveau.getYZoe();
            affecterNiveauZoe(Zoe, niveau);
            Zoe.niveauActuel = niveau.getNumNiveau();

            Monstre[] monstresDuNiveau = niveau.getMonstres();
            prejeuMonstre(monstresDuNiveau, niveau, bonus);
            // Tant que Zoe n'a pas l'hexaforce et est pres de la sortie.
            while (!Zoe.getPeutSortir()) {
                afficherJeu(Zoe, niveau);
                // Zoe fait son(ses) tour(s)
                int nbToursZoe = tourZoe(Zoe);
                tourMonstres(monstresDuNiveau, nbToursZoe, Zoe);
            }
            Zoe.setPeutSortir(false);
        }
        // Si la personne reussi toute la boucle sans que le jeu se ferme, c'est qu'elle a gagne
        victoire();
    }

    /* Fonction qui donne a Zoe une reference vers le niveau actuel
     *
     * @param Zoe
     * @param niveau
     */
    public static void affecterNiveauZoe(Zoe Zoe, Niveau niveau) {
        Zoe.setNiveau(niveau);
    }

    /* Fonction qui donne aux monstres une reference vers le niveau actuel et met s'il y a presence de bonus
     *
     * @param monstres
     * @param niveau
     * @param bonus
     */
    public static void prejeuMonstre(Monstre[] monstres, Niveau niveau, boolean bonus) {
        // Boucle qui met le bon niveau a tous les monstres
        for (int i=0; i<monstres.length; i++) {
            monstres[i].setNiveau(niveau);
            monstres[i].setBonus(bonus);
        }
    }

    /* Fonction qui affiche tous le visuel du jeu
     *
     * @param Zoe
     * @param niveau
     */
    public static void afficherJeu(Zoe Zoe, Niveau niveau){
        // Affiche les coeurs et les hexaforces
        String attributZoe = Zoe.affichageZoe(Zoe.viesActuels);
        System.out.println(attributZoe);
        // Affiche le niveau avec tous les symbols
        System.out.println(niveau.afficherCarte());
    }

    /* Fonction qui execute le(s) tour(s) de Zoe
     *
     * @param Zoe
     */
    public static int tourZoe(Zoe Zoe) {
        Scanner scanner = new Scanner(System.in);
        String actionZoe = scanner.nextLine();
        Zoe.actions(actionZoe);
        int nombreTours = actionZoe.length();
        return nombreTours;
    }
    /* Fonction qui execute l'action des monstres. Le nombre de tours de Zoe affecte le
     * nombre de tour de chaques monstres.
     *
     * @param monstres
     * @param nbToursZoe
     * @param Zoe
     */
    public static void tourMonstres(Monstre[] monstres, int nbToursZoe, Zoe Zoe){
        for (int i=0; i<nbToursZoe; i++) {
            for (int j=0; j<monstres.length; j++) {
                monstres[j].actions(Zoe);
            }
        }
    }

    public static void afficherIntro() { Messages.afficherIntro(); }

    public static void defaite() { Messages.afficherDefaite(); }

    public static void victoire() {  Messages.afficherVictoire(); }
}
