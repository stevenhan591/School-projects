import java.io.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Scanner;

public class TP3 {
    private static ArrayList<String> nomNoeuds = new ArrayList<>();
    private static int[][] matriceAdjacente;
    private static HashMap rues = new HashMap();
    // Sera un graphe ou chaque element sera de forme [a e 9, b c 3, ...]
    private static ArrayList<String> graphe = new ArrayList<>();
    private static ArrayList<String> sortie = new ArrayList<>();
    private static int coutTotalArbre = 0;

    public static void main(String[] args) throws Exception{
        try {
            String text = lireFichier(args[0]);
            String[] textSeparee = text.split("---");
            affecterNomNoeud(textSeparee[0]);
            affecterRuesHashMap(textSeparee[1]);
            construireMatAdjacente();
            Prim algoPrim = new Prim();
            algoPrim.setNbSommets(nomNoeuds.size());
            ArrayList<String> sortieARM = algoPrim.primAlgo(matriceAdjacente, nomNoeuds);
            affecterSortieNomNoeud();
            affecterSortieRues(sortieARM);
            ecrireFichier(args[1]);
        } catch (FileNotFoundException e) {
            throw new FileNotFoundException("Erreur, fichier inexistant");
        }
    }
    /* Fonction qui lit un fichier et l'uniformise pour faciliter le coupure de ligne.
     *
     * @param nomFichier : fichier.txt a traiter
     * @return text : retourne le fichier sous forme de string avec chaque saut de ligne au bon endroit
     */
    public static String lireFichier(String nomFichier) throws FileNotFoundException {
        try {
            FileReader reader = new FileReader(nomFichier);
            Scanner in = new Scanner(reader);
            String text = "";

            while (in.hasNext()) {
                String ligne = in.nextLine();
                text += ligne + " ";
            }
            in.close();
            return text;
        } catch (FileNotFoundException e) {
            throw new FileNotFoundException("Erreur, fichier inexistant.");
        }
    }

    /* Fonction qui ecrira notre fichier.
     *
     * @param nomFichier : nom du fichier dans lequel il contiendra le contenu
     */
    public static void ecrireFichier(String nomFichier) {
        try {
            FileWriter fw = new FileWriter(nomFichier);
            BufferedWriter writer = new BufferedWriter(fw);
            for (String output : sortie) {
                writer.write(output);
                writer.newLine();
            }
            writer.close();
        } catch (IOException ex) {
            System.out.println("Erreur lors de l'ecriture du fichier");
        }

    }

    /* Fonction qui ajoute le nom de tous les sommets du graphe.
     *
     * @param nomDesNoeuds : le nom de tous les noeuds sous forme (a b c d ...). Attention, les espaces ne sont pas
     * uniforme
     */
    public static void affecterNomNoeud (String nomDesNoeuds) {
        // \\s+ pour uniformiser les espaces
        String[] tabNomNoeuds = nomDesNoeuds.split("\\s+");
        for (int i = 0; i < tabNomNoeuds.length; i++) {
            nomNoeuds.add(tabNomNoeuds[i]);
        }
        // Mettre le nom des noeuds en ordre croissant pour pouvoir se referer a leur indice.
        Collections.sort(nomNoeuds);
        // Notre matrice adjacente sera toujours caree (meme longueur et largeur)
        matriceAdjacente = new int[nomNoeuds.size()][nomNoeuds.size()];
    }

    /* Fonction qui ajoute le nom des rues avec leur informations dans un hashMap. Un exemple serait : rue0 = a b 3
     *
     * @param pleinDeRues : tous les informations sur les rues sous forme (rue3 : b c 4 ; rue10 : l o 38 ; ...)
     */
    public static void affecterRuesHashMap (String pleinDeRues) {
        // Separe chaque rue
        String[] tabRues = pleinDeRues.trim().split(";");
        for (int i = 0; i < tabRues.length; i++) {
            // Separe les informations de la rue particuliere
            String[] uneRue = tabRues[i].trim().split(":");
            String nomRue = uneRue[0].trim();
            String infoRue = uneRue[1].trim();
            // Attention, la cle et valeur peut etre contre intuitif, mais c'est plus simple cette maniere.
            rues.put(infoRue, nomRue);
            // Construit le graphe.
            graphe.add(infoRue);
        }
    }

    /* Fonction qui construira notre matrice adjacente en une matrice2D de tableau. Il se servira de la liste des
     * informations des rues (la variable graphe)
     *
     */
    public static void construireMatAdjacente () {
        for (String rueActuelle : graphe) {
            String[] infoNoeud = rueActuelle.split("\\s+");
            int indicePremierNoeud = nomNoeuds.indexOf(infoNoeud[0]);
            int indiceDeuxiemeNoeud = nomNoeuds.indexOf(infoNoeud[1]);
            int valeurArret = Integer.parseInt(infoNoeud[2]);
            // Il ne faut pas oublier qu'une matrice adjacente est symetrique
            matriceAdjacente[indicePremierNoeud][indiceDeuxiemeNoeud] = valeurArret;
            matriceAdjacente[indiceDeuxiemeNoeud][indicePremierNoeud] = valeurArret;
        }
    }

    // Petite fonction utilitaire pour affecter le nom des sommets a la sortie
    public static void affecterSortieNomNoeud() {
        for (String nomNoeud : nomNoeuds) {
            sortie.add(nomNoeud);
        }
    }

    /* Fonction qui ajoutera la sortie des rues dans la variable sortie.
     *
     * @param listeRues : la liste des rues sous forme (a b 4, c d 5, ...)
     */
    public static void affecterSortieRues(ArrayList<String> listeRues) {
        // Important que la liste soit triee pour la sortie
        Collections.sort(listeRues);
        for (String infoRue : listeRues) {
            String nomRue = (String) rues.get(infoRue);
            // Les espaces ont ete formattes et donc on peut les separers sous 1 espace
            String[] infoRueSeparee = infoRue.split(" ");
            int poidArret = Integer.parseInt(infoRueSeparee[2]);
            coutTotalArbre += poidArret;
            // Si on a pas de nom de rue, c'est que les sommets sont inverses
            if (nomRue == null) {
                String[] rueSeparee = infoRue.split(" ");
                String inverserNoeud = rueSeparee[1] + " " + rueSeparee[0] + " " + rueSeparee[2];
                nomRue = (String) rues.get(inverserNoeud);
            }
            infoRue = formatterRues(infoRue);
            sortie.add(nomRue + "\t" + infoRue);
        }
        sortie.add("---");
        sortie.add(coutTotalArbre + "");
    }

    /* Fonction utilitaire qui formatte l'affichage des rues.
     *
     * @param ligneAChanger : une ligne de rue sous forme (rue0 a b 4)
     */
    public static String formatterRues(String ligneAChanger) {
        // Les espaces ont ete formattes et donc on peut les separers sous 1 espace
        String[] tabSeparee = ligneAChanger.split(" ");
        String resultat = tabSeparee[0] + "\t" + tabSeparee[1] + "\t" + tabSeparee[2];
        return resultat;
    }
}
