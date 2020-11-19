import javax.naming.InsufficientResourcesException;
import java.io.*;
import java.util.ArrayList;
import java.util.Scanner;

/* Code fait par Steven Han, 20129462
 *               
 */
public class Fonction {

    /* Preambule : Dans ce code, on fait comme-ci les clients sont anglophones (erreur et affichage en anglais), mais le
     * code est en francais. Je tenais a vous le dire pour clarifiez les ambiguites de langues.
     */
    // Liste qui contiendra tous nos entrepots
    private static ArrayList<Entrepot> entrepots = new ArrayList<Entrepot>();
    // Liste qui contiendra les informations de sortie de type : distance: x    Number of boxes :x ...
    private static ArrayList<String> sortie = new ArrayList<>();
    private static Camion camion;
    // Longitude et latitude du camion
    private static double longitude;
    private static double latitude;
    // Le fichier qui contient tous les donnees
    private static String donnee;
    // Nombre de boite qui reste a charger
    private static int nbBoiteRestante;
    // Distance la plus courte entre le camion et le prochain entrepot que le monte-charge ira
    private static double distProchainEntrepot;

    public static void main(String args[]){
        long tempsDepart = System.nanoTime();
        try {
            lireFichier(args[0]);
            affecterNbBoiteEtCapaciteCamion();
            listerEntrepot();
            trouverMax();
            while (nbBoiteRestante > 0) {
                affecterEntrepot();
            }
        } catch (FileNotFoundException ex){
            System.out.println("Error, file could not be found");
        }
        // Ecrit dans le fichier du deuxieme parametre donnee
        try {
            FileWriter fw = new FileWriter(args[1]);
            BufferedWriter writer = new BufferedWriter(fw);
            for (String output : sortie) {
                writer.append(output);
                writer.newLine();
            }
            writer.close();
            long tempsFin = System.nanoTime();
            System.out.println("Le programme a pris : " + (tempsFin - tempsDepart) * 1e-9 + " secondes");
        } catch (IOException ex) {
            System.out.println("Error, cannot write to a file");
        }
    }

    /* Fonction qui lit le contenu d'un fichier et le sauvegarde dans un string
     *
     * @param fichier : fichier dans lequel on veut lire les donnees, doit etre de type String
     */
    public static void lireFichier(String fichier) throws FileNotFoundException {
        FileReader reader = new FileReader(fichier);
        Scanner in = new Scanner(reader);
        String text = "";

        while (in.hasNext()) {
            String line = in.nextLine();
            text += line + "\n";
        }
        in.close();
        donnee = text;
    }

    // Fonction qui trouve le nombre de boite a charger et la capacite du camion a partir du fichier en String
    public static void affecterNbBoiteEtCapaciteCamion() throws NumberFormatException {
        // Decoupe le fichier ligne par ligne
        String[] fichier = donnee.split("\n");
        // Seul la premiere ligne nous interesse
        String[] premiereLigne = fichier[0].split(" ");
        // Le premier chiffre est le nombre de boite a charger et le deuxieme est la capacite du camion
        nbBoiteRestante = Integer.parseInt(premiereLigne[0]);
        camion = new Camion(Integer.parseInt(premiereLigne[1]));
        // Si le camion n'est pas assez grand pour transporter toute la cargaison demandee
        if (nbBoiteRestante > camion.getNbBoite()) {
            throw new NumberFormatException("Error, insufficient truck capacity.");
        }
    }

    public static void listerEntrepot(){
        String[] liste = donnee.split("\n");
        String[][] liste1 = new String[liste.length][];

        // Boucle qui traverse chaque ligne du fichier
        for(int i = 1; i < liste.length; i++){
            liste1[i] = liste[i].split("\\) ");
            // Boucle qui traverse tous les champs d'une ligne
            for(int j = 0; j < liste1[i].length; j++) {
                String[] liste2 = liste1[i][j].split(" \\(|\\,");
                try {
                    Entrepot entrepot = new Entrepot(Integer.parseInt(liste2[0]),
                            Double.parseDouble(liste2[1]),
                            Double.parseDouble(liste2[2]));
                    entrepots.add(entrepot);
                } catch (NumberFormatException e) {
                    throw new NumberFormatException("Error, wrong format at line " + (i + 1) + ", do not forget the" +
                            " space at the end.");
                }
            }
        }
    }

    /* Fonction qui trouvera l'entrepot qui contient le plus grand nombre de boit et affectera la latitude et
     * la longitude du camion. On utilise l'algorithme de max le plus simple.
     */
    public static void trouverMax(){
        int maxEntrepot = 0;
        for (Entrepot entrepotActuelle : entrepots) {
            if (entrepotActuelle.getNbBoite() > maxEntrepot) {
                latitude = entrepotActuelle.getLatitude();
                longitude = entrepotActuelle.getLongitude();
                maxEntrepot = entrepotActuelle.getNbBoite();
            }
        }
        sortie.add("Truck position: " + "(" + latitude + "," + longitude + ")");
    }

    /* Fonction qui calculera la distance entre deux point par la formule d'haversine. Il faudra convertie nos valeurs
     * en radiant, @see convertirRadiant(double degree). Pour la formule complete, allez voir sur wikipedia.
     *
     * @param lat1 : latitude du premier point
     * @param long1 : longitude du premier point
     * @param lat2 : latitude du deuxieme point
     * @param long2 : longitude du deuxieme point
     * @return Math.abs(dist) : retourne la valeur absolue de la distance
     */
    public static Double distMin(double lat1, double long1, double lat2, double long2) {
        int rayonTerre = 6371000;
        lat1 = convertirRadiant(lat1);
        long1 = convertirRadiant(long1);
        lat2 = convertirRadiant(lat2);
        long2 = convertirRadiant(long2);
        double sinDiffLatCarree = Math.sin((lat2 - lat1)/2) * Math.sin((lat2 - lat1)/2);
        double sinDiffLongCarree = Math.sin((long2 - long1)/2) * Math.sin((long2 - long1)/2);
        double dist = 2 * rayonTerre *
                Math.asin((Math.sqrt(sinDiffLatCarree + Math.cos(lat1) * Math.cos(lat2) * sinDiffLongCarree)));
        // Valeur absolue, car le domaine de asin est -1 a 1
        return Math.abs(dist);
    }

    /* Fonction qui converti des degrees en radiant en utilisant la formule Radian = degree * PI / 180
     * @param degree : mesure en degree
     * @return degree * Math.PI/180 : Retourne notre valeur en radiant
     */
    public static double convertirRadiant (double degree) {
        return degree * Math.PI/180;
    }

    /* Fonction qui trouve l'entrepot qui est le plus proche du camion. Meme idee d'algorithme que le max, mais la
     * on cherche le min.
     *
     * @param return n : indice de l'entrepot le plus proche du camion
     */
    public static int minEntrepot() throws InsufficientResourcesException {
        /* Si nos entrepots ne contiennent pas assez de boite pour remplir la cargaison. On aura epuise tous les
         * entrepots et notre liste d'entrepots restant sera vide.
         */
        if (entrepots.size() == 0) {
            throw new InsufficientResourcesException();
        }
        Entrepot premierEntrepot = entrepots.get(0);
        double minDist = distMin(premierEntrepot.getLatitude(), premierEntrepot.getLongitude(), latitude, longitude);
        // Indice de l'entrepot
        int n = 0;
        for (Entrepot entrepotActuel : entrepots) {
            double latEntrepotActuel = entrepotActuel.getLatitude();
            double longEntrepotActuel = entrepotActuel.getLongitude();
            double distEntrepotActuelEtCamion = distMin(latEntrepotActuel, longEntrepotActuel, latitude, longitude);
            if (distEntrepotActuelEtCamion < minDist) {
                minDist = distEntrepotActuelEtCamion;
                n = entrepots.indexOf(entrepotActuel);
                distProchainEntrepot = minDist;
            }
            // Si on a deux distances pareil, on prendra la plus petite latitude
            else if (distEntrepotActuelEtCamion == minDist) {
                Entrepot ancienMinEntrepot = entrepots.get(n);
                // L'indice n contient l'indice de l'ancienne plus petite distance.
                if (latEntrepotActuel < ancienMinEntrepot.getLatitude()) {
                    n = entrepots.indexOf(entrepotActuel);
                    distProchainEntrepot = minDist;
                } else if (latEntrepotActuel == ancienMinEntrepot.getLatitude()) {
                    if (longEntrepotActuel < ancienMinEntrepot.getLongitude()) {
                        n = entrepots.indexOf(entrepotActuel);
                        distProchainEntrepot = minDist;
                    }
                }
            }
        }
        return n;
    }

    /* Fonction qui affectera le nombre de boite des entrepots et des boites restantes lorsque le monte-charge fait
     * son travail
     */
    public static void affecterEntrepot() {
        String distance;
        int indiceMinEntrepot;
        try {
            indiceMinEntrepot = minEntrepot();
            Entrepot entrepotActuel = entrepots.get(indiceMinEntrepot);
            // Si l'entrepot possede moins de boite qu'on a besoin, on les prendra toutes
            if (nbBoiteRestante > entrepotActuel.getNbBoite()) {
                nbBoiteRestante -= entrepotActuel.getNbBoite();
                entrepotActuel.setNbBoite(0);
            } else { // Si l'entrepot a plus de boite qu'on a besoin, on a fini
                entrepotActuel.setNbBoite(entrepotActuel.getNbBoite() - nbBoiteRestante);
                nbBoiteRestante = 0;
            }
            // Arrondi a un chiffre apres la virgule
            distProchainEntrepot = Math.round(distProchainEntrepot * 10) / 10.0;
            // On ne veut pas de 0.0
            if (distProchainEntrepot == 0) {
                distance = "Distance:" + 0 + "               ";
            } else {
                distance = "Distance:" + distProchainEntrepot + "         ";
            }
            sortie.add(distance +
                    "Number of boxes:" + entrepotActuel.getNbBoite() + "         " +
                    "Position:(" + entrepotActuel.getLatitude() + "," + entrepotActuel.getLongitude() + ")");
            // On enleve l'entrepot de notre liste pour faciliter les prochains entrepots a visiter
            entrepots.remove(indiceMinEntrepot);
        } catch (InsufficientResourcesException e) {
            System.out.println("Error, the total number of boxes is not enough to cover the requirements");
            System.exit(0);
        }
    }

    // ---------------------------------- FONCTION OPTIONNEL -----------------------------------------------------
    /* Fonction qui prend un fichier et met des espaces en fin de ligne pour que notre programme fonctionne. Peut
       etre utlise a la personne qui utilise ce code
       @param fichier : le fichier qu'on lira et on reecrira dessus
     */
    public static void mettreEspaceFin(String fichier) throws FileNotFoundException, IOException {
        FileReader reader = new FileReader(fichier);
        Scanner in = new Scanner(reader);
        String text = "";
        while (in.hasNext()) {
            String line = in.nextLine();
            text += line + " " + "\n";
        }
        in.close();
        FileWriter fw = new FileWriter(fichier);
        BufferedWriter writer = new BufferedWriter(fw);
        writer.append(text);
        writer.close();
    }
    // ------------------------------------------------------------------------------------------------------------

}
