import java.io.*;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

public class tp2 {

    // Liste qui contiendra les elements de la sortie finale
    private static ArrayList<String> sortie = new ArrayList<>();
    // Hashmap qui contiendra le nombre de chaque medicament a commander et leur nombre (occurrence)
    private static HashMap occurrenceCommandes = new HashMap();
    private static int compteurPrescription = 0;
    private static AVLtree arbreAVL = new AVLtree();
    private static String datePlusRecenteFichier;

    public static void main (String[] args) throws ParseException, FileNotFoundException {
        try {
            String text = lireFichier(args[0]);
            // Notre fichier sera separer par les fonctions. Ex [APPROV : ...., DATE ..., ...]
            String[] tabFonctions = text.split(";");
            agirSelonFonction(tabFonctions);
            ecrireFichier(args[1]);
        } catch (FileNotFoundException e) {
            throw new FileNotFoundException("Erreur, fichier inexistant.");
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
                text += ligne;
                // Ignore les lignes vide
                if (!ligne.equals("")) {
                    // Met un saut de ligne a chaque fois qu'on voit un ";"
                    if (!((ligne.substring(ligne.length() - 1)).equals(";"))) {
                        text += "\n";
                    }
                }
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
                writer.append(output);
                writer.newLine();
            }
            writer.close();
        } catch (IOException ex) {
            System.out.println("Erreur lors de l'ecriture du fichier");
        }

    }

    /* Fonction centrale qui lie chaque fonction du fichier (APPROV, STOCK, PRESCRIPTION, DATE)
     *
     * @param textSeparee : fichier d'entree separee par fonction. Il sera de forme [APPROV ..., PRESCRIPTION ..., ...]
     */
    public static void agirSelonFonction(String[] textSeparee) throws ParseException {
        for (int i = 0; i < textSeparee.length; i++) {
            /* On separe le nom de la fonction de ses arguments. Ex [APPROV :, Medicament1...,]
             * le \\s+ fait abstraction du nombre d'espace que l'utilisateur met. Ex : "         " = " ".
             */
            String[] tabFonction = textSeparee[i].split("\\s+");
            // 0 car ce sera le nom de la fonction
            switch (tabFonction[0]) {
                case "APPROV": {
                    try {
                        approv(textSeparee[i]);
                    } catch (ParseException e) {
                        throw new ParseException("Erreur de format pour " + textSeparee[i], 2);
                    }
                } break;
                case "DATE": {
                    datePlusRecenteFichier = tabFonction[1];
                    date(datePlusRecenteFichier);
                } break;
                case "PRESCRIPTION": prescription(textSeparee[i]); break;
                case "STOCK": stock(datePlusRecenteFichier); break;
                default : throw new NoSuchElementException("Erreur, mot inconnue : " + tabFonction[0]);
            }
        }
    }

    /* Fonction qui converti les dates en jours. Le point de depart est l'an 2000 et le maximum est l'an 2020
     *
     * @param date : date en string a convertir en type Date
     * @return Date : retourne un type Date
     */
    public static Date convertirStringDate (String date) throws NumberFormatException{
        String[] tabDate = date.split("-");
        int annee = Integer.parseInt(tabDate[0]);
        if (annee < 2000 || annee > 2020) {
            throw new NumberFormatException("Erreur, la date du medicament est en bas de l'an 2000.");
        } else {
            SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd");
            try {
                return format.parse(date);
            } catch (Exception e) {
                throw new NumberFormatException("Erreur, la date n'est pas dans le bon format (YYYY/MM/DD)");
            }
        }
    }

    /* Fonction qui se fait appeler lorsqu'on lit DATE. On genere la liste des medicaments a commander.
     *
     * @param date : date a laquelle la commande s'effectuera
     */
    public static void date(String date) {
        // Si notre liste de commande est vide
        if (occurrenceCommandes.size() == 0) {
            sortie.add(date + " OK" + "\n");
        }
        // Si on a des commandes a traiter
        else {
            ArrayList<String> commandesSortie = new ArrayList<>();
            // On parcours toutes les valeurs du hashmap et on les affectes dans la liste de sortie
            Iterator it = occurrenceCommandes.entrySet().iterator();
            while (it.hasNext()) {
                Map.Entry pair = (Map.Entry)it.next();
                commandesSortie.add(pair.getKey() + " " + pair.getValue());
            }
            // On le met dans l'ordre croissant des noms des medicaments
            Collections.sort(commandesSortie);
            sortie.add(date + " COMMANDES :");
            affecterSortie(commandesSortie);
            // On reinitialise les variables qui concerne la commande pour les nouvelles commandes du futur
            occurrenceCommandes = new HashMap();
        }
    }

    /* Fonction qui se fait appeler lorsqu'on lit le mot PRESCRIPTION. Il cherchera le noeud dans l'arbre et
     * dependamment de la reponse, on ajoutera un OK ou un COMMANDE.
     *
     * @param prescriptions : liste de prescriptions en string
     * @see AVLtree.rechercher(int identificateur, Node noeud, int quantiteMed, Date dateActuelle)
     */
    public static void prescription(String prescriptions) throws ParseException{
        compteurPrescription++;
        // On separe notre string pour qu'il ait la forme [PRESCRIPTION, MedicamentX _ _, MedicamentY _ _, ...]
        String[] tabPrescription = prescriptions.split("\n");
        ArrayList<String> sortiePrescription = new ArrayList<>();
        sortiePrescription.add("PRESCRITPION " + compteurPrescription);
        for (int i = 1; i < tabPrescription.length; i++) {
            /* Les champs ont beaucoup d'espace entre eux et \\s+ uniformise l'espace.
               Ex : [Medicament1, "", "", .., 5, ..., DATE] sera [Medicament1,5,DATE] Il enleve les espaces blancs
            */
            String[] med = tabPrescription[i].split("\\s+");
            try {
                int identificateur = parserNumeroMed(med[0]);
                int dose = Integer.parseInt(med[1]);
                int repetition = Integer.parseInt(med[2]);
                int quantite = dose * repetition;
                Date date = convertirStringDate(datePlusRecenteFichier);
                boolean estDansArbre = arbreAVL.rechercher(identificateur, arbreAVL.getRacine(), quantite, date);
                // Si le medicament n'est pas valide pour la prescription
                if (!estDansArbre) {
                    sortiePrescription.add("Medicament" + identificateur + " " + dose + " " + repetition + " COMMANDE");
                    // Si c'est un medicament qui est deja dans la commande, on doit les fusionner
                    if (occurrenceCommandes.containsKey("Medicament" + identificateur)) {
                        int ancienneValeur = (int) occurrenceCommandes.get("Medicament" + identificateur);
                        occurrenceCommandes.replace("Medicament" + identificateur, ancienneValeur + quantite);
                    }
                    // Si c'est un nouveau medicament qui n'est pas dans la liste des commandes
                    else {
                        occurrenceCommandes.put("Medicament" + identificateur, quantite);
                    }
                }
                // Si le medicament est valide pour la prescription
                else {
                    sortiePrescription.add("Medicament" + identificateur + " " + dose + " " + repetition + " OK");
                }
            } catch (ParseException e) {
                throw new ParseException("Erreur de format pour le " + med[0], 2);
            }
        }
        affecterSortie(sortiePrescription);
    }

    /* Fonction qui parsera le numero du medicament.
     *
     * @param nomMedicament : le nom du medicament a parser sous forme : MedicamentX
     * @return int : le numero du medicament
     */
    public static int parserNumeroMed (String nomMedicament) throws ParseException {
        try {
            // Le mot "medicament" finira toujours par un t
            String[] tabTemporaire = nomMedicament.split("t");
            return Integer.parseInt(tabTemporaire[1]);
        } catch (Exception e) {
            throw new ParseException("Erreur de format du nom de medicament pour le " + nomMedicament, 2);
        }
    }

    /* Fonction qui se fait appeler lorsqu'on lit le mot APPROV. Cette fonction ajoute dans l'arbre AVL les nouveaux
     * medicaments de la section APPROV.
     *
     * @param listeMed : liste de medicament sous forme "APPROV : \n MedicamentX _ _ \n ...
     */
    public static void approv(String listeMed) throws ParseException {
        // Separe chaque medicament et les met dans un tableau
        String[] tabMed = listeMed.split("\n");
        // On ignore le premier element, car c'est APPROV
        for (int i = 1; i < tabMed.length; i++) {
            // \\s+ uniformise les espaces, comme mentionne dans la fonction prescription
            String[] champsMedicament = tabMed[i].split("\\s+");
            try {
                int identificateur = parserNumeroMed(champsMedicament[0]);
                int quantite = Integer.parseInt(champsMedicament[1]);
                String date = champsMedicament[2];
                Medicament nouveauMedicament = new Medicament("Medicament" + identificateur, quantite, date);
                // Insert le nouveau medicament dans l'arbre
                arbreAVL.setRacine(arbreAVL.checkInsert(arbreAVL.getRacine(), identificateur, nouveauMedicament));
            } catch (ParseException e) {
                throw new ParseException("Erreur de format pour le " + champsMedicament[0], 2);
            }
        }
        sortie.add("APPROV OK");
    }

    /* Fonction qui se fait appeler lorqu'on lit le mot "STOCK". On prend la sortie de l'arbre en preOrdre et on
     * le trie.
     *
     * @param date : date  du fichier
     * @see arbreAVL.affecterSortieArbre(Arraylist)
     */
    public static void stock(String date) {
        Date dateActuelle = convertirStringDate(date);
        // On enleve les medicaments expires de l'arbre
        effacerMedExpire(arbreAVL.getRacine(), dateActuelle);
        // On ne veut pas l'ancien stock concatener avec le nouveau stock
        arbreAVL.setSortie(new ArrayList<>());
        sortie.add("STOCK " + date);
        // Une variable globale dans AVLtree aura notre sortie en preOrdre
        arbreAVL.affecterSortieArbre(arbreAVL.getRacine());
        ArrayList<String> sortieStock = arbreAVL.getSortie();
        Collections.sort(sortieStock);
        sortieStock = classerSelonDate(sortieStock);
        affecterSortie(sortieStock);
    }

    /* Fonction qui verifie si le est valide ou pas. On utilisera la recursion pour parcourir l'entierete de l'arbre.
     *
     * @param noeud : noeud dans lequel on verifiera ses medicaments
     * @date : date du fichier, sert pour determiner les medicaments expires
     */
    public static void effacerMedExpire(Node noeud, Date date) {
        if (noeud != null) {
            // Regarde chaque medicament de chaque noeud
            for (Iterator<Medicament> i = noeud.getListeMedicament().iterator(); i.hasNext();) {
                Medicament med = i.next();
                Date dateMed = convertirStringDate(med.getDate());
                int quantiteMed = med.getQuantite();
                /* Si la date de l'expiration du medicament est strictement avant la date du fichier (les medicaments
                 * qui ont la meme date que la date du fichier ne sont pas expirees) ou il ne reste plus de medicament
                 */
                if (dateMed.before(date) || quantiteMed <= 0) {
                    i.remove();
                }
            }
            effacerMedExpire(noeud.getGauche(), date);
            effacerMedExpire(noeud.getDroite(), date);
        }
    }

    /* Fonction qui ajoutera chaque element du parametre dans la variable globale sortie. Chaque element du parametre
     * correspond a une ligne.
     *
     * @param listeDeSortie : liste a integrer dans sortie
     */
    public static void affecterSortie(ArrayList<String> listeDeSortie) {
        for (int i = 0; i < listeDeSortie.size(); i++) {
            // Si on atteint la fin de notre liste, on met un saut de ligne pour separer les fonctions entre eu
            if (i == listeDeSortie.size() - 1) {
                sortie.add(listeDeSortie.get(i) + "\n");
            } else {
                sortie.add(listeDeSortie.get(i));
            }
        }
    }

    /* Fonction qui classe les medicaments qui sont plus qu'un par ordre croissant de date d'expiration.
     *
     * @param listeTree : listee qui est deja triee par ordre croissant de numero de medicament
     */
    public static ArrayList<String> classerSelonDate(ArrayList<String> listeTriee) {
        for (int i = 0; i < listeTriee.size(); i++) {
            // On ne regarde pas le dernier element
            if (i != listeTriee.size() - 1) {
                String[] med1 = listeTriee.get(i).split(" ");
                String[] med2 = listeTriee.get(i + 1).split(" ");
                // Si les nom de medicaments sont egaux
                if (med1[0].equals(med2[0])) {
                    Date dateMed1 = convertirStringDate(med1[2]);
                    Date dateMed2 = convertirStringDate(med2[2]);
                    // On echange les places
                    if (dateMed2.before(dateMed1)) {
                        Collections.swap(listeTriee, i, i+1);
                    }
                }
            }
        }
        return listeTriee;
    }
}
