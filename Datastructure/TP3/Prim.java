import java.util.*;
import java.lang.*;

public class Prim {
    private int nbSommets;
    // Lorsqu'on parcours un sommet, on ajoutera son index dans cette liste. Utilise pour la priorite des noeuds
    private ArrayList<Integer> indexAChercher = new ArrayList<>();

    /* Trouve le sommet avec la valeur minimal, mais qui n'est pas dans le graphe
     *
     * @param distance : la liste des tous les arrets accessible
     * @param lienSommets : liste qui indique si le sommet a ete visite
     * @param matriceAdj : matrice adjacente du graphe
     */
    public int sommetMin(int[] distance, Boolean[] lienSommets, int[][] matriceAdjacente){
        int min = Integer.MAX_VALUE;
        int indexMin = -10;

        // Boucle qui cherche l'arret minimum avec les sommets qu'on a visite
        for (int i = 0; i < nbSommets; i++){
            if (!lienSommets[i] && distance[i] < min){
                min = distance[i];
                indexMin = i;
            }
        }
        /* Priorise l'ordre alphanumerique des sommets (file d'attente prioritaire).
         * L'idee est que pour chaque sommet visite, on regardera leurs lignes dans la matrice d'adjacente. S'il y
         * a une egalite et que le sommet n'a pas encore ete visite, on le prend. On trouve toujours celui qui est
         * le plus petit.
         */
        Collections.sort(indexAChercher);
        for (Integer index : indexAChercher) {
            for (int j = 0; j < matriceAdjacente[index].length; j++) {
                if (matriceAdjacente[index][j] == min && !lienSommets[j]) {
                    return j;
                }
            }
        }
        return indexMin;
    }

    /* Fonction qui retournera notre sortie de forme ARM.
     *
     * @param noeudsVisite : tableau qui contient les noeuds visites dans l'ordre
     * @param matAdj : matrice adjacente du graphe
     * @param noeudsDarrives : liste de tous les noeuds du graphe, non organisee
     */
    public ArrayList<String> sortieARM(int[] noeudsVisite, int[][] matAdj, ArrayList<String> nomNoeuds) {
        String[] noeudsDepart = transformerNomSommets(noeudsVisite, nomNoeuds);
        ArrayList<String> resultat = new ArrayList<>();
        for (int i = 1; i < nbSommets; i++) {
            int arret = matAdj[i][noeudsVisite[i]];
            resultat.add(noeudsDepart[i] + " " + nomNoeuds.get(i) + " " + arret);
        }
        return resultat;
    }

    /* Fonction qui transforme nos sommets de depart parcourus (qui sont en chiffres) en lettre. L'idee est qu'a
     * l'indice 1, ce sera le premier noeud. A l'indice 2, ce sera le deuxieme noeud, etc. Les noeuds sont classes
     * en ordre croissant d'alphabet.
     *
     * @param tabSommetInt : tableau qui contient les noeuds/sommets visites dans l'ordre
     * @param listeNoeuds : la liste de nom des noeuds
     */
    public String[] transformerNomSommets (int[] tabSommetInt, ArrayList<String> listeNoeuds) {
        String[] resultat = new String[tabSommetInt.length];
        for (int i = 1; i < tabSommetInt.length; i++) {
            resultat[i] = listeNoeuds.get(tabSommetInt[i]);
        }
        return resultat;
    }

    /* Contruction d'un parcours ARM. Il est a noter que tous les elements des declarations des "array" ([]) sont dans
     * l'ordre des noeuds. Par exemple, si nos noeuds sont de A a D, alors l'indice 0 des arrays sont pour le sommet A;
     * l'indice 1 des arrays sont pour le sommet B, etc.
     *
     * @param matriceAdjacente : matrice adjacente des sommets
     * @param listeDesNoeuds : la liste des noms des noeuds
     */
    public ArrayList<String> primAlgo(int matriceAdjacente[][], ArrayList<String> listeDesNoeuds){
        ArrayList<String> sortieARM;
        // Les noeuds visites par notre parcours ARM dans l'ordre
        int[] noeudsVisites = new int[nbSommets];
        // Tableau qui contiendra tous les arrets minimal de chaque sommets
        int[] distances = new int[nbSommets];
        // Indique si un noeud a ete visite
        Boolean[] aEteVisitee = new Boolean[nbSommets];

        // Initialise nos tableaux
        for (int i = 0; i < nbSommets; i++){
            distances[i] = Integer.MAX_VALUE;
            aEteVisitee[i] = false;
        }

        // On commence au premier sommet
        distances[0] = 0;
        noeudsVisites[0] = -1; // Racine de ARM

        for (int i = 0; i < nbSommets - 1; i++){
            int min = sommetMin(distances, aEteVisitee, matriceAdjacente);
            aEteVisitee[min] = true;
            indexAChercher.add(min);
            for (int j = 0; j < nbSommets; j++){
                // On sait deja quel sommet chercher, il faut trouver quel element est le bon
                if (matriceAdjacente[min][j] != 0 && !aEteVisitee[j] && matriceAdjacente[min][j] < distances[j]){
                    noeudsVisites[j] = min;
                    distances[j] = matriceAdjacente[min][j];
                }
            }
        }
        sortieARM = sortieARM(noeudsVisites, matriceAdjacente, listeDesNoeuds);
        return sortieARM;
    }


    public void setNbSommets(int nbSommets) {
        this.nbSommets = nbSommets;
    }

}
