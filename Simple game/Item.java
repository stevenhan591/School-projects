public class Item {

    /* Type de l'item. Il y en a 3. Un Item aura comme attribut un string
       de son propre nom. 3 choix possibles :
       1- un coeur
       2- une potion de vie
       3- un morceau d'Hexaforce
    */
    private String nomItem;
    // Coordonne de l'item
    private int posX;
    private int posY;

    /* Fonction constructeur des items du jeu
     *
     * @param nomItem
     * @param posY
     * @param posX
     */
    public Item(String nomItem, int posY, int posX) {
        this.nomItem = nomItem;
        this.posX = posX;
        this.posY = posY;
    }

    /* Redefinition des noms des items pour faciliter l'affichage. Affecte l'attribut nom
     *
     * @param ancienNom
     */
    public void setNom(String ancienNom) {
        if (ancienNom.equals("coeur")) {
            nomItem = "un coeur !";
        } else if (ancienNom.equals("potionvie")) {
            nomItem = "une potion de vie !";
        } else {
            nomItem = "un morceau d'Hexaforce !";
        }
    }

    // @return nomItem
    public String getNom() {
        return nomItem;
    }

    // @return posX
    public int getPosX() {
        return posX;
    }

    // @return posY
    public int getPosY() {
        return posY;
    }

}
