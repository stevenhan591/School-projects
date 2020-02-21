/* Auteurs : Steven Han (20129462) et Vina Chakour (20134501)
   Date : 14 Decembre 2018
   Titre : Tp2
*/
'use strict';
/* Variable global du tableau contenant des 0 et 1 pour cocher et decocher
   Les cases qui auront une valeur de 0 seront considerees non cocher
   et les cases contenant une valeur de 1 seront considerees cocher
*/
var tab01 = '';

// Dom qui se charge au moment d'ouvrir la page
document.addEventListener('DOMContentLoaded', function() {

    var cal = document.getElementById("calendrier");
    var nbHeures = cal.dataset.nbheures;
    var nbJours = cal.dataset.nbjours;
    // Les + transforme les valeurs String en numerique
    nbHeures = +nbHeures;
    nbJours = +nbJours;
    // Creation d'un tablau 2D pour stocker les donnes
    tab01 = Array(nbHeures);
    for (var i = 0; i < nbHeures; i++) {
        tab01[i] = Array(nbJours);
        for (var j = 0; j < tab01[i].length; j++) {
            // Au depart, rien n'est cocher
            tab01[i][j] = 0;
        }
    }
});

// Fonction qui est appele lorsqu'on clique sur la grille des disponibilites
function onClick(event) {
    /* La variable t contient l'élément HTML sur lequel le clic a été
       fait. Notez qu'il ne s'agit pas forcément d'une case <td> du
       tableau
    */
    var t = event.target;
    // Prend le id ou la souris de l'utilisateur est
    id = onMove(event);

    // Attribut id de l'élément sur lequel le clic a été fait
    var id = t.id;

    // Coupe notre id String en tableau afin d'avoir des coordonnes (x,y);
    var coordonne = id.split("-");
    // Si la case n'a pas de crochet
    if (tab01[coordonne[0]][coordonne[1]] == 0) {
        // &#10004 est la valeur decimal d'un crochet en HTML
        document.getElementById(id).innerHTML = "&#10004;";
        // La case devient maintenant "cliquee"
        tab01[coordonne[0]][coordonne[1]] = 1;
    }
    // Case avec un crochet
    else {
        // Enleve le crochet qui est sur la case
        document.getElementById(id).innerHTML = " ";
        // Met la case disponible pour un futur clique
        tab01[coordonne[0]][coordonne[1]] = 0;
    }
}

/* Fonction qui prend le id en fonction de la position de la souris de
   l'utilisateur.
*/
function onMove(event) {
    var t = event.target;
    var id = t.id;
    return id;
}

/* Fonction qui retourne les disponibilites qui sont sous forme de tableau et
   renvoie un string.
*/
var compacterDisponibilites = function() {

    // Nous donnera le string sous forme "0,1,0,0,1,..."
    tab01 = tab01.join(",");

    // Nous donnera le tableau sous forme ["0","1","0","0","1",...]
    tab01 = tab01.split(",");

    // Nous donnera le string sous forme "01001"
    tab01 = tab01.join("");

    return tab01;
};
