/* Auteurs : Steven Han (20129462)
   Date : 14 Decembre 2018
   Titre : Tp2
*/
'use strict';

var http = require("http");
var fs = require('fs');
var urlParse = require('url').parse;
var pathParse = require('path').parse;
var querystring = require('querystring');

var port = 1337;
var hostUrl = 'http://localhost:' + port + '/';
var defaultPage = '/index.html';

var mimes = {
    '.html': 'text/html',
    '.css': 'text/css',
    '.js': 'text/javascript',
};

// --- Helpers ---
var readFile = function(path) {
    return fs.readFileSync(path).toString('utf8');
};

var writeFile = function(path, texte) {
    fs.writeFileSync(path, texte);
};

// --- Server handler ---
var redirect = function(reponse, path, query) {
    var newLocation = path + (query == null ? '' : '?' + query);
    reponse.writeHeader(302, {
        'Location': newLocation
    });
    reponse.end('302 page déplacé');
};

var getDocument = function(url) {
    var pathname = url.pathname;
    var parsedPath = pathParse(url.pathname);
    var result = {
        data: null,
        status: 200,
        type: null
    };

    if (parsedPath.ext in mimes) {
        result.type = mimes[parsedPath.ext];
    } else {
        result.type = 'text/plain';
    }
    try {
        result.data = readFile('./public' + pathname);
        console.log('[' + new Date().toLocaleString('iso') + "] GET " + url.path);
    } catch (e) {
        // File not found.
        console.log('[' + new Date().toLocaleString('iso') + "] GET " +
            url.path + ' not found');
        result.data = readFile('template/error404.html');
        result.type = 'text/html';
        result.status = 404;
    }

    return result;
};
var sendPage = function(reponse, page) {
    reponse.writeHeader(page.status, {
        'Content-Type': page.type
    });
    reponse.end(page.data);
};

var indexQuery = function(query) {

    var resultat = {
        exists: false,
        id: null
    };

    if (query !== null) {

        query = querystring.parse(query);
        if ('id' in query && 'titre' in query &&
            query.id.length > 0 && query.titre.length > 0) {

            resultat.exists = creerSondage(
                query.titre, query.id,
                query.dateDebut, query.dateFin,
                query.heureDebut, query.heureFin);
        }

        if (resultat.exists) {
            resultat.id = query.id;
        }
    }

    return resultat;
};

var calQuery = function(id, query) {
    if (query !== null) {
        query = querystring.parse(query);
        // query = { nom: ..., disponibilites: ... }
        ajouterParticipant(id, query.nom, query.disponibilites);
        return true;
    }
    return false;
};

var getIndex = function(replacements) {
    return {
        status: 200,
        data: readFile('template/index.html'),
        type: 'text/html'
    };
};


// --- À compléter ---
var mois = [
    'Jan', 'Fév', 'Mar', 'Avr', 'Mai', 'Juin',
    'Juil', 'Août', 'Sep', 'Oct', 'Nov', 'Dec'
];

var MILLIS_PAR_JOUR = (24 * 60 * 60 * 1000);


/* --------------------- IMPORTANT ------------------------------------

Les fonctions en dessous de getCalendar, getResults, genColor,
ajouterParticipant et creeSondage sont des fonctions propres pour eux.
Il y a des fonctions communes et des variables communes qui sont juste
en dessous de cette section. Chaque commentaire est en haut de la va
riable qui lui est associé.

*/
// ----- FONCTIONS UTILISEES DANS getCalendar ET getResult-----

/* Fonction qui calcule le nombre de jour total du mois et tient compte
   des annees bissextile.
*/
var joursTotalMois = function (moisDebut) {
    var mois31 = [1, 3, 5, 7, 8, 10, 12];
    // String vide pour avoir la meme longueur de tableau
    var mois30 = [4, 6, 9, 11, '','',''];
    var fev = 28;
    var anneeBissex = tabSondage.debutAnnee % 4;
    if (anneeBissex == 0) {
        fev = 29;
    }
    var nbJoursMois = 0;
    for (var i = 0; i < mois.length; i++) {
        if (moisDebut == mois31[i]) {
            nbJoursMois = 31;
            return nbJoursMois;
        }
        if (moisDebut == mois30[i]) {
            nbJoursMois = 30;
            return nbJoursMois;
        }
        if (moisDebut == 2) {
            nbJoursMois = fev;
            return nbJoursMois;
        }
    }
};
// Fonction qui cree une matrice 2D
var creeMatrice2D = function(hauteur, largeur) {
    var matrice2D = Array(hauteur);
    for (var i = 0; i < matrice2D.length; i++) {
        matrice2D[i] = Array(largeur);
    }
    return matrice2D;
};

// Fonction qui construit un tableau contenant les dates et les heures.
var tabValeur = function(heureDebut, heureFin, jourDebut, jourFin, moisAuDebut,
                         moisALaFin, hauteurTable, largeurTable) {
    var grille = creeMatrice2D(hauteurTable, largeurTable);
    /* Mois de la date du debut en mot. On fait -1, car on commence en janvier
       qui est le mois 1 et non 0. Ex :'janv'
    */
    var moisDebutEnMot = mois[moisAuDebut - 1];
    // Mois de la date de la fin en mot. Ex : 'fev'
    var moisFinEnMot = mois[moisALaFin - 1];
    var jourTotalDebutMois = joursTotalMois(moisAuDebut);
    // Creation de la grille avec les dates et les jours.
    for (var i = 0; i < hauteurTable; i++) {
        // +1 car il y a le coin et on inclu les dates de debut et de fin
        grille[i] = Array(largeurTable);
        for (var j = 0; j < grille[i].length; j++) {
            if (i == 0 && j > 0) {
                grille[i][j] = jourDebut + " " + moisDebutEnMot;
                // Si le jour atteint la fin de son mois, on reajuste
                if (jourDebut == jourTotalDebutMois) {
                    moisDebutEnMot = moisFinEnMot;
                    // On le met a 0, le jourDebut++ en bas va l'affecter
                    jourDebut = 0;
                }
                jourDebut++;
            }
            if (i > 0 && j == 0) {
                grille[i][j] = heureDebut;
                heureDebut++;
            }

        }

    }
    return grille;
};

/* Fonction qui prend un tableau 2D et met des balises de table
   pour bien l'afficher. Le tableau2D vien avec les id ou class
*/
var construireTable = function(tableau2D, tabSpan) {
    var table = '';
    // Indice k qui parcourera tous les elements de tabSpan
    var k = 0;
    //  Boucle qui parcours la hauteur du tableau
    for (var i = 0; i < tableau2D.length; i++) {
        //      Debut de rangee
        table += '<tr>';
        //      Boucle qui parcours chaque element de la hauteur
        for (var j = 0; j < tableau2D[i].length; j++) {

            //  Coin tout en haut a gauche de la grille (vide)
            if (i == 0 && j == 0) {
                table += '<th>' + '</th>';
            }
            //  La rangee des jours et le mois
            if (i == 0 && j > 0) {
                table += '<th>' + tableau2D[i][j] + '</th>';
            }
            //  La rangee des heures
            if (j == 0 && i > 0) {
                table += '<th>' + tableau2D[i][j] + 'h' + '</th>';
            }
            // Les cases des disponibilites
            if (j > 0 && i > 0) {
                table += '<td ' + tableau2D[i][j] + '>' + tabSpan[k] + '</td>';
                k++;
            }
        }
        // Fin d'une rangee
        table += '</tr>';
    }
    return table;
};

// fonction qui met un string dans un string. ex : 'id="0-2"'
var doubleString = function(valeur) {
    var string = '""';
    /* Un double string est de la forme "..\"..\"" (ex : 'id=\"0-2\"") et on
       le coupe a partir du \". On obtiendra ["","",""]
    */
    string = string.split("\"");
    /* Pour garder la forme d'une double string, il faut le mettre dans la
       valeur du milieu du tableau ci-dessus.
    */
    string[1] = valeur + "";
    string = string.join("\"");
    return string;
};

// Fonction qui remplace les balises HTML en un contenu voulue
var remplacerBalise = function(texte, balise, motRemplacer) {
    texte = texte.split(balise);
    texte = texte.join(motRemplacer);
    return texte;
};

// ------------- FIN DES FONCTIONS COMMUNES ------------------------------

// ------------- SECTION DES VARIABLES GLOBALES --------------------------

// Variable globale qui garde en memoire les valeurs du sondage
var tabSondage = [];

/*  Nombre de jours total du mois du debut du sondage. On la met comme
    varriable globale pour pouvoir l'utliser dans d'autre fonctions.
*/
var nbJoursDebutMois = 0;

/* Garde en memoire les disponibilites de chaque personne, avec leur id et
   leur nom. Doit etre global pour garder en memoire chaque participant.
*/
var dispoPersonnes = [];

/* Indice de personne, doit rester globale pour savoir combien
   de personnes on a.
*/
var personneTotal = 0;

/* Tableau de couleur ou chaque indice de ce tableau reprensente, dans
   l'ordre,la couleur RGB en hexa de l'individus. Il doit etre globale pour
   etre utiliser plusieurs fois.
*/
var tabCouleur = [];

// ---------- FIN DE LA SECTION DES VARIABLES GLOBALES -------------------


/*  Retourne le texte HTML à afficher à l'utilisateur pour répondre au
    sondage demandé. Doit retourner false si le calendrier demandé
    n'existe pas
*/
var getCalendar = function(sondageId) {
    // +2, car il y a le coin en haut a gauche et l'heure du debut a inclure
    var hauteurTable = tabSondage.heureFin - tabSondage.heureDebut + 2;
    // + 1 car on compte le coin en haut a gauche
    var largeurTable = differenceJours(tabSondage.dateDebut,
                                       tabSondage.dateFin) + 1;
    // Tableau contenant seulement les dates et les heures.
    var grille = tabValeur(tabSondage.heureDebut,
                           tabSondage.heureFin,
                           tabSondage.debutJour,
                           tabSondage.finJour,
                           tabSondage.debutMois,
                           tabSondage.finMois,
                           hauteurTable,
                           largeurTable);

    grille = mettreID(grille);
    /*  Tableau qui prend notre grille de dates, heures et id et met les
        balises <tr> <td> <th>
    */
    var tabVideDispo = Array((grille.length - 1) * (grille[0].length - 1));
    tabVideDispo.fill('');
    var table = construireTable(grille, tabVideDispo);

    // grille.length donne le nombre de rangee, mais on ne veut pas la premiere
    var nbHeures = doubleString(grille.length - 1);
    // On le met en double string pour l'entete
    var nbJours = doubleString(differenceJours(tabSondage.dateDebut,
                                               tabSondage.dateFin));
    var tableEntete = '<table id="calendrier"' +
        ' onmousedown="onClick(event)"' +
        ' onmouseover="onMove(event)"' +
        ' data-nbjours=' + nbJours +
        ' data-nbheures=' + nbHeures + '>';
    table = tableEntete + table + '</table>';

    // Calendrier HTML
    var calHTML = readFile('template/calendar.html');
    var titre = tabSondage.titre;
    var url = "http://localhost:1337/" + sondageId;
    // Remplace les balises appropries
    calHTML = remplacerBalise(calHTML, "{{titre}}", titre);
    calHTML = remplacerBalise(calHTML, "{{table}}", table);
    calHTML = remplacerBalise(calHTML, "{{url}}", url);
    calHTML = calHTML + '<script src="calendar.js"> </script>';
    return calHTML;
};

/* Fonction qui calcule le nb de jours entre 2 dates qui ont maximum 30 jours
   de difference. L'idee est de prendre les deux dates et de calculer les
   millisecondes de chaque date, de les soustraires et de les transformer
   en jour. C'est la methode la plus efficace.
*/
var differenceJours = function(debutDate, finDate) {
    /* Separe notre date "2018-02-03" en ["2018", "02", "03"]. le new Date()
       est un objet qui prend la date en parametre et cree une date
    */
    var joursDebut = new Date(debutDate.split("-"));
    var joursFin = new Date(finDate.split("-"));
    /* Difference entre les dates. getTime() prend la date et permet de faire
       des calculs. La valeur est le nombre de millisecondes entre les dates.
    */
    var diff = joursFin.getTime() - joursDebut.getTime();
    if (diff < 0) return false;
    /* On divise par le nombre de milliseconde en 1 journee pour avoir les
       jours. On aroundi, car il peut avoir des decimaux. On fait +1, car
       la difference de jour n'inclu pas le jour actuel.
    */
    diff = Math.round(diff / MILLIS_PAR_JOUR) + 1;
    return diff;
};

/* Fonction qui met les balises id. L'idee est de commencer a
   (1,1) afin de toucher les case ou il n'y a pas de dates ou d'heures.
   La case de debut a comme coordonne (0,0) et la case juste a droite est (0,1),
   la case en bas de (0,0) est (0,1), etc.
*/
var mettreID = function (tableau2D) {
    var idY = 0;
    for (var i = 1; i < tableau2D.length; i++) {
        var idX = 0;
        for (var j = 1; j < tableau2D[i].length; j++) {
            tableau2D[i][j] = idY + "-" + idX;
            // On veut une forme 'id="x-y"' pour que le id est bien defini.
            tableau2D[i][j] = 'id=' + doubleString(tableau2D[i][j]);
            idX++;
        }
        idY++;
    }
    return tableau2D;
}

/* Fonction qui prend le tableau des resultats et le fusionne avec le tableau
   des couleurs de fond.
*/
var mettreCouleurFond = function(tableResultat, tabSommeDispo) {
    // L'indice 0 du tableau retourne de la fonction est le max
    var max = maxMinTab(tabSommeDispo)[0];
    // l'indice 1 du tableau retourne de la fonction est le min
    var min = maxMinTab(tabSommeDispo)[1];
    // Tableau contenant les class min et max
    var tabCouleurFond = couleurFond(max, min, tabSommeDispo);
    // Trace la couleur de chaque personne
    var k = 0;
    // Fusion des tableaux de couleurs de fond, des dates/heures et des barres
    for (var i = 1; i < tableResultat.length; i++) {
        for (var j = 1; j < tableResultat[i].length; j++) {
            tableResultat[i][j] = tabCouleurFond[k];
            k++;
        }
    }
    return tableResultat;
};

/* Fonction qui concatone les disponibilites de tous le monde pour plus tard
   calculer le max et le min. Par exemple, si deux cases ont un 1, alors le
   resultat sera 2. On prend tableResultat en parametre seulement pour
   calculer la longueur du tableau qu'on doit retourner.
*/
var sommeDispo01 = function(tableResultat) {
    /*  La longueur de la grille des disponibilites est le nombre de rangees
        totales - 1 multiplier par le nombre de colonnes totales - 1. -1 a
        cause du coin en haut a gauche.
    */
    var longGrilleDispo = (tableResultat.length - 1) *
        (tableResultat[0].length - 1);
    var dispoTtLeMonde = Array(longGrilleDispo);
    /*  On le remplit de 0, pour additionner chaque element de ce tableau2D
        a chaque element des tableaux de tous les participants
    */
    dispoTtLeMonde.fill(0);
    /*  Additionne les disponibilites du nouveau participant au tableau
        des disponibilites de tout le monde
    */
    for (var i = 0; i < dispoPersonnes.length; i++) {
        /* Comme notre tableau de max et min est en 1D et notre tableau des
           resultats est en 2D, il faudra une valeur k pour parcourir tous
           les elements du tableau a 1 dimension.
        */
        for (var j = 0; j < (dispoPersonnes[i].disponibilites).length; j++) {
            dispoTtLeMonde[j] += dispoPersonnes[i].disponibilites[j];
        }
    }
    return dispoTtLeMonde;
};

/* Retourne le texte HTML à afficher à l'utilisateur pour voir les
   résultats du sondage demandé

   Doit retourner false si le calendrier demandé n'existe pas
*/
var getResults = function(sondageId) {
    /* +2, car on compte le coin en haut a gauche et les heures sont inclus.
       Ex : il y a 11h entre 7 et 17h
    */
    var hauteurTable = tabSondage.heureFin - tabSondage.heureDebut + 2;
    var largeurTable = differenceJours(tabSondage.dateDebut,
                                       tabSondage.dateFin) + 1;

    // Tableau des valeurs des resultas, sans couleurs
    var tableResultat = tabValeur(tabSondage.heureDebut,
                                  tabSondage.heureFin,
                                  tabSondage.debutJour,
                                  tabSondage.finJour,
                                  tabSondage.debutMois,
                                  tabSondage.finMois,
                                  hauteurTable,
                                  largeurTable);
    // Cree un tableau pour seulement calculer le max et le min
    var sommeDispo = sommeDispo01(tableResultat);
    // Affecte les couleurs de fond a notre tableau de resultat
    tableResultat = mettreCouleurFond(tableResultat, sommeDispo);
    // Met les spans ensemble
    var sommeCouleurBarres = sommeSpan();
    // Assemble la table HTML qui sera affichee
    tableResultat = '<table>' +
        construireTable(tableResultat, sommeCouleurBarres) +
        '</table>';
    var resultatHTML = readFile('template/results.html');
    var url = "http://localhost:1337/" + sondageId;
    var table = tableResultat;
    var titre = tabSondage.titre;
    var legende = legendeHTML();
    resultatHTML = remplacerBalise(resultatHTML, "{{url}}", url);
    resultatHTML = remplacerBalise(resultatHTML, "{{table}}", table);
    resultatHTML = remplacerBalise(resultatHTML, "{{titre}}", titre);
    resultatHTML = remplacerBalise(resultatHTML, "{{legende}}", legende);
    return resultatHTML;
};

// Fonction qui trouve le maximum et le minimum du tableau
var maxMinTab = function(tableau) {
    var max = tableau[0];
    var min = tableau[0];
    for (var i = 0; i < tableau.length; i++) {
        if (tableau[i] > max) {
            max = tableau[i];
        }
        if (tableau[i] < min) {
            min = tableau[i];
        }
    }
    // Retourne le max et le min sous forme de tableau
    return [max, min];
};

/* Fonction qui prend un tableau et associe la couleur de fond pour le min et
   le max.
*/
var couleurFond = function(max, min, tableau) {
    for (var i = 0; i < tableau.length; i++) {
        if (tableau[i] == max) {
            tableau[i] = 'class=' + doubleString("max");
        }
        if (tableau[i] == min) {
            tableau[i] = 'class=' + doubleString("min");
        }
    }
    return tableau;
};

// Fonction qui fait met ensemble tous les couleurs de barre des participants
var sommeSpan = function() {
    var sommeBarres = Array(spanStyle(0).length);
    sommeBarres.fill('');
    for (var n = 0; n < personneTotal; n++) {
        var couleurBarres = spanStyle(n);
        for (var k = 0; k < couleurBarres.length; k++) {
            sommeBarres[k] += couleurBarres[k];
        }
    }
    return sommeBarres;
};

// Fonction qui la n ieme personne et lui met les sa couleur approprie
var spanStyle = function(n) {
    // Longueur du tableau de disponibilites
    var longueurTab = (dispoPersonnes[n].disponibilites).length;
    // Creation d'un nouveau tableau pour les spans
    var dispoTab = [];
    // On cherche les disponibilites de la personne
    recherchez1:
        for (var i = 0; i < longueurTab; i++) {
            // Si la personnes est disponible (a cocher la case)
            if (dispoPersonnes[n].disponibilites[i] == 1) {
                var couleurBarre = assemblerCouleurs(n);
                var span = '<span style=' + couleurBarre + '>.</span>';
                dispoTab.push(span);
                continue recherchez1;
            }
            dispoTab.push("");
        }
    return dispoTab;
};

// Double string qui met la couleur de fond et la couleur de la barre
var assemblerCouleurs = function(n) {
    var couleurFond = 'background-color :' + tabCouleur[n] + ';';
    var couleur = 'color: ' + tabCouleur[n] + ';';
    var couleurEnsemble = doubleString(couleurFond + couleur);
    return couleurEnsemble;
};

// Fonction qui construit la legende avec les balistes des listes (<ul> et <li>)
var legendeHTML = function() {
    var liste = '';
    for (var i = 0; i < dispoPersonnes.length; i++) {
        var couleurFond = "background-color :" + tabCouleur[i];
        // Liste des baliste <li>
        liste += '<li style=' + doubleString(couleurFond) + '>' +
            dispoPersonnes[i].nom + '</li>'
    }
    return liste;
};

/* Crée un sondage à partir des informations entrées

   Doit retourner false si les informations ne sont pas valides, ou
   true si le sondage a été créé correctement.
*/
var creerSondage = function(titre, id, dateDebut, dateFin, heureDebut, heureFin) {
    id = idVerification(id);
    if (id == false) return false;
    if (+heureDebut > +heureFin) return false;
    if (dateDebut > dateFin) return false;
    // Verifie si le jours est maximum 30
    var veriJour = differenceJours(dateDebut, dateFin);
    if (veriJour > 30) return false;

    // Separe la date de debut en [aaaa,mm,jj]
    var dateDebutTab = dateDebut.split("-");
    // Separe date de fin en [aaaa,mm,jj]
    var dateFinTab = dateFin.split("-");

    /* Garde en memoire les donnes du sondage, les + servent a mettre les
       valeurs string en numerique pour faciliter les calcules
    */
    tabSondage = {titre: titre,
                  id: id,
                  dateDebut : dateDebut,
                  dateFin : dateFin,
                  debutAnnee: +dateDebutTab[0],
                  debutMois: +dateDebutTab[1],
                  debutJour: +dateDebutTab[2],
                  finAnnee: +dateFinTab[0],
                  finMois: +dateFinTab[1],
                  finJour: +dateFinTab[2],
                  heureDebut: +heureDebut,
                  heureFin: +heureFin
    };
    // S'il n'y a pas eu de retour false, alors tout est correct
    return true;
};
// Fonction qui regarde chaque charactere s'il est conforme aux criteres
var idVerification = function(id) {
    // Regarde chaque charactere et le compare au code Unicode desiree
    boucleId:
        for (var i = 0; i < id.length; i++) {
                // verifie si le charactere est un chiffre
            if (id.charCodeAt(i) >= 48 && id.charCodeAt(i) <= 57
                ||
                // verifie si le charactere est une lettre majuscule
                id.charCodeAt(i) >= 65 && id.charCodeAt(i) <= 90
                ||
                // verifie si le charactere est une lettre minuscule
                id.charCodeAt(i) >= 97 && id.charCodeAt(i) <= 122
                ||
                // verifie si le charactere est un tiret i.e (-)
                id.charCodeAt(i) == 45) {
                    // Si le charactere est valide, passez au prochain
                    continue boucleId;
                }
            // Si le charactere n'est pas valide, retourne false
            else return false;
        }
    return true;
};

/* Ajoute un participant et ses disponibilités aux résultats d'un
   sondage. Les disponibilités sont envoyées au format textuel
   fourni par la fonction compacterDisponibilites() de public/calendar.js
   Cette fonction ne retourne rien et recree a chaque nouveau participant
   un nouveau tableau de donne
*/
var ajouterParticipant = function(sondageId, nom, disponibilites) {
    //  Disponibilite de la derniere personne qui vient de participer
    var dispo = disponibilites.split("");
    //  Change les elements string en nombre
    for (var i = 0; i < dispo.length; i++) {
        dispo[i] = +dispo[i];
    }
    // Quand on ajoute un participant, le nb total de participant monte de 1
    personneTotal++;
    /* Cree la table de couleur de tous les participants et le recalcule
       a chaque fois qu'un nouveau participant s'ajoute. Il doit recalculer
       a chaque fois, car le nb de participant total change et la couleur
       varie en fonction de cela.
    */
    tabCouleur = Array(personneTotal);
    for (var j = 0; j < personneTotal; j++) {
        var couleur = genColor(j, personneTotal);
        tabCouleur[j] = couleur;
    }
    // Garde en memoire les donnes du participant
    dispoPersonnes.push({
        id: sondageId,
        nom: nom,
        disponibilites: dispo
    });
};

/* Génère la `i`ème couleur parmi un nombre total `total` au format
   hexadécimal HTML

   Notez que pour un grand nombre de couleurs (ex.: 250), générer
   toutes les couleurs et les afficher devrait donner un joli dégradé qui
   commence en rouge, qui passe par toutes les autres couleurs et qui
   revient à rouge.
*/
var genColor = function(i, nbTotal) {
    // i = numero de couleur ou i eme participant
    var teinte = i / nbTotal * 360;
    var h = teinte / 60;
    var c = 0.7;
    var x = c * (1 - Math.abs(h % 2 - 1));
    var RGBVoulu = RGB(h, c, x);
    /* Nos valeurs de RGB sont entre 0 et 1. Pour les avoirs entre 0 et 255,
       il suffit de prendre chaque parametre (r,g,b) et de le multiplier
       par 255
    */
    RGBVoulu = RGBVoulu.map(function(x) {
        return Math.floor(x * 255);
    });
    // Decimal en hexadecimal ou les lettres sont numerique pour l'instant.
    RGBVoulu = RGBAHex(RGBVoulu);
    // Ajoute les lettres a notre valeur hexadecimal
    RGBVoulu = valeurHex(RGBVoulu);
    RGBVoulu = sixCharactere(RGBVoulu);
    // Met nos valeurs sous forme string "a,b,c,d"
    RGBVoulu = RGBVoulu.join("");
    // Met nos valeurs sous forme de tableau String (["a","b","c","d"])
    RGBVoulu = RGBVoulu.split(",");
    // Colle les valeurs du tableau sous forme "abcd"
    RGBVoulu = RGBVoulu.join("");
    return '#' + RGBVoulu;
};
/* Fonction qui calcule les parametres RGB a choisir selon des cas.
   Les valeurs de ce RGB va de 0 a 1 inclu.
*/
var RGB = function(h, c, x) {
    switch (Math.floor(h)) {
        case 0:
            return [c, x, 0];
        case 1:
            return [x, c, 0];
        case 2:
            return [0, c, x];
        case 3:
            return [0, x, c];
        case 4:
            return [x, 0, c];
        case 5:
            return [c, 0, x];
        default:
            return [0, 0, 0];
    }
};

/* Fonction qui transforme la valeur decimal en hexa, mais sans les lettres.
   Par exemple, le chiffre 90 en base 10 donnera ["5","10"]. Une autre
   fonction les convertira. L'idee est de faire une conversion de base 10 en
   base 16 par division succesive et de prendre les restes a l'envers pour
   avoir notre chiffre.
*/
var RGBAHex = function(tabRGB) {
    var tableauReste = [[],[],[]];
    for (var i = 0; i < tabRGB.length; i++) {
        var reste = 1;
        var chiffreADiviser = tabRGB[i];
        // Le chiffre 0 en base 10 est 0 en base 16
        if (chiffreADiviser == 0) {
            tableauReste[i].push(0);
            reste = 0;
        }
        while (reste > 0) {
            // On divise une premiere fois par la base
            var division = chiffreADiviser / 16;
            // On arondi a l'entier pres
            var resultatDivision = Math.floor(division);
            var plusGrosEntier = resultatDivision * 16;
            // On soustrait le plus gros entier qui est divisible par 16
            reste = chiffreADiviser - plusGrosEntier;
            // S'il n'y a plus rien a diviser
            if (reste == 0) break;
            tableauReste[i].push(reste);
            /* Le prochain chiffre a diviser est l'entier de la
               premiere division
            */
            chiffreADiviser = resultatDivision;
        }
        /* Notre reponse est inverse. On prend le dernier reste comme premier
           chiffre, l'avant dernier comme 2e, etc.
        */
        tableauReste[i] = inverserTableau(tableauReste[i]);
    }
    return tableauReste;
};

/* Fonction qui inverse les elements d'un tableau. Par exemple, le tableau
   [1,2,3,4,5,6] deviendra [6,5,4,3,2,1].
*/
var inverserTableau = function(tableau) {
    var tabInverser = [];
    for (var i = 0; i < tableau.length; i++) {
        tabInverser.push(tableau[tableau.length - 1 - i]);
    }
    return tabInverser;
};

/* Fonction qui prend un tableau 2D de string RGB et retourne les valeurs
   finales de RGB en hexadecimal avec les bonnes lettres s'il y en a.
*/
var valeurHex = function(tab2DRGB) {
    for (var i = 0; i < tab2DRGB.length; i++) {
        for (var j = 0; j < tab2DRGB[i].length; j++) {
            switch (tab2DRGB[i][j]) {
                case 10:
                    tab2DRGB[i][j] = "a";
                    break;
                case 11:
                    tab2DRGB[i][j] = "b";
                    break;
                case 12:
                    tab2DRGB[i][j] = "c";
                    break;
                case 13:
                    tab2DRGB[i][j] = "d";
                    break;
                case 14:
                    tab2DRGB[i][j] = "e";
                    break;
                case 15:
                    tab2DRGB[i][j] = "f";
                    break;
            }
        }
    }
    return tab2DRGB;
};

/* Fonction qui met notre valeur RGB sur 6 bits ou chaque champ R,G et B
   ont 2 bits.
*/
var sixCharactere = function(tableau2D) {
    var resultat = [[],[],[]];
    for (var i = 0; i < tableau2D.length; i++) {
        for (var j = 0; j < tableau2D[i].length; j++) {
            // Si un champs R,G ou B n'a pas 2 bits, mettez des 0 avant
            if (tableau2D[i].length < 2) {
                resultat[i].push(0);
                resultat[i].push(tableau2D[i][j]);
            }
            // Sinon, prenez les valeurs telles quelles
            else resultat[i].push(tableau2D[i][j]);
        }
    }
    return resultat;
};

/*
 * Création du serveur HTTP
 * Note : pas besoin de toucher au code ici (sauf peut-être si vous
 * faites les bonus)
 */
http.createServer(function(requete, reponse) {
    var url = urlParse(requete.url);

    // Redirect to index.html
    if (url.pathname == '/') {
        redirect(reponse, defaultPage, url.query);
        return;
    }

    var doc;

    if (url.pathname == defaultPage) {
        var res = indexQuery(url.query);

        if (res.exists) {
            redirect(reponse, res.id);
            return;
        } else {
            doc = getIndex(res.data);
        }
    } else {
        var parsedPath = pathParse(url.pathname);

        if (parsedPath.ext.length == 0) {
            var id;

            if (parsedPath.dir == '/') {
                id = parsedPath.base;

                if (calQuery(id, url.query)) {
                    redirect(reponse, '/' + id + '/results')
                    return;
                }

                var data = getCalendar(id);

                if (data === false) {
                    redirect(reponse, '/error404.html');
                    return;
                }

                doc = {
                    status: 200,
                    data: data,
                    type: 'text/html'
                };
            } else {
                if (parsedPath.base == 'results') {
                    id = parsedPath.dir.slice(1);
                    var data = getResults(id);

                    if (data === false) {
                        redirect(reponse, '/error404.html');
                        return;
                    }

                    doc = {
                        status: 200,
                        data: data,
                        type: 'text/html'
                    };
                } else {
                    redirect(reponse, '/error404.html');
                    return;
                }
            }
        } else {
            doc = getDocument(url);
        }
    }

    sendPage(reponse, doc);

}).listen(port);
