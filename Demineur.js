// Tp1 : construire un jeu similaire au demineur
// Par Steven Han (20129462)

/* L'idee de ce programme est de localiser (avec la fonction getMouse())
la tuile que l'utilisateur a cliquee. Ensuite, le programme calculera les
bombes adjacentes et affichera l'image associee. Si la tuile ne contient
aucune bombes autour, le programme devoilera les tuiles adjacentes. */


load("images.js");
var afficherImage = function(x, y, colormap, image) {
	// Fonction qui affiche les images du jeu demineur
    
    var pixelY = y * 16; // 16 pixels verticaux equivaut y=1
    for (var i = 0; i < 16; i++) { // Boucle qui cree 16 rangees pixels
        var pixelX = x * 16; // Reinitialise la position du pixel en x
        for (var j = 0; j < 16; j++) { // Boucle qui cree 16 colonnes de pixels
            setPixel(pixelX, pixelY, colormap[images[image][i][j]]);
            // Chaque numero du tableau images correspond a une couleur RGB
            // du tableau colormap et on affiche le pixel.
            pixelX++;
        }
        pixelY++;
    }
};

var attendreClic = function() { 
    // Fonction qui cherche les coordonnes (x,y) de la souris
    
    while (getMouse().down == false) { // Boucle qui attend le clic
        pause(0.01);
    }
    while (getMouse().down == true) { // Boucle qui attend le relachement du clic
        pause(0.01);
    }
    var coordSouris = [getMouse().x, getMouse().y];
    // Coordonne (x,y) de l'utilisateur
    return coordSouris;
};

var placerMines = function(largeur, hauteur, nbMines, x, y) {
    // Fonction qui place les mines dans le jeu
    
    if (nbMines >= 1 && nbMines < largeur * hauteur 
        && largeur > 0 && hauteur > 0 
        && x >= 0 && x <= largeur 
        && y >= 0 && y <= hauteur) {
        // Conditions pour que le jeu ait du sens 
        var grille = Array(largeur);
        for (var i = 0; i < largeur; i++) { // Boucle qui cree la hauteur du jeu
            grille[i] = Array(hauteur);
            for (var j = 0; j < hauteur; j++) { // Met false a toutes les cases
                grille[i][j] = false; 

            }
        }

        while (nbMines != 0) { // Placer tous les mines dans la grille

            var mineX = Math.floor(Math.random() * largeur); // Coordonne x de la mine
            var mineY = Math.floor(Math.random() * hauteur); // Coordonne y de la mine
            grille[x][y] = false;
            if (grille[mineX][mineY] == false
                && (mineX != x || mineY != y)) {
            // Si la tuile n'est pas une mine et n'est pas sur la souris
                grille[mineX][mineY] = true;
                nbMines--;
            }
        }
        return grille; // Retourne la valeur du tableau du jeu avec mines
    } else return "erreur";

};

var devoilerTuile = function(x, y, tuileFermee, grille, hauteur, largeur) {
    // Fonction qui devoile les tuiles
    
    var indice = 0; // Nombre de mine autour de la tuile cliquee
    for (var voisinX = -1; voisinX <= 1; voisinX++) {
        // Cherche les bombes autour de la tuile de gauche a droite
        for (var voisinY = -1; voisinY <= 1; voisinY++) {
            // Cherche les bombes autour de la tuile de bas en haut
            var i = x + voisinX;
            // Delimite le voisinage aux extremites de gauche et de droite
            var j = y + voisinY;
            // Delimite le voisinage aux extremites de haut et de bas
            if (j > -1 && j < hauteur && i > -1 && i < largeur) {
                // Ignore le voisinage hors de la grille de jeu  
                var voisinage = grille[i][j];
                if (voisinage == true) {
                    indice++;
                }
            }
        }
    }
    afficherImage(x, y, colormap, indice);
    grille[x][y] = "devoilee"; // Pour ne pas afficher la meme case 2 fois
    tuileFermee--;
    if (indice == 0) { // Devoilement des tuiles voisines
        tuileFermee = devoilerVoisins0(x, y, tuileFermee, grille, hauteur, largeur);
    }
    return tuileFermee;

};

var devoilerVoisins0 = function(x, y, tuileFermee,  grille, hauteur, largeur) {
	// Fonction qui devoile les tuiles voisins de 0
    
    for (var y2 = y + 1; y2 >= y - 1; y2--) {
        // Boucle qui prend les valeurs de y autour de la tuile 0
        boucleX2:
            for (var x2 = x - 1; x2 <= x + 1; x2++) {
                // Boucle qui prend les valeurs de x autour de la tuile 0
                var indice = 0;
                for (var voisinX = -1; voisinX <= 1; voisinX++) {
                    // meme boucle qu'en haut pour trouver le voisinage
                    
                    for (var voisinY = -1; voisinY <= 1; voisinY++) {
                        var i = x2 + voisinX;
                        var j = y2 + voisinY;
                        if (j > -1 && j < hauteur && i > -1 && i < largeur) {  
                            var voisinage = grille[i][j];
                            if (voisinage == true) {
                                indice++;
                            }
                        }
                    }
                }
                if (x2 >= 0 && x2 < largeur 
                    && y2 >= 0 && y2 < hauteur 
                    && grille[x2][y2] == false) {
                 // Voisinage dans la grille de jeu et non devoilee
                    
                    if (x2 == x && y2 == y) { // Ne pas prendre la case cliquee
                        continue boucleX2;
                    }
                    afficherImage(x2, y2, colormap, indice);
                    grille[x2][y2] = "devoilee";
                    tuileFermee--;
                }
            }
    }
    return tuileFermee;
};

var demineur = function(largeur, hauteur, nbMines) {
    // Fonction qui contient le mechanisme du jeu
    
    setScreenMode(largeur * 16, hauteur * 16);
    for (var i = 0; i < largeur; i++) { 
        for (var j = 0; j < hauteur; j++) { 
            afficherImage(i, j, colormap, 11);
            // Met des tuiles non-devoilee sur la grille de jeu
        }
    }
    var coordSouris = attendreClic(); // Attend le premier clic pour placer les mines
    var x = Math.floor(coordSouris[0] / 16);
    var y = Math.floor(coordSouris[1] / 16);
    var grille = placerMines(largeur, hauteur, nbMines, x, y);
    var tuileFermee = largeur * hauteur;

    while (grille[x][y] == false || grille[x][y] == "devoilee") {
  		// Boucle du jeu. Continue tant que le joueur n'a pas cliquee sur une mine
        if (grille[x][y] == false) { // Case qui n'a jamais ete cliquee
            tuileFermee = devoilerTuile(x, y, tuileFermee, grille, hauteur, largeur);
            if (tuileFermee == nbMines) { // Condition pour gagner
                break; 
            }
        }

        coordSouris = attendreClic();
        x = Math.floor(coordSouris[0] / 16);
        y = Math.floor(coordSouris[1] / 16);
    }

    for (var i = 0; i < largeur; i++) { // affiche tous les bombes
        for (var j = 0; j < hauteur; j++) {
            if (grille[i][j] == true)
                afficherImage(i, j, colormap, 9);
        }
    }
    if (tuileFermee != nbMines) { // Si l'utilisateur perd
        afficherImage(x, y, colormap, 10);
    }

};

var testDemineur = function() {
    setScreenMode(16,16);
    assert(placerMines(2, 2, 0, 1, 1) == "erreur"); // Teste nbMines = 0
    assert(placerMines(2, 2, 5, 1, 1) == "erreur"); // Teste nbMines < largeur*hauteur
	assert(placerMines(2, 2, 1, -1, 1) == "erreur"); // Teste valeur de x
    assert(placerMines(2, 2, 1, 1, -1) == "erreur"); // Teste valeur de y
    assert(placerMines(2, 2, 1, 5, 1) == "erreur"); // Teste valeur de x > largeur
    assert(placerMines(2, 2, 1, 1, 5) == "erreur"); // Teste valeur de y > hauteur
    assert(placerMines(2, 2, 2, 1, 1) == "false,true,true,false"
          								 || "true,false,true,false"
           								 || "true,true,false,false"
                                         );
    assert(exportScreen(afficherImage(0, 0, colormap, 1)) == "#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#0000ff#0000ff#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#0000ff#0000ff#0000ff#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#0000ff#0000ff#0000ff#0000ff#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#0000ff#0000ff#0000ff#0000ff#0000ff#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#0000ff#0000ff#0000ff#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#0000ff#0000ff#0000ff#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#0000ff#0000ff#0000ff#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#0000ff#0000ff#0000ff#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#0000ff#0000ff#0000ff#0000ff#0000ff#0000ff#0000ff#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#0000ff#0000ff#0000ff#0000ff#0000ff#0000ff#0000ff#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0");
	assert(exportScreen(afficherImage(0, 0, colormap, 3)) == "#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#ff0000#ff0000#ff0000#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#ff0000#ff0000#ff0000#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#ff0000#ff0000#ff0000#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#ff0000#ff0000#ff0000#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0");
    assert(exportScreen(afficherImage(0, 0, colormap, 10)) == "#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080\n#808080#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000\n#808080#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#000000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000\n#808080#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#000000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000\n#808080#ff0000#ff0000#ff0000#000000#ff0000#000000#000000#000000#000000#000000#ff0000#000000#ff0000#ff0000#ff0000\n#808080#ff0000#ff0000#ff0000#ff0000#000000#000000#000000#000000#000000#000000#000000#ff0000#ff0000#ff0000#ff0000\n#808080#ff0000#ff0000#ff0000#000000#000000#ffffff#ffffff#000000#000000#000000#000000#000000#ff0000#ff0000#ff0000\n#808080#ff0000#ff0000#ff0000#000000#000000#ffffff#ffffff#000000#000000#000000#000000#000000#ff0000#ff0000#ff0000\n#808080#ff0000#000000#000000#000000#000000#000000#000000#000000#000000#000000#000000#000000#000000#000000#ff0000\n#808080#ff0000#ff0000#ff0000#000000#000000#000000#000000#000000#000000#000000#000000#000000#ff0000#ff0000#ff0000\n#808080#ff0000#ff0000#ff0000#000000#000000#000000#000000#000000#000000#000000#000000#000000#ff0000#ff0000#ff0000\n#808080#ff0000#ff0000#ff0000#ff0000#000000#000000#000000#000000#000000#000000#000000#ff0000#ff0000#ff0000#ff0000\n#808080#ff0000#ff0000#ff0000#000000#ff0000#000000#000000#000000#000000#000000#ff0000#000000#ff0000#ff0000#ff0000\n#808080#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#000000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000\n#808080#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#000000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000\n#808080#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000#ff0000");
	assert(exportScreen(afficherImage(0, 0, colormap, 11)) == "#ffffff#ffffff#ffffff#ffffff#ffffff#ffffff#ffffff#ffffff#ffffff#ffffff#ffffff#ffffff#ffffff#ffffff#ffffff#c0c0c0\n#ffffff#ffffff#ffffff#ffffff#ffffff#ffffff#ffffff#ffffff#ffffff#ffffff#ffffff#ffffff#ffffff#ffffff#c0c0c0#808080\n#ffffff#ffffff#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#808080#808080\n#ffffff#ffffff#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#808080#808080\n#ffffff#ffffff#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#808080#808080\n#ffffff#ffffff#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#808080#808080\n#ffffff#ffffff#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#808080#808080\n#ffffff#ffffff#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#808080#808080\n#ffffff#ffffff#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#808080#808080\n#ffffff#ffffff#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#808080#808080\n#ffffff#ffffff#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#808080#808080\n#ffffff#ffffff#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#808080#808080\n#ffffff#ffffff#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#808080#808080\n#ffffff#ffffff#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#808080#808080\n#ffffff#c0c0c0#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080\n#c0c0c0#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080");
    assert(exportScreen(afficherImage(0, 0, colormap, 6)) == "#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080#808080\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#c0c0c0#008080#008080#008080#008080#008080#008080#008080#008080#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#008080#008080#008080#008080#008080#008080#008080#008080#008080#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#008080#008080#008080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#008080#008080#008080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#008080#008080#008080#008080#008080#008080#008080#008080#008080#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#008080#008080#008080#008080#008080#008080#008080#008080#008080#008080#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#008080#008080#008080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#008080#008080#008080#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#008080#008080#008080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#008080#008080#008080#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#008080#008080#008080#008080#008080#008080#008080#008080#008080#008080#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#c0c0c0#008080#008080#008080#008080#008080#008080#008080#008080#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0\n#808080#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0#c0c0c0");
};
testDemineur();
// Pour jouer, ne faites pas appel a la fonction testDemineur() 
// ou enlevez les 5 derniers assert et setScreenMode(16,16)




     