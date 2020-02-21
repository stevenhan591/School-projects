// -------------------------- IMPORTANT LISEZ-MOI --------------------------------
/* Je n'ai pas mis d'accent sur ce code pour eviter l'incompatibilite des machines
 * Ce sera a vous de juger s'il y a un accent ou pas pour la comprehension
 */
#include <stdlib.h>
#include "mymalloc.h"

#include <stdio.h>
#include <string.h>
// taille de notre cartographie
char blocMemoire[4096];

int compteur = 0;

// Structure de notre index
struct bloc{
 size_t tailleBloc;
 // Indicateur si le bloc est disponible ou pas. Il n'y a pas de boolean en C.
 // 1 = oui (disponible)
 // 0 = non (pas disponible)
 int libre;
 struct bloc *prochain;
};

// Pointeur qui pointera au tout debut de notre bloc de memoire
struct bloc *listeVide = (void*) blocMemoire;

/* Fonction qui peut servir de test unitaire. Ce qui est interessant est le myfree(p)
 * appele plusieurs fois sans planter le programme. Vous pouvez enlevez les commentaires
 * pour le tester si vous voulez.
 */
/*
int main(){
    int *a = malloc(2);
    printf("La valeur de refinc(a) est %d\n", refinc(a)); // Imprime un message d'erreur et la valeur retourne est 0
    myfree(a); // Imprime un message d'erreur
     int *p= mymalloc(sizeof(int));
     int *q= mymalloc(250*sizeof(char));
     int *r= mymalloc(1000*sizeof(int)); // Erreur, car 4000 octet est trop proche de 4096
     int *p2 = p;
     *p = 30;
     printf("La valeur de p est %d\n", *p);
     printf("La valeur de refinc(p) est %d\n", refinc(p));
     *p = *p + 20;
     refinc(p);
     printf("La valeur de refinc(p) est %d\n", refinc(p));
     myfree(p);
     myfree(p);
     myfree(p);
     *p = *p + 1;
     if (p2 == p) {
         printf("la valeur de p2 est %d\n", *p2);
     } else {
         printf("Mes pointeurs font n'importequoi\n");
     }
     myfree(p);
     refinc(q);
     // Devrait etre du nimporte quoi et non 51
     printf("La valeur du pointeur p apres avoir liberer sa memoire est %d\n", *p);
     printf("La valeur de refinc(q) est %d\n", refinc(q));
     char *w= mymalloc(700);
     myfree(r); // Erreur, le pointeur n'existe pas
     int *k= mymalloc(500*sizeof(int));
     printf("Test effectue avec succes");
}*/

/* Fonction qui met a jour les pointeurs qui sont libre. L'idee est de remplacer l'index qui pointe vers le bloc
 * libere par le prochain que le bloc libere pointait.
 */
void misAJourPointeur(){
    struct bloc *pointeurActuel,*pointeurPrecedent;
    // Pointe au tout debut de notre bloc de 4ko
    pointeurActuel = listeVide;
    // Traverse tous les blocs d'information de notre bloc de 4ko
    while((pointeurActuel -> prochain) != NULL){
        // Si le prochain bloc doit etre liberer
        if(pointeurActuel -> prochain -> libre == 1){
            pointeurActuel -> tailleBloc += (pointeurActuel -> prochain -> tailleBloc) + sizeof(struct bloc);
            // Si on a atteint la fin des blocs
            if (pointeurActuel -> prochain -> prochain == NULL) {
                pointeurActuel -> prochain = NULL;
            } else {
                pointeurActuel -> prochain = pointeurActuel -> prochain -> prochain;
            }
        }
        // Si jamais apres l'affectation le prochain devient null, on doit retourner
        if (pointeurActuel -> prochain == NULL) {
            return;
        }
        // Passe au prochain bloc
        else {
            pointeurPrecedent = pointeurActuel;
            pointeurActuel = pointeurActuel -> prochain;
        }
    }
}

void *mymalloc(size_t size){
    // On a besoin d'un pointeur pour traverser listeVide
    struct bloc *pointeurActuel,*pointeurPrecedent;
    // le pointeur qui pointera notre bloc d'information
    void *resultat;
    // Si la memoire n'a pas ete initialisee
    if(!(listeVide -> tailleBloc)){
         // Initialise le bloc de 4ko
         listeVide -> tailleBloc = (4096) - sizeof(struct bloc);
         listeVide -> libre = 1;
         listeVide -> prochain = NULL;
         printf("Memoire initialisee\n");
    }
    // Pointe au premier index de notre cartographie
    pointeurActuel = listeVide;
    // Si vrai, le bloc d'information ou le pointeur est ne peut pas etre utiliser, alors on prend le prochain
    while((((pointeurActuel -> tailleBloc) < size) || ((pointeurActuel -> libre) == 0)) && (pointeurActuel -> prochain != NULL)) {
        pointeurPrecedent = pointeurActuel;
        pointeurActuel = pointeurActuel -> prochain;
    }
    // Si la taille demandee par le programmeur est exactement la taille disponible
   if ((pointeurActuel -> tailleBloc) == size) {
       // Indique que la memoire a ete allouee
       pointeurActuel -> libre = 0;
       pointeurActuel -> tailleBloc = size;
       resultat = (void*)(++pointeurActuel);
       return resultat;
   }
   // Si la taille demandee est plus petite que notre bloc de memoire, on le coupera
   else if ((pointeurActuel -> tailleBloc) > (size + sizeof(struct bloc))) {
       // Separe la taille du bloc ou le pointeur est
       struct bloc *nouveauPointeur=(void*)((void*)pointeurActuel + size + sizeof(struct bloc));
       nouveauPointeur -> tailleBloc = (pointeurActuel -> tailleBloc) - size - sizeof(struct bloc);
       nouveauPointeur -> libre = 1;
       nouveauPointeur -> prochain = pointeurActuel -> prochain;
       pointeurActuel -> tailleBloc = size;
       pointeurActuel -> libre = 0;
       pointeurActuel -> prochain = nouveauPointeur;
       resultat = (void*)(++pointeurActuel);
       return resultat;
    }
    // Si la taille demandee est plus grande que 4ko, on ne gere pas ce cas. Trop difficile.
    else {
       resultat = NULL;
       printf("Desole, pas assez de memoire pour la savegarder\n");
       return resultat;
    }
}

int refinc(void *ptr){
    // Regarde si le pointeur est dans le bloc de 4ko
    if(((void*)blocMemoire <= ptr) && (ptr <= (void*)(blocMemoire + (4096)))){
        struct bloc* pointeurActuel = ptr;
        pointeurActuel = pointeurActuel -> prochain;
        // Incremente le compteur
        compteur++;
    }
    else printf("Erreur, le pointeur n'est pas dans la plage du bloc de 4ko\n");
}

void myfree(void *ptr){
    // Regarde si le pointeur est dans le bloc de 4ko
    if(((void*)blocMemoire <= ptr) && (ptr <= (void*)(blocMemoire + (4096)))){
        struct bloc* pointeurActuel = ptr;
        if (compteur == 0) {
            --pointeurActuel;
            pointeurActuel -> libre = 1;
            misAJourPointeur();
        } else {
            ++pointeurActuel;
            pointeurActuel -> libre = 0;
            // Decremente le compteur
            compteur--;
        }
    }
    else printf("Erreur, le pointeur n'est pas un pointeur retourne par mymalloc\n");
}
