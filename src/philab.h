#ifndef _INTERFACE_HEADER
#define _INTERFACE_HEADER

#define _USE_READLINE


/* Déclaration des fonctions */

void init_env(void); /* Initialise l'environnement */
char *getpwd(void); /* Retourne le repertoire courant */
char *get_prompt(void); /* Retourne le prompt */
void builtin_cd(char *path); /* La commande cd */
void external_cmd(char **argv); /* La commande d'execution */

/* La fonction d'analyse de la saisie */
void make_cmd(char *str);
/* Une petite macro utilisée dans make_cmd()
 * pour liberer la memoire */
#define FREE_ARGV() do { \
   i = 0; \
   while(argv[i] != NULL) \
   free(argv[i++]); \
} while(0)


/* Taille maximale de la saisie si
 * _USE_READLINE n'est pas definit */
#define SIZE 512

/* Quelques constantes */
#define RUNTIME "runtime" /* Le nom de l'executable fortran */
#define PRINT_CMD "print" /* Le nom de la commande print */
#define PROD_CMD "prod" /* Le nom de la commande pour le produit */
#define SUM_CMD "sum" /* Le nom de la commande pour la sommme */
#define EXTENSION ".mat" /* L'extension des fichiers de matrices */


/* Structure d'une matrice chargée */
typedef struct _matrix
{
   char *name; /* Le nom (l'alias) */
   char *file; /* Le path du fichier */
   struct _matrix *next; /* C'est une liste chainée */

} matrix;

/* Strucure d'une commande philab */
typedef struct _builtin
{
   char *name; /* Le nom de la commande */
   char *description; /* Description */
   char *example; /* Un exemple d'utilisation */
   void (*function)(char *); /* La fonction a appeler */
} builtin;

/* Les fonction représentées par les commande philab */
void help(char *p); /* Affiche l'aide */
void load(char *path); /* Charge une matrice */
void unload(char *name); /* Décharge une matrice */
void print(char *name); /* Affiche une matrice */

void operateur(char *mat1, char *mat2, char *op);

#endif /* _INTERFACE_HEADER */
