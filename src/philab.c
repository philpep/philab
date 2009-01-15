#define _BSD_SOURCE /* macro setenv pour glib */
#include <stdio.h> /* printf, fprintf, sprintf */
#include <stdlib.h> /* malloc, free, realloc, NULL */
#include <string.h> /* strlen, strcpy, strcat, strcmp, strncmp */
#include <unistd.h> /* getcwd, getuid */
#include <pwd.h> /* getpwuid, struct passwd */
#include <errno.h> /* errno */
#include <sys/wait.h> /* wait */
#include <sys/types.h> /* size_t, uid_t, pid_t */
#include <sys/utsname.h> /* struct utsname, uname */
#include <assert.h> /* macro assert() */

#include "philab.h"

#ifdef _USE_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif /* _USE_READLINE */

/* La liste des commandes de philab */
const builtin builtin_cmd[] = {
   {"help", "Affiche l'aide", "help [cmd]", help, NULL, NULL},
   {"load", "Charge une matrice dans la memoire", "load a.mat ('a' sera un alias vers la matrice a.mat)", load, NULL, NULL},
   {"unload", "Décharge une matrice", "unload A", unload, NULL, NULL},
   {PRINT, "Affiche une matrice", PRINT" matrix_alias, ou plus simplement matrix_alias", NULL, two_param, NULL},
   {TRACE, "Affiche la trace d'une matrice", TRACE" A", NULL, two_param, NULL},
   {"+", "Affiche la somme de deux matrices", "A + B (Avec les espaces svp)", NULL, NULL, NULL},
   {"*", "Affiche le produit matriciel de deux matrices", "A * B (with spaces please)", NULL, NULL, NULL},
   {NORME, "Affiche la norme d'une matrice", NORME" [1|inf|fro] A", NULL, NULL, tree_param},
   {GAUSS, "Resoudre un système du type Ax = b", GAUSS" A b", NULL, NULL, tree_param},
   {PW_ITER, "Trouver la plus grande valeur propre en module d'une matrice par la methode de la puissance itérée", PW_ITER" U v, où U est la matrice et v le premier vecteur (Le programme va jusqu'a l'ordre 10000", NULL, NULL, tree_param},
   {"exit", "Quitte le programme", "exit", NULL, NULL, NULL},
   {NULL, NULL, NULL, NULL, NULL, NULL}
};

const char *autorised_cmd[10] = {"mkdir", "rm", "cp", "mv", "ls", "pwd", "vim", "emacs", "cat", NULL};


matrix *ll_matrix = NULL;

/* 
 * init_env() : initialise les variables
 * d'environnements
 */
void init_env(void)
{
   char *pwd, *p;
   uid_t uid = getuid();
   struct passwd *user = getpwuid(uid);
   struct utsname host;

   uname(&host);
   /* On utilise assert car si setenv
    * échoue le programme est inutilisable */

   assert(setenv("HOME", user->pw_dir, 0) == 0);
   assert(setenv("HOST", host.nodename, 0) == 0);

   pwd = getpwd();

   p = malloc(sizeof(char) * (2+strlen(pwd)+strlen(RUNTIME)));

   strcpy(p, pwd);
   strcat(p, "/"RUNTIME);

   assert(setenv("RUNTIME_PATH", p, 1) == 0);

   free(p);
   free(pwd);

   return;
}

/* 
 * Cette fonction renvoie le répertoire courant sous
 * forme d'une chaine de caractère
 */
char *getpwd(void)
{
   char *p;
   /*
    * Au début, j'utilisais une boucle avec
    * realloc pour augmenter la taille de p
    * mais au bout d'un moment ça segfault
    * pour une raison que j'ignore encore...
    */
   p = malloc(sizeof(char) * 256);
   if(NULL == getcwd(p, 256))
      strcpy(p, "somewere");
   return p;
}

/* 
 * La commande cd, elle n'est pas comme les autres car elle
 * doit absolument être exécutée dans le processus courant.
 * (Sinon c'est le processus fils qui changerais son répertoire
 * courant...) 
 */
void builtin_cd(char *path)
{
   /* On change tout simplement le répertoire courant */
   struct passwd *user;
   /* Si la commande a été lancée sans argument on va dans
    * le $HOME */
   if(path == NULL)
   {
      user = getpwuid(getuid());
      chdir(user->pw_dir);
   }
   else
      if(-1 == chdir(path))
	 switch(errno)
	 {
	    /* On traite les erreurs possibles (Il en manque, mais c'est des cas extrêmes) */
	    case EACCES:
	       fprintf(stderr, "Philab: %s permission non accordée\n", path);
	       break;
	    case EIO:
	       fprintf(stderr, "Philab: %s erreur d'entrée/sortie\n", path);
	       break;
	    case ENAMETOOLONG:
	       fprintf(stderr, "Philab: %s path trop long\n", path);
	       break;
	    case ENOENT:
	       fprintf(stderr, "Philab: aucun fichier ou dossier de ce type : %s\n", path);
	       break;
	    case EFAULT:
	       fprintf(stderr, "Philab: %s pointe en dehors de l'espace d'adressage accessible\n", path);
	    case ENOTDIR:
	       fprintf(stderr, "Philab : %s n'est pas un dossier\n", path);
	       break;
	    default:
	       fprintf(stderr, "Philab : Impossible de se déplacer vers %s\n", path);
	       break;
	 }
   return;
}

/* 
 * get_prompt renvoie une chaîne de caractère qui
 * contient le prompt
 */
char *get_prompt(void)
{
   /* Déclarations */
   char *pwd, *q, *prompt, *home, *host;
   home = getenv("HOME");
   host = getenv("HOST");
   /* Si on ne peut avoir le répertoire courant on renvoie un prompt minimal */
   if(NULL == (pwd = getpwd()))
   {
      prompt = malloc(sizeof(char) * 2);
      strcpy(prompt, ">");
      return prompt;
   }

   q = malloc(sizeof(char) * 1+strlen(pwd));
   /* Cette partie remplace $HOME par ~ dans dans et met
    * la nouvelle chaîne obtenue dans q */
   if(!strncmp(pwd, home, strlen(home)))
   {
      strcpy(q, "~");
      strcat(q, pwd+strlen(home));
   }
   else
      strcpy(q, pwd);

   /* On écrit le prompt */
   prompt = malloc(sizeof(char) * 50+strlen(q)+strlen(host));
   strcpy(prompt, "[\033[31mphilab\033[37m@\033[36m");
   strcat(prompt, host);
   strcat(prompt, "\033[37m : \033[34m");
   strcat(prompt, q);
   strcat(prompt, "\033[37m] % ");
   free(q);
   free(pwd);

   return prompt;
}

/* 
 * Permet de lancer des commandes externes
 * en dupliquant le processus courant 
 */
void external_cmd(char **argv)
{
   /* 
    * Le processus est dupliqué fork()
    * le nouveau processus exécute la commande
    * et le père (philab) attend la mort du processus
    * fils
    */
   pid_t pid;
   if(argv[0] == NULL)
      return;
   pid = fork();
   /* Fils */
   if(pid == 0)
   {
      /* cf man 3 exec */
      if(argv[0][0] == '/')
	 exit(execv(argv[0], argv));
      else
	 exit(execvp(argv[0], argv));
   }
   /* 
    * Avec exit, on s'assure que le processus fils sera quand même tué
    * (si la commande n'existe pas par exemple)
    */

   /* Père : il attend la mort du fils */
   else if (pid != -1)
      wait(NULL);
   /* 
    * Si on est arrivé jusque là c'est qu'il y a un gros
    * problème (plus possible de dupliquer des processus...)
    * Donc on quitte brutalement 
    */
   else
      abort();
   return;
}


/* La fonction main, tout commence ici */
int main(void)
{
   char *prompt, *saisie;
#ifndef _USE_READLINE
   char *p, c;
#endif
   /* On initialise les variables d'environnement */
   init_env();
   /* On affiche le message de bienvenue */
   printf(" ____  _     _ _       _     \n");
   printf("|  _ \\| |__ (_) | __ _| |__  \n");
   printf("| |_) | '_ \\| | |/ _` | '_ \\ \n");
   printf("|  __/| | | | | | (_| | |_) |\n");
   printf("|_|   |_| |_|_|_|\\__,_|_.__/ \n");
   printf("Tapez 'help' pour l'aide \n");
   /* La boucle de philab */
   for(;;)
   {
      prompt = get_prompt();
      /* On récupère la saisie avec ou sans readline */
#ifdef _USE_READLINE
      /* TODO : readline permet de faire de la complétion
       * personnalisée, on pourrait l'utiliser mais je suis
       * trop feignant */
      saisie = readline(prompt);
      if(saisie)
	 add_history(saisie);
#else
      printf(prompt);
      saisie = malloc(sizeof(char) * SIZE);
      fgets(saisie, SIZE, stdin);

      /* On supprime le '\n' de la fin */
      if(NULL != (p = strrchr(saisie, '\n')))
	 *p = '\0';
      else
	 while('\n' != (c = fgetc(stdin)) && c != EOF);

#endif /* _USE_READLINE */

      free(prompt);
      /* make_cmd analyse la saisie et exécute ce qu'il faut
       * exécuter */
      make_cmd(saisie);
      free(saisie);
   }
   return 0;
}

/* 
 * La fonction qui va parser la saisie et appeler les bonnes
 * fonctions.
 */
void make_cmd(char *str)
{
   char *argv[MAX_ARG];
   char *outfile = NULL;
   char *p;
   const char *shell_cmd;
   size_t i = 0;
   const builtin *p_builtin = builtin_cmd;

   /* On trouve le nom du fichier de sortie */
   if(NULL != (p = strchr(str, '>')))
   {
      *p = '\0';
      p++;
      /* On supprime les espace du début */
      while(*p == ' ')
	 *(p++) = '\0';
      outfile = p;

      /* Mais aussi ceux de la fin */
      if(NULL != (p = strchr(outfile, ' ')))
	 *p = '\0';
   }


   /* On parse la saisie suivant les espaces */
   p = strtok(str, " ");
   while(p != NULL && i < MAX_ARG-1)
   {
      argv[i] = malloc(sizeof(char) * (1+strlen(p)));
      strcpy(argv[i++], p);
      p = strtok(NULL, " ");
   }

   /* On fixe tous les arguments restant de argv à NULL */
   while(i < MAX_ARG)
      argv[i++] = NULL;

   /* Si la commande est vide */
   if(argv[0] == NULL)
      return;

   /* parfois il faut quitter philab */
   if(!strcmp(argv[0], "exit") || !strcmp(argv[0], "quit"))
   {
      FREE_ARGV();
      return exit_func(str);
   }

   /* On teste les commandes philab */
   while(p_builtin->name != NULL)
   {
      if(!strcmp(p_builtin->name, argv[0]))
      {
	 /* On exécute f ou f2 ou f3 */
	 if(p_builtin->f != NULL)
	    p_builtin->f(argv[1]);
	 else if(p_builtin->f2 != NULL)
	    p_builtin->f2(argv[0], argv[1]);
	 else if(p_builtin->f3 != NULL)
	    p_builtin->f3(argv[0], argv[1], argv[2], outfile);
	 else
	    break;
	 FREE_ARGV();
	 return;
      }
      p_builtin++;
   }

   /* builtin_cd */
   if(!strcmp(argv[0], "cd"))
   {
      builtin_cd(argv[1]);
      FREE_ARGV();
      return;
   }

   /* On teste sur les commandes shell autorisées */
   i = 0;
   shell_cmd = autorised_cmd[0];
   while(shell_cmd != NULL)
   {
      if(!strcmp(argv[0], shell_cmd))
      {
	 external_cmd(argv);
	 FREE_ARGV();
	 return;
      }
      shell_cmd = autorised_cmd[++i];
   }

   /* Si on veut effectuer la somme ou le produit de deux matrices */
   if(argv[1] != NULL && (!strcmp(argv[1], "+")||!strcmp(argv[1], "*")))
   {
      operateur(argv[0], argv[2], argv[1], outfile);
      FREE_ARGV();
      return;
   }

   /* Sinon on essaye d'afficher la matrice argv[0] */
   two_param(PRINT, argv[0]);
   FREE_ARGV();
   return;
}

/* Affiche l'aide en couleurs avec des belles indentations */
void help(char *p)
{
   const builtin *p_builtin = builtin_cmd;

   /* Pour afficher toutes les commandes */
   if(p == NULL)
   {
      while(p_builtin->name != NULL)
      {
	 printf("\033[36m%s\n", p_builtin->name);
	 printf("\033[34m\tDescription : \033[37m%s\n", p_builtin->description);
	 printf("\033[34m\tExample : \033[37m%s\n\n", p_builtin->example);
	 p_builtin++;
      }
      return;
   }
   
   /* Pour afficher seulement l'aide pour une commande */
   while(p_builtin->name != NULL)
   {
      if(!strcmp(p, p_builtin->name))
      {
	 printf("USAGE : \033[36m%s\n\033[34m\tDescription : \033[37m%s\n\033[34m\tExample : \033[37m%s\n\n", p, p_builtin->description, p_builtin->example);
	 return;
      }
      p_builtin++;
   }

   /* Si on arrive ici c'est qu'on a pas trouvé la commande */
   printf("Error : \"%s\" is not a valid philab command\n", p);
   return;
}

/* Permet de charger en mémoire une matrice dans la liste chaînée des matrices */
void load(char *path)
{
   matrix *new, *p_ll = ll_matrix;
   FILE *fd;
   char *pwd = getpwd(), *p, *q;
   if(NULL == path)
      return help("load");

   /* Affiche la liste des matrices chargées en mémoire */
   if(!strcmp(path, "-l"))
   {
      new = ll_matrix;
      while(new != NULL)
      {
	 printf("%s loaded in %s\n", new->file, new->name);
	 new = new->next;
      }
      return;
   }

   /* On teste si le fichier peut être ouvert */
   if (NULL == (fd = fopen(path, "r"))||NULL == pwd)
   {
      fprintf(stderr,"Philab: Unable to open %s file\n", path);
      return;
   }

   /* On ferme le fichier */
   fclose(fd);

   /* 
    * On veut tester si le fichier à la bonne extension
    * à la fin de cette opération p pointera sur le début du
    * nom du fichier 
    */
   if(NULL != (p = strrchr(path, '/')))
      p++;
   else
      p = path;

   if(NULL == (q = strrchr(p, '.'))||strcmp(q, EXTENSION)||q == p)
   {
      fprintf(stderr,"Philab: format error, file must have a "EXTENSION" extension\n");
      free(pwd);
      return;
   }

   /* On ajoute le nouvel élément dans la liste chaînée */
   new = malloc(sizeof(matrix));
   new->name = malloc(sizeof(char) * (q-p));
   strncpy(new->name, p, q-p);
   new->name[q-p] = '\0';

   /* On teste si l'alias n'est pas déjà en cours d'utilisation */
   while(p_ll != NULL)
   {
      if(!strcmp(p_ll->name, new->name))
      {
	 /* Cas de la matrice de sortie par défaut :
	  * on ne la recharge pas */
	 if(!strcmp(p_ll->name, "ans"))
	    return two_param(PRINT, "ans");

	 fprintf(stderr,"Philab: warning: matrix %s is already exists, overwrite...\n", new->name);
	 unload(p_ll->name);
	 break;
      }
      p_ll = p_ll->next;
   }

   /* 
    * On veut mettre le path absolu du fichier dans
    * new->file pour ne pas être dépendant du répertoire
    * courant dans lequel on est...
    */
   if(path[0] == '/')
   {
      new->file = malloc(sizeof(char) * (1+strlen(path)));
      strcpy(new->file, path);
   }
   else
   {
      new->file = malloc(sizeof(char) * (2+strlen(pwd)+strlen(path)));
      strcpy(new->file, pwd);
      strcat(new->file, "/");
      strcat(new->file, path);
   }
   /* On lie le nouvel élément dans la liste chaînée */
   new->next = ll_matrix;
   ll_matrix = new;
   /* On affiche la matrice chargée */
   printf("%s = \n", new->name);
   two_param(PRINT, ll_matrix->name);
   return;
}

/* Permet de décharger une matrice de la mémoire */
void unload(char *name)
{
   matrix *mat, *prev;

   if(name == NULL)
      return help("unload");

   mat = ll_matrix;
   prev = ll_matrix;

   /* Code classique pour la suppression d'un élément dans
    * une liste simplement chaînée */
   while(mat != NULL)
   {
      if(!strcmp(mat->name, name))
      {
	 free(mat->name);
	 free(mat->file);
	 /* TODO : j'ai l'impression qu'il y a un bug ici */
	 if(prev != ll_matrix)
	    prev->next = mat->next;
	 else
	    ll_matrix = ll_matrix->next;
	 free(mat);
	 mat = NULL;
	 return;
      }
      prev = mat;
      mat = mat->next;
   }
   fprintf(stderr,"Philab: %s is not a loaded matrix\n", name);
   return;
}


/* calcul d'une opération + ou * */
void operateur(char *mat1, char *mat2, char *op, char *outfile)
{

   /* Distingue simplement le + du *
    * TODO :  pourquoi ne pas y ajouter gauss avec
    * une syntaxe matlab : A \ b */
   if(!strcmp(op, "+"))
      return tree_param(SUM, mat1, mat2, outfile);
   else if(!strcmp(op, "*"))
      return tree_param(PROD, mat1, mat2, outfile);
   else
      fprintf(stderr,"Philab: Il y a eu une érreur\n");
   return;
}

/* La fonction d'exécution à deux paramètres */
void two_param(char *func, char *mat)
{
   char *cmd[5] = {getenv("RUNTIME_PATH"), func, search_matrix(mat), NULL};

   /* Si l'argument est NULL on renvoie l'aide */
   if(NULL == mat)
      return help(func);

   /* Sinon on exécute la bonne fonction */
   if(cmd[2] != NULL)
      return external_cmd(cmd);
   fprintf(stderr, "Philab: %s n'est pas un nom de matrice chargée, voyez help load\n", mat);
   return;
}


/* La fonction d'exécution à trois paramètres */
void tree_param(char *func, char *mat1, char *mat2, char *outfile)
{
   char *cmd[7] = {getenv("RUNTIME_PATH"), func, search_matrix(mat1), search_matrix(mat2), outfile, NULL};

   /* Si l'argument est NULL on renvoie l'aide */
   if(NULL == mat1 || NULL == mat2)
      return help(func);

   /* Petit Hack pour que la commande norme marche :-) */
   if(!strcmp(func, NORME))
      cmd[2] = mat1;


   if(cmd[2] != NULL && cmd[3] != NULL)
   {
      external_cmd(cmd);

      /* Si les matrices ont été exportés on les charge en mémoire
       * (ce qui va permettre de les afficher entres autres) */
      if(!strcmp(func, SUM) || !strcmp(func, PROD))
      {
	 if(NULL == outfile)
	    load("ans.mat");
	 else
	    load(outfile);
      }
      return;
   }
   fprintf(stderr,"Philab: au moins une des deux matrice %s et %s n'est pas chargée, voyez help load\n", mat1, mat2);
   return;
}

/* Cette fonction renvoie le fichier de la matrice name */
char *search_matrix(char *name)
{
   matrix *p_ll = ll_matrix;

   /* On ne sait jamais */
   if(name == NULL)
      return NULL;

   /* On boucle sur la liste chaînée
    * jusqu'à trouver la matrice */
   while(p_ll != NULL)
   {
      if(!strcmp(p_ll->name, name))
	 return p_ll->file;
      p_ll = p_ll->next;
   }

   return NULL;
}

/* La fonction qui quitte philab
 * elle récupère la dernière saisie pour
 * la libérer et elle libère toute la liste
 * chaînée */
void exit_func(char *str)
{
   matrix *p_ll = ll_matrix, *next;

   /* On libère la saisie */

   free(str);
   /* On parcoure la liste chaînée en libérant
    * chaque élément */
   while(p_ll != NULL)
   {
      free(p_ll->name);
      free(p_ll->file);
      next = p_ll->next;
      free(p_ll);
      p_ll = next;
   }

   /* On quitte le programme */
   exit(0);
   return;
}
