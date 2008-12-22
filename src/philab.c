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

#include "philab.h"

#ifdef _USE_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif /* _USE_READLINE */

/* La liste des commandes de philab */
const builtin builtin_cmd[] = {
   {"help", "print help", "help [cmd]", help},
   {"load", "load a matrix by a file", "load a.mat (matrix a.mat will be aliased to 'a')", load},
   {"unload", "unload a matrix", "unload A", unload},
   {"print", "print a matrix", "print matrix_name, or simply matrix_name", print},
   {"norm1", "print the norm 1 of a matrix", "norm1 A", NULL},
   {"norminf", "print the infinity norm of a matrix", "norminf A", NULL},
   {"frobenius", "print the frobenius norm of a matrix", "frobenius A", NULL},
   {"+", "Just print the sum of two matrix", "A + B (with spaces please)", NULL},
   {"*", "Just print the multiplication of two matrix", "A * B (with spaces please)", NULL},
   {NULL, NULL, NULL, NULL}
};

const char *autorised_cmd[10] = {"mkdir", "rm", "cp", "mv", "ls", "pwd", "vim", "emacs", "cat", NULL};


matrix *ll_matrix = NULL;

/* init_env() : initialise les variables
 * d'environement */
/* {{{ init_env() */
void init_env(void)
{
   /* TODO : si un seul setenv echoue
    * le programme est unitilisable */
   char *pwd, *p;
   setenv("PHILAB_PROD", PROD_CMD, 1);
   setenv("PHILAB_SUM", SUM_CMD, 1);
   setenv("PHILAB_PRINT", PRINT_CMD, 1);
   pwd = getpwd();
   p = malloc(sizeof(char) * (2+strlen(pwd)+strlen(RUNTIME)));
   strcpy(p, pwd);
   strcat(p, "/"RUNTIME);
   setenv("RUNTIME_PATH", p, 1);
   free(p);
   free(pwd);
   return;
}
/* }}} */

/* Cette fonction renvoie le repertoire courant sous
 * forme d'une chaine de caractère */
/* {{{ getpwd() */
char *getpwd(void)
{
   char *p;
   p = malloc(sizeof(char) * 256);
   if(NULL == getcwd(p, 256))
      strcpy(p, "somewere");
   return p;
}
/* }}} */

/* La commande cd, elle n'est pas comme les autres car elle
 * doit absolument être éxecutée dans le processus courant.
 * (Sinon c'est le processus fils qui changerais son repertoire
 * courant...) */
/* {{{ builtin_cd() */
void builtin_cd(char *path)
{
   /* On change tout simplement le repertoire courant */
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
	    /* On traite les erreurs possibles (Il en manque, mais c'est des cas extremes) */
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
	       fprintf(stderr, "Philab : Impossible de se deplacer vers %s\n", path);
	       break;
	 }
   return;
}
/* }}} */

/* get_prompt renvoie une chaine de caractère qui
 * contient le prompt */
/* {{{ get_prompt() */
char *get_prompt(void)
{
   /* Déclarations */
   uid_t uid; /* l'uid du processus courant */
   struct passwd *user; /* l'entrée utilisateur dans /etc/passwd cf man pwd.h */
   struct utsname host; /* L'hostname de la machine */
   char *pwd, *q, *prompt;
   if(NULL == (pwd = getpwd()))
   {
      prompt = malloc(sizeof(char) * 2);
      strcpy(prompt, ">");
      return prompt;
   }
   /* On va chercher les information dont on a besoin */
   uname(&host);
   uid = getuid(); 
   user = getpwuid(uid);
   q = malloc(sizeof(char) * 1+strlen(pwd));
   /* Cette partie remplace $HOME par ~ dans dans et met
    * la nouvelle chaine obtenue dans q */
   if(!strncmp(pwd, user->pw_dir, strlen(user->pw_dir)))
   {
      strcpy(q, "~");
      strcat(q, pwd+strlen(user->pw_dir));
   }
   else
      strcpy(q, pwd);
   /* On écrit le prompt */
   prompt = malloc(sizeof(char) * 50+strlen(q)+strlen(host.nodename));
   strcpy(prompt, "[\033[31mphilab\033[37m@\033[36m");
   strcat(prompt, host.nodename);
   strcat(prompt, "\033[37m : \033[34m");
   strcat(prompt, q);
   strcat(prompt, "\033[37m] % ");
   free(q);
   free(pwd);
   return prompt;
}
/* }}} */

/* Permet de lancer des commandes externes
 * en dupliquant le processus courant */
/* {{{ external_cmd() */
void external_cmd(char **argv)
{
   /* Le processus est dupliqué fork()
    * le nouveau processus execute la commande
    * et le père (philab) attend la mort du processus
    * fils */
   pid_t pid;
   if(argv[0] == NULL)
      return;
   pid = fork();
   /* Fils */
   if(pid == 0)
   {
      if(argv[0][0] == '/')
	 exit(execv(argv[0], argv));
      else
	 exit(execvp(argv[0], argv));
   }
   /* Avec exit, on s'assure que le processus fils sera quand même tué
    * (si la commande n'existe pas par exemple) */
   /* Père : il attend la mort du fils */
   else if (pid != -1)
      wait(NULL);
   /* Si on est arrivé jusque là c'est qu'il y a un gros
    * problème (plus possible de dupliquer des processus...
    * Donc on quitte brutalement */
   else
      abort();
   return;
}
/* }}} */


/* La fonction main, tout commence ici */
/* {{{ main() */
int main(void)
{
   char *prompt, *saisie;
#ifndef _USE_READLINE
   char *p, c;
#endif
   /* On initialise les variables d'environement */
   init_env();
   /* On affiche le message de bienvennue */
   printf(" ____  _     _ _       _     \n");
   printf("|  _ \\| |__ (_) | __ _| |__  \n");
   printf("| |_) | '_ \\| | |/ _` | '_ \\ \n");
   printf("|  __/| | | | | | (_| | |_) |\n");
   printf("|_|   |_| |_|_|_|\\__,_|_.__/ \n");
   printf("Tapez 'help' pour l'aide \n");
   /* La boucle de philab */
   while(1)
   {
      prompt = get_prompt();
      /* On recupère la saisie avec ou sans readline */
#ifdef _USE_READLINE
      saisie = readline(prompt);
      if(saisie)
	 add_history(saisie);
#else
      printf("%s", prompt);
      saisie = malloc(sizeof(char) * SIZE);
      fgets(saisie, SIZE, stdin);
      if(NULL != (p = strrchr(saisie, '\n')))
	 *p = '\0';
      else
	 while('\n' != (c = fgetc(stdin)) && c != EOF);
#endif /* _USE_READLINE */
      if(!strcmp(saisie, "exit") || !strcmp(saisie, "quit"))
	 break;
      /* make_cmd analyse la saisie et execute ce qu'il faut
       * executer */
      make_cmd(saisie);
      free(saisie);
      free(prompt);
   }
   return 0;
}
/* }}} */

/* {{{ make_cmd() */
void make_cmd(char *str)
{
   char *argv[10];
   const char *p;
   size_t i = 0;
   const builtin *p_builtin = builtin_cmd;
   /***********/
   p = strtok(str, " ");
   while(p != NULL && i < 9)
   {
      argv[i] = malloc(sizeof(char) * (1+strlen(p)));
      strcpy(argv[i++], p);
      p = strtok(NULL, " ");
   }
   while(i < 10)
      argv[i++] = NULL;
   if(argv[0] == NULL)
      return;
   while(p_builtin->name != NULL)
   {
      if(!strcmp(p_builtin->name, argv[0]))
	 if(p_builtin->function != NULL)
	 {
	    p_builtin->function(argv[1]);
	    FREE_ARGV();
	    return;
	 }
      p_builtin++;
   }
   if(!strcmp(argv[0], "cd"))
   {
      builtin_cd(argv[1]);
      FREE_ARGV();
      return;
   }
   i = 0;
   p = autorised_cmd[0];
   while(p != NULL)
   {
      if(!strcmp(argv[0], p))
      {
	 external_cmd(argv);
	 FREE_ARGV();
	 return;
      }
      p = autorised_cmd[++i];
   }
   if(argv[1] != NULL && (!strcmp(argv[1], "+")||!strcmp(argv[1], "*")))
   {
      operateur(argv[0], argv[2], argv[1]);
      FREE_ARGV();
      return;
   }
   print(argv[0]);
   FREE_ARGV();
   return;
}
/* }}} */

/* Affiche l'aide en couleurs avec des belles intentations */
/* {{{ help() */
void help(char *p)
{
   const builtin *p_builtin = builtin_cmd;
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
   while(p_builtin->name != NULL)
   {
      if(!strcmp(p, p_builtin->name))
      {
	 printf("USAGE : \033[36m%s\n\033[34m\tDescription : \033[37m%s\n\033[34m\tExample : \033[37m%s\n\n", p, p_builtin->description, p_builtin->example);
	 return;
      }
      p_builtin++;
   }
   printf("Error : \"%s\" is not a valid philab command\n", p);
   return;
}
/* }}} */

/* {{{ load() */
void load(char *path)
{
   matrix *new, *p_ll = ll_matrix;
   FILE *fd;
   char *pwd = getpwd(), *p, *q;
   if(NULL == path)
      return help("load");
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
   if (NULL == (fd = fopen(path, "r"))||NULL == pwd)
      fprintf(stderr,"Philab: Unable to open %s file\n", path);
   else
   {
      fclose(fd);
      if(NULL != (p = strrchr(path, '/')))
	 p++;
      else
	 p = path;
      if(NULL == (q = strrchr(p, '.'))||strcmp(q, EXTENSION)||q == p)
	 fprintf(stderr,"Philab: format error, file must have a .mat extension\n");
      else
      {
	 new = malloc(sizeof(matrix));
	 new->name = malloc(sizeof(char) * (q-p));
	 strncpy(new->name, p, q-p);
	 new->name[q-p] = '\0';
	 while(p_ll != NULL)
	 {
	    if(!strcmp(p_ll->name, new->name))
	    {
	       fprintf(stderr,"Philab: warning: matrix %s is already exist, overwrite...\n", new->name);
	       unload(p_ll->name);
	       break;
	    }
	    p_ll = p_ll->next;
	 }
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
	 new->next = ll_matrix;
	 ll_matrix = new;
	 print(ll_matrix->name);
      }
   }
   return;
}
/* }}} */

/* {{{ unload() */
void unload(char *name)
{
   matrix *mat = ll_matrix, *prev = ll_matrix;
   if(name == NULL)
      return help("unload");
   while(mat != NULL)
   {
      if(!strcmp(mat->name, name))
      {
	 free(mat->name);
	 free(mat->file);
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
/* }}} */

/* {{{ print() */
void print(char *name)
{
   char *cmd[4];
   matrix *p_mat = ll_matrix;
   if(NULL == name)
      return help("print");
   cmd[0] = getenv("RUNTIME_PATH");
   cmd[1] = getenv("PHILAB_PRINT");
   while(p_mat != NULL)
   {
      if(!strcmp(p_mat->name, name))
      {
	 cmd[2] = p_mat->file;
	 cmd[3] = NULL;
	 printf("%s = \n", p_mat->name);
	 return external_cmd(cmd);
      }
      p_mat = p_mat->next;
   }
   fprintf(stderr, "Philab: %s n'est pas un nom de matrice ni une commande valide\n", name);
   return;
}
/* }}} */

/* {{{ operateur() */
void operateur(char *mat1, char *mat2, char *op)
{
   char *cmd[5] = {NULL, NULL, NULL, NULL, NULL};
   matrix *p_mat = ll_matrix;
   if(NULL == mat1 || NULL == mat2)
      return help(op);
   cmd[0] = getenv("RUNTIME_PATH");
   if('+' == op[0])
      cmd[1] = getenv("PHILAB_SUM");
   else
      cmd[1] = getenv("PHILAB_PROD");
   while(p_mat != NULL)
   {
      if(!strcmp(p_mat->name, mat1))
	 cmd[2] = p_mat->file;
      if(!strcmp(p_mat->name, mat2))
	 cmd[3] = p_mat->file;
      p_mat = p_mat->next;
   }
   if(cmd[2] == NULL)
   {
      fprintf(stderr, "Philab: la matrice '%s' n'existe pas\n", mat1);
      return;
   }
   if(cmd[3] == NULL)
   {
      fprintf(stderr, "Philab: la matrice '%s' n'existe pas\n", mat2);
      return;
   }
   printf("%s %s %s = \n", mat1, op, mat2);
   external_cmd(cmd);
   return;
}
/* }}} */
