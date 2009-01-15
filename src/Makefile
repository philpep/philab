# Le sous makefile
# SRC = tous les fichier .f90
SRC=$(wildcard *.f90)
# OBJ = tous les fichier de SRC avec le suffixe .o
OBJ=$(SRC:.f90=.o)


# La cible par defaut
all: ../runtime ../philab

# Pour construire le runtime il nous faut tous les fichiers .o
# $@ = le nom de la cible
# $^ = toutes les dependances
runtime: ../runtime
../runtime: $(OBJ)
	$(CF) -o $@ $^

# La regle qui permet à partir d'un fichier .f90 d'avoir un fichier .o
# $< = nom de la première dependance
%.o: %.f90
	$(CF) -o $@ -c $< $(FLAGS)

# La liste des dependances pour chaque fichier
mod_mat_creuse.o: mod_mat_creuse.f90

mod_normes.o: mod_normes.f90 mod_mat_creuse.o

mod_utils.o: mod_utils.f90 mod_mat_creuse.o

mod_gauss.o: mod_gauss.f90 mod_mat_creuse.o mod_utils.o

mod_pw_iter.o: mod_pw_iter.f90 mod_mat_creuse.o

mod_trace.o: mod_trace.f90 mod_mat_creuse.o

runtime.o: runtime.f90 mod_mat_creuse.o mod_normes.o mod_gauss.o mod_utils.o mod_trace.o mod_pw_iter.o


# L'interface en C
philab: ../philab
../philab: philab.c philab.h
	$(CC) -o $@ $< $(CFLAGS) $(LDFLAGS)

# Permet de supprimer tous les fichier object et les fichiers de modules
clean:
	rm -f *.o *.mod

# Même chose mais on supprime aussi les programmes
mrproper: clean
	rm -f ../runtime ../philab
