SRC=$(wildcard *.f90)
OBJ=$(SRC:.f90=.o)


all: ../runtime ../philab

runtime: ../runtime
../runtime: $(OBJ)
	$(CF) -o $@ $^

%.o: %.f90
	$(CF) -o $@ -c $< $(FLAGS)

mod_mat_creuse.o: mod_mat_creuse.f90
mod_normes.o: mod_normes.f90
mod_utils.o: mod_utils.f90
mod_gauss.o: mod_gauss.f90
main.o: main.f90 mod_mat_creuse.o mod_normes.o mod_gauss.o mod_utils.o

philab: ../philab
../philab: philab.c philab.h
	$(CC) -o $@ $< $(CFLAGS) $(LDFLAGS)

.PHONY: clean mrproper

clean:
	rm -f *.o *.mod

mrproper: clean
	rm -f ../runtime ../philab
