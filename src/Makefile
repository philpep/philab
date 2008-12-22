PROG="../philab"
RUNTIME="../runtime"
SRC=$(wildcard *.f90)
OBJ=$(SRC:.f90=.o)


all: $(RUNTIME) $(PROG)

$(RUNTIME): $(OBJ)
	$(CF) -o $@ $^


mod_mat_creuse.o: mod_mat_creuse.f90
mod_normes.o: mod_normes.f90
mod_utils.o: mod_utils.f90
mod_gauss.o: mod_gauss.f90
main.o: main.f90 mod_mat_creuse.o mod_normes.o mod_gauss.o mod_utils.o

%.o: %.f90
	$(CF) -o $@ -c $< $(FLAGS)

$(PROG): philab.c philab.h
	$(CC) -o $@ $< $(CFLAGS) $(LDFLAGS)

.PHONY: clean mrproper

clean:
	rm -f *.o *.mod

mrproper: clean
	rm -f $(RUNTIME) $(PROG)
