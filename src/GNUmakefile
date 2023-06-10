.PHONY: veryclean clean force export help

ifdef debug
F_OPTS := -ggdb
endif

F:= ifort

SRC := rpn_stack.f90 rpn_stack_sm.f90 linked_list.f90 amap.f90
OBJ := $(SRC:%.f90=%.o)

hp: ../app/main.f90 $(OBJ)
	$(F) -o $@ ../app/main.f90 $(OBJ) $(F_OPTS)

hp.exe: /app/main.f90 GNUmakefile
	$(F) -o $@ $< $(F_OPTS)
	
rpn_stack_sm.o: rpn_stack_sm.f90
	$(F) -c -o $@ $< $(F_OPTS)
	
rpn_stack.o: rpn_stack.f90
	$(F) -c -o $@ $< $(F_OPTS)

linked_list.o: linked_list.f90
	$(F) -c -o $@ $< $(F_OPTS)

amap.o: amap.f90
	$(F) -c -o $@ $< $(F_OPTS)

test_amap: ../test/test_amap.f90 amap.o
	$(F) -o $@ ../test/test_amap.f90 amap.o

clean:
	@rm -vf *.o *.mod *.smod *~

veryclean: clean
	@rm -vf hp hp.exe hp.tar

force: veryclean
	$(MAKE)

export: hp.tar

hp.tar: GNUmakefile ../app/hp.f90 $(SRC)
	tar cf $@ ../app/hp.f90 $(SRC) GNUmakefile

help:
	@echo "SRC = $(SRC)"
	@echo "OBJ = $(OBJ)"
