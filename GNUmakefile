.PHONY: veryclean clean force export help test run

F := ifx
BUILD_DIR := build

HP_LIB := $(BUILD_DIR)/libhp.a

# Untested NAG support
ifeq ($(F),nagfor)
F_OPTS := -fpic -I $(BUILD_DIR)

else
F_OPTS := -fpic -module $(BUILD_DIR)

ifdef debug
ifeq ($(F),ifx)
F_OPTS += -g
else
F_OPTS += -ggdb
endif
endif

endif

SRC := $(wildcard src/*.f90)
OBJ := $(SRC:src/%.f90=$(BUILD_DIR)/%.o)

EXE := $(BUILD_DIR)/hp

$(EXE): $(BUILD_DIR) app/main.f90 $(HP_LIB)
	$(F) -o $@ app/main.f90 $(HP_LIB) $(F_OPTS)

$(HP_LIB): $(BUILD_DIR) $(OBJ)
	ar crv $@ $(OBJ)

$(OBJ) : $(BUILD_DIR)/%.o : src/%.f90
	$(F) -c -o $@ $< $(F_OPTS)

test_amap: test/test_amap.f90 $(BUILD_DIR)/amap.o
	$(F) -o $@  $(F_OPTS) test/test_amap.f90 $(BUILD_DIR)/amap.o
	
test: test_amap
	./test_amap

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)
	
clean:
	@rm -vf $(OBJ) $(BUILD_DIR)/*.mod $(BUILD_DIR)/*.smod *~

veryclean: clean
	@rm -vf $(EXE) hp.tar

force: veryclean
	$(MAKE)

export: hp.tar

run: $(EXE)
	./$<
	
hp.tar: GNUmakefile app/main.f90 $(SRC)
	tar cf $@ app/main.f90 $(SRC) GNUmakefile

help:
	@echo "SRC = $(SRC)"
	@echo "OBJ = $(OBJ)"
