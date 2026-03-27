.PHONY: veryclean clean force export help test run

F_EXTRA_GF  := --check=all -frealloc-lhs -Wno-unused-dummy-argument -Wno-unused-value
F_LOPTS_GF  := -Wl,-z,execstack
F_LOPTS_IFX := -Wl,-z,noexecstack

foptions.mk: generate_fopts.tcl
	tclsh9.1 $<

include foptions.mk

BUILD_DIR := $(ODIR)
HP_LIB    := $(BUILD_DIR)/libhp.a

SRC  := $(wildcard src/*.f90)
OBJ  := $(SRC:src/%.f90=$(BUILD_DIR)/%.o)

# --- Module dependencies (auto-generated) ---
depends.mk: fortran_deps.tcl $(SRC) app/main.f90
	tclsh9.1 $< src $@ app/main.f90

-include depends.mk

EXE := $(BUILD_DIR)/hp

all: foptions.mk $(EXE)

$(EXE): $(BUILD_DIR) app/main.f90 $(HP_LIB)
	$(F) -o $@ app/main.f90 $(HP_LIB) $(F_OPTS) $(F_LOPTS)

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
