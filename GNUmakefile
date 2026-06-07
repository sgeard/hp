.PHONY: all clean veryclean distclean utest ftest help
.SUFFIXES:
.DEFAULT_GOAL := all

F_EXTRA_GF  := --check=all -frealloc-lhs -Wno-unused-dummy-argument -Wno-unused-value

# --- Compiler selection: default ifx (release), validated ------------------
F ?= ifx
VALID_F := gfortran ifx lfortran flang
ifeq ($(filter $(F),$(VALID_F)),)
  $(error Unknown Fortran compiler 'F=$(F)' -- choose one of: $(VALID_F))
endif

# Canonical compiler options, generated into foptions_$(F).mk by generate_fopts.tcl
OPTIONS_FNAME := foptions_$(F).mk
$(OPTIONS_FNAME): generate_fopts.tcl
	tclsh generate_fopts.tcl $(F) $(OPTIONS_FNAME)

-include $(OPTIONS_FNAME)

# gfortran/flang mishandle LEN-parameterised module variables (a module-scope
# stack_t(5) yields a 0-sized component), so they use the fixed-size (non-PDT)
# implementation; PDT stays the ifx/lfortran path. rpn_stack.f90 self-selects
# this from the compiler's own predefined macro, so no -DNO_PDT is needed here
# -- this also makes the fpm build correct for every compiler.

all: $(OPTIONS_FNAME) hp$(EXT) utest_numerical$(EXT)

# External autodiff library (forward-mode AD)
ADIFF_DIR := ../../autodiff-git
ADIFF_LIB := avd

idir    := $(ADIFF_DIR)/$(ODIR)
AVD_LIB := $(idir)/lib$(ADIFF_LIB).a
AVD_MOD := $(idir)/avd.mod

# Build the autodiff library if needed
$(AVD_LIB) $(AVD_MOD):
	$(MAKE) -C $(ADIFF_DIR) F=$(F) $(if $(debug),debug=1)

F_OPTS  := $(F_BASE) $(F_BUILD) $(MOD_OPTS) -I$(idir)
LFLAGS  := $(F_LOPTS) -L$(idir) -l$(ADIFF_LIB)

# --- Sources (modules live in src/, the program in app/, tests in test/) ---
HP_SRC = numerical.f90 numerical_sm.f90 hp_maths.f90 hp_maths_sm.f90 \
         linked_list.f90 linked_list_sm.f90 amap.f90 \
         rpn_stack.f90 rpn_stack_sm.f90 \
         calc_state.f90 calc_state_sm.f90 \
         clib.f90

depends.mk: fortran_deps.tcl $(addprefix src/,$(HP_SRC))
	tclsh $< src $@ app/main.f90 test/utest_numerical.f90

-include depends.mk

# --- Compilation rules ---
# Grouped targets (&:, requires Make 4.3+): each module source produces both
# .o and .mod in one compilation. Downstream files depend on .mod (the interface),
# not .o (the implementation), so changing a submodule doesn't cascade unnecessarily.

$(ODIR)/numerical.o $(ODIR)/numerical.mod &: src/numerical.f90 $(AVD_MOD) | $(ODIR)
	$(F) -c $(F_OPTS) -o $(ODIR)/numerical.o $<
	@touch $(ODIR)/numerical.mod

$(ODIR)/linked_list.o $(ODIR)/linked_list.mod &: src/linked_list.f90 | $(ODIR)
	$(F) -c $(F_OPTS) -o $(ODIR)/linked_list.o $<
	@touch $(ODIR)/linked_list.mod

$(ODIR)/amap.o $(ODIR)/amap.mod &: src/amap.f90 | $(ODIR)
	$(F) -c $(F_OPTS) -o $(ODIR)/amap.o $<
	@touch $(ODIR)/amap.mod

# Pure elementary-maths kernels (no autodiff dependency)
$(ODIR)/hp_maths.o $(ODIR)/hp_maths.mod &: src/hp_maths.f90 | $(ODIR)
	$(F) -c $(F_OPTS) -o $(ODIR)/hp_maths.o $<
	@touch $(ODIR)/hp_maths.mod

# libc wrappers (signal/_exit graceful-exit shim, iso_c_binding only)
$(ODIR)/clib.o $(ODIR)/clib.mod &: src/clib.f90 | $(ODIR)
	$(F) -c $(F_OPTS) -o $(ODIR)/clib.o $<
	@touch $(ODIR)/clib.mod

# Submodules depend on their parent's .mod (interface), not .o (implementation)
$(ODIR)/numerical_sm.o: src/numerical_sm.f90 $(ODIR)/numerical.mod $(AVD_MOD) | $(ODIR)
	$(F) -c $(F_OPTS) -o $@ $<

$(ODIR)/hp_maths_sm.o: src/hp_maths_sm.f90 $(ODIR)/hp_maths.mod | $(ODIR)
	$(F) -c $(F_OPTS) -o $@ $<

$(ODIR)/linked_list_sm.o: src/linked_list_sm.f90 $(ODIR)/linked_list.mod | $(ODIR)
	$(F) -c $(F_OPTS) -o $@ $<

$(ODIR)/rpn_stack.o $(ODIR)/rpn_stack.mod &: src/rpn_stack.f90 $(ODIR)/numerical.mod $(AVD_MOD) | $(ODIR)
	$(F) -c $(F_OPTS) -o $(ODIR)/rpn_stack.o $<
	@touch $(ODIR)/rpn_stack.mod

$(ODIR)/rpn_stack_sm.o: src/rpn_stack_sm.f90 $(ODIR)/rpn_stack.mod $(ODIR)/numerical.mod $(ODIR)/hp_maths.mod $(AVD_MOD) | $(ODIR)
	$(F) -c $(F_OPTS) -o $@ $<

# Calculator state module + its implementation submodule
$(ODIR)/calc_state.o $(ODIR)/calc_state.mod &: src/calc_state.f90 $(ODIR)/rpn_stack.mod $(ODIR)/amap.mod | $(ODIR)
	$(F) -c $(F_OPTS) -o $(ODIR)/calc_state.o $<
	@touch $(ODIR)/calc_state.mod

$(ODIR)/calc_state_sm.o: src/calc_state_sm.f90 $(ODIR)/calc_state.mod $(ODIR)/rpn_stack.mod | $(ODIR)
	$(F) -c $(F_OPTS) -o $@ $<

# --- Targets ---

UTEST_OBJS := $(ODIR)/numerical.o $(ODIR)/numerical_sm.o $(ODIR)/hp_maths.o $(ODIR)/hp_maths_sm.o

HP_OBJS := $(addprefix $(ODIR)/,$(HP_SRC:.f90=.o))

utest: utest_numerical$(EXT)

utest_numerical$(EXT): test/utest_numerical.f90 $(UTEST_OBJS) $(AVD_LIB) | $(ODIR)
	$(F) $(F_OPTS) -o $@ $< $(UTEST_OBJS) $(LFLAGS)

hp$(EXT): app/main.f90 $(HP_OBJS) $(AVD_LIB) | $(ODIR)
	$(F) $(F_OPTS) -o $@ $< $(HP_OBJS) $(LFLAGS)

$(ODIR):
	mkdir -p $@

ftest: hp$(EXT)
	@tclsh test_hp.tcl

clean:
	@rm -vf $(ODIR)/*.o $(ODIR)/*.mod $(ODIR)/*.smod *~ hp$(EXT) utest_numerical$(EXT) $(OPTIONS_FNAME)

veryclean: clean
	@rm -vfr $(ODIR)

distclean:
	@rm -vrf obj_* *~ *.mod *.smod foptions_*.mk depends.mk hp hp_d utest_numerical utest_numerical_d

help:
	@echo "Targets : all, utest, ftest, clean, veryclean, distclean"
	@echo "Options : F=gfortran|ifx|lfortran|flang (default ifx)  debug=1"
	@echo "HP_SRC  = $(HP_SRC)"
	@echo "HP_OBJS = $(HP_OBJS)"
	@echo "ODIR    = $(ODIR)"
	@echo "F_OPTS  = $(F_OPTS)"
