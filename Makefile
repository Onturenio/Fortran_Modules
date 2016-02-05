FC=gfortran
CFLAGS=-O3 -c
LDFLAGS=-L. -I. -O3
LDLIBS=-lfortranmodules
MODULES=mod_Kinds.o mod_Sorting.o mod_Random.o mod_Timers.o mod_LUdcmp.o mod_Cmdline.o


# target: libraries - Compilates modules and creates large static library
libraries: libfortranmodules.a

# target: example - Compiles and runs the test program
example: test
	@echo
	@echo =======================================
	@echo = Running example to test all modules =
	@echo =======================================
	@./test option1 option2 option3

# target: test - Creates simple program to test modules
test: test.f90 libfortranmodules.a
	$(FC) $(LDFLAGS) -o $@ $^

libfortranmodules.a: $(MODULES)
	$(AR) cr $@ $(MODULES)

mod_Kinds.o: mod_Kinds.f03
	$(FC) $(CFLAGS) $<
mod_Sorting.o: mod_Sorting.f90 mod_Kinds.o
	$(FC) $(CFLAGS) $<
mod_Random.o: mod_Random.f03 mod_Kinds.o
	$(FC) $(CFLAGS) $<
mod_Timers.o: mod_Timers.f03 mod_Kinds.o
	$(FC) $(CFLAGS) $<
mod_LUdcmp.o: mod_LUdcmp.f90 mod_Kinds.o
	$(FC) $(CFLAGS) $<
mod_Cmdline.o: mod_Cmdline.f03
	$(FC) $(CFLAGS) $<

# target: clean - Removes intermediate and object files
clean:
	rm -f *.o *.mod test *a

# target: help - Display callable targets.
help:
	@egrep "^# target:" [Mm]akefile

.PHONY: clean help example
