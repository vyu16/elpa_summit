FC = mpif90
FFLAGS = -O2

CUDA_LIB = -cudalib=cublas
ELPA_VER = 2022.05.001
ELPA_INC = -I/path/to/elpa/build/include/elpa-$(ELPA_VER)/modules
ELPA_LIB = -L/path/to/elpa/build/lib -lelpa
MATH_LIB = -L/path/to/scalapack -lscalapack -L$(OLCF_ESSL_ROOT)/lib64 -lessl $(OLCF_NETLIB_LAPACK_ROOT)/lib64/liblapack.a

SRC = test_mpi_cmplx.f90
EXE = $(SRC:.f90=.x)

all: $(EXE)

%.x: %.f90
	$(FC) -o $@ $< $(ELPA_INC) $(ELPA_LIB) $(CUDA_LIB) $(MATH_LIB)

clean:
	rm -f *.[ox]
