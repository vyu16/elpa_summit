Loaded modules:

```
1) lsf-tools/2.0
2) hsi/5.0.2.p5
3) xalt/1.2.1
4) DefApps
5) nvhpc/21.9
6) nsight-compute/2021.2.1
7) nsight-systems/2021.3.1.54
8) cuda/11.0.3
9) spectrum-mpi/10.4.0.3-20210112
10) essl/6.3.0
11) netlib-lapack/3.9.1
12) git/2.36.1
```

ELPA:

```
wget https://elpa.mpcdf.mpg.de/software/tarball-archive/Releases/2022.05.001/elpa-2022.05.001.tar.gz
tar zxf elpa-2022.05.001.tar.gz
cd elpa-2022.05.001
mkdir build
cd build
../configure --prefix=`pwd` FC=mpif90 CC=mpicc FCFLAGS=-O2 CFLAGS=-O2 CPP='cpp -E' LDFLAGS='-L/ccs/home/vwzyu/opt/scalapack-2.2.0 -lscalapack -L/sw/summit/essl/6.3.0/essl/6.3/lib64 -lessl -L/sw/summit/spack-envs/base/opt/linux-rhel8-ppc64le/nvhpc-21.9/netlib-lapack-3.9.1-ujqhjoujj2ycjdgzuzscvqmicdicltnc/lib64 -llapack' --enable-nvidia-gpu --with-cuda-path=/sw/summit/cuda/11.0.3 --with-NVIDIA-GPU-compute-capability=sm_70 --disable-sse-assembly --disable-sse --disable-avx --disable-avx2 --disable-avx512
make -j8
make install
```
