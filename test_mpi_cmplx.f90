! This program tests ELPA.
program test_mpi_cmplx

   use ELPA
   use MPI

   implicit none

   integer, parameter :: dp = selected_real_kind(15,300)

   character(len=10) :: arg1
   character(len=10) :: arg2
   character(len=10) :: arg3

   integer :: nptot
   integer :: nprow
   integer :: npcol
   integer :: myid
   integer :: myprow
   integer :: mypcol
   integer :: comm
   integer :: ierr
   integer :: blk
   integer :: ctxt
   integer :: desc(9)
   integer :: ndim
   integer :: nevc
   integer :: nlrow
   integer :: nlcol
   integer :: ldm
   integer :: n
   integer :: i

   real(dp) :: t1
   real(dp) :: t2

   integer, allocatable :: seed(:)

   real(dp), allocatable :: tmpr(:,:)
   real(dp), allocatable :: eval(:)

   complex(dp), allocatable :: mat(:,:)
   complex(dp), allocatable :: tmpc(:,:)
   complex(dp), allocatable :: evec(:,:)

   class(elpa_t), pointer :: eh

   integer, external :: numroc

   complex(dp), parameter :: one = (1.0_dp,0.0_dp)

   ! Initialize MPI
   call MPI_Init(ierr)
   comm = MPI_COMM_WORLD
   call MPI_Comm_size(comm,nptot,ierr)
   call MPI_Comm_rank(comm,myid,ierr)

   ! Read command line arguments
   if(command_argument_count() == 3) then
      call get_command_argument(1,arg1)
      call get_command_argument(2,arg2)
      call get_command_argument(3,arg3)

      read(arg1,*) ndim
      if(ndim <= 0) then
         ndim = 1000
      end if

      read(arg2,*) nevc
      if(nevc < 0 .or. nevc > ndim) then
         nevc = ndim
      end if

      read(arg3,*) blk
   else
      if(myid == 0) then
         write(*,"(2X,A)") "################################################"
         write(*,"(2X,A)") "##  Wrong number of command line arguments!!  ##"
         write(*,"(2X,A)") "##  Arg#1: Size of test matrix.               ##"
         write(*,"(2X,A)") "##  Arg#2: Number of eigenvectors to compute. ##"
         write(*,"(2X,A)") "##  Arg#3: Block size                         ##"
         write(*,"(2X,A)") "################################################"
         call MPI_Abort(comm,0,ierr)
         stop
      end if
   end if

   ! Set up square-like processor grid
   do npcol = nint(sqrt(real(nptot))),2,-1
      if(mod(nptot,npcol) == 0) exit
   end do
   nprow = nptot/npcol

   ! Set up BLACS
   ctxt = comm

   call BLACS_Gridinit(ctxt,"r",nprow,npcol)
   call BLACS_Gridinfo(ctxt,nprow,npcol,myprow,mypcol)

   nlrow = numroc(ndim,blk,myprow,0,nprow)
   nlcol = numroc(ndim,blk,mypcol,0,npcol)

   call descinit(desc,ndim,ndim,blk,blk,0,0,ctxt,max(nlrow,1),ierr)

   ! Generate a random matrix
   call random_seed(size=n)

   allocate(seed(n))
   allocate(mat(nlrow,nlcol))
   allocate(tmpc(nlrow,nlcol))
   allocate(tmpr(nlrow,nlcol))

   seed(:) = myid+1

   call random_seed(put=seed)
   call random_number(tmpr)

   ! Symmetrize test matrix
   tmpc(:,:) = tmpr+(0.0_dp,1.0_dp)*tmpr

   call pztranc(ndim,ndim,one,tmpc,1,1,desc,one,mat,1,1,desc)

   deallocate(tmpc)
   deallocate(tmpr)
   deallocate(seed)
   allocate(evec(nlrow,nlcol))
   allocate(eval(ndim))

   if(myid == 0) then
      write(*,*)
      write(*,"(2X,A)") "Complex test matrix generated"
      write(*,"(2X,A,I10)") "| Matrix size  :",ndim
      write(*,"(2X,A,I10)") "| Eigenvectors :",nevc
      write(*,"(2X,A,I10)") "| Block size   :",blk
      write(*,"(2X,A,I10)") "| MPI tasks    :",nptot
      write(*,*)
   end if

   ! Initialize ELPA
   ierr = elpa_init(20180525)
   eh => elpa_allocate(ierr)

   call eh%set("na",ndim,ierr)
   call eh%set("nev",nevc,ierr)
   call eh%set("nblk",blk,ierr)
   call eh%set("local_nrows",nlrow,ierr)
   call eh%set("local_ncols",nlcol,ierr)
   call eh%set("mpi_comm_parent",comm,ierr)
   call eh%set("process_row",myprow,ierr)
   call eh%set("process_col",mypcol,ierr)
   call eh%set("timings",1,ierr)

   ierr = eh%setup()

   if(ierr /= 0) then
      write(*,"(2X,A)") "Error: setup"
      call MPI_Abort(comm,0,ierr)
      stop
   end if

   t1 = MPI_Wtime()

   ! Solve
   call eh%eigenvectors(mat,eval,evec,ierr)

   t2 = MPI_Wtime()

   if(ierr /= 0) then
      write(*,"(2X,A)") "Error: solve"
      call MPI_Abort(comm,0,ierr)
      stop
   end if

   if(myid == 0) then
      write(*,"(2X,A)") "ELPA solver finished"
      write(*,"(2X,A,F10.3,A)") "| Time  :",t2-t1,"s"

      call eh%print_times()
   end if

   ! Finalize ELPA
   call elpa_deallocate(eh,ierr)

   nullify(eh)

   deallocate(mat)
   deallocate(eval)
   deallocate(evec)

   call BLACS_Gridexit(ctxt)
   call BLACS_Exit(1)
   call MPI_Finalize(ierr)

end program
