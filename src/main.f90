program main
use mod_mat_creuse
use mod_normes
use mod_utils
implicit none
character (len=256), dimension(4) :: argv
character (len=256) :: outfile
character (len=256), dimension(2) :: infile
character, dimension(2) :: T
type(element), dimension(:), allocatable :: A,B
integer, dimension(:,:), allocatable :: C,D
integer :: i

do i = 1,min(iargc(),4)
   call getarg(i, argv(i))
end do

select case (iargc())
case(0,1)
   print*,'USAGE : todo'
   stop
case (2)
   if(argv(1) == 'prod' .or. argv(1) == 'sum') then
      write(0,*) 'We need 2 operands for',argv(1)
      stop
   end if
   infile(1) = argv(2)
case (3)
   if(argv(1) == 'prod' .or. argv(1) == 'sum') then
      outfile = 'ans.mat'
   end if
   infile(1) = argv(2)
   infile(2) = argv(3)
case default
   infile(1) = argv(2)
   infile(2) = argv(3)
   outfile = argv(4)
end select

open(unit=15, status='OLD', file=infile(1))
read(15,*) T(1)
close(15)
if(T(1) == 'C') then
   call readmat(A,infile(1))
   elseif (T(1) == 'I') then
   call readmat(C,infile(1))
else
   write(0,*) 'Format error :: ', infile(1)
   stop
end if

if (iargc() > 2) then
   open(unit=15, status='OLD', file=infile(2))
   read(15,*) T(2)
   close(15)
   if(T(2) == 'C') then
      call readmat(B,infile(2))
      elseif (T(2) == 'I') then
      call readmat(D,infile(2))
   else
      write(0,*) 'Format error :: ', infile(2)
      stop
   end if
end if



select case (argv(1))
case('norme1')
   if(T(1) == 'C') then
      print*,norme_1(A)
   else
      print*,norme_1(C)
   end if
case('normeinf')
   if(T(1) == 'C') then
      print*,norme_inf(A)
   else
      print*,norme_inf(C)
   end if
case('frobenius')
   if(T(1) == 'C') then
      print*,frobenius(A)
   else
      print*,frobenius(C)
   end if
case('print')
   if(T(1) == 'C') then
      call mprint(A)
   else
      call mprint(C)
   end if
case('prod')
   if(T(1) == 'C' .and. T(2) == 'C') then
      call mprint(A*B)
   elseif (T(1) == 'C' .and. T(2) == 'I') then
      call mprint(A*D)
   elseif (T(1) == 'I' .and. T(2) == 'C') then
      call mprint(C*B)
   elseif (T(1) == 'I' .and. T(2) == 'I') then
      call mprint(matmul(C, D))
   else
      print*,'FATAL ERROR'
   end if
case('sum')
   if(T(1) == 'C' .and. T(2) == 'C') then
      call mprint(A+B)
   elseif (T(1) == 'C' .and. T(2) == 'I') then
      call mprint(A+D)
   elseif (T(1) == 'I' .and. T(2) == 'C') then
      call mprint(B+C)
   elseif (T(1) == 'I' .and. T(2) == 'I') then
      call mprint(C+D)
   else
      print*,'FATAL ERROR'
   end if
case default
   print*,'foo bar'
end select


! On libère la memoire
if(allocated(A)) then
   deallocate(A)
end if
if(allocated(B)) then
   deallocate(B)
end if
if(allocated(C)) then
   deallocate(C)
end if
if(allocated(D)) then
   deallocate(D)
end if

end program main
