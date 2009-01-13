module mod_gauss
use mod_mat_creuse
implicit none


contains

function gaussi (A,b)
integer, dimension(:,:), intent(in) :: A
integer, dimension(:,:), intent(in) :: b
real, dimension(size(A,1),size(A,1)) :: U
integer, dimension(size(A,1),1) :: gaussi
integer :: i,j,k,n
real :: c
n = size(A,1)


if(n /= size(A,2)) then
   write(0,*) 'La matrice A doit être carrée'
elseif (size(b,1) /= n .or. size(b,2) /= 1) then
   write(0,*) 'La matrice b doit être de taille',n,'x1'
else
   gaussi = b
   U = A
   do i = 1,n-1
      do j = i+1,n
         if(U(i,j) /= 0) then
            c = U(j,i)/U(i,i)
            do k = i+1,n
               U(j,k) = U(j,k)-c*U(i,k)
            end do
            gaussi(j,1) = gaussi(j,1) - c*gaussi(i,1)
         else
            write(0,*) 'Un ellement de la diagonale est null !'
            stop ! TODO faire une sortie plus propre
         end if
      end do
   end do
end if
print*,'U ='
print*,U
print*,'b = '
print*,gaussi
end function gaussi

end module mod_gauss
