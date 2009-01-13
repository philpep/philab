module mod_gauss
use mod_mat_creuse
use mod_utils
implicit none


contains

! La methode de gauss pour les matrice integer
! Resolution d'un système Ax = b où A est de dimension n x n
! et b de dimension n x 1
function gaussi (A,b)
! Pour fortran dimension(:,1) est différent de dimension(:)
! Donc on est obligé de faire des matrices dimension(size(A,1),1)...
integer, dimension(:,:), intent(in) :: A
integer, dimension(:,:), intent(in) :: b
! Les matrices integer ne permettent pas de stocker
! des nombres reels, donc on les convertis :-)
real, dimension(size(A,1),size(A,1)) :: U
real, dimension(size(A,1),1) :: gaussi,v
integer :: i,j,k,n
real :: c
! n = dimension de la matrice b
n = size(A,1)
! On initialise le resultat à 0
gaussi = 0

! Methode de gauss classique avec des tests
! sur les érreurs
if(n /= size(A,2)) then
   write(0,*) 'La matrice A doit être carrée'
elseif (size(b,1) /= n .or. size(b,2) /= 1) then
   write(0,*) 'La matrice b doit être de taille',n,'x1'
else
   v = b
   U = A
   do i = 1,n-1
      do j = i+1,n
         if(U(i,i) /= 0) then
            c = U(j,i)/U(i,i)
            do k = i+1,n
               ! Pour se convaincre qu'on triangularise U superieurement :-)
               ! print*,'U(',j,',',k,') = U(',j,',',k,')-',c,'*U(',i,',',k,')'
               U(j,k) = U(j,k)-c*U(i,k)
            end do
            v(j,1) = v(j,1) - c*v(i,1)
         else
            write(0,*) 'Un ellement de la diagonale est null !'
            stop ! TODO faire une sortie plus propre
         end if
      end do
   end do
end if

! Ici resoud le système Ux = v avec U triangulaire,
! en réalité elle ne l'est pas, mais ça n'influence pas
! le calcul.
gaussi = v
gaussi(n,1) = gaussi(n,1)/U(n,n)
! On recherche la ième solution sachant qu'on a déjà
! celles de i+1 à n
do i = n-1,1,-1
   do k = i+1,n
      gaussi(i,1) = gaussi(i,1) - U(i,k)*gaussi(k,1)
   end do
   ! Ne pas oublier de diviser x par son "pivot"
   gaussi(i,1) = gaussi(i,1)/U(i,i)
end do
end function gaussi

end module mod_gauss
