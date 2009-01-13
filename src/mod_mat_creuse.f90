module mod_mat_creuse
implicit none
! Definition du type matrice creuse
type element
   integer :: val
   integer :: ind_l, ind_c
end type element

! Surcharge de + et * pour le type element
interface operator (+)
   module procedure somme_ei, somme_ee
end interface

interface operator (*)
   module procedure prod_ie, prod_ee, prod_ei
end interface

contains


! La fonction mat2mat permet de convertir une
! matrice creuse en un matrice normale
function mat2mat (A)
   implicit none

   type(element), dimension(:), intent(in) :: A
   integer, dimension(maxval(A%ind_l),maxval(A%ind_c)) :: mat2mat
   integer :: i

   mat2mat = 0

   ! Rien de bien compliqué
   do i = 1,size(A)
      mat2mat(A(i)%ind_l,A(i)%ind_c) = A(i)%val
   end do

end function mat2mat

! Surcharge d'operateurs. La methode est toujours la
! même mais avec différents types.

! matrice creuse + matrice creuse
function somme_ee (A, B)
   
   implicit none
   type(element), dimension(:), intent(in) :: A,B
   ! On peut considerer qu'une matrice creuse est de dimension infinie avec des 0
   ! partout
   integer, dimension(max(maxval(A%ind_l),maxval(B%ind_l)),max(maxval(A%ind_c),maxval(B%ind_c))) :: somme_ee
   integer :: i
   somme_ee = 0
   
   do i = 1,size(A)
      somme_ee(A(i)%ind_l, A(i)%ind_c) = A(i)%val
   end do
   
   do i = 1,size(B)
      somme_ee(B(i)%ind_l, B(i)%ind_c) = somme_ee(B(i)%ind_l, B(i)%ind_c) + B(i)%val
   end do
end function somme_ee

! matrice creuse + matrice integer
! La methode est pratiquement la même
function somme_ei (A,B)
   implicit none
   type(element), dimension(:), intent(in) :: A
   integer, dimension(:,:), intent(in) :: B
   integer, dimension(size(B,1), size(B,2)) :: somme_ei
   integer :: n,m,i
   
   n = maxval(A%ind_l)
   m = maxval(A%ind_c)
   
   somme_ei = 0
   
   if(n > size(B,1) .or. m > size(B,2)) then
      write(0,*) 'Array must have the same dimension'
   else
   
      somme_ei = B
      do i = 1,size(A)
         somme_ei(A(i)%ind_l, A(i)%ind_c) = somme_ei(A(i)%ind_l,A(i)%ind_c) + A(i)%val
      end do
   
   end if
end function somme_ei

! Les fonctions de produit :

! matrice creuse * matrice creuse
function prod_ee (A,B)
   
   implicit none
   type(element), dimension(:), intent(in) :: A,B
   integer, dimension(maxval(A%ind_l),maxval(B%ind_c)) :: prod_ee
   prod_ee = 0
   ! TODO
end function prod_ee

! Matrice creuse * matrice integer
function prod_ei (A,B)
   implicit none
   
   type(element), dimension(:), intent(in) :: A
   integer, dimension(:,:), intent(in) :: B
   integer, dimension(maxval(A%ind_l), size(B,2)) :: prod_ei
   integer :: i,j
   
   prod_ei = 0
   
   ! On peut supposer qu'une matrice creuse est de
   ! dimensions infinie (une infinitée de 0), d'où la
   ! condition > et non /=
   
   if (maxval(A%ind_c) > size(B,1)) then
      write(0,*) 'Matrix must have good dimension'
   else
   
      ! Pour chaque valeur non nulle de A
      ! on rajoute le produit valeur*B(ind_c,j) au
      ! vecteur prod_ei(ind_l,:)
      ! Je ne suis pas sûr que ce soit le plus rapide
      ! mais ça marche bien :)
      do i = 1,size(A)
         do j = 1,size(B,2)
            prod_ei(A(i)%ind_l,j) = prod_ei(A(i)%ind_l,j) + A(i)%val*B(A(i)%ind_c,j)
         end do
      end do
   
   end if
   
end function prod_ei

! le produit n'est pas comutatif :-)
! Matrice integer * matrice creuse
! Même methode en remplacant ligne par colonne
function prod_ie (B,A)
   
   implicit none
   integer, dimension(:,:), intent(in) :: B
   type(element), dimension(:), intent(in) :: A
   integer, dimension(size(B,1),maxval(A%ind_c)) :: prod_ie
   integer :: i,j
   
   prod_ie = 0
   
   if(size(B,2) < maxval(A%ind_l)) then
      write(0,*) 'Matrix must have good dimension'
   else
   
      do i = 1,size(A)
         do j = 1,size(B,1)
            prod_ie(j,A(i)%ind_c) = prod_ie(j,A(i)%ind_c) + A(i)%val*B(j,A(i)%ind_l)
         end do
      end do
   
   end if
   
end function prod_ie


end module mod_mat_creuse

