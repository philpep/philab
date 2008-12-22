!   _   _  ___  ____  __  __ _____ ____  
!  | \ | |/ _ \|  _ \|  \/  | ____/ ___| 
!  |  \| | | | | |_) | |\/| |  _| \___ \ 
!  | |\  | |_| |  _ <| |  | | |___ ___) |
!  |_| \_|\___/|_| \_\_|  |_|_____|____/ 
! Ce module contient les fonctions
! de calculs de normes
! - Norme 1
! - Norme infinie
! - Norme de Frobenius
module mod_normes
use mod_mat_creuse
implicit none

! Interfaces génériques pour les
! différentes normes
! Norme 1
interface norme_1
   module procedure norme_1_i, norme_1_e
end interface norme_1
! Norme 2
interface norme_inf
   module procedure norme_inf_i, norme_inf_e
end interface norme_inf
! Norme de Frobenius
interface frobenius
   module procedure frobenius_i, frobenius_e
end interface frobenius

contains
!  
!   _   _                             _ 
!  | \ | | ___  _ __ _ __ ___   ___  / |
!  |  \| |/ _ \| '__| '_ ` _ \ / _ \ | |
!  | |\  | (_) | |  | | | | | |  __/ | |
!  |_| \_|\___/|_|  |_| |_| |_|\___| |_|
! {{{ norme_1                                      
! Cette fonction calcule la norme 1
! d'un tableau de type 'real'
! La norme 1 est le maximum des sommes
! sur les colonnes (en valeur absolue)
function norme_1_i (A)
implicit none
! Déclaration de la variable d'entrée
integer, dimension(:,:), intent(in) :: A
! Déclaration de la variable de sortie
integer :: norme_1_i
! Ce tableau contiendra les valeurs de
! la somme sur les colonnes
integer, dimension(size(A,2)) :: sum_rows
integer :: i ! Variable qui servira dans la boucle
! Pour chaque colonne, on calcule la
! somme des elements qu'il y a sur cette
! colonne.
do i = 1,size(A,2)
   sum_rows(i) = sum(abs(A(:,i)))
end do
! norme_1_r = valeur maximale des sommes
! sur les colonnes.
norme_1_i = maxval(sum_rows)
end function norme_1_i

! Cette fonction calcule la norme 1 d'une matrice creuse
function norme_1_e (A)
implicit none
type(element), dimension(:), intent(in) :: A
integer :: norme_1_e
real, dimension(maxval(A%ind_c)) :: sum_rows
integer :: i
sum_rows = 0.
do i = 1,size(A)
   sum_rows(A(i)%ind_c) = sum_rows(A(i)%ind_c) + abs(A(i)%val)
end do
norme_1_e = maxval(sum_rows)
end function norme_1_e
! }}}
!   _   _                             _        __ 
!  | \ | | ___  _ __ _ __ ___   ___  (_)_ __  / _|
!  |  \| |/ _ \| '__| '_ ` _ \ / _ \ | | '_ \| |_ 
!  | |\  | (_) | |  | | | | | |  __/ | | | | |  _|
!  |_| \_|\___/|_|  |_| |_| |_|\___| |_|_| |_|_|  
!                                                 
! {{{ norme_inf
! La norme infinie a la même definition
! que la norme 1 en remplaçant 'colonne'
! par 'ligne'
function norme_inf_i (A)
implicit none
integer, dimension(:,:), intent(in) :: A
integer :: norme_inf_i
integer :: i
integer, dimension(size(A,1)) :: sum_ligne
do i = 1,size(A,1)
   sum_ligne(i) = sum(abs(A(i,:)))
end do
norme_inf_i = maxval(sum_ligne)
end function norme_inf_i

! Fonction pour le type matrice creuse 
function norme_inf_e (A)
implicit none
type(element), dimension(:), intent(in) :: A
integer :: norme_inf_e
integer, dimension(maxval(A%ind_l)) :: sum_ligne
integer :: i
sum_ligne = 0.
do i = 1,size(A)
   sum_ligne(A(i)%ind_l) = sum_ligne(A(i)%ind_l) + abs(A(i)%val)
end do
norme_inf_e = maxval(sum_ligne)
end function norme_inf_e
! }}}

!  
!   _____          _                _           
!  |  ___| __ ___ | |__   ___ _ __ (_)_   _ ___ 
!  | |_ | '__/ _ \| '_ \ / _ \ '_ \| | | | / __|
!  |  _|| | | (_) | |_) |  __/ | | | | |_| \__ \
!  |_|  |_|  \___/|_.__/ \___|_| |_|_|\__,_|___/
! {{{ frobenius
! On somme les carrée de tous les coeficients et
! on prend la racine carrée de ce nombre...
function frobenius_i(A)
implicit none
integer, dimension(:,:), intent(in) :: A
real :: frobenius_i
integer :: i,j
frobenius_i = 0.
do i = 1,size(A,1)
   do j = 1,size(A,2)
      frobenius_i = frobenius_i + A(i,j)**2
   end do
end do
frobenius_i = sqrt(frobenius_i)
end function frobenius_i

! Pour le type matrice creuse
function frobenius_e (A)
type(element), dimension(:), intent(in) :: A
real :: frobenius_e
integer :: i
frobenius_e = 0.
do i = 1,size(A)
   frobenius_e = frobenius_e + A(i)%val**2 
end do
frobenius_e = sqrt(frobenius_e)
end function frobenius_e
! }}}


end module mod_normes