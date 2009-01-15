! Ce module permet de calculer la trace d'une
! matrice
module mod_trace
  use mod_mat_creuse

  implicit none

  interface trace
     module procedure trace_i, trace_e
  end interface

contains

  ! Pour le type matrice creuse
  function trace_e(A)
    implicit none
    type(element), dimension(:), intent(in) :: A
    integer :: trace_e
    integer :: i
    trace_e = 0
    ! Il n'y a pas de condition sur les dimensions
    ! de la matrice, puisque toute matrice creuse
    ! peut être une matrice carrée
    do i = 1,size(A)
       ! Si ind_l == ind_c alors on est sur la
       ! diagonale
       if(A(i)%ind_l == A(i)%ind_c) then
          trace_e = trace_e + A(i)%val
       end if
    end do
  end function trace_e

  ! Pour le type matrice integer
  function trace_i(A)
    implicit none
    integer, dimension(:,:), intent(in) :: A
    integer :: trace_i
    integer :: i
    trace_i = 0
    ! On teste les dimension de la matrice
    if(size(A,1) /= size(A,2)) then
       write(0,*) 'La matrice doit être carrée'
    else
       ! On somme les coefficients diagonaux
       do i = 1,size(A,1)
          trace_i = trace_i + A(i,i)
       end do
    end if
  end function trace_i

end module mod_trace
