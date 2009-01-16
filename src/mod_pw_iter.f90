module mod_pw_iter
  ! Méthode de la puissance itérée
  use mod_mat_creuse
  implicit none

contains

  function pw_iter(A,u)

    implicit none
    integer, dimension(:,:), intent(in) :: A
    integer, dimension(:,:), intent(in) :: u
    real, dimension(size(u,1)) :: x,y
    real :: pw_iter
    real :: eps = 1

    pw_iter = 0.

    ! On teste si la matrice est bien carrée et si u est bien un vecteur
    if(size(u,2) /= 1 .or. size(A,1) /= size(A,2) .or. size(u,1) /= size(A,2)) then
       write(0,*) 'Matrices non conformes'
    else
       x  = u(:,1)
       ! On teste si le vecteur est bien de norme 1
       if(maxval(abs(x)) /= 1) then
          write(0,*) 'Erreur, le premier vecteur doit être de norme infinie = 1'
       else
          ! Ici commence l'algorithme
          do while(eps > 0.001)
             y = x
             x = matmul(A,x)/maxval(abs(matmul(A,x)))
             eps = maxval(abs(x-y))
             print*,'eps =', eps
          end do
          pw_iter = maxval(abs(matmul(A,x)))
       end if

    end if
  end function pw_iter

end module mod_pw_iter
