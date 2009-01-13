module mod_pw_iter
  ! Methode de la puissance itérée
  use mod_mat_creuse
  implicit none

contains

  function pw_iter(A,u)
    integer, dimension(:,:), intent(in) :: A
    integer, dimension(:,:), intent(in) :: u
    integer, dimension(size(A,1),size(u,2)) :: pw_iter
    integer :: i, n = 10000

    if(size(u,2) /= 1 .or. size(A,1) /= size(A,2)) then
       write(0,*) 'Error'
    else
       pw_iter = u/maxval(u(:,1))

       do i = 1,n
          pw_iter = matmul(A,pw_iter)
          pw_iter = pw_iter/maxval(pw_iter(:,1))
       end do
    end if
  end function pw_iter

end module mod_pw_iter
