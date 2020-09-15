! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The musica_math module

!> Commonly used math functions
module musica_math

  use musica_constants,                only : musica_dk, musica_ik

  implicit none
  private

  public :: chebyshev

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the value of a Chebyshev polynomial at a given point
  !!
  !! The algoritm is based on \citep{William1992} page 193.
  !!
  !! If an out-of-bounds value is provided and x is out of the range
  !! \f$a<=x<=b\f$, the out-of-bounds value will be returned. If an
  !! out-of-bounds value is not provided and x is out-of-bounds, an error
  !! is thrown.
  !!
  real(kind=musica_dk) function chebyshev( a, b, c, m, x, out_of_bounds_value )

    use musica_assert,                 only : die

    !> Lower bound of parameterization
    real(kind=musica_dk), intent(in) :: a
    !> Upper bound of parameterization
    real(kind=musica_dk), intent(in) :: b
    !> Chebyshev coefficients c[1...m]
    real(kind=musica_dk), intent(in) :: c(:)
    !> Number of elements of c[] to use in calculation
    integer(kind=musica_ik), intent(in) :: m
    !> Independent variable
    real(kind=musica_dk), intent(in) :: x
    !> Out-of-bounds value
    real(kind=musica_dk), intent(in), optional :: out_of_bounds_value

    integer(kind=musica_ik) :: j
    real(kind=musica_dk) :: d, dd, sv, y, y2

    if( ( x - a ) * ( x - b ) > 0._musica_dk ) then
      if( present( out_of_bounds_value ) ) then
        chebyshev = out_of_bounds_value
      else
        call die( 155206939 )
      end if
    else
      d  = 0._musica_dk
      dd = 0._musica_dk
      y  = ( 2._musica_dk * x - a - b ) / ( b - a )
      y2 = 2._musica_dk * y
      do j = m, 2, -1
        sv = d
        d  = y2 * d - dd + real( c( j ) )
        dd = sv
      end do
      chebyshev = y * d - dd + 0.5_musica_dk * real( c( 1 ) )
    end if

  end function chebyshev

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module musica_math
