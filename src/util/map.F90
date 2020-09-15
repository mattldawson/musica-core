! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!>\file
!> The musica_map module

!> The map_t type and related functions for mapping between datasets.
module musica_map

  use musica_constants,                only : musica_dk, musica_ik

  implicit none
  private

  public :: map_t

  !> Paired data element
  !!
  !! Adjustments are applied as:
  !! ```
  !!   primary_value = secondary_value * scale_factor + offset
  !! ```
  !!
  type :: pair_t
    private
    !> Primary index
    integer(kind=musica_ik) :: primary_id_ = -1
    !> Secondary index
    integer(kind=musica_ik) :: secondary_id_ = -1
    !> Scaling factor
    real(kind=musica_dk) :: scale_factor_ = 1.0_musica_dk
    !> Offset
    real(kind=musica_dk) :: offset_ = 0.0_musica_dk
  end type pair_t

  !> A map between datasets
  !!
  !! \todo add example usage for map_t
  type :: map_t
    private
    !> Set of paired data
    type(pair_t), allocatable :: pairs_(:)
  contains
    !> Convert secondary values to primary values
    procedure :: to_primary
    !> Convert primary values to secondary values
    procedure :: to_secondary
  end type map_t

  !> Constructor
  interface map_t
    module procedure :: constructor_values
  end interface map_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructor of map_t objects from two sets of values
  function constructor_values( primary, secondary ) result( new_obj )

    use musica_assert,                 only : die

    !> New map
    type(map_t) :: new_obj
    !> Primary dataset values
    real(kind=musica_dk), intent(in) :: primary(:)
    !> Secondary dataset values
    real(kind=musica_dk), intent(in) :: secondary(:)

    !! \todo get interpolation algorithm for map_t construction
    call die( 357950612 )

  end function constructor_values

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Convert secondary values to primary values
  subroutine to_primary( this, primary, secondary )

    !> Dataset map
    class(map_t), intent(in) :: this
    !> Calculated primary values
    real(kind=musica_dk), intent(out) :: primary(:)
    !> Secondary values
    real(kind=musica_dk), intent(in) :: secondary(:)

    integer(kind=musica_ik) :: i_pair

    do i_pair = 1, size( this%pairs_ )
      associate( pair => this%pairs_( i_pair ) )
        primary( pair%primary_id_ ) =                                         &
            secondary( pair%secondary_id_ )                                   &
            * pair%scale_factor_ + pair%offset_
      end associate
    end do

  end subroutine to_primary

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Convert primary values to secondary values
  subroutine to_secondary( this, primary, secondary )

    !> Dataset map
    class(map_t), intent(in) :: this
    !> Primary values
    real(kind=musica_dk), intent(in) :: primary(:)
    !> Calculated secondary values
    real(kind=musica_dk), intent(out) :: secondary(:)

    integer(kind=musica_ik) :: i_pair

    do i_pair = 1, size( this%pairs_ )
      associate( pair => this%pairs_( i_pair ) )
        secondary( pair%secondary_id_ ) =                                     &
            ( primary( pair%primary_id_ ) - pair%offset_ )                    &
            / pair%scale_factor_
      end associate
    end do

  end subroutine to_secondary

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module musica_map
