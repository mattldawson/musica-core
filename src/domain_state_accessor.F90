! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The musica_domain_state_accessor module

!> The abstract domain_state_accessor_t type and related functions
module musica_domain_state_accessor

  implicit none
  private

  public :: domain_state_accessor_t, domain_state_accessor_ptr

  !> Abstract domain state accessor
  type, abstract :: domain_state_accessor_t
  contains
    !> Returns the accessible property
    procedure(property), deferred :: property
  end type domain_state_accessor_t

  !> Accessor pointer
  type domain_state_accessor_ptr
    class(domain_state_accessor_t), pointer :: val_ => null( )
  contains
    final :: finalize
  end type domain_state_accessor_ptr

interface

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the accessible property
  function property( this )
    use musica_property,               only : property_t
    import domain_state_accessor_t
    !> Accessible property
    class(property_t), pointer :: property
    !> Domain state accessor
    class(domain_state_accessor_t), intent(in) :: this
  end function property

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalizes the pointer
  subroutine finalize( this )

    !> Domain pointer
    type(domain_state_accessor_ptr), intent(inout) :: this

    if( associated( this%val_ ) ) deallocate( this%val_ )

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module musica_domain_state_accessor
