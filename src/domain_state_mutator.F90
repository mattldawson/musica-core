! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The musica_domain_state_mutator module

!> The abstract domain_state_mutator_t type and related functions
module musica_domain_state_mutator

  implicit none
  private

  public :: domain_state_mutator_t, domain_state_mutator_ptr

  !> Abstract domain state mutator
  type, abstract :: domain_state_mutator_t
  contains
    !> Returns the modifiable property
    procedure(property), deferred :: property
  end type domain_state_mutator_t

  !> Mutator pointer
  type domain_state_mutator_ptr
    class(domain_state_mutator_t), pointer :: val_ => null( )
  contains
    final :: finalize
  end type domain_state_mutator_ptr

interface

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the modifiable property
  function property( this )
    use musica_property,               only : property_t
    import domain_state_mutator_t
    !> Modifiable property
    class(property_t), pointer :: property
    !> Domain state mutator
    class(domain_state_mutator_t), intent(in) :: this
  end function property

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalizes the pointer
  subroutine finalize( this )

    !> Domain pointer
    type(domain_state_mutator_ptr), intent(inout) :: this

    if( associated( this%val_ ) ) deallocate( this%val_ )

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module musica_domain_state_mutator
