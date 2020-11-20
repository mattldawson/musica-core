! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The musica_domain_state module

!> The abstract domain_state_t type and related functions
module musica_domain_state

  implicit none
  private

  public :: domain_state_t, domain_state_ptr

  !> Abstract domain state
  !!
  !! A domain state maintains all time-evolving conditions for a domain_t
  !! model domain.
  !!
  type, abstract :: domain_state_t
  contains
    !> Gets the value of a state property
    procedure(get), deferred :: get
    !> Updates the value of a state property
    procedure(update), deferred :: update
  end type domain_state_t

  !> State pointer
  type domain_state_ptr
    class(domain_state_t), pointer :: val_ => null( )
  contains
    final :: finalize
  end type domain_state_ptr

interface

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets the value of a registered state property
  !!
  !! The value returned will be in the units specified when the accessor was
  !! created.
  subroutine get( this, iterator, accessor, state_value )
    use musica_domain_iterator,        only : domain_iterator_t
    use musica_domain_state_accessor,  only : domain_state_accessor_t
    import domain_state_t
    !> Domain state
    class(domain_state_t), intent(in) :: this
    !> Domain iterator
    class(domain_iterator_t), intent(in) :: iterator
    !> Accessor for the registered state property
    class(domain_state_accessor_t), intent(in) :: accessor
    !> Value of the property
    class(*), intent(out) :: state_value
  end subroutine get

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Updates the value of a registered state property
  !!
  !! The units for the value passed to this function must be the same as
  !! those specified when the mutator was created.
  subroutine update( this, iterator, mutator, state_value )
    use musica_domain_iterator,        only : domain_iterator_t
    use musica_domain_state_mutator,   only : domain_state_mutator_t
    import domain_state_t
    !> Domain state
    class(domain_state_t), intent(inout) :: this
    !> Domain iterator
    class(domain_iterator_t), intent(in) :: iterator
    !> Mutator for registered state property
    class(domain_state_mutator_t), intent(in) :: mutator
    !> New value
    class(*), intent(in) :: state_value
  end subroutine update

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalize a state pointer
  subroutine finalize( this )

    !> Domain pointer
    type(domain_state_ptr), intent(inout) :: this

    if( associated( this%val_ ) ) deallocate( this%val_ )

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module musica_domain_state
