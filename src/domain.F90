! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The musica_domain module

!> The abstract domain_t type and related functions
module musica_domain

  use musica_constants,                only : musica_dk, musica_ik
  use musica_domain_state_accessor,    only : domain_state_accessor_t,        &
                                              domain_state_accessor_ptr
  use musica_domain_state_mutator,     only : domain_state_mutator_t,         &
                                              domain_state_mutator_ptr
  use musica_iterator,                 only : iterator_t
  use musica_property_set,             only : property_set_t
  use musica_target,                   only : target_t

  implicit none
  private

  public :: domain_t, domain_state_t, domain_iterator_t,                      &
            domain_ptr, domain_state_ptr, domain_iterator_ptr, target_cells_t,&
            target_columns_t, target_surface_cells_t, target_model_top_cells_t

  !> A model domain of abstract structure
  !!
  !! Extending classes of domain_t define the structure of the domain and can
  !! be used to build domain state objects and related accessors/mutators.
  !!
  !! The general usage of \c domain_t objects is to:
  !! - create a domain_t object using the
  !!   \c musica_domain_factory::domain_builder function
  !! - register any needed state variables and properies using the \c domain_t
  !!   type-bound \c register \c mutator and \c accessor functions for the
  !!   domain subset you are interested in (e.g., all cells, surface cells,
  !!   columns)
  !! - use the \c domain_t type-bound \c new_state function to get a state
  !!   object to use for the domain
  !! - during solving, use the accessors and mutators registered during
  !!   initialization with the \c domain_state_t::get and
  !!   \c domain_state_t::update functions to access or modify the current
  !!   values of state variables
  !!
  !! Although the structure of the abstract domain types permits run-time
  !! registration of state parameters and variables, it is compatible with
  !! models that use a fixed set of parameters. In this case the domain
  !! registration, accessor and mutator functions would check to make sure
  !! a state variable that is requested is present in the model, and return
  !! an error or warning if they are not found.
  !!
  !! \todo develop a complete set of \c domain_t examples
  !!
  type, abstract :: domain_t
    private
    !> Flag indicating whether the domain configuration has been finalized
    logical :: is_locked_ = .false.
    !> Set of properties that define the domain state
    type(property_set_t), pointer :: properties_ => null( )
  contains
    !> Locks the domain configuration
    procedure :: lock
    !> Returns a flag indicating whether the domain configuration has been
    !! locked
    procedure :: is_locked
    !> @name Registers domain state properties
    !! @{
    procedure, private :: register_property
    procedure, private :: register_property_set
    generic :: register => register_property, register_property_set
    !! @}
    !> Returns the set of registered properties for the domain
    procedure :: properties
    !> Returns the domain type as a string
    procedure(domain_type), deferred :: type
    !> Creates a new state for the domain
    procedure(new_state), deferred :: new_state
    !> Requests a mutator for a domain state property
    procedure(mutator), deferred :: mutator
    !> Requests mutators for a domain state property set
    procedure(mutator_set), deferred :: mutator_set
    !> Requests an accessor for a domain state property
    procedure(accessor), deferred :: accessor
    !> Requests accessors for a domain state property set
    procedure(accessor_set), deferred :: accessor_set
    !> Indicates whether a domain state property exists
    procedure(is_registered), deferred :: is_registered
    !> Returns the units of a domain state property
    procedure(units), deferred :: units
    !> Returns an iterator for the domain or a supported domain subset
    procedure(iterator), deferred :: iterator
    !> Outputs the registered mutators and accessors
    procedure(output_registry), deferred :: output_registry
    !> Private constructor (should only be called by extending types)
    procedure :: private_constructor
    !> Private destructor (should only be called by extending types)
    procedure :: private_destructor
  end type domain_t

  !> Abstract domain state
  type, abstract :: domain_state_t
  contains
    !> Gets the value of a state property
    procedure(state_get), deferred :: get
    !> Updates the value of a state property
    procedure(state_update), deferred :: update
  end type domain_state_t

  !> Domain iterator
  type, abstract, extends(iterator_t) :: domain_iterator_t
  end type domain_iterator_t

  !> Pointer types for building arrays of abstract objects
  !! @{

  !> Domain pointer
  type domain_ptr
    class(domain_t), pointer :: val_ => null( )
  contains
    final :: domain_ptr_finalize
  end type domain_ptr

  !> State pointer
  type domain_state_ptr
    class(domain_state_t), pointer :: val_ => null( )
  contains
    final :: domain_state_ptr_finalize
  end type domain_state_ptr

  !> Iterator pointer
  type domain_iterator_ptr
    class(domain_iterator_t), pointer :: val_ => null( )
  contains
    final :: domain_iterator_ptr_finalize
  end type domain_iterator_ptr

  !> @}

  !> Potential sub-domain targets for state properties and iterators
  !! @{

  !> All cells
  type, extends(target_t) :: target_cells_t
  contains
    procedure :: name => target_cells_t_name
    procedure :: equals_target => target_cells_t_equals_target
  end type target_cells_t

  !> All columns
  type, extends(target_t) :: target_columns_t
  contains
    procedure :: name => target_columns_t_name
    procedure :: equals_target => target_columns_t_equals_target
  end type target_columns_t

  !> All surface cells
  type, extends(target_t) :: target_surface_cells_t
  contains
    procedure :: name => target_surface_cells_t_name
    procedure :: equals_target => target_surface_cells_t_equals_target
  end type target_surface_cells_t

  !> All topmost model cells
  type, extends(target_t) :: target_model_top_cells_t
  contains
    procedure :: name => target_model_top_cells_t_name
    procedure :: equals_target => target_model_top_cells_t_equals_target
  end type target_model_top_cells_t

  !! @}

interface
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the domain type as a string
  function domain_type( this )
    use musica_string,                 only : string_t
    import domain_t
    !> Domain type
    type(string_t) :: domain_type
    !> Domain
    class(domain_t), intent(in) :: this
  end function domain_type

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Creates a new domain state object
  function new_state( this )
    import domain_t
    import domain_state_t
    !> New domain state
    class(domain_state_t), pointer :: new_state
    !> Domain
    class(domain_t), intent(in) :: this
  end function new_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Requests a mutator for a domain state property
  function mutator( this, property ) result( new_mutator )
    use musica_domain_state_mutator,   only : domain_state_mutator_t
    use musica_property,               only : property_t
    import domain_t
    !> Mutator for the requested state variable
    class(domain_state_mutator_t), pointer :: new_mutator
    !> Domain
    class(domain_t), intent(inout) :: this
    !> Property to request mutator for
    class(property_t), intent(in) :: property
  end function mutator

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Requests mutators for a set of domain state properties
  !!
  !! All members of the property set must have the same units and target
  !! (sub)domain. Otherwise, the mutators must be requested individually.
  !!
  function mutator_set( this, variable_name, units, data_type, applies_to,    &
      requestor ) result( new_mutators )
    use musica_data_type,              only : data_type_t
    use musica_domain_state_mutator,   only : domain_state_mutator_ptr
    use musica_target,                 only : target_t
    import domain_t
    !> Mutators for the requested state variable set
    class(domain_state_mutator_ptr), pointer :: new_mutators(:)
    !> Domain
    class(domain_t), intent(inout) :: this
    !> Name of the property set
    character(len=*), intent(in) :: variable_name
    !> Units for the property set members
    character(len=*), intent(in) :: units
    !> Data type for the property set members
    class(data_type_t), intent(in) :: data_type
    !> Model element(s) to which the properties apply
    class(target_t), intent(in) :: applies_to
    !> Name of the model component requesting the mutators
    character(len=*), intent(in) :: requestor
  end function mutator_set

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Requests a accessor for a domain state property
  function accessor( this, property ) result( new_accessor )
    use musica_property,               only : property_t
    use musica_domain_state_accessor,  only : domain_state_accessor_t
    import domain_t
    !> Accessor for the requested state variable
    class(domain_state_accessor_t), pointer :: new_accessor
    !> Domain
    class(domain_t), intent(inout) :: this
    !> Property to request accessor for
    class(property_t), intent(in) :: property
  end function accessor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Requests accessors for a set of domain state properties
  !!
  !! All members of the property set must have the same units and target
  !! (sub)domain. Otherwise, the accessors must be requested individually.
  !!
  function accessor_set( this, variable_name, units, data_type, applies_to,    &
      requestor ) result( new_accessors )
    use musica_data_type,              only : data_type_t
    use musica_domain_state_accessor,  only : domain_state_accessor_ptr
    use musica_target,                 only : target_t
    import domain_t
    !> Accessors for the requested state variable set
    class(domain_state_accessor_ptr), pointer :: new_accessors(:)
    !> Domain
    class(domain_t), intent(inout) :: this
    !> Name of the property set
    character(len=*), intent(in) :: variable_name
    !> Units for the property set members
    character(len=*), intent(in) :: units
    !> Data type for the property set members
    class(data_type_t), intent(in) :: data_type
    !> Model element(s) to which the properties apply
    class(target_t), intent(in) :: applies_to
    !> Name of the model component requesting the accessors
    character(len=*), intent(in) :: requestor
  end function accessor_set

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Indicates whether a domain state property has been registered
  logical function is_registered( this, property_name )
    use musica_property,               only : property_t
    import domain_t
    !> Domain
    class(domain_t), intent(in) :: this
    !> Name of the property to look for
    character(len=*), intent(in) :: property_name
  end function is_registered

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the units of a domain state property
  function units( this, property_name )
    use musica_string,                 only : string_t
    import domain_t
    !> Units for the property
    type(string_t) :: units
    !> Domain
    class(domain_t), intent(in) :: this
    !> Name of the registered state property
    character(len=*), intent(in) :: property_name
  end function units

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns an iterator for the domain or a supported domain subset
  function iterator( this, target_domain )
    use musica_target,                 only : target_t
    import domain_t
    import domain_iterator_t
    !> New iterator
    class(domain_iterator_t), pointer :: iterator
    !> Domain
    class(domain_t), intent(in) :: this
    !> Target for the iterator
    class(target_t), intent(in) :: target_domain
  end function iterator

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Outputs the registered mutators and accessors
  subroutine output_registry( this, file_unit )
    import domain_t
    !> Domain
    class(domain_t), intent(in) :: this
    !> File unit to output to
    integer, intent(in), optional :: file_unit
  end subroutine output_registry

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets the value of a registered state property
  !!
  !! The value returned will be in the units specified when the accessor was
  !! created.
  subroutine state_get( this, iterator, accessor, state_value )
    use musica_domain_state_accessor,  only : domain_state_accessor_t
    import domain_iterator_t
    import domain_state_t
    !> Domain state
    class(domain_state_t), intent(in) :: this
    !> Domain iterator
    class(domain_iterator_t), intent(in) :: iterator
    !> Accessor for the registered state property
    class(domain_state_accessor_t), intent(in) :: accessor
    !> Value of the property
    class(*), intent(out) :: state_value
  end subroutine state_get

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Updates the value of a registered state property
  !!
  !! The units for the value passed to this function must be the same as
  !! those specified when the mutator was created.
  subroutine state_update( this, iterator, mutator, state_value )
    use musica_domain_state_mutator,   only : domain_state_mutator_t
    import domain_state_t
    import domain_iterator_t
    !> Domain state
    class(domain_state_t), intent(inout) :: this
    !> Domain iterator
    class(domain_iterator_t), intent(in) :: iterator
    !> Mutator for registered state property
    class(domain_state_mutator_t), intent(in) :: mutator
    !> New value
    class(*), intent(in) :: state_value
  end subroutine state_update

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Locks the domain configuration
  subroutine lock( this )

    !> Domain
    class(domain_t), intent(inout) :: this

    this%is_locked_ = .true.

  end subroutine lock

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns whether the domain configuration has been locked
  logical function is_locked( this )

    !> Domain
    class(domain_t), intent(in) :: this

    is_locked = this%is_locked_

  end function is_locked

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Registers a domain state property
  subroutine register_property( this, property )

    use musica_assert,                 only : assert
    use musica_property,               only : property_t

    !> Domain
    class(domain_t), intent(inout) :: this
    !> Property to add to the domain registry
    class(property_t), intent(in) :: property

    call assert( 823250106, .not. this%is_locked_ )
    call this%properties_%add( property )

  end subroutine register_property

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Registers a domain property set
  subroutine register_property_set( this, prefix, property_set )

    use musica_assert,                 only : assert
    use musica_property,               only : property_t, property_ptr
    use musica_property_set,           only : property_set_t
    use musica_string,                 only : string_t

    !> Domain
    class(domain_t), intent(inout) :: this
    !> Prefix to attach to the property names
    character(len=*), intent(in) :: prefix
    !> Property set to add to the domain registry
    class(property_set_t), intent(in) :: property_set

    integer(kind=musica_ik) :: i_prop
    class(property_t), pointer :: prop, new_prop
    type(string_t) :: new_name, defined_by

    call assert( 995312544, .not. this%is_locked_ )
    do i_prop = 1, property_set%size( )
      prop => property_set%get( i_prop )
      new_name = prop%name( )
      if( len( prefix ) .gt. 0 ) new_name = prefix//"%"//new_name
      defined_by = prop%defined_by( )
      new_prop => property_t( prop, defined_by%to_char( ),                    &
                              name = new_name%to_char( ) )
      call this%properties_%add( new_prop )
      deallocate( new_prop )
      deallocate( prop     )
    end do

  end subroutine register_property_set

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the set of registered properties for the domain
  function properties( this )

    use musica_assert,                 only : assert

    !> Registered properties
    class(property_set_t), pointer :: properties
    !> Domain
    class(domain_t), intent(in) :: this

    call assert( 544133630, associated( this%properties_ ) )
    call assert( 203819822, this%is_locked_ )
    properties => this%properties_%subset( )

  end function properties

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Private constructor (should only be called by extending types)
  subroutine private_constructor( this )

    !> Domain
    class(domain_t), intent(inout) :: this

    this%properties_ => property_set_t( )

  end subroutine private_constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Private destructor (should only be called by extending types)
  subroutine private_destructor( this )

    !> Domain
    class(domain_t), intent(inout) :: this

    if( associated( this%properties_ ) ) deallocate( this%properties_ )

  end subroutine private_destructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalize pointer
  subroutine domain_ptr_finalize( this )

    !> Domain pointer
    type(domain_ptr), intent(inout) :: this

    if( associated( this%val_ ) ) deallocate( this%val_ )

  end subroutine domain_ptr_finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalize pointer
  subroutine domain_state_ptr_finalize( this )

    !> Domain pointer
    type(domain_state_ptr), intent(inout) :: this

    if( associated( this%val_ ) ) deallocate( this%val_ )

  end subroutine domain_state_ptr_finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalize pointer
  subroutine domain_iterator_ptr_finalize( this )

    !> Domain pointer
    type(domain_iterator_ptr), intent(inout) :: this

    if( associated( this%val_ ) ) deallocate( this%val_ )

  end subroutine domain_iterator_ptr_finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the name of the target
  type(string_t) function target_cells_t_name( this ) result( my_name )

    use musica_string,                 only : string_t

    !> Target
    class(target_cells_t), intent(in) :: this

    my_name = "all domain cells"

  end function target_cells_t_name

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Equality comparison
  logical function target_cells_t_equals_target( a, b ) result( eq )

    !> Target
    class(target_cells_t), intent(in) :: a
    !> Other target
    class(target_t), intent(in) :: b

    select type( b )
    class is( target_cells_t )
      eq = .true.
    class default
      eq = .false.
    end select

  end function target_cells_t_equals_target

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the name of the target
  type(string_t) function target_columns_t_name( this ) result( my_name )

    use musica_string,                 only : string_t

    !> Target
    class(target_columns_t), intent(in) :: this

    my_name = "all domain columns"

  end function target_columns_t_name

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Equality comparison
  logical function target_columns_t_equals_target( a, b ) result( eq )

    !> Target
    class(target_columns_t), intent(in) :: a
    !> Other target
    class(target_t), intent(in) :: b

    select type( b )
    class is( target_columns_t )
      eq = .true.
    class default
      eq = .false.
    end select

  end function target_columns_t_equals_target

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the name of the target
  type(string_t) function target_surface_cells_t_name( this ) result( my_name )

    use musica_string,                 only : string_t

    !> Target
    class(target_surface_cells_t), intent(in) :: this

    my_name = "all domain cells at the surface"

  end function target_surface_cells_t_name

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Equality comparison
  logical function target_surface_cells_t_equals_target( a, b ) result( eq )

    !> Target
    class(target_surface_cells_t), intent(in) :: a
    !> Other target
    class(target_t), intent(in) :: b

    select type( b )
    class is( target_surface_cells_t )
      eq = .true.
    class default
      eq = .false.
    end select

  end function target_surface_cells_t_equals_target

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the name of the target
  type(string_t) function target_model_top_cells_t_name( this )               &
      result( my_name )

    use musica_string,                 only : string_t

    !> Target
    class(target_model_top_cells_t), intent(in) :: this

    my_name = "all domain cells at the model top"

  end function target_model_top_cells_t_name

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Equality comparison
  logical function target_model_top_cells_t_equals_target( a, b )             &
      result( eq )

    !> Target
    class(target_model_top_cells_t), intent(in) :: a
    !> Other target
    class(target_t), intent(in) :: b

    select type( b )
    class is( target_model_top_cells_t )
      eq = .true.
    class default
      eq = .false.
    end select

  end function target_model_top_cells_t_equals_target

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module musica_domain
