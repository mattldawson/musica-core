! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The musica_domain_cell module

!> The domain_cell_t type and related functions
module musica_domain_cell

  use musica_constants,                only : musica_dk, musica_ik,           &
                                              musica_lk, musica_rk
  use musica_domain,                   only : domain_t, domain_state_t
  use musica_domain_iterator,          only : domain_iterator_t
  use musica_domain_state_accessor,    only : domain_state_accessor_t
  use musica_domain_state_mutator,     only : domain_state_mutator_t
  use musica_property,                 only : property_ptr
  use musica_property_set,             only : property_set_t
  use musica_string,                   only : string_t

  implicit none
  private

  public :: domain_cell_t, domain_cell_state_t

  !> Registered pairs
  type :: registered_pair_t
    !> Name of the registering model component
    type(string_t) :: owner_
    !> Registered property
    type(property_ptr) :: property_
  contains
    !> Finalize the pair
    final :: registered_pair_finalize
  end type registered_pair_t

  !> @}

  !> Model domain for a collection of unrelated cells or boxes
  type, extends(domain_t) :: domain_cell_t
    private
    !> Registered mutators
    type(registered_pair_t), allocatable :: mutators_(:)
    !> Registered accessors
    type(registered_pair_t), allocatable :: accessors_(:)
  contains
    !> Returns the domain type as a string
    procedure :: type => domain_type
    !> Creates a new state for the domain
    procedure :: new_state
    !> Requests a mutator for a domain state property
    procedure :: mutator
    !> Reuests mutators for a domain state property set
    procedure :: mutator_set
    !> Requests an accessor for a domain state property
    procedure :: accessor
    !> Requests accessors for a set of domain state properties
    procedure :: accessor_set
    !> Indicates whether a domain state property exists
    procedure :: is_registered
    !> Returns the units of a domain state property
    procedure :: units
    !> Returns an iterator for the domain or a supported domain subset
    procedure :: iterator
    !> Outputs the registered mutators and accessors
    procedure :: output_registry
    !> Finalize the domain
    final :: finalize
  end type domain_cell_t

  !> domain_cell_t constructor
  interface domain_cell_t
    module procedure :: constructor
  end interface domain_cell_t

  !> Cell state
  type, extends(domain_state_t) :: domain_cell_state_t
    !> Integer properties
    integer(kind=musica_ik), allocatable :: integers_(:)
    !> Single-precision floating point properties
    real(kind=musica_rk), allocatable :: floats_(:)
    !> Double-precision floating point properties
    real(kind=musica_dk), allocatable :: doubles_(:)
    !> Boolean properties
    logical(kind=musica_lk), allocatable :: booleans_(:)
  contains
    !> Gets the value of a state variable
    procedure :: get => state_get
    !> Updates the value of a state variable
    procedure :: update => state_update
  end type domain_cell_state_t

  !> @name Mutators for cell state properties
  !! @{

  !> Generic cell mutator
  type, extends(domain_state_mutator_t) :: mutator_cell_t
    private
    !> Property modified by the mutator
    type(property_ptr) :: property_
    !> Index of the property in the data-type specific domain state arrays
    integer(kind=musica_ik) :: index_ = -99999
  contains
    !> Returns the property modified by the mutator
    procedure :: property => mutator_property
  end type mutator_cell_t

  !> Integer property mutator
  type, extends(mutator_cell_t) :: mutator_integer_t
  end type mutator_integer_t

  !> Single-precision floating point property mutator
  type, extends(mutator_cell_t) :: mutator_float_t
  end type mutator_float_t

  !> Double-precision floating point property mutator
  type, extends(mutator_cell_t) :: mutator_double_t
  end type mutator_double_t

  !> Boolean property mutator
  type, extends(mutator_cell_t) :: mutator_boolean_t
  end type mutator_boolean_t

  !> @}
  !> @name Accessors for cell state properties
  !! @{

  !> Generic cell accessor
  type, extends(domain_state_accessor_t) :: accessor_cell_t
    private
    !> Property accessed by the accessor
    type(property_ptr) :: property_
    !> Index of the property in the data-type specific domain state arrays
    integer(kind=musica_ik) :: index_ = -99999
  contains
    !> Returns the property accessed by the accessor
    procedure :: property => accessor_property
  end type accessor_cell_t

  !> Integer property accessor
  type, extends(accessor_cell_t) :: accessor_integer_t
  end type accessor_integer_t

  !> Single-precision floating point property accessor
  type, extends(accessor_cell_t) :: accessor_float_t
  end type accessor_float_t

  !> Double-precision floating point property accessor
  type, extends(accessor_cell_t) :: accessor_double_t
  end type accessor_double_t

  !> Boolean property accessor
  type, extends(accessor_cell_t) :: accessor_boolean_t
  end type accessor_boolean_t

  !> @}

  !> Domain iterator
  type, extends(domain_iterator_t) :: cell_iterator_t
    private
    !> Current cell id
    integer(kind=musica_ik) :: current_cell_ = 0
    !> Last cell id
    integer(kind=musica_ik) :: last_cell_ = 1
  contains
    !> Advances the iterator
    procedure :: next => domain_cell_iterator_next
    !> Resets the iterator
    procedure :: reset => domain_cell_iterator_reset
  end type cell_iterator_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructor for the cell domain
  function constructor( config ) result( new_obj )

    use musica_config,                 only : config_t

    !> Pointer to the new domain
    type(domain_cell_t), pointer :: new_obj
    !> Domain configuration data
    type(config_t), intent(inout) :: config

    allocate( new_obj )
    allocate( new_obj%mutators_(  0 ) )
    allocate( new_obj%accessors_( 0 ) )
    call new_obj%private_constructor( )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the domain type as a string
  function domain_type( this )

    !> Domain type
    type(string_t) :: domain_type
    !> Domain
    class(domain_cell_t), intent(in) :: this

    domain_type = "cell"

  end function domain_type

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Creates a new domain state object
  function new_state( this )

    use musica_assert,                 only : assert
    use musica_data_type,              only : kInteger, kFloat, kDouble,      &
                                              kBoolean

    !> New domain state
    class(domain_state_t), pointer :: new_state
    !> Domain
    class(domain_cell_t), intent(in) :: this

    type(property_ptr) :: prop
    class(property_set_t), pointer :: props, int_props, float_props,          &
                                      double_props, bool_props
    integer(kind=musica_ik) :: i_int, i_float, i_double, i_bool

    props        => this%properties( )
    int_props    => props%subset( data_type = kInteger )
    float_props  => props%subset( data_type = kFloat )
    double_props => props%subset( data_type = kDouble )
    bool_props   => props%subset( data_type = kBoolean )

    allocate( domain_cell_state_t :: new_state )

    select type( new_state )
    class is( domain_cell_state_t )
      allocate( new_state%integers_( int_props%size( )    ) )
      allocate( new_state%floats_(   float_props%size( )  ) )
      allocate( new_state%doubles_(  double_props%size( ) ) )
      allocate( new_state%booleans_( bool_props%size( )   ) )
      do i_int = 1, int_props%size( )
        prop%val_ => int_props%get( i_int )
        call prop%val_%get_default( new_state%integers_( i_int ) )
        deallocate( prop%val_ )
      end do
      do i_float = 1, float_props%size( )
        prop%val_ => float_props%get( i_float )
        call prop%val_%get_default( new_state%floats_( i_float ) )
        deallocate( prop%val_ )
      end do
      do i_double = 1, double_props%size( )
        prop%val_ => double_props%get( i_double )
        call prop%val_%get_default( new_state%doubles_( i_double ) )
        deallocate( prop%val_ )
      end do
      do i_bool = 1, bool_props%size( )
        prop%val_ => bool_props%get( i_bool )
        call prop%val_%get_default( new_state%booleans_( i_bool ) )
        deallocate( prop%val_ )
      end do
    end select
    deallocate( props        )
    deallocate( int_props    )
    deallocate( float_props  )
    deallocate( double_props )
    deallocate( bool_props   )

  end function new_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Requests a mutator for a domain state property
  function mutator( this, property ) result( new_mutator )

    use musica_assert,                 only : die_msg, die
    use musica_data_type,              only : data_type_t, kBoolean, kDouble, &
                                              kFloat, kInteger
    use musica_domain,                 only : target_cells_t
    use musica_property,               only : property_t
    use musica_target,                 only : target_t

    !> Mutator for the requested property
    class(domain_state_mutator_t), pointer :: new_mutator
    !> Domain
    class(domain_cell_t), intent(inout) :: this
    !> Property to get mutator for
    class(property_t), intent(in) :: property

    class(property_set_t), pointer :: props, type_props
    integer(kind=musica_ik) :: property_id
    type(registered_pair_t) :: new_pair
    class(target_t), pointer :: mutator_target
    type(string_t) :: target_name, prop_name
    type(data_type_t) :: data_type

    props                   => this%properties( )
    new_pair%owner_         =  property%defined_by( )
    prop_name = property%name( )
    new_pair%property_%val_ =>  props%get( prop_name%to_char( ) )
    call add_registered_pair_to_array( this%mutators_, new_pair )

    ! create the mutator
    mutator_target => new_pair%property_%val_%applies_to( )
    data_type      =  new_pair%property_%val_%data_type( )
    select type( mutator_target )
    class is( target_cells_t )
      if( data_type .eq. kInteger ) then
        allocate( mutator_integer_t :: new_mutator )
      else if( data_type .eq. kFloat ) then
        allocate( mutator_float_t   :: new_mutator )
      else if( data_type .eq. kDouble ) then
        allocate( mutator_double_t  :: new_mutator )
      else if( data_type .eq. kBoolean ) then
        allocate( mutator_boolean_t :: new_mutator )
      else
        call die_msg( 399592430, "Unsupported data type requested for cell "//&
                      "domain mutator for property '"//                       &
                      new_pair%property_%val_%name( )//"'" )
      end if
    class default
      target_name = mutator_target%name( )
      call die_msg( 829993249, "Cell domains to not currently support '"//    &
                    target_name%to_char( )//"' as a property target" )
    end select
    select type( new_mutator )
    class is( mutator_cell_t )
      new_mutator%property_%val_ => props%get( prop_name%to_char( ) )
      type_props => props%subset( data_type = data_type )
      new_mutator%index_ = type_props%index( property )
      deallocate( type_props )
    class default
      call die( 101776555 )
    end select
    deallocate( mutator_target )
    deallocate( props )

  end function mutator

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Requests mutators for a set of domain state properties
  !!
  !! All members of the property set must have the same units and target
  !! (sub)domain. Otherwise, the mutators must be requested individually.
  !!
  function mutator_set( this, variable_name, units, data_type, applies_to,    &
      requestor ) result( new_mutators )

    use musica_assert,                 only : assert, assert_msg, die_msg
    use musica_data_type,              only : data_type_t
    use musica_domain_state_mutator,   only : domain_state_mutator_ptr
    use musica_property,               only : property_t
    use musica_target,                 only : target_t

    !> Mutators for the requested state variable set
    class(domain_state_mutator_ptr), pointer :: new_mutators(:)
    !> Domain
    class(domain_cell_t), intent(inout) :: this
    !> Name of the property set
    character(len=*), intent(in) :: variable_name
    !> Units for the property set members
    character(len=*), intent(in) :: units
    !> Data type for the property set members
    class(data_type_t), intent(in) :: data_type
    !> Model element(s) to which the properties apply
    class(target_t), intent(in) :: applies_to
    !> Name of the model component requesting the mutator
    character(len=*), intent(in) :: requestor

    type(property_ptr) :: prop_base, prop_spec, reg_prop
    class(property_set_t), pointer :: all_props, subset_props
    integer(kind=musica_ik) :: num_props, i_mutator
    type(string_t) :: prop_name

    call assert( 394642233, len( trim( variable_name ) ) .gt. 0 )

    all_props => this%properties( )
    subset_props => all_props%subset( prefix = variable_name )
    num_props = subset_props%size( )
    prop_base%val_ => property_t( requestor,                                  &
                                  units = units,                              &
                                  data_type = data_type,                      &
                                  applies_to = applies_to )
    allocate( new_mutators(    num_props ) )
    do i_mutator = 1, num_props
      reg_prop%val_ => subset_props%get( i_mutator )
      prop_name = reg_prop%val_%name( )
      prop_spec%val_ => property_t( prop_base%val_, requestor,                &
                                    name = prop_name%to_char( ) )
      new_mutators( i_mutator )%val_ => this%mutator( prop_spec%val_ )
      deallocate( prop_spec%val_ )
      deallocate( reg_prop%val_ )
    end do
    deallocate( all_props )
    deallocate( subset_props )

  end function mutator_set

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Requests a accessor for a domain state property
  function accessor( this, property ) result( new_accessor )

    use musica_assert,                 only : die_msg, die
    use musica_data_type,              only : data_type_t, kBoolean, kDouble,  &
                                              kFloat, kInteger
    use musica_domain,                 only : target_cells_t
    use musica_property,               only : property_t
    use musica_target,                 only : target_t

    !> Accessor for the requested property
    class(domain_state_accessor_t), pointer :: new_accessor
    !> Domain
    class(domain_cell_t), intent(inout) :: this
    !> Property to access
    class(property_t), intent(in) :: property

    class(property_set_t), pointer :: props, type_props
    integer(kind=musica_ik) :: property_id
    type(registered_pair_t) :: new_pair
    class(target_t), pointer :: accessor_target
    type(string_t) :: target_name, prop_name
    type(data_type_t) :: data_type

    props                   => this%properties( )
    new_pair%owner_         =  property%defined_by( )
    prop_name = property%name( )
    new_pair%property_%val_ => props%get( prop_name%to_char( ) )
    call add_registered_pair_to_array( this%accessors_, new_pair )

    ! create the accessor
    accessor_target => new_pair%property_%val_%applies_to( )
    data_type      =  new_pair%property_%val_%data_type( )
    select type( accessor_target )
    class is( target_cells_t )
      if( data_type .eq. kInteger ) then
        allocate( accessor_integer_t :: new_accessor )
      else if( data_type .eq. kFloat ) then
        allocate( accessor_float_t   :: new_accessor )
      else if( data_type .eq. kDouble ) then
        allocate( accessor_double_t  :: new_accessor )
      else if( data_type .eq. kBoolean ) then
        allocate( accessor_boolean_t :: new_accessor )
      else
        call die_msg( 711130520, "Unsupported data type requested for cell "//&
                      "domain accessor for property '"//                       &
                      new_pair%property_%val_%name( )//"'" )
      end if
    class default
      target_name = accessor_target%name( )
      call die_msg( 823448865, "Cell domains to not currently support '"//    &
                    target_name%to_char( )//"' as a property target" )
    end select
    select type( new_accessor )
    class is( accessor_cell_t )
      new_accessor%property_%val_ => props%get( prop_name%to_char( ) )
      type_props => props%subset( data_type = data_type )
      new_accessor%index_ = type_props%index( property )
      deallocate( type_props )
    class default
      call die( 883192958 )
    end select
    deallocate( accessor_target )
    deallocate( props )

  end function accessor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets accessors for a set of state variables for each cell in the domain
  !!
  !! All members of the property set must have the same units and target
  !! (sub)domain. Otherwise, the mutators must be requested individually.
  !!
  function accessor_set( this, variable_name, units, data_type, applies_to,    &
      requestor ) result( new_accessors )

    use musica_assert,                 only : assert, assert_msg, die_msg
    use musica_data_type,              only : data_type_t
    use musica_domain_state_accessor,  only : domain_state_accessor_ptr
    use musica_property,               only : property_t
    use musica_target,                 only : target_t

    !> Accessors for the requested state variable set
    class(domain_state_accessor_ptr), pointer :: new_accessors(:)
    !> Domain
    class(domain_cell_t), intent(inout) :: this
    !> Name of the property set
    character(len=*), intent(in) :: variable_name
    !> Units for the property set members
    character(len=*), intent(in) :: units
    !> Data type for the property set members
    class(data_type_t), intent(in) :: data_type
    !> Model element(s) to which the properties apply
    class(target_t), intent(in) :: applies_to
    !> Name of the model component requesting the accessor
    character(len=*), intent(in) :: requestor

    type(property_ptr) :: prop_base, prop_spec, reg_prop
    class(property_set_t), pointer :: all_props, subset_props
    integer(kind=musica_ik) :: num_props, i_accessor
    type(string_t) :: prop_name

    call assert( 367457939, len( trim( variable_name ) ) .gt. 0 )

    all_props => this%properties( )
    subset_props => all_props%subset( prefix = variable_name )
    num_props = subset_props%size( )
    prop_base%val_ => property_t( requestor,                                  &
                                  units = units,                              &
                                  data_type = data_type,                      &
                                  applies_to = applies_to )
    allocate( new_accessors(    num_props ) )
    do i_accessor = 1, num_props
      reg_prop%val_ => subset_props%get( i_accessor )
      prop_name = reg_prop%val_%name( )
      prop_spec%val_ => property_t( prop_base%val_, requestor,                &
                                    name = prop_name%to_char( ) )
      new_accessors( i_accessor )%val_ => this%accessor( prop_spec%val_ )
      deallocate( prop_spec%val_ )
      deallocate( reg_prop%val_ )
    end do
    deallocate( all_props )
    deallocate( subset_props )

  end function accessor_set

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Indicates whether a domain state property has been registered
  logical function is_registered( this, property_name )

    use musica_property,               only : property_t

    !> Domain
    class(domain_cell_t), intent(in) :: this
    !> Name of the property to look for
    character(len=*), intent(in) :: property_name

    type(property_set_t), pointer :: props
    class(property_t), pointer :: prop
    integer(kind=musica_ik) :: var_id

    props => this%properties( )
    prop => props%get( property_name, found = is_registered )
    deallocate( props )
    if( associated( prop ) ) deallocate( prop )

  end function is_registered

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the units of a domain state property
  function units( this, property_name )

    use musica_assert,                 only : assert
    use musica_property,               only : property_t
    use musica_string,                 only : string_t

    !> Units for the property
    type(string_t) :: units
    !> Domain
    class(domain_cell_t), intent(in) :: this
    !> Name of the registered state property
    character(len=*), intent(in) :: property_name

    type(property_set_t), pointer :: props
    class(property_t), pointer :: prop

    call assert( 425563855, len( trim( property_name ) ) .gt. 0 )
    props => this%properties( )
    prop => props%get( property_name )
    units = prop%units( )
    deallocate( props )
    deallocate( prop )

  end function units

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns an iterator for the domain or a supported domain subset
  function iterator( this, target_domain )

    use musica_assert,                 only : die, die_msg
    use musica_domain,                 only : target_cells_t
    use musica_target,                 only : target_t

    !> New iterator
    class(domain_iterator_t), pointer :: iterator
    !> Domain
    class(domain_cell_t), intent(in) :: this
    !> Target domain for the iterator
    class(target_t), intent(in) :: target_domain

    select type( target_domain )
    class is( target_cells_t )
      allocate( cell_iterator_t :: iterator )
      select type( iterator )
      class is( cell_iterator_t )
      class default
        call die( 774884423 )
      end select
    class default
      call die_msg( 946946861, "Iterators for target domain '"//              &
                    target_domain%name( )//"' are not supported by cell "//   &
                    "domains." )
    end select

  end function iterator

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Outputs the registered mutators and accessors
  subroutine output_registry( this, file_unit )

    use musica_string,                 only : output_table

    !> Domain
    class(domain_cell_t), intent(in) :: this
    !> File unit to output to
    integer, intent(in), optional :: file_unit

    integer(kind=musica_ik) :: f, i_elem
    type(string_t) :: header(2)
    type(string_t), allocatable :: table_data(:,:)
    type(property_set_t), pointer :: props

    f = 6
    if( present( file_unit ) ) f = file_unit
    props => this%properties( )
    write(f,*)
    write(f,*) "Registered domain properties"
    write(f,*)
    call props%output( f )
    header(1) = "Owner"
    header(2) = "Property"
    allocate( table_data( 2, size( this%mutators_ ) ) )
    do i_elem = 1, size( table_data )
      table_data( 1, i_elem ) = this%mutators_( i_elem )%owner_
      table_data( 2, i_elem ) = this%mutators_( i_elem )%property_%val_%name( )
    end do
    write(f,*)
    write(f,*) "Registered mutators"
    write(f,*)
    call output_table( header, table_data, f )
    deallocate( table_data )
    allocate( table_data( 2, size( this%accessors_ ) ) )
    do i_elem = 1, size( table_data )
      table_data( 1, i_elem ) = this%accessors_( i_elem )%owner_
      table_data( 2, i_elem ) =                                               &
          this%accessors_( i_elem )%property_%val_%name( )
    end do
    write(f,*)
    write(f,*) "Registered accessors"
    write(f,*)
    call output_table( header, table_data, f )
    deallocate( props )

  end subroutine output_registry

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalize the domain
  subroutine finalize( this )

    !> Domain
    type(domain_cell_t), intent(inout) :: this

    call this%private_destructor( )

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalize a registered pair
  subroutine registered_pair_finalize( this )

    !> Registered pair
    type(registered_pair_t), intent(inout) :: this

    if( associated( this%property_%val_ ) ) deallocate( this%property_%val_ )

  end subroutine registered_pair_finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> @name Type-bound domain_cell_state_t functions
  !!
  !! @{

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets the value of a registered state property
  subroutine state_get( this, iterator, accessor, state_value )

    use musica_assert,                 only : die

    !> Domain state
    class(domain_cell_state_t), intent(in) :: this
    !> Domain iterator
    class(domain_iterator_t), intent(in) :: iterator
    !> Accessor for the state property
    class(domain_state_accessor_t), intent(in) :: accessor
    !> Value of the property or state variable
    class(*), intent(out) :: state_value

    select type( iterator )
    class is( cell_iterator_t )
      select type( accessor )
      class is( accessor_integer_t )
        select type( state_value )
        type is( integer(kind=musica_ik) )
          state_value = this%integers_( accessor%index_ )
        class default
          call die( 217442083 )
        end select
      class is( accessor_float_t )
        select type( state_value )
        type is( real(kind=musica_rk) )
          state_value = this%floats_( accessor%index_ )
        class default
          call die( 882729073 )
        end select
      class is( accessor_double_t )
        select type( state_value )
        type is( real(kind=musica_dk) )
          state_value = this%doubles_( accessor%index_ )
        class default
          call die( 258486777 )
        end select
      class is( accessor_boolean_t )
        select type( state_value )
        type is( logical )
          state_value = this%booleans_( accessor%index_ )
        class default
          call die( 411397521 )
        end select
      class default
        call die( 583459959 )
      end select
    class default
      call die( 302890244 )
    end select

  end subroutine state_get

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Updates the value of a registered state property
  subroutine state_update( this, iterator, mutator, state_value )

    use musica_assert,                 only : die

    !> Domain state
    class(domain_cell_state_t), intent(inout) :: this
    !> Domain iterator
    class(domain_iterator_t), intent(in) :: iterator
    !> Mutator for registered property or state variable
    class(domain_state_mutator_t), intent(in) :: mutator
    !> New value
    class(*), intent(in) :: state_value

    select type( iterator )
    class is( cell_iterator_t )
      select type( mutator )
      class is( mutator_integer_t )
        select type( state_value )
        type is( integer(kind=musica_ik) )
          this%integers_( mutator%index_ ) = state_value
        class default
          call die( 442818191 )
        end select
      class is( mutator_float_t )
        select type( state_value )
        type is( real(kind=musica_rk) )
          this%floats_( mutator%index_ ) = state_value
        class default
          call die( 772603385 )
        end select
      class is( mutator_double_t )
        select type( state_value )
        type is( real(kind=musica_dk) )
          this%doubles_( mutator%index_ ) = state_value
        class default
          call die( 602446481 )
        end select
      class is( mutator_boolean_t )
        select type( state_value )
        type is( logical )
          this%booleans_( mutator%index_ ) = state_value
        class default
          call die( 432289577 )
        end select
      class default
        call die( 327141073 )
      end select
    class default
      call die( 492033670 )
    end select

  end subroutine state_update

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> @}

  !> @name Functions of mutator_cell_t and accessor_cell_t
  !!
  !! @{

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the property modified by the mutator
  function mutator_property( this )

    use musica_property,               only : property_t

    !> Property modified
    class(property_t), pointer :: mutator_property
    !> Domain state mutator
    class(mutator_cell_t), intent(in) :: this

    allocate( mutator_property, mold = this%property_%val_ )
    mutator_property = this%property_%val_

  end function mutator_property

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the property accessed by the accessor
  function accessor_property( this )

    use musica_property,               only : property_t

    !> Property accessed
    class(property_t), pointer :: accessor_property
    !> Domain state accessor
    class(accessor_cell_t), intent(in) :: this

    allocate( accessor_property, mold = this%property_%val_ )
    accessor_property = this%property_%val_

  end function accessor_property

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> @}

  !> @name Functions of cell_iterator_t types
  !!
  !! @{

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Advances the iterator
  !!
  !! Returns false if the end of the collection has been reached
  logical function domain_cell_iterator_next( this )

    !> Iterator
    class(cell_iterator_t), intent(inout) :: this

    this%current_cell_ = this%current_cell_ + 1

    if( this%current_cell_ .gt. this%last_cell_ ) then
      domain_cell_iterator_next = .false.
    else
      domain_cell_iterator_next = .true.
    end if

  end function domain_cell_iterator_next

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Resets the iterator
  subroutine domain_cell_iterator_reset( this, parent )

    use musica_iterator,               only : iterator_t

    !> Iterator
    class(cell_iterator_t), intent(inout) :: this
    !> Iterator for parent model element
    class(iterator_t), intent(in), optional :: parent

    this%current_cell_ = 0

  end subroutine domain_cell_iterator_reset

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> @}

  !> @name Private functions of the musica_domain_cell module
  !!
  !! @{

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Adds a registered pair to an array of registered pairs
  subroutine add_registered_pair_to_array( array, new_pair )

    use musica_assert,                 only : assert

    !> Array to add to
    type(registered_pair_t), allocatable, intent(inout)  :: array(:)
    !> Pair to add to array
    type(registered_pair_t), intent(in) :: new_pair

    type(registered_pair_t), allocatable :: temp_pairs(:)
    integer(kind=musica_ik) :: i_elem, n_elem

    ! this could be made more efficient, if necessary

    call assert( 454015072, allocated( array ) )
    n_elem = size( array )
    allocate( temp_pairs( n_elem ) )
    temp_pairs(:) = array(:)
    do i_elem = 1, size( array )
      array( i_elem )%property_%val_ => null( )
    end do
    deallocate( array )
    allocate( array( n_elem + 1 ) )
    array( :n_elem ) = temp_pairs(:)
    do i_elem = 1, size( temp_pairs )
      temp_pairs( i_elem )%property_%val_ => null( )
    end do
    array( n_elem + 1 )%owner_ = new_pair%owner_
    allocate( array( n_elem + 1 )%property_%val_,                             &
              mold = new_pair%property_%val_ )
    array( n_elem + 1 )%property_%val_ = new_pair%property_%val_

  end subroutine add_registered_pair_to_array

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> @}

end module musica_domain_cell
