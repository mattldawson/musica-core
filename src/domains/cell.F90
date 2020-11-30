! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The musica_domain_cell module

!> The domain_cell_t type and related functions
module musica_domain_cell

  use musica_constants,                only : musica_dk, musica_ik,           &
                                              musica_lk, musica_rk
  use musica_domain,                   only : domain_t
  use musica_domain_state,             only : domain_state_t
  use musica_domain_iterator,          only : domain_iterator_t
  use musica_domain_state_accessor,    only : domain_state_accessor_t
  use musica_domain_state_mutator,     only : domain_state_mutator_t

  implicit none
  private

  public :: domain_cell_t, domain_cell_state_t

  !> Model domain for a collection of unrelated cells or boxes
  type, extends(domain_t) :: domain_cell_t
  contains
    !> Returns the domain type as a string
    procedure :: type => domain_type
    !> Creates a new state for the domain
    procedure :: new_state
    !> Returns an iterator for the domain or a supported domain subset
    procedure :: iterator
    !> Allocates a mutator for a given data type and target domain
    procedure, private :: allocate_mutator
    !> Allocates an accessor for a given data type and target domain
    procedure, private :: allocate_accessor
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
    !> Index of the property in the data-type specific domain state arrays
    integer(kind=musica_ik) :: index_ = -99999
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
    !> Index of the property in the data-type specific domain state arrays
    integer(kind=musica_ik) :: index_ = -99999
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
    call new_obj%private_constructor( )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the domain type as a string
  function domain_type( this )

    use musica_string,                 only : string_t

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
    use musica_property,               only : property_ptr
    use musica_property_set,           only : property_set_t

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

  !> Returns an iterator for the domain or a supported domain subset
  function iterator( this, target_domain )

    use musica_assert,                 only : die, die_msg
    use musica_domain_target_cells,    only : domain_target_cells_t
    use musica_target,                 only : target_t

    !> New iterator
    class(domain_iterator_t), pointer :: iterator
    !> Domain
    class(domain_cell_t), intent(in) :: this
    !> Target domain for the iterator
    class(target_t), intent(in) :: target_domain

    select type( target_domain )
    class is( domain_target_cells_t )
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

  !> Allocates a mutator for a given target domain and data type
  function allocate_mutator( this, target_domain, data_type, property_index ) &
      result( mutator )

    use musica_assert,                 only : die, die_msg
    use musica_data_type,              only : data_type_t, kBoolean, kDouble, &
                                              kFloat, kInteger
    use musica_domain_target_cells,    only : domain_target_cells_t
    use musica_string,                 only : string_t
    use musica_target,                 only : target_t

    !> Mutator
    class(domain_state_mutator_t), pointer :: mutator
    !> Domain
    class(domain_cell_t), intent(in) :: this
    !> Target domain for the mutator
    class(target_t), intent(in) :: target_domain
    !> Data type for the mutatable property
    type(data_type_t), intent(in) :: data_type
    !> Index for the property among registered properties of the same
    !! target domain and data type
    integer(kind=musica_ik), intent(in) :: property_index

    type(string_t) :: target_name

    select type( target_domain )
    class is( domain_target_cells_t )
      if( data_type .eq. kInteger ) then
        allocate( mutator_integer_t :: mutator )
      else if( data_type .eq. kFloat ) then
        allocate( mutator_float_t   :: mutator )
      else if( data_type .eq. kDouble ) then
        allocate( mutator_double_t  :: mutator )
      else if( data_type .eq. kBoolean ) then
        allocate( mutator_boolean_t :: mutator )
      else
        call die_msg( 706918026, "Unsupported data type requested for cell "//&
                      "domain mutator." )
      end if
    class default
      target_name = target_domain%name( )
      call die_msg( 594599681, "Cell domains to not currently support '"//    &
                    target_name%to_char( )//"' as a property target" )
    end select
    select type( mutator )
    class is( mutator_cell_t )
      mutator%index_ = property_index
    class default
      call die( 534855588 )
    end select

  end function allocate_mutator

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Allocates a accessor for a given target domain and data type
  function allocate_accessor( this, target_domain, data_type, property_index ) &
      result( accessor )

    use musica_assert,                 only : die, die_msg
    use musica_data_type,              only : data_type_t, kBoolean, kDouble, &
                                              kFloat, kInteger
    use musica_domain_target_cells,    only : domain_target_cells_t
    use musica_string,                 only : string_t
    use musica_target,                 only : target_t

    !> Accessor
    class(domain_state_accessor_t), pointer :: accessor
    !> Domain
    class(domain_cell_t), intent(in) :: this
    !> Target domain for the accessor
    class(target_t), intent(in) :: target_domain
    !> Data type for the accessible property
    type(data_type_t), intent(in) :: data_type
    !> Index for the property among registered properties of the same
    !! target domain and data type
    integer(kind=musica_ik), intent(in) :: property_index

    type(string_t) :: target_name

    select type( target_domain )
    class is( domain_target_cells_t )
      if( data_type .eq. kInteger ) then
        allocate( accessor_integer_t :: accessor )
      else if( data_type .eq. kFloat ) then
        allocate( accessor_float_t   :: accessor )
      else if( data_type .eq. kDouble ) then
        allocate( accessor_double_t  :: accessor )
      else if( data_type .eq. kBoolean ) then
        allocate( accessor_boolean_t :: accessor )
      else
        call die_msg( 711130520, "Unsupported data type requested for cell "//&
                      "domain accessor." )
      end if
    class default
      target_name = target_domain%name( )
      call die_msg( 823448865, "Cell domains to not currently support '"//    &
                    target_name%to_char( )//"' as a property target" )
    end select
    select type( accessor )
    class is( accessor_cell_t )
      accessor%index_ = property_index
    class default
      call die( 883192958 )
    end select

  end function allocate_accessor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalize the domain
  subroutine finalize( this )

    !> Domain
    type(domain_cell_t), intent(inout) :: this

    call this%private_destructor( )

  end subroutine finalize

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

end module musica_domain_cell
