! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The musica_io module

!> The input_output_processor_t type and related functions
module musica_input_output_processor

  use musica_constants,                only : musica_ik, musica_dk
  use musica_file,                     only : file_t
  use musica_file_dimension,           only : file_dimension_t
  use musica_file_updater,             only : file_updater_ptr
  use musica_domain,                   only : domain_iterator_t

  implicit none
  private

  public :: input_output_processor_t, input_output_processor_ptr

  !> Input/Output processor
  !!
  !! One input/output processor should be set up for each source/destination.
  !!
  !! \todo add full description and examples of I/O processors
  !!
  type :: input_output_processor_t
    !> File attributes and functions
    class(file_t), pointer :: file_ => null( )
    !> Time dimension
    class(file_dimension_t), pointer :: time_ => null( )
    !> Last time index used
    integer(kind=musica_ik) :: last_time_index_ = 1
    !> Updaters for successfully paired MUSICA <-> I/O variables
    type(file_updater_ptr), allocatable :: updaters_(:)
    !> Iterator over all domain cells
    class(domain_iterator_t), pointer :: iterator_ => null( )
  contains
    !> Registers a state variable for output
    procedure :: register_output_variable
    !> Get the times corresponding to entries (for input data) [s]
    procedure :: entry_times__s
    !> Updates the model state with input data
    procedure :: update_state
    !> Outputs the current domain state
    procedure :: output
    !> Print the input/output configuration information
    procedure :: print => do_print
    !> Loads input file variable names and units
    procedure, private :: load_input_variables
    !> Finalize the input/output processor
    final :: finalize
  end type input_output_processor_t

  !> Input/output pointer
  type :: input_output_processor_ptr
    class(input_output_processor_t), pointer :: val_ => null( )
  contains
    final :: input_output_processor_ptr_finalize
  end type input_output_processor_ptr

  !> Constructor
  interface input_output_processor_t
    module procedure :: constructor
  end interface input_output_processor_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Creates a connection to an input or output file
  !!
  !! At minimum, \c config must include a top-level key-value pair "intent",
  !! which can be either "input" or "output".
  !!
  !! A "file name" is also required for files openned for input. This is
  !! optional for output files, with the default name starting with "output"
  !! and having an appropriate file extension.
  !!
  !! Input files require the model domain, object for mapping between model
  !! domain and file variables.
  !!
  function constructor( config, domain ) result( new_obj )

    use musica_assert,                 only : die_msg
    use musica_config,                 only : config_t
    use musica_domain,                 only : domain_t
    use musica_file_factory,           only : file_builder
    use musica_string,                 only : string_t

    !> New input/output processor
    type(input_output_processor_t), pointer :: new_obj
    !> Configuration data
    class(config_t), intent(inout) :: config
    !> Model domain
    class(domain_t), intent(inout), optional :: domain

    character(len=*), parameter :: my_name = 'I/O processor constructor'
    type(string_t) :: temp_str
    logical :: found, is_input

    allocate( new_obj )

    ! set up the file
    new_obj%file_ => file_builder( config )

    allocate( new_obj%updaters_( 0 ) )

    ! load the variable names and units from input files
    if( new_obj%file_%is_input( ) ) then
      if( present( domain ) ) then
        call new_obj%load_input_variables( domain, config )
      else
        call die_msg( 264651720, "Input files require the model domain "//    &
                                 "during initialization for mapping." )
      end if
    end if

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Registers a state variable for output
  !!
  !! Any scaling, conversion, or interpolation for the variable should be
  !! set up by extending types when this function is called.
  !!
  subroutine register_output_variable( this, domain, domain_variable_name,    &
      units, io_variable_name )

    use musica_assert,                 only : assert_msg
    use musica_config,                 only : config_t
    use musica_domain,                 only : domain_t
    use musica_file_updater,           only : file_updater_t
    use musica_file_variable,          only : file_variable_t
    use musica_file_variable_factory,  only : file_variable_builder
    use musica_string,                 only : string_t

    !> Input/output
    class(input_output_processor_t), intent(inout) :: this
    !> Model domain
    class(domain_t), intent(inout) :: domain
    !> Variable to register
    character(len=*), intent(in) :: domain_variable_name
    !> Units to use for intput/output data
    character(len=*), intent(in) :: units
    !> Optional custom name to use in file
    !!
    !! If not included, the standard MUSICA name will be used.
    character(len=*), intent(in), optional :: io_variable_name

    character(len=*), parameter :: my_name = "Output variable registrar"
    type(string_t) :: file_name, io_var_name
    type(config_t) :: variable_config, temp_config
    class(file_variable_t), pointer :: new_var
    type(file_updater_ptr), allocatable :: temp_updaters(:)

    file_name = this%file_%name( )
    call assert_msg( 567596084, this%file_%is_output( ),                      &
                     "Cannot register output of '"//                          &
                     domain_variable_name//"' to input file '"//              &
                     file_name%to_char( )//"'" )
    call variable_config%empty( )
    call variable_config%add( "units", units, my_name )
    call variable_config%add( "MusicBox name", domain_variable_name, my_name )
    call temp_config%empty( )
    call temp_config%add( io_variable_name, variable_config, my_name )
    call variable_config%empty( )
    call variable_config%add( "properties", temp_config, my_name )
    call variable_config%add( "type", this%file_%type( ), my_name )
    call temp_config%finalize( )
    if( present( io_variable_name ) ) then
      io_var_name = io_variable_name
    else
      io_var_name = domain_variable_name
    end if
    new_var => file_variable_builder( variable_config, domain, this%file_,    &
                                      io_var_name%to_char( ) )
    call variable_config%finalize( )
    call assert_msg( 987906252, associated( new_var ),                        &
                     "Could not find domain state variable '"//               &
                     domain_variable_name//"' to register as '"//             &
                     io_var_name%to_char( )//"' in output file '"//           &
                     file_name%to_char( )//"'" )
    allocate( temp_updaters( size( this%updaters_ ) ) )
    temp_updaters(:) = this%updaters_(:)
    deallocate( this%updaters_ )
    allocate( this%updaters_( size( temp_updaters ) + 1 ) )
    this%updaters_( 1:size( temp_updaters ) ) = temp_updaters(:)
    deallocate( temp_updaters )
    this%updaters_( size( this%updaters_ ) )%val_ =>                        &
        file_updater_t( this%file_, domain, new_var )
    deallocate( new_var )

  end subroutine register_output_variable

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Get the times corresponding to entries (for input data) [s]
  !!
  !! These times include any adjustments specified in the configuration data
  function entry_times__s( this )

    !> Entry times [s]
    real(kind=musica_dk), allocatable :: entry_times__s(:)
    !> Input/output
    class(input_output_processor_t), intent(inout) :: this

    entry_times__s = this%time_%get_values( )

  end function entry_times__s

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Updates the model state with input data
  !!
  !! If a time is included, input data for the specified time (with any
  !! necessary interpolation) will be used to update domain state variables
  !! registered with the \c input_output_processor_t type during intialization.
  !!
  !! If no time is provided the first entry in the input data will be used
  !! to update the domain state (used for initial conditions).
  !!
  subroutine update_state( this, domain, domain_state, time__s )

    use musica_assert,                 only : assert
    use musica_domain,                 only : domain_t, domain_state_t

    !> Input/output
    class(input_output_processor_t), intent(inout) :: this
    !> Model domain
    class(domain_t), intent(in) :: domain
    !> Domain state to update
    class(domain_state_t), intent(inout) :: domain_state
    !> Current simulation time [s]
    real(kind=musica_dk), intent(in), optional :: time__s

    integer(kind=musica_ik) :: i_data, i_updater
    logical :: found

    call assert( 682224647, this%file_%is_input( ) )
    i_data = 1
    if( present( time__s ) ) then
      i_data = this%time_%get_index( time__s, is_exact = found,               &
                                     guess = this%last_time_index_ )
      if( .not. found ) return
    end if
    if( .not. associated( this%iterator_ ) ) then
      this%iterator_ => domain%cell_iterator( )
    end if
    do i_updater = 1, size( this%updaters_ )
      call this%updaters_( i_updater )%val_%update_state( this%file_, i_data, &
                                                this%iterator_, domain_state )
    end do

  end subroutine update_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Outputs the current domain state
  !!
  !! Domain state variables registered with the \c input_output_processor_t
  !! type during initialization will be output for the current simulation
  !! time.
  !!
  subroutine output( this, time__s, domain, domain_state )

    use musica_assert,                 only : die_msg
    use musica_domain,                 only : domain_t, domain_state_t

    !> Input/output
    class(input_output_processor_t), intent(inout) :: this
    !> Current simulation time [s]
    real(kind=musica_dk), intent(in) :: time__s
    !> Model domain
    class(domain_t), intent(in) :: domain
    !> Domain state
    class(domain_state_t), intent(in) :: domain_state

    integer(kind=musica_ik) :: i_updater

    if( .not. associated( this%iterator_ ) ) then
      this%iterator_ => domain%cell_iterator( )
    end if

    do i_updater = 1, size( this%updaters_ )
      call this%updaters_( i_updater )%val_%output( this%file_, time__s,      &
        domain, domain_state, this%iterator_ )
    end do

  end subroutine output

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Print the input/output configuration information
  subroutine do_print( this )

    !> Input/output
    class(input_output_processor_t), intent(in) :: this

    integer(kind=musica_ik) :: i

    write(*,*) "***** Input/Output Processor Configuration *****"
    write(*,*) ""
    call this%file_%print( )
    write(*,*) ""
    call this%time_%print( )
    write(*,*) ""
    write(*,*) "---------------------"
    write(*,*) " State/Output Updaters"
    write(*,*) "---------------------"
    if( allocated( this%updaters_ ) ) then
      do i = 1, size( this%updaters_ )
        call this%updaters_( i )%val_%print( )
      end do
    end if
    write(*,*) ""
    write(*,*) "***** End Input/Output Processor Configuration *****"

  end subroutine do_print

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Loads input file variable names and units and id
  subroutine load_input_variables( this, domain, config )

    use musica_assert,                 only : assert, assert_msg
    use musica_config,                 only : config_t
    use musica_domain,                 only : domain_t
    use musica_file_dimension_factory, only : file_dimension_builder
    use musica_file_updater,           only : file_updater_t
    use musica_file_variable,          only : file_variable_ptr
    use musica_file_variable_factory,  only : file_variable_builder
    use musica_string,                 only : string_t, to_char

    !> Input/Output processor
    class(input_output_processor_t), intent(inout) :: this
    !> MUSICA domain
    class(domain_t), intent(inout) :: domain
    !> Input/output configuration
    type(config_t), intent(inout) :: config

    character(len=*), parameter :: my_name = "Load input file variables"
    type(string_t) :: file_name
    integer(kind=musica_ik) :: n_dims, n_vars, i_var, n_match, i_match
    type(file_variable_ptr), allocatable :: vars(:)

    call this%file_%check_open( )
    file_name = this%file_%name( )
    n_dims    = this%file_%number_of_dimensions( )
    n_vars    = this%file_%number_of_variables( )
    allocate( vars( n_vars ) )
    n_match = 0
    do i_var = 1, n_vars
    vars( i_var )%val_ =>                                                     &
        file_variable_builder( config, domain, this%file_,                    &
                               variable_id = i_var )
      if( associated( vars( i_var )%val_ ) ) then
        if( vars( i_var )%val_%musica_name( ) .eq. "time" ) cycle
        n_match = n_match + 1
      end if
    end do
    if( allocated( this%updaters_ ) ) deallocate( this%updaters_ )
    allocate( this%updaters_( n_match ) )
    i_match = 0
    do i_var = 1, n_vars
      if( associated( vars( i_var )%val_ ) ) then
        if( vars( i_var )%val_%musica_name( ) .eq. "time" ) then
          this%time_ =>                                                       &
              file_dimension_builder( config, this%file_, vars( i_var )%val_ )
          cycle
        end if
        i_match = i_match + 1
        this%updaters_( i_match )%val_ =>                                     &
            file_updater_t( this%file_, domain, vars( i_var )%val_ )
      end if
    end do
    do i_var = 1, n_vars
      if( associated( vars( i_var )%val_ ) ) deallocate( vars( i_var )%val_ )
    end do
    call assert( 177448848, i_match .eq. n_match )
    deallocate( vars )

  end subroutine load_input_variables

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalize the input/output processor
  subroutine finalize( this )

    !> Input/Output processor
    type(input_output_processor_t), intent(inout) :: this

    integer(kind=musica_ik) :: i_updater

    if( associated( this%file_ ) ) deallocate( this%file_ )
    if( associated( this%time_ ) ) deallocate( this%time_ )
    if( allocated( this%updaters_ ) ) then
      do i_updater = 1, size( this%updaters_ )
        if( associated( this%updaters_( i_updater )%val_ ) ) then
          deallocate( this%updaters_( i_updater )%val_ )
        end if
      end do
      deallocate( this%updaters_ )
    end if
    if( associated( this%iterator_ ) ) deallocate( this%iterator_ )

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalize an input/output pointer
  subroutine input_output_processor_ptr_finalize( this )

    !> Input/output pointer
    type(input_output_processor_ptr), intent(inout) :: this

    if( associated( this%val_ ) ) deallocate( this%val_ )

  end subroutine input_output_processor_ptr_finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module musica_input_output_processor
