! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The musica_file_updater module

!> The file_updater_t type and related functions
module musica_file_updater

  use musica_constants,                only : musica_dk, musica_ik
  use musica_domain,                   only : domain_state_accessor_t,        &
                                              domain_state_mutator_t
  use musica_file_variable,            only : file_variable_t

  implicit none
  private

  public :: file_updater_t, file_updater_ptr

  !> Max length of staged data array
  integer(kind=musica_ik), parameter :: kMaxStagedData = 100

  !> Updater for a paired MUSICA <->  variable
  !!
  !! Staging data and functions for updating MUSICA state variables from input
  !! data and updating output files from the MUSICA state.
  !!
  type :: file_updater_t
    private
    !> Mutator for the variable
    class(domain_state_mutator_t), pointer :: mutator_ => null( )
    !> Accessor for the variable
    class(domain_state_accessor_t), pointer :: accessor_ => null( )
    !> Variable information
    class(file_variable_t), pointer :: variable_ => null( )
    !> Index of first staged data
    integer(kind=musica_ik) :: first_staged_index_ = 1
    !> Number of staged data
    integer(kind=musica_ik) :: number_staged_ = 0
    !> Staged data
    real(kind=musica_dk) :: staged_data_(kMaxStagedData) = -huge(1.0_musica_dk)
  contains
    !> Updates the state for a given index in the temporal dimension
    procedure :: update_state
    !> Outputs data to the file
    procedure :: output
    !> Prints the properties of the updater
    procedure :: print => do_print
    !> Updates the staged data
    procedure, private :: update_staged_data
    !> Finalize the updater
    final :: finalize
  end type file_updater_t

  !> Constructor
  interface file_updater_t
    module procedure :: constructor
  end interface file_updater_t

  !> Pointer to file_updater_t objects
  type :: file_updater_ptr
    type(file_updater_t), pointer :: val_ => null( )
  end type file_updater_ptr

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Creates a MUSICA <-> File variable match
  function constructor( file, domain, variable ) result( new_obj )

    use musica_assert,                 only : die
    use musica_domain,                 only : domain_t
    use musica_file,                   only : file_t
    use musica_file_variable,          only : file_variable_t
    use musica_string,                 only : string_t

    !> New MUSICA<->File variable match
    type(file_updater_t), pointer :: new_obj
    !> File to update to or from
    class(file_t), intent(inout) :: file
    !> Model domain
    class(domain_t), intent(inout) :: domain
    !> File variable
    class(file_variable_t), intent(in) :: variable

    character(len=*), parameter :: my_name = "File updater constructor"
    type(string_t) :: std_units, var_name

    allocate( new_obj )
    allocate( new_obj%variable_, source = variable )
    var_name = variable%musica_name( )
    std_units = domain%cell_state_units( var_name%to_char( ) )
    if( file%is_input( ) ) then
      new_obj%mutator_ => domain%cell_state_mutator( var_name%to_char( ),     & !- state variable name
                                                     std_units%to_char( ),    & !- MUSICA units
                                                     my_name )
    end if
    if( file%is_output( ) ) then
      new_obj%accessor_ => domain%cell_state_accessor( var_name%to_char( ),   & !- state variable name
                                                       std_units%to_char( ),  & !- MUSICA units
                                                       my_name )
    end if

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Updates a domain state for a given index in the temporal dimension
  subroutine update_state( this, file, index, iterator, state )

    use musica_assert,                 only : assert
    use musica_domain,                 only : domain_iterator_t,              &
                                              domain_state_t
    use musica_file,                   only : file_t

    !> File updater
    class(file_updater_t), intent(inout) :: this
    !> File file
    class(file_t), intent(inout) :: file
    !> Index in the temporal dimension to update from
    integer(kind=musica_ik), intent(in) :: index
    !> Domain state iterator
    class(domain_iterator_t), intent(inout) :: iterator
    !> Domain state to update
    class(domain_state_t), intent(inout) :: state

    if( index .lt. this%first_staged_index_ .or.                             &
        index .gt. ( this%first_staged_index_ + this%number_staged_ ) - 1 )  &
      call this%update_staged_data( file, index )
    call assert( 269276238, associated( this%mutator_ ) )
    call iterator%reset( )
    do while( iterator%next( ) )
      call state%update( iterator, this%mutator_,                             &
                    this%staged_data_( index - this%first_staged_index_ + 1 ) )
    end do

  end subroutine update_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Outputs data to a file
  subroutine output( this, file, time__s, domain, domain_state, iterator )

    use musica_assert,                 only : assert_msg
    use musica_domain,                 only : domain_t, domain_state_t,       &
                                              domain_iterator_t
    use musica_file,                   only : file_t

    !> File updater
    class(file_updater_t), intent(inout) :: this
    !> Output file
    class(file_t), intent(inout) :: file
    !> Current simulation time [s]
    real(kind=musica_dk), intent(in) :: time__s
    !> Model domain
    class(domain_t), intent(in) :: domain
    !> Domain state
    class(domain_state_t), intent(in) :: domain_state
    !> Domain iterator
    class(domain_iterator_t), intent(inout) :: iterator

    real(kind=musica_dk) :: state_value

    call iterator%reset( )
    if( iterator%next( ) ) then
      call domain_state%get( iterator, this%accessor_, state_value )
      call this%variable_%output( file, time__s, state_value )
    end if
    call assert_msg( 608265274, .not. iterator%next( ), "Output files are "// &
                     "not yet set up for multiple cells." )

  end subroutine output

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Prints the contents of the updater
  subroutine do_print( this )

    !> File updater
    class(file_updater_t), intent(in) :: this

    call this%variable_%print( )

  end subroutine do_print

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Updates the staged data to start from a given index
  subroutine update_staged_data( this, file, index )

    use musica_assert,                 only : assert
    use musica_file,                   only : file_t

    !> File updater
    class(file_updater_t), intent(inout) :: this
    !> File file
    class(file_t), intent(inout) :: file
    !> New starting index
    integer(kind=musica_ik), intent(in) :: index

    integer(kind=musica_ik) :: n_times

    n_times = min( kMaxStagedData,                                            &
                   this%variable_%time_dimension_size( ) - index + 1 )
    call file%check_open( )
    call assert( 382334001, index .gt. 0 .and. n_times .ge. 1 )
    this%staged_data_(:) = -huge( 1.0_musica_dk )
    call this%variable_%get_data( file, index, n_times, this%staged_data_ )
    this%first_staged_index_ = index
    this%number_staged_      = n_times

  end subroutine update_staged_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalize the File updater
  subroutine finalize( this )

    !> File updater
    type(file_updater_t), intent(inout) :: this

    if( associated( this%mutator_  ) ) deallocate( this%mutator_  )
    if( associated( this%accessor_ ) ) deallocate( this%accessor_ )
    if( associated( this%variable_ ) ) deallocate( this%variable_ )
  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module musica_file_updater
