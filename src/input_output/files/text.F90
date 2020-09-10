! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The musica_file_text module

!> The file_text_t type and related functions
module musica_file_text

  use musica_constants,                only : musica_ik, musica_dk
  use musica_file,                     only : file_t
  use musica_string,                   only : string_t

  implicit none
  private

  public :: file_text_t

  !> Maximum length of a text file line
  integer, parameter :: kMaxFileLine = 5000
  !> Flag for rows
  integer, parameter :: kRows = 1
  !> Flag for columns
  integer, parameter :: kColumns = 2

  !> A text file
  type, extends(file_t) :: file_text_t
    private
    !> Flag indicating whether the file is open
    logical :: is_open_ = .false.
    !> Fortran file unit
    integer(kind=musica_ik) :: file_unit_ = -1
    !> Delimiter used in file
    type(string_t) :: delimiter_
    !> Indicator of whether the time axis is along rows or columns
    integer(kind=musica_ik) :: time_axis_ = kRows
    !> File variable names
    type(string_t), allocatable :: variable_names_(:)
    !> Number of entries in the file
    integer(kind=musica_ik) :: number_of_entries_ = -1
    !> Current line in the file
    integer(kind=musica_ik) :: current_line_ = -1
    !> Data staged for output
    real(kind=musica_dk), allocatable :: staged_data_(:)
    !> Time associated with staged data [s]
    real(kind=musica_dk) :: staged_data_time__s_ = -huge( 1.0_musica_dk )
  contains
    !> Returns the type of file as a string
    procedure :: type => file_type
    !> Returns the number of dimensions in the file
    procedure :: number_of_dimensions
    !> Returns the number of variables in the file
    procedure :: number_of_variables
    !> Opens the file if it is not currently open
    procedure :: check_open
    !> Finds the row or column index of a variable in the file
    procedure :: get_variable_id
    !> Finds the index for a dimension in the file
    procedure :: get_dimension_id
    !> Finds a variable name by its row or column index
    procedure :: get_variable_name
    !> Adds a variable to the set of variable names (output only)
    procedure :: add_variable
    !> Returns the number of entries in the files
    procedure :: number_of_entries
    !> Gets a subset of the data for one variable
    procedure :: get_data
    !> Outputs data for one variable at one time step
    procedure :: output
    !> Closes the file
    procedure :: close
    !> Initialize the file variable names
    procedure, private :: initialize_variable_names
    !> Count the number of lines in the file
    procedure, private :: count_lines
    !> Rewind the file to the beginning
    procedure, private :: rewind => do_rewind
    !> Move to a specified line in the file
    procedure, private :: move_to_line
    !> Read a line of the file
    procedure, private :: read_line
    !> Output the file header
    procedure, private :: output_header
    !> Flush the staged output data to the file
    procedure, private :: flush_output
    !> Finalizes the file
    final :: finalize
  end type file_text_t

  !> Constructor
  interface file_text_t
    module procedure :: constructor
  end interface file_text_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Creates a file_text_t object for a text file
  function constructor( config ) result( new_obj )

    use musica_assert,                 only : assert_msg, die_msg
    use musica_config,                 only : config_t
    use musica_file,                   only : private_constructor

    !> New text file object
    class(file_t), pointer :: new_obj
    !> Text file configuration
    type(config_t), intent(inout) :: config

    character(len=*), parameter :: my_name = "Text file constructor"
    type(string_t) :: file_name, time_axis
    logical :: found

    allocate( file_text_t :: new_obj )

    call private_constructor( new_obj, config,                                &
                              default_output_file_name = "output.csv" )

    call assert_msg( 137876340,                                               &
                     .not. ( new_obj%is_input( ) .and. new_obj%is_output( ) ),&
                     "Input/Output text files are not supported." )

    select type( new_obj )
    class is( file_text_t )

    ! file structure options
    call config%get( "delimiter", new_obj%delimiter_, my_name, default = "," )
    call config%get( "time axis", time_axis, my_name, default = "rows" )
    if( time_axis .eq. "rows" ) then
      new_obj%time_axis_ = kRows
    else if( time_axis .eq. "columns" ) then
      new_obj%time_axis_ = kColumns
    else
      file_name = new_obj%name( )
      call die_msg( 396740549, "Invalid time axis specified for text file '"//&
                    file_name%to_char( )//"': "//time_axis%to_char( ) )
    end if

    call new_obj%initialize_variable_names( )

    end select

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the type of file as a string
  type(string_t) function file_type( this )

    !> Text file
    class(file_text_t), intent(in) :: this

    file_type = "text"

  end function file_type

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the number of dimensions in the file
  integer(kind=musica_ik) function number_of_dimensions( this )

    use musica_constants,              only : musica_ik

    !> Text file
    class(file_text_t), intent(inout) :: this

    number_of_dimensions = 1

  end function number_of_dimensions

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the number of variables in the file
  integer(kind=musica_ik) function number_of_variables( this )

    use musica_constants,              only : musica_ik

    !> Text file
    class(file_text_t), intent(inout) :: this

    number_of_variables = size( this%variable_names_ )

  end function number_of_variables

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Opens the file if it is not open already
  subroutine check_open( this )

    use musica_assert,                 only : assert_msg, die
    use musica_file,                   only : get_file_unit

    !> Text file
    class(file_text_t), intent(inout) :: this

    integer(kind=musica_ik) :: io_status
    type(string_t) :: file_name

    if( this%is_open_ ) return
    this%file_unit_ = get_file_unit( )
    file_name = this%name( )
    if( this%is_input( ) .and. .not. this%is_output( ) ) then
      open( unit = this%file_unit_, file = file_name%to_char( ),              &
            action = 'READ', iostat = io_status )
    else if( .not. this%is_input( ) .and. this%is_output( ) ) then
      open( unit = this%file_unit_, file = file_name%to_char( ),              &
            action = 'WRITE' , iostat = io_status )
    else
      call die( 320375411 )
    end if
    call assert_msg( 113099184, io_status .eq. 0,                             &
                     "Error opening text file '"//file_name%to_char( )//"'" )
    this%is_open_ = .true.
    this%current_line_ = 0
    if( this%is_output( ) ) this%number_of_entries_ = 0

  end subroutine check_open

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finds the row or column of a variable in the file (starting from 1)
  function get_variable_id( this, variable_name ) result( id )

    use musica_assert,                 only : die_msg

    !> Variable id
    integer(kind=musica_ik) :: id
    !> Text file
    class(file_text_t), intent(inout) :: this
    !> Variable name to find
    character(len=*), intent(in) :: variable_name

    integer(kind=musica_ik) :: i_var
    type(string_t) :: file_name

    do i_var = 1, size( this%variable_names_ )
      if( this%variable_names_( i_var ) .eq. variable_name ) then
        id = i_var
        return
      end if
    end do
    file_name = this%name( )
    call die_msg( 141959683, "Variable '"//variable_name//                    &
                  "' not found in file '"//file_name%to_char( )//"'" )

  end function get_variable_id

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finds an index for a dimension in the text file
  !!
  !! Dimension indices are not rows/columns and currently there is only
  !! one dimension (time) with index 1
  !!
  function get_dimension_id( this, dimension_name ) result( id )

    !> Dimension id
    integer(kind=musica_ik) :: id
    !> Text file
    class(file_text_t), intent(inout) :: this
    !> Name of the dimension
    type(string_t), intent(in) :: dimension_name

    id = 1

  end function get_dimension_id

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finds a variable name by its row or column index (starting from 1)
  function get_variable_name( this, id ) result( name )

    use musica_assert,                 only : assert_msg
    use musica_string,                 only : to_char

    !> Variable name
    type(string_t) :: name
    !> Text file
    class(file_text_t), intent(inout) :: this
    !> Variable id
    integer(kind=musica_ik), intent(in) :: id

    type(string_t) :: file_name

    file_name = this%name( )
    call assert_msg( 356520054, id .gt. 0 .and.                               &
                                id .le. size( this%variable_names_ ),         &
                     "Row/column id "//to_char( id )//" out of bounds for "// &
                     "variables in file '"//file_name%to_char( )//"'" )
    name = this%variable_names_( id )

  end function get_variable_name

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Adds a variable to the set of variable names (output only)
  !!
  !! If the variable name already exists, an error occurs
  !!
  function add_variable( this, variable_name ) result( id )

    use musica_array,                  only : add_to_array
    use musica_assert,                 only : assert_msg, die_msg

    !> Index in the set of variable names for the new variable
    integer(kind=musica_ik) :: id
    !> Text file
    class(file_text_t), intent(inout) :: this
    !> Variable name to add
    character(len=*), intent(in) :: variable_name

    type(string_t) :: file_name
    integer(kind=musica_ik) :: i_var

    file_name = this%name( )
    call assert_msg( 407390789, this%is_output( ),                            &
                     "Cannot add variable '"//variable_name//"' to output "// &
                     "file '"//file_name%to_char( )//"'" )

    do i_var = 1, size( this%variable_names_ )
      if( this%variable_names_( i_var ) .eq. variable_name ) then
        call die_msg( 408844028, "Variable '"//variable_name//                &
                      "' already exists is output file '"//                   &
                      file_name%to_char( )//"'" )
      end if
    end do

    call add_to_array( this%variable_names_, variable_name )
    id = size( this%variable_names_ )

  end function add_variable

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the number of entries in the file
  !!
  !! Each entry is assumed to correspond to a time or date-time
  !!
  function number_of_entries( this )

    use musica_assert,                 only : assert_msg, die

    !> Number of entries in the file
    integer(kind=musica_ik) :: number_of_entries
    !> Text file
    class(file_text_t), intent(inout) :: this

    type(string_t) :: temp_str
    type(string_t), allocatable :: vars(:)

    call assert_msg( 710220160, .not. this%is_output( ),                      &
                     "Number of entries is not available for output files." )
    if( this%number_of_entries_ .ge. 0 ) then
      number_of_entries = this%number_of_entries_
      return
    end if
    if( this%time_axis_ .eq. kRows ) then
      number_of_entries = this%count_lines( ) - 1
    else if( this%time_axis_ .eq. kColumns ) then
      call this%check_open( )
      call this%rewind( )
      temp_str = this%read_line( )
      vars = temp_str%split( this%delimiter_ )
      number_of_entries = size( vars ) - 1
      call this%rewind( )
    else
      call die( 738970281 )
    end if
    this%number_of_entries_ = number_of_entries

  end function number_of_entries

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns a subset of the data for one variable
  subroutine get_data( this, variable_id, start, count, values )

    use musica_assert,                 only : assert, die

    !> Text file
    class(file_text_t), intent(inout) :: this
    !> Row or column index for the variable (starting at 1)
    integer(kind=musica_ik), intent(in) :: variable_id
    !> Starting index in the temporal dimension to return values for
    !! (starting from 1)
    integer(kind=musica_ik), intent(in) :: start
    !> Number of entries to return
    integer(kind=musica_ik), intent(in) :: count
    !> Values to return
    real(kind=musica_dk), intent(out) :: values(count)

    type(string_t) :: temp_str
    type(string_t), allocatable :: str_values(:)
    integer(kind=musica_ik) :: i_val

    call assert( 812525828, this%is_input( ) )
    call assert( 959156473, start .gt. 0 .and.                                &
                            start + count - 1 .le. this%number_of_entries_ )
    if( this%time_axis_ .eq. kRows ) then
      call this%move_to_line( start )
      do while( this%current_line_ .lt. start + count )
        temp_str = this%read_line( )
        str_values = temp_str%split( this%delimiter_ )
        values( this%current_line_ - start ) = str_values( variable_id )
      end do
    else if( this%time_axis_ .eq. kColumns ) then
      call this%move_to_line( variable_id - 1 )
      temp_str = this%read_line( )
      str_values = temp_str%split( this%delimiter_ )
      do i_val = start, start + count - 1
        values( i_val ) = str_values( i_val + 1 )
      end do
    else
      call die( 251684270 )
    end if

  end subroutine get_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Outputs data for one variable at one timestep
  subroutine output( this, variable_id, time__s, output_value )

    !> Text file
    class(file_text_t), intent(inout) :: this
    !> Variable column id
    integer(kind=musica_ik), intent(in) :: variable_id
    !> Simulation time [s]
    real(kind=musica_dk), intent(in) :: time__s
    !> Value to output
    real(kind=musica_dk), intent(in) :: output_value

    if( .not. allocated( this%staged_data_ ) ) then
      allocate( this%staged_data_( size( this%variable_names_ ) ) )
      this%staged_data_(:) = -huge( 1.0_musica_dk )
      this%staged_data_time__s_ = time__s
    end if

    if( this%current_line_ .lt. 1 ) call this%output_header( )
    if( time__s .ne. this%staged_data_time__s_ ) call this%flush_output( )
    this%staged_data_time__s_ = time__s
    this%staged_data_( variable_id ) = output_value

  end subroutine output

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Closes the text file
  subroutine close( this )

    use musica_assert,                 only : assert_msg
    use musica_file,                   only : free_file_unit

    !> Text file
    class(file_text_t), intent(inout) :: this

    integer(kind=musica_ik) :: io_status
    type(string_t) :: file_name

    if( .not. this%is_open_ ) return
    if( this%is_output( ) ) call this%flush_output( )
    close( this%file_unit_, iostat = io_status )
    file_name = this%name( )
    call assert_msg( 428448342, io_status .eq. 0,                             &
                     "Error closing text file '"//file_name%to_char( ) )
    this%is_open_ = .false.
    call free_file_unit( this%file_unit_ )
    this%file_unit_ = -1

  end subroutine close

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize the set of file variables names
  !!
  !! Output files initially have no variables; input files are openned and the
  !! variable names are read into the variable_names_ data member
  !!
  subroutine initialize_variable_names( this )

    use musica_assert,                 only : die

    !> Text file
    class(file_text_t), intent(inout) :: this

    integer(kind=musica_ik) :: io_status, i_var, n_var
    type(string_t) :: temp_str
    type(string_t), allocatable :: values(:)

    if( .not. this%is_input( ) ) then
      allocate( this%variable_names_( 0 ) )
      return
    end if

    call this%check_open( )
    if( this%time_axis_ .eq. kRows ) then
      temp_str = this%read_line( )
      this%variable_names_ = temp_str%split( this%delimiter_ )
    else if( this%time_axis_ .eq. kColumns ) then
      n_var = this%count_lines( )
      allocate( this%variable_names_( n_var ) )
      do i_var = 1, n_var
        temp_str = this%read_line( )
        values = temp_str%split( this%delimiter_ )
        this%variable_names_( i_var ) = values(1)
      end do
      call this%rewind( )
    else
      call die( 373666226 )
    end if
    do i_var = 1, size( this%variable_names_ )
      this%variable_names_( i_var ) =                                         &
          adjustl( trim( this%variable_names_( i_var )%to_char( ) ) )
    end do

  end subroutine initialize_variable_names

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Count the lines in a file
  function count_lines( this ) result( n_lines )

    use musica_assert,                 only : assert

    !> Number of lines in the file
    integer(kind=musica_ik) :: n_lines
    !> Text file
    class(file_text_t), intent(inout) :: this

    integer :: io_status

    call assert( 776402509, this%is_open_ )
    call this%rewind( )
    n_lines = 0
    do
      read( this%file_unit_, *, iostat = io_status )
      if( io_status .ne. 0 ) exit
      n_lines = n_lines + 1
    end do
    call this%rewind( )

  end function count_lines

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Rewind the file to the beginning
  subroutine do_rewind( this )

    use musica_assert,                 only : assert

    !> Text file
    class(file_text_t), intent(inout) :: this

    call assert( 790830712, this%is_open_ )
    rewind( this%file_unit_ )
    this%current_line_ = 0

  end subroutine do_rewind

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Move to a specified line (starting at 1) in the file
  subroutine move_to_line( this, line )

    !> Text file
    class(file_text_t), intent(inout) :: this
    !> Line number to move to
    integer(kind=musica_ik) :: line

    if( this%current_line_ .gt. line ) call this%rewind( )
    do while( this%current_line_ .lt. line )
      read( this%file_unit_, * )
      this%current_line_ = this%current_line_ + 1
    end do

  end subroutine move_to_line

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Read a line in the file (must not advance past the end)
  function read_line( this )

    use musica_assert,                 only : assert, assert_msg

    !> Line read as a string
    type(string_t) :: read_line
    !> Text file
    class(file_text_t), intent(inout) :: this

    integer(kind=musica_ik) :: io_status
    character(len=kMaxFileLine) :: line
    type(string_t) :: file_name

    call assert( 442346119, this%is_open_ )
    read( this%file_unit_, '(a)', iostat = io_status ) line
    file_name = this%name( )
    call assert_msg( 383054321, io_status .eq. 0, "Error reading line in "//  &
                     "file '"//file_name%to_char( )//"'" )
    read_line = line
    this%current_line_ = this%current_line_ + 1

  end function read_line

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Output the file header
  subroutine output_header( this )

    !> Text file
    class(file_text_t), intent(inout) :: this

    integer(kind=musica_ik) :: i_var

    write(this%file_unit_,'(A)',advance="no") "time"
    do i_var = 1, size( this%variable_names_ )
      write(this%file_unit_,'(", ",A)',advance="no")                          &
          this%variable_names_( i_var )%to_char( )
    end do
    write(this%file_unit_,*) ""
    this%current_line_ = this%current_line_ + 1

  end subroutine output_header

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Flush the staged output data to the file
  subroutine flush_output( this )

    !> Text file
    class(file_text_t), intent(inout) :: this

    integer(kind=musica_ik) :: i_var

    write(this%file_unit_,'(D30.20)',advance="no") this%staged_data_time__s_
    do i_var = 1, size( this%staged_data_ )
      write(this%file_unit_,'(", ",D30.20)',advance="no")                     &
          this%staged_data_( i_var )
    end do
    write(this%file_unit_,*) ""
    this%current_line_        = this%current_line_ + 1
    this%staged_data_time__s_ = -huge( 1.0_musica_dk )
    this%staged_data_(:)      = -huge( 1.0_musica_dk )

  end subroutine flush_output

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalizes the file object
  subroutine finalize( this )

    !> Text file
    type(file_text_t), intent(inout) :: this

    call this%close( )

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module musica_file_text
