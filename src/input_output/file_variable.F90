! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The musica_file_variable module

!> The file_variable_t type and related functions
module musica_file_variable

  use musica_constants,                only : musica_dk, musica_ik
  use musica_convert,                  only : convert_t
  use musica_string,                   only : string_t

  implicit none
  private

  public :: file_variable_t, file_variable_ptr, find_variable_by_name,    &
            find_variable_by_musica_name, private_constructor

  !> A File variable
  !!
  !! Only variables that have been successfully matched to a MUSICA domain
  !! state variable are allowed. All matching criteria are passed to the
  !! file_variable_t constructor. If a match is found, a new object is
  !! returned.
  !!
  !! The file_variable_t handles all conversions, offsetting, scaling,
  !! etc. and can be used to return sub-sets of the file data during the
  !! simulation in MUSICA units after applying any specified conversions.
  !!
  type, abstract :: file_variable_t
    private
    !> Name in the file
    type(string_t) :: name_
    !> Expected MUSICA name
    type(string_t) :: musica_name_
    !> Units for variable in file data
    type(string_t) :: units_
    !> Converter to MUSICA units
    type(convert_t) :: converter_
    !> Scaling factor
    real(kind=musica_dk) :: scale_factor_ = 1.0_musica_dk
    !> Offset (applied to file data after scaling and before unit conversion)
    real(kind=musica_dk) :: offset_ = 0.0_musica_dk
    !> Shift (applied after unit conversion)
    real(kind=musica_dk) :: shift_ = 0.0_musica_dk
  contains
    !> Returns the name of the variable
    procedure :: name => variable_name
    !> Returns the MUSICA name for the variable
    procedure :: musica_name
    !> Returns the units used in the file for the variable
    procedure :: units
    !> Scale, offset, convert units, and shift a set of data to musica values
    !! (Should only be called by extending types)
    procedure :: convert_to_musica_values
    !> Scale, offset, convert units, and shift a set of data to file values
    !! (Should only be called by extending types)
    procedure :: convert_to_file_values
    !> Gets the number of entries for the variable in the temporal dimension
    procedure(time_dimension_size), deferred :: time_dimension_size
    !> Gets a sub-set of the variable data for a specified index range
    !!
    !! Data are returned after applying conversions set up during
    !! initialization.
    !!
    procedure(get_data), deferred :: get_data
    !> Outputs data to the file for a given time step
    procedure(output), deferred :: output
    !> Prints the properties of the variable
    procedure :: print => do_print
    !> Sets the NetCDF <-> MUSICA matching criteria
    procedure, private :: set_matching_criteria
    !> Does standard NetCDF -> MUSICA name conversions
    procedure, private :: do_standard_name_conversions
    !> Attempts to match to a MUSICA domain state variable
    procedure, private :: do_match
    !> Sets any specified data adjustments
    procedure, private :: set_adjustments
  end type file_variable_t

  !> Pointer to file_variable_t objects
  type :: file_variable_ptr
    class(file_variable_t), pointer :: val_ => null( )
  end type file_variable_ptr

interface

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets the number of entries of the variable in the temporal dimension
  integer(kind=musica_ik) function time_dimension_size( this )
    use musica_constants,              only : musica_ik
    import file_variable_t
    !> File variable
    class(file_variable_t), intent(in) :: this
  end function time_dimension_size

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets a sub-set of the data from the file
  !!
  !! Conversions are applied in the following order:
  !! - scaling
  !! - offsetting
  !! - conversion to MUSICA units
  !! - shifting
  !!
  subroutine get_data( this, file, start, count, values )
    use musica_constants,              only : musica_ik, musica_dk
    use musica_file,                   only : file_t
    import file_variable_t
    !> File variable
    class(file_variable_t), intent(in) :: this
    !> Input/Output file
    class(file_t), intent(inout) :: file
    !> Starting index for returned data
    integer(kind=musica_ik), intent(in) :: start
    !> Number of data points to return
    integer(kind=musica_ik), intent(in) :: count
    !> Values to return
    real(kind=musica_dk), intent(out) :: values(count)
  end subroutine get_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Outputs data to the file for a given timestep
  !!
  !! The state_value will be converted according to the variable configuration
  !! prior to outputting it to the file.
  !!
  subroutine output( this, file, time__s, state_value )
    use musica_constants,              only : musica_dk
    use musica_file,                   only : file_t
    import file_variable_t
    !> File variable
    class(file_variable_t), intent(inout) :: this
    !> Output file
    class(file_t), intent(inout) :: file
    !> Current simulation time [s]
    real(kind=musica_dk), intent(in) :: time__s
    !> Value to output
    real(kind=musica_dk), intent(in) :: state_value
  end subroutine output

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the name of the variable
  type(string_t) function variable_name( this )

    !> File variable
    class(file_variable_t), intent(in) :: this

    variable_name = this%name_

  end function variable_name

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the expected MUSICA name for the variable
  type(string_t) function musica_name( this )

    !> File variable
    class(file_variable_t), intent(in) :: this

    musica_name = this%musica_name_

  end function musica_name

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the units used in the file for the variable
  type(string_t) function units( this )

    !> File variable
    class(file_variable_t), intent(in) :: this

    units = this%units_

  end function units

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Scale, offset, convert units, and shift results for a set of data
  subroutine convert_to_musica_values( this, values )

    !> File variable
    class(file_variable_t), intent(in) :: this
    !> Data to process
    real(kind=musica_dk), intent(inout) :: values(:)

    integer(kind=musica_ik) :: i_val

    do i_val = 1, size( values )
      values( i_val ) = values( i_val ) * this%scale_factor_ + this%offset_
      values( i_val ) = this%converter_%to_standard( values( i_val ) )
      values( i_val ) = values( i_val ) + this%shift_
    end do

  end subroutine convert_to_musica_values

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Scale, offset, convert units, and shift a set of data to file values
  !! (Should only be called by extending types)
  subroutine convert_to_file_values( this, values )

    !> File variable
    class(file_variable_t), intent(in) :: this
    !> Data to process
    real(kind=musica_dk), intent(inout) :: values(:)

    integer(kind=musica_ik) :: i_val

    do i_val = 1, size( values )
      values( i_val ) = values( i_val ) - this%shift_
      values( i_val ) = this%converter_%to_non_standard( values( i_val ) )
      values( i_val ) = ( values( i_val ) - this%offset_ ) / this%scale_factor_
    end do

  end subroutine convert_to_file_values

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Prints the properties of the variable
  subroutine do_print( this )

    !> File variable
    class(file_variable_t), intent(in) :: this

    write(*,*) "*** Variable: "//this%name_%to_char( )//" ***"
    write(*,*) "MUSICA name: "//this%musica_name_%to_char( )
    write(*,*) "units: "//this%units_%to_char( )
    write(*,*) "scale factor:", this%scale_factor_
    write(*,*) "offset:", this%offset_
    write(*,*) "shift:", this%shift_

  end subroutine do_print

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Sets up matching between MUSICA and File variables
  subroutine set_matching_criteria( this, config )

    use musica_config,                 only : config_t

    !> File variable
    class(file_variable_t), intent(inout) :: this
    !> Configuration describing how to match to MUSICA variables
    !!
    !! If omitted, standard matching is applied
    class(config_t), intent(inout), optional :: config

    character(len=*), parameter :: my_name = "File variable matching"
    type(config_t) :: vars, var_data
    logical :: found, general_match

    ! default to File variable name
    this%musica_name_ = this%name_

    ! get specific property matching if present
    call config%get( "properties", vars, my_name, found = found )

    ! look for specific and then general variable information
    general_match = .false.
    if( found ) then
      call vars%get( this%name_%to_char( ), var_data, my_name, found = found )
      if( .not. found ) then
        call vars%get( "*", var_data, my_name, found = found )
        general_match = found
      end if
      call vars%finalize( )
    end if

    ! update matching criteria as specified in configuration
    if( found ) then
      call var_data%get( "MusicBox name", this%musica_name_, my_name,       &
                         default = this%musica_name_%to_char( ) )
      call var_data%get( "units", this%units_, my_name,                     &
                         default = this%units_%to_char( ) )
      call var_data%finalize( )
      if( general_match ) then
        this%musica_name_ = this%musica_name_%replace( "*",                   &
                                                       this%name_%to_char( ) )
      end if
    end if

    ! do standard name conversions
    call this%do_standard_name_conversions( )

  end subroutine set_matching_criteria

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Does standard name conversions between NetCDF and MUSICA
  subroutine do_standard_name_conversions( this )

    !> File variable
    class(file_variable_t), intent(inout) :: this

    type(string_t), allocatable :: strs(:)

    associate( str => this%musica_name_ )
      str = str%replace( "CONC.", "chemical_species%" )
      str = str%replace( "ENV.",  "" )
      str = str%replace( "EMIS.", "emission_rates%" )
      str = str%replace( "LOSS.", "loss_rate_constants%" )
      str = str%replace( "PHOT.", "photolysis_rate_constants%" )
    end associate

    ! extract units if included in name
    strs = this%musica_name_%split( "." )
    if( size( strs ) .eq. 2 ) then
      this%musica_name_ = strs(1)
      this%units_       = strs(2)
    end if

  end subroutine do_standard_name_conversions

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Attempts to find the variable in the MUSICA domain or create the variable
  !! in the domain for certain input variables.
  !!
  logical function do_match( this, domain )

    use musica_domain,                 only : domain_t

    !> File variable
    class(file_variable_t), intent(inout) :: this
    !> MUSICA domain
    class(domain_t), intent(inout) :: domain

    character(len=*), parameter :: my_name = "File variable matcher"

    ! create state variables for emissions and loss rates
    if( this%musica_name_%substring( 1, 15 ) .eq. "emission_rates%" ) then
      call domain%register_cell_state_variable( this%musica_name_%to_char( ), & !- state variable name
                                                "mol m-3 s-1",                & !- MUSICA units
                                                0.0d0,                        & !- default units
                                                my_name )
    else if( this%musica_name_%substring( 1, 20 )                             &
             .eq. "loss_rate_constants%" ) then
      call domain%register_cell_state_variable( this%musica_name_%to_char( ), & !- state variable name
                                                "s-1",                        & !- MUSICA units
                                                0.0d0,                        & !- default value
                                                my_name )
    end if

    ! look for state variables
    do_match = domain%is_cell_state_variable( this%musica_name_%to_char( ) )

    ! look for standard MUSICA dimensions and set up conversions
    if( .not. do_match ) then
      if( this%musica_name_ .eq. "time" ) then
        do_match = .true.
        if( this%units_ .eq. "" ) this%units_ = "s"
        this%converter_ = convert_t( "s", this%units_ )
      end if
    else
      if( this%units_ .eq. "" ) then
        this%units_ = domain%cell_state_units( this%musica_name_%to_char( ) )
      end if
      this%converter_ = convert_t(                                            &
        domain%cell_state_units( this%musica_name_%to_char( ) ), this%units_ )
    end if

  end function do_match

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Sets any specified data adjustments
  subroutine set_adjustments( this, file, config )

    use musica_assert,                 only : assert_msg
    use musica_config,                 only : config_t
    use musica_datetime,               only : datetime_t
    use musica_file,                   only : file_t

    !> File variable
    class(file_variable_t), intent(inout) :: this
    !> Input/Output File
    class(file_t), intent(inout) :: file
    !> Configuration data
    type(config_t), intent(inout) :: config

    character(len=*), parameter :: my_name = "File variable adjustments"
    type(config_t) :: vars, var_data, shift_data
    logical :: found
    real(kind=musica_dk) :: values(1)
    type(datetime_t) :: shift
    type(string_t) :: units

    ! get specific property data if present
    call config%get( "properties", vars, my_name, found = found )

    ! look for specific and then general variable information
    if( found ) then
      call vars%get( this%name_%to_char( ), var_data, my_name, found = found )
      if( .not. found ) then
        call vars%get( "*", var_data, my_name, found = found )
      end if
      call vars%finalize( )
    end if

    ! update matching criteria as specified in configuration
    if( found ) then
      call var_data%get( "shift first entry to", shift_data, my_name,         &
                         found = found )
      if( found ) then
        units = this%converter_%standard_units( )
        ! for now, just handle time shifts
        call assert_msg( 850243996, units .eq. "s",                           &
                         "Data shifts are not currently supported for "//     &
                         "units of: "//units%to_char( ) )
        call this%get_data( file, 1, 1, values )
        shift = datetime_t( shift_data )
        this%shift_ = shift%in_seconds( ) - values(1)
        call shift_data%finalize( )
      end if
      call var_data%finalize( )
    end if

  end subroutine set_adjustments

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Private constructor for common data elements
  !! (Should only be called by constructors of extending types)
  subroutine private_constructor( this, config, file, domain, name, units )

    use musica_config,                 only : config_t
    use musica_domain,                 only : domain_t
    use musica_file,                   only : file_t

    !> File variable
    class(file_variable_t), intent(inout), pointer :: this
    !> Variable configuration
    class(config_t), intent(inout) :: config
    !> Input/Output file
    class(file_t), intent(inout) :: file
    !> Model domain
    class(domain_t), intent(inout) :: domain
    !> Name used in the file for the variable
    type(string_t), intent(in) :: name
    !> Units used in the file for the variable
    !! (If not included, standard MUSICA units will be assumed)
    type(string_t), intent(in), optional :: units

    this%name_ = name
    if( present( units ) ) then
      this%units_ = units
    else
      this%units_ = ""
    end if

    call this%set_matching_criteria( config )
    if( .not. this%do_match( domain ) ) then
      deallocate( this )
      this => null( )
      return
    end if
    call this%set_adjustments( file, config )

  end subroutine private_constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finds a File variable by name in a set of variables
  !!
  !! Variable matching is case-insensitive
  !!
  function find_variable_by_name( set, name, found ) result( var_id )

    !> Index of variable in set (-1 if not found)
    integer(musica_ik) :: var_id
    !> Set of File variables
    class(file_variable_t), intent(in) :: set(:)
    !> Variable name to locate
    type(string_t), intent(in) :: name
    !> Flag indicating whether variable was found
    logical, intent(out), optional :: found

    type(string_t) :: l_name, var_name

    l_name = name%to_lower( )
    do var_id = 1, size( set )
      if( set( var_id )%name_%to_lower( ) .eq. l_name ) then
        if( present( found ) ) found = .true.
        return
      end if
    end do
    if( present( found ) ) found = .false.

  end function find_variable_by_name

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finds a File variable id by its expected MUSICA name
  !!
  !! Variable matching is case-insensitive
  !!
  function find_variable_by_musica_name( set, name, found ) result( var_id )

    !> Index of variable in set (-1 if not found)
    integer(musica_ik) :: var_id
    !> Set of File variables
    class(file_variable_t), intent(in) :: set(:)
    !> Domain variable name to locate
    type(string_t), intent(in) :: name
    !> Flag indicating whether variable was found
    logical, intent(out), optional :: found

    type(string_t) :: l_name, musica_name

    l_name = name%to_lower( )
    do var_id = 1, size( set )
      if( set( var_id )%musica_name_%to_lower( ) .eq. l_name ) then
        if( present( found ) ) found = .true.
        return
      end if
    end do
    if( present( found ) ) found = .false.

  end function find_variable_by_musica_name

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module musica_file_variable
