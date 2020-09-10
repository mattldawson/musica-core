! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The musica_file_netcdf module

!> The file_netcdf_t type and related functions
module musica_file_netcdf

  use musica_constants,                only : musica_ik, musica_dk
  use musica_file,                     only : file_t

  implicit none
  private

  public :: file_netcdf_t

  !> A NetCDF file
  type, extends(file_t) :: file_netcdf_t
    private
    !> Flag indicating whether the file is open
    logical :: is_open_ = .false.
    !> NetCDF file id
    integer(kind=musica_ik) :: id_
  contains
    !> Returns the type of file as a string
    procedure :: type => file_type
    !> Returns the NetCDF file id
    procedure :: id
    !> Returns the number of dimensions in the file
    procedure :: number_of_dimensions
    !> Returns the number of variables in the file
    procedure :: number_of_variables
    !> Opens the file if it is not currently open
    procedure :: check_open
    !> Checks a returned NetCDF status code and fail if an error occurred
    procedure :: check_status
    !> Closes the file
    procedure :: close
    !> Finalizes the file
    final :: finalize
  end type file_netcdf_t

  !> Constructor
  interface file_netcdf_t
    module procedure :: constructor
  end interface file_netcdf_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Creates a file_netcdf_t object for a NetCDF file
  function constructor( config ) result( new_obj )

    use musica_assert,                 only : assert_msg
    use musica_config,                 only : config_t
    use musica_file,                   only : private_constructor
    use musica_string,                 only : string_t

    !> New NetCDF file object
    class(file_t), pointer :: new_obj
    !> File configuration
    type(config_t), intent(inout) :: config

    allocate( file_netcdf_t :: new_obj )

    call private_constructor( new_obj, config )

    call assert_msg( 467374411, .not. new_obj%is_output( ),                   &
                     "Output NetCDF files are not yet supported." )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the type of file as a string
  type(string_t) function file_type( this )

    use musica_string,                 only : string_t

    !> NetCDF file
    class(file_netcdf_t), intent(in) :: this

    file_type = "netcdf"

  end function file_type

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the NetCDF file id
  function id( this )

    !> NetCDF file id
    integer(kind=musica_ik) :: id
    !> NetCDF file
    class(file_netcdf_t), intent(in) :: this

    id = this%id_

  end function id

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the number of dimensions in the file
  integer(kind=musica_ik) function number_of_dimensions( this )

    use musica_assert,                 only : assert_msg
    use musica_constants,              only : musica_ik
    use musica_string,                 only : to_char, string_t
    use netcdf,                        only : nf90_inquire

    !> NetCDF file
    class(file_netcdf_t), intent(inout) :: this

    type(string_t) :: file_name

    call this%check_open( )
    file_name = this%name( )
    call this%check_status( 639211977,                                        &
                        nf90_inquire( this%id_,                               &
                                      nDimensions = number_of_dimensions ),   &
                       "Error getting dimension information for NetCDF file '"&
                       //file_name%to_char( )//"'" )
    call assert_msg( 938882534, number_of_dimensions .eq. 1,                  &
                     "NetCDF files are currently only set up for one "//      &
                     "dimension of time. File '"//file_name%to_char( )//      &
                     "' has "//to_char( number_of_dimensions )//" dimensions" )

  end function number_of_dimensions

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the number of variables in the file
  integer(kind=musica_ik) function number_of_variables( this )

    use musica_constants,              only : musica_ik
    use musica_string,                 only : string_t
    use netcdf,                        only : nf90_inquire

    !> NetCDF file
    class(file_netcdf_t), intent(inout) :: this

    type(string_t) :: file_name

    call this%check_open( )
    file_name = this%name( )
    call this%check_status( 150118878,                                        &
                       nf90_inquire( this%id_,                                &
                                     nVariables = number_of_variables ),      &
                       "Error getting variable information for NetCDF file '" &
                       //file_name%to_char( )//"'" )

  end function number_of_variables

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Opens the file if it is not open already
  subroutine check_open( this )

    use musica_string,                 only : string_t
    use netcdf,                        only : nf90_open, NF90_NOWRITE

    !> NetCDF file
    class(file_netcdf_t), intent(inout) :: this

    type(string_t) :: file_name

    if( this%is_open_ ) return
    file_name = this%name( )
    call this%check_status( 172405314,                                        &
        nf90_open( file_name%to_char( ), NF90_NOWRITE, this%id_ ),            &
                   "Error opening NetCDF file" )
    this%is_open_ = .true.

  end subroutine check_open

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Checks a NetCDF and fail with a message if an error occurred
  subroutine check_status( this, code, status, error_message )

    use musica_assert,                 only : die_msg
    use musica_string,                 only : string_t
    use netcdf,                        only : NF90_NOERR, nf90_strerror

    !> NetCDF file
    class(file_netcdf_t), intent(in) :: this
    !> Unique code for the assertion
    integer(kind=musica_ik), intent(in) :: code
    !> Status code
    integer(kind=musica_ik), intent(in) :: status
    !> Error message
    character(len=*), intent(in) :: error_message

    type(string_t) :: file_name

    if( status .eq. NF90_NOERR ) return
    file_name = this%name( )
    call die_msg( code, "NetCDF file '"//file_name%to_char( )//               &
                  "': "//trim( error_message )//": "//                        &
                  trim( nf90_strerror( status ) ) )

  end subroutine check_status

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Closes the NetCDF file
  subroutine close( this )

    use musica_string,                 only : string_t
    use netcdf,                        only : nf90_close

    !> NetCDF file
    class(file_netcdf_t), intent(inout) :: this

    type(string_t) :: file_name

    if( this%is_open_ ) return
    file_name = this%name( )
    call this%check_status( 633660547, nf90_close( this%id_ ),                &
                            "Error closing NetCDF file '"//                   &
                            file_name%to_char( )//"'" )
    this%is_open_ = .false.

  end subroutine close

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalizes the file object
  subroutine finalize( this )

    !> NetCDF file
    type(file_netcdf_t), intent(inout) :: this

    call this%close( )

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module musica_file_netcdf
