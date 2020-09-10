! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The musica_file_dimension_netcdf module

!> The file_dimension_netcdf_t type and related functions
module musica_file_dimension_netcdf

  use musica_constants,                only : musica_dk, musica_ik
  use musica_file_dimension,           only : file_dimension_t

  implicit none
  private

  public :: file_dimension_netcdf_t

  !> A NetCDF dimension
  type, extends(file_dimension_t) :: file_dimension_netcdf_t
    private
    !> NetCDF dimension id
    integer(kind=musica_ik) :: id_ = -1
  contains
    !> Gets the number of values associated with a dimension
    procedure, private :: number_of_values
    !> Finalize the object
    final :: finalize
  end type file_dimension_netcdf_t

  !> Constructor
  interface file_dimension_netcdf_t
    module procedure :: constructor
  end interface file_dimension_netcdf_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Creates a file_dimension_netcdf_t object for a NetCDF dimension
  function constructor( file, variable ) result( new_obj )

    use musica_assert,                 only : die
    use musica_file_dimension,         only : private_constructor
    use musica_file,                   only : file_t
    use musica_file_netcdf,            only : file_netcdf_t
    use musica_file_variable,          only : file_variable_t
    use musica_string,                 only : string_t
    use netcdf,                        only : nf90_inq_dimid

    !> Pointer to the new NetCDF dimension object
    class(file_dimension_t), pointer :: new_obj
    !> NetCDF file
    class(file_t), intent(inout) :: file
    !> NetCDF variable associated with the dimension
    class(file_variable_t), intent(in) :: variable

    type(string_t) :: var_name
    integer(kind=musica_ik) :: n_values

    allocate( file_dimension_netcdf_t :: new_obj )
    select type( new_obj )
    class is( file_dimension_netcdf_t )
      select type( file )
      class is( file_netcdf_t )
        var_name = variable%name( )
        call file%check_open( )
        call file%check_status( 140723118,                                    &
            nf90_inq_dimid( file%id( ), var_name%to_char( ), new_obj%id_ ),   &
            "Error finding id for dimension '"//var_name%to_char( )//"'" )
        n_values = new_obj%number_of_values( file, variable )
      class default
        call die( 345843533 )
      end select
    class default
      call die( 353013374 )
    end select
    call private_constructor( new_obj, file, variable, n_values )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Prints the properties of the dimension
  subroutine do_print( this )

    use musica_string,                 only : to_char

    !> NetCDF dimension
    class(file_dimension_netcdf_t), intent(in) :: this

    write(*,*) "*** Dimension id: "//to_char( this%id_ )//" ***"
    write(*,*) "*** End Dimension ***"

  end subroutine do_print

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the numnber of values for the dimension from the NetCDF file
  integer(kind=musica_ik) function number_of_values( this, file, variable )   &
      result( n_values )

    use musica_assert,                 only : die
    use musica_file,                   only : file_t
    use musica_file_netcdf,            only : file_netcdf_t
    use musica_file_variable,          only : file_variable_t
    use musica_string,                 only : string_t
    use netcdf,                        only : nf90_inquire_dimension

    !> NetCDF dimension
    class(file_dimension_netcdf_t), intent(inout) :: this
    !> NetCDF file
    class(file_t), intent(inout) :: file
    !> Variable associated with the dimension
    class(file_variable_t), intent(in) :: variable

    type(string_t) :: var_name

    select type( file )
      class is( file_netcdf_t )
        var_name = variable%name( )
        call file%check_status( 649288296,                                    &
                                nf90_inquire_dimension( file%id( ),           &
                                                        this%id_,             &
                                                        len = n_values ),     &
                                "Error getting values for dimension '"//      &
                                var_name%to_char( )//"'" )
      class default
        call die( 775240150 )
    end select

  end function number_of_values

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalize the object
  subroutine finalize( this )

    !> NetCDF dimension
    type(file_dimension_netcdf_t), intent(inout) :: this

    call this%private_finalize( )

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module musica_file_dimension_netcdf
