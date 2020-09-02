! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The musica_file_variable_netcdf module

!> The file_variable_netcdf_t type and related functions
module musica_file_variable_netcdf

  use musica_constants,                only : musica_dk, musica_ik
  use musica_file_variable,            only : file_variable_t

  implicit none
  private

  public :: file_variable_netcdf_t

  !> A NetCDF variable
  !!
  !! In addition to standard file_variable_t functions, a NetCDF variable
  !! provides access to the NetCDF variable id.
  !!
  type, extends(file_variable_t) :: file_variable_netcdf_t
    private
    !> NetCDF variable id
    integer(kind=musica_ik) :: id_ = -1
    !> Variable dimensions
    integer(kind=musica_ik) :: dimensions_(1) = (/ 0 /)
  contains
    !> Returns the NetCDF variable id
    procedure :: id
    !> Gets the number of entries for the variable in the temporal dimension
    procedure :: time_dimension_size
    !> Gets a sub-set of the variable data for a specified index range
    !!
    !! Data are returned after applying conversions set up during
    !! initialization.
    !!
    procedure :: get_data
    !> Outputs data to the file for a given time step
    procedure :: output
    !> Prints the variable properties
    procedure :: print => do_print
  end type file_variable_netcdf_t

  !> Constructor
  interface file_variable_netcdf_t
    module procedure :: constructor_name, constructor_id
  end interface file_variable_netcdf_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Creates a file_variable_netcdf_t object for an existing NetCDF variable
  !> by name
  !!
  !! If no matching state variable is found in the MUSICA domain, a null
  !! pointer is returned.
  !!
  function constructor_name( domain, file, variable_name, config )            &
      result( new_obj )

    use musica_assert,                 only : die
    use musica_config,                 only : config_t
    use musica_domain,                 only : domain_t
    use musica_file,                   only : file_t
    use musica_file_netcdf,            only : file_netcdf_t
    use netcdf,                        only : nf90_inq_varid

    !> New NetCDF variable
    class(file_variable_t), pointer :: new_obj
    !> MUSICA domain
    class(domain_t), intent(inout) :: domain
    !> NetCDF file
    class(file_t), intent(inout) :: file
    !> Variable name
    character(len=*), intent(in) :: variable_name
    !> Configuration describing how to match to MUSICA variables
    !!
    !! If omitted, standard matching is applied
    class(config_t), intent(inout), optional :: config

    integer(kind=musica_ik) :: variable_id

    select type( file )
    class is( file_netcdf_t )
      call file%check_open( )
      call file%check_status( 542234258,                                      &
          nf90_inq_varid( file%id( ), variable_name, variable_id ),           &
          "Error getting variable id for '"//variable_name//"'" )
    class default
      call die( 952578143 )
    end select
    new_obj => constructor_id( domain, file, variable_id, config )

  end function constructor_name

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Creates a file_variable_netcdf_t object for an existing NetCDF variable
  !> by id
  !!
  !! If no matching state variable is found in the MUSICA domain, a null
  !! pointer is returned.
  !!
  function constructor_id( domain, file, variable_id, config )                &
      result( new_obj )

    use musica_assert,                 only : assert_msg, die
    use musica_config,                 only : config_t
    use musica_domain,                 only : domain_t
    use musica_file,                   only : file_t
    use musica_file_netcdf,            only : file_netcdf_t
    use musica_file_variable,          only : private_constructor
    use musica_string,                 only : string_t, to_char
    use netcdf,                        only : NF90_MAX_NAME,                  &
                                              nf90_inquire_variable,          &
                                              nf90_inquire_dimension,         &
                                              nf90_inq_attname,               &
                                              nf90_get_att

    !> New NetCDF variable
    class(file_variable_t), pointer :: new_obj
    !> MUSICA domain
    class(domain_t), intent(inout) :: domain
    !> NetCDF file
    class(file_t), intent(inout) :: file
    !> Variable ID
    integer(kind=musica_ik), intent(in) :: variable_id
    !> Configuration describing how to match to MUSICA variables
    !!
    !! If omitted, standard matching is applied
    class(config_t), intent(inout), optional :: config

    character(len=NF90_MAX_NAME) :: name, units
    type(string_t) :: file_name, att_name, var_name, var_units
    integer(kind=musica_ik) :: dimids(1), n_values, i_att, n_attributes

    allocate( file_variable_netcdf_t :: new_obj )

    select type( new_obj )
    class is( file_variable_netcdf_t )
      select type( file )
      class is( file_netcdf_t )
        file_name = file%name( )
        call file%check_open( )
        call file%check_status( 206732462,                                    &
                            nf90_inquire_variable( file%id( ),                &
                                                   variable_id,               &
                                                   name = name,               &
                                                   dimids = dimids,           &
                                                   nAtts = n_attributes ),    &
                            "Error getting variable information for id: "//   &
                            to_char( variable_id )//"'" )
        var_name    = name
        var_units   = ""
        new_obj%id_ = variable_id
        call file%check_status( 661270149,                                    &
                            nf90_inquire_dimension( file%id( ),               &
                                                    dimids(1),                &
                                                    len = n_values ),         &
                            "Error getting dimensions of variable '"//        &
                            var_name%to_char( )//"'" )
        new_obj%dimensions_(1) = n_values
        do i_att = 1, n_attributes
          call file%check_status( 485848938,                                  &
                              nf90_inq_attname( file%id( ),                   &
                                                variable_id,                  &
                                                i_att,                        &
                                                name ),                       &
                              "Error getting attribute "//to_char( i_att )//  &
                              " name for variable '"//                        &
                              var_name%to_char( )//"'" )
          att_name = trim( name )
          att_name = att_name%to_lower( )
          if( att_name .eq. "units" .or. att_name .eq. "unit" ) then
            call file%check_status( 992960877,                                &
                                nf90_get_att( file%id( ), new_obj%id_, name,  &
                                              units ),                        &
                                "Error getting units for variable '"//        &
                                var_name%to_char( )//"'" )
            var_units = trim( units )
          end if
        end do
        call assert_msg( 738503497, units .ne. "",                            &
                         "No units found for variable '"//var_name%to_char( ) &
                         //"' in NetCDF file '"//file_name%to_char( )//"'" )
      class default
        call die( 185049127 )
      end select
    class default
      call die( 857053663 )
    end select

    call private_constructor( new_obj, config, file, domain, var_name,        &
                              units = var_units )

  end function constructor_id

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the NetCDF variable index
  integer(kind=musica_ik) function id( this )

    !> NetCDF variable
    class(file_variable_netcdf_t), intent(in) :: this

    id = this%id_

  end function id

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets the number of entries of the variable in the temporal dimension
  integer(kind=musica_ik) function time_dimension_size( this )

    !> NetCDF variable
    class(file_variable_netcdf_t), intent(in) :: this

    time_dimension_size = this%dimensions_(1)

  end function time_dimension_size

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets a sub-set of the data from the file
  !!
  !! Applies any necessary conversions to the raw input data
  !!
  subroutine get_data( this, file, start, count, values )

    use musica_assert,                 only : die
    use musica_file,                   only : file_t
    use musica_file_netcdf,            only : file_netcdf_t
    use netcdf,                        only : nf90_get_var

    !> NetCDF variable
    class(file_variable_netcdf_t), intent(in) :: this
    !> NetCDF file
    class(file_t), intent(inout) :: file
    !> Starting index for returned data
    integer(kind=musica_ik), intent(in) :: start
    !> Number of data points to return
    integer(kind=musica_ik), intent(in) :: count
    !> Values to return
    real(kind=musica_dk), intent(out) :: values(count)

    integer(kind=musica_ik) :: l_count(1), l_start(1), i_val

    select type( file )
    class is( file_netcdf_t )
    l_start(1) = start
    l_count(1) = count
    call file%check_status( 448163017, nf90_get_var( file%id( ), this%id_,    &
                                                   values, l_start, l_count ),&
                            "Error getting values for NetCDF variable" )
    call this%convert_to_musica_values( values )
    class default
      call die( 636979049 )
    end select

  end subroutine get_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Outputs data to the file for a given timestep
  !!
  !! The state_value will be converted according to the variable configuration
  !! prior to outputting it to the file.
  !!
  subroutine output( this, file, time__s, state_value )

    use musica_assert,                 only : die_msg
    use musica_file,                   only : file_t

    !> NetCDF variable
    class(file_variable_netcdf_t), intent(inout) :: this
    !> Output file
    class(file_t), intent(inout) :: file
    !> Current simulation time [s]
    real(kind=musica_dk), intent(in) :: time__s
    !> Value to output
    real(kind=musica_dk), intent(in) :: state_value

    call die_msg( 835094621, "NetCDF output is not ready yet." )

  end subroutine output

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Prints the properties of the variable
  subroutine do_print( this )

    use musica_string,                 only : string_t

    !> NetCDF variable
    class(file_variable_netcdf_t), intent(in) :: this

    type(string_t) :: units, musica_name, var_name

    var_name    = this%name( )
    musica_name = this%musica_name( )
    units       = this%units( )
    write(*,*) "*** Variable: "//var_name%to_char( )//" ***"
    write(*,*) "MUSICA name: "//musica_name%to_char( )
    write(*,*) "NetCDF variable id:", this%id_
    write(*,*) "dimension sizes:", this%dimensions_
    write(*,*) "units: "//units%to_char( )

  end subroutine do_print

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module musica_file_variable_netcdf
