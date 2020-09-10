! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The musica_file_variable_text module

!> The file_variable_text_t type and related functions
module musica_file_variable_text

  use musica_constants,                only : musica_dk, musica_ik
  use musica_file_variable,            only : file_variable_t

  implicit none
  private

  public :: file_variable_text_t

  !> A text file variable
  !!
  !! In addition to standard file_variable_t functions, a text file variable
  !! provides access to the column or row the variable is located in.
  !!
  type, extends(file_variable_t) :: file_variable_text_t
    private
    !> Row or column the variable is located in (starting at 1)
    integer(kind=musica_ik) :: id_ = -1
    !> Number of entries for the variable in the temporal dimension
    integer(kind=musica_ik) :: time_dimension_size_ = 0
  contains
    !> Returns the row or column the variable is located in (starting at 1)
    procedure :: id
    !> Gets the number of entries for the variable in the temporal dimenion
    procedure :: time_dimension_size
    !> Gets a sub-set of the variable data for a specified index range
    !!
    !! Data are returned after applying conversions set up during
    !! initialization.
    !!
    procedure :: get_data
    !> Outputs data to the file for a given time step
    procedure :: output
  end type file_variable_text_t

  !> Constructor
  interface file_variable_text_t
    module procedure :: constructor_name, constructor_id
  end interface file_variable_text_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Creates a file_variable_text_t object for an existing text file variable
  !! by name for input files, or a new file variable for output files.
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
    use musica_file_text,              only : file_text_t

    !> New text file variable
    class(file_variable_t), pointer :: new_obj
    !> MUSICA domain
    class(domain_t), intent(inout) :: domain
    !> Text file
    class(file_t), intent(inout) :: file
    !> Variable name
    character(len=*), intent(in) :: variable_name
    !> Configuration describing how to match to MUSICA variables
    !!
    !! If omitted, standard matching is applied
    class(config_t), intent(inout), optional :: config

    integer(kind=musica_ik) :: variable_id

    call file%check_open( )
    select type( file )
    class is( file_text_t )
      if( file%is_input( ) .and. .not. file%is_output( ) ) then
        variable_id = file%get_variable_id( variable_name )
      else if( .not. file%is_input( ) .and. file%is_output( ) ) then
        variable_id = file%add_variable( variable_name )
      else
        call die( 113385982 )
      end if
    class default
      call die( 542896218 )
    end select
    new_obj => constructor_id( domain, file, variable_id, config )

  end function constructor_name

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Creates a file_variable_text_t object for an existing text variable by id
  !!
  !! If no matching state variable is found in the MUSICA domain, a null
  !! pointer is returned.
  !!
  function constructor_id( domain, file, variable_id, config )                &
      result( new_obj )

    use musica_assert,                 only : die
    use musica_config,                 only : config_t
    use musica_domain,                 only : domain_t
    use musica_file,                   only : file_t
    use musica_file_text,              only : file_text_t
    use musica_file_variable,          only : private_constructor
    use musica_string,                 only : string_t

    !> New text file variable
    class(file_variable_t), pointer :: new_obj
    !> MUSICA domain
    class(domain_t), intent(inout) :: domain
    !> Text file
    class(file_t), intent(inout) :: file
    !> Variable row or column position (starting from 1)
    integer(kind=musica_ik), intent(in) :: variable_id
    !> Configuration describing how to match to MUSICA variables
    class(config_t), intent(inout) :: config

    type(string_t) :: var_name

    allocate( file_variable_text_t :: new_obj )
    select type( new_obj )
    class is( file_variable_text_t )
      select type( file )
      class is( file_text_t )
        var_name    = file%get_variable_name( variable_id )
        new_obj%id_ = variable_id
        if( file%is_input( ) ) then
          new_obj%time_dimension_size_ = file%number_of_entries( )
        else
          new_obj%time_dimension_size_ = 0
        end if
      class default
        call die( 569766402 )
      end select
    class default
      call die( 511927843 )
    end select

    call private_constructor( new_obj, config, file, domain, var_name )

  end function constructor_id

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the row or column of the variable in the file (starting at 1)
  integer(kind=musica_ik) function id( this )

    !> Text file variable
    class(file_variable_text_t), intent(in) :: this

    id = this%id_

  end function id

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the number of entries of the variable in the temporal dimension
  integer(kind=musica_ik) function time_dimension_size( this )

    !> Text file variable
    class(file_variable_text_t), intent(in) :: this

    time_dimension_size = this%time_dimension_size_

  end function time_dimension_size

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets a subset of the data from the file
  !!
  !! Applies any necessary conversions to the raw input data
  !!
  subroutine get_data( this, file, start, count, values )

    use musica_assert,                 only : die
    use musica_file,                   only : file_t
    use musica_file_text,              only : file_text_t

    !> Text file variable
    class(file_variable_text_t), intent(in) :: this
    !> Text file
    class(file_t), intent(inout) :: file
    !> Starting index for the returned data
    integer(kind=musica_ik), intent(in) :: start
    !> Number of data points to return
    integer(kind=musica_ik), intent(in) :: count
    !> Values to return
    real(kind=musica_dk), intent(out) :: values(count)

    select type( file )
    class is( file_text_t )
      call file%get_data( this%id_, start, count, values )
    class default
      call die( 980276126 )
    end select
    call this%convert_to_musica_values( values )

  end subroutine get_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Outputs data to the file for a given timestep
  !!
  !! The state_value will be converted according to the variable configuration
  !! prior to outputting it to the file.
  !!
  subroutine output( this, file, time__s, state_value )

    use musica_assert,                 only : die
    use musica_file,                   only : file_t
    use musica_file_text,              only : file_text_t

    !> Text file variable
    class(file_variable_text_t), intent(inout) :: this
    !> Text output file
    class(file_t), intent(inout) :: file
    !> Current simulation time [s]
    real(kind=musica_dk), intent(in) :: time__s
    !> Value to output
    real(kind=musica_dk), intent(in) :: state_value

    real(kind=musica_dk) :: temp_val(1)

    select type( file )
      class is( file_text_t )
        temp_val(1) = state_value
        call this%convert_to_file_values( temp_val )
        call file%output( this%id_, time__s, temp_val(1) )
      class default
        call die( 786618560 )
    end select

  end subroutine output

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module musica_file_variable_text
