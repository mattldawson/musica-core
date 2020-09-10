! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The musica_file_dimension_text module

!> The file_dimension_text_t type and related functions
module musica_file_dimension_text

  use musica_constants,                only : musica_dk, musica_ik
  use musica_file_dimension,           only : file_dimension_t

  implicit none
  private

  public :: file_dimension_text_t

  !> A text file dimension
  type, extends(file_dimension_t) :: file_dimension_text_t
    private
    !> Text dimension id
    integer(kind=musica_ik) :: id_ = -1
  contains
    !> Finalize the object
    final :: finalize
  end type file_dimension_text_t

  !> Constructor
  interface file_dimension_text_t
    module procedure :: constructor
  end interface file_dimension_text_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Creates a file_dimension_text_t object for a text file dimension
  function constructor( file, variable ) result( new_obj )

    use musica_assert,                 only : assert, die
    use musica_file_dimension,         only : private_constructor
    use musica_file,                   only : file_t
    use musica_file_text,              only : file_text_t
    use musica_file_variable,          only : file_variable_t
    use musica_string,                 only : string_t

    !> Pointer to the new text file dimension object
    class(file_dimension_t), pointer :: new_obj
    !> Text file
    class(file_t), intent(inout) :: file
    !> Text file variable associated with the dimension
    class(file_variable_t), intent(in) :: variable

    integer(kind=musica_ik) :: n_values
    type(string_t) :: dim_name

    ! there is only a time dimension in text files
    dim_name = variable%musica_name( )
    call assert( 103261211, dim_name .eq. "time" )
    allocate( file_dimension_text_t :: new_obj )
    select type( new_obj )
    class is( file_dimension_text_t )
      select type( file )
      class is( file_text_t )
        new_obj%id_ = file%get_dimension_id( variable%name( ) )
        n_values = file%number_of_entries( )
      class default
        call die( 361184159 )
      end select
    class default
      call die( 585820849 )
    end select
    call private_constructor( new_obj, file, variable, n_values )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalize the object
  subroutine finalize( this )

    !> Text file dimension
    type(file_dimension_text_t), intent(inout) :: this

    call this%private_finalize( )

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module musica_file_dimension_text
