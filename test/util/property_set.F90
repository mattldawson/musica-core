! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests for the musica_propert_set module

!> Tests for the property_set_t type
module test_util_property_set_module

  use musica_target,                   only : target_t

  implicit none

  !> Mock target
  type, extends(target_t) :: mock_target_t
  contains
    procedure :: name => target_name
    procedure :: equals_target
  end type mock_target_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the name of the target
  type(string_t) function target_name( this )

    use musica_string,                 only : string_t

    !> Target
    class(mock_target_t), intent(in) :: this

    target_name = "mock target"

  end function target_name

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Equality comparison
  logical function equals_target( a, b ) result( eq )

    !> Target
    class(mock_target_t), intent(in) :: a
    !> Other target
    class(target_t), intent(in) :: b

    select type( b )
    class is( mock_target_t )
      eq = .true.
    class default
      eq = .false.
    end select

  end function equals_target

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module test_util_property_set_module

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

program test_util_property_set

  use musica_assert
  use musica_constants,                only : musica_dk, musica_ik, musica_lk
  use musica_property_set
  use test_util_property_set_module

  implicit none

  call test_property_set_t( )

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Test property_set_t functionality
  subroutine test_property_set_t( )

    use musica_data_type,              only : kBoolean, kDouble, kFloat
    use musica_property,               only : property_t

    character(len=*), parameter :: my_name = "property set tests"
    type(property_t), pointer :: a, b, c, d
    type(property_set_t), pointer :: set, subset
    logical :: found
    type(mock_target_t) :: prop_target

    ! set up properties
    a => property_t( my_name,                                                 &
                     name = "prop a",                                         &
                     data_type = kFloat,                                      &
                     applies_to = prop_target,                                &
                     units = "m" )
    b => property_t( my_name,                                                 &
                     name = "my subset%prop b",                               &
                     data_type = kDouble,                                     &
                     applies_to = prop_target,                                &
                     units = "s" )
    c => property_t( "other definer",                                         &
                     name = "my subset%prop c",                               &
                     data_type = kDouble,                                     &
                     applies_to = prop_target,                                &
                     units = "m" )

    set => property_set_t( )

    ! add elements
    call set%add( a )
    call set%add( b )
    call set%add( c )

    ! size
    call assert( 878811384, set%size( ) .eq. 3 )

    ! index
    call assert( 472035937, set%index( b ) .eq. 2 )
    call assert( 418008446, set%index( a ) .eq. 1 )
    call assert( 865376292, set%index( c ) .eq. 3 )

    ! get by index
    d => set%get( 2 )
    call assert( 233964155, associated( d ) )
    call assert( 570919190, d .eq. b )
    call assert( 460506379, d .ne. a )
    call assert( 237775223, d .ne. c )
    deallocate( d )

    ! get by name
    d => set%get( "my subset%prop b" )
    call assert( 220529063, associated( d ) )
    call assert( 615322657, d .eq. b )
    call assert( 162690504, d .ne. a )
    call assert( 957541999, d .ne. c )
    deallocate( d )
    d => set%get( "not there", found = found )
    call assert( 108663013, .not. associated( d ) )
    call assert( 385873955, .not. found )

    ! full sub set
    subset => set%subset( )
    call assert( 379156409, associated( subset ) )
    call assert( 603793099, subset%size( ) .eq. 3 )
    d => subset%get( 1 )
    call assert( 935483827, d .eq. a )
    deallocate( d )
    d => subset%get( 2 )
    call assert( 479492901, d .eq. b )
    deallocate( d )
    d => subset%get( 3 )
    call assert( 591811246, d .eq. c )
    deallocate( d )
    deallocate( subset )

    ! prefix sub set
    subset => set%subset( prefix = "my subset" )
    call assert( 621326382, associated( subset ) )
    call assert( 168694229, subset%size( ) .eq. 2 )
    d => subset%get( 1 )
    call assert( 898537324, d .eq. b )
    deallocate( d )
    d => subset%get( 2 )
    call assert( 110855670, d .eq. c )
    deallocate( d )
    deallocate( subset )

    ! data type sub set
    subset => set%subset( data_type = kBoolean )
    call assert( 441093159, associated( subset ) )
    call assert( 888461005, subset%size( ) .eq. 0 )
    deallocate( subset )
    subset => set%subset( data_type = kDouble )
    call assert( 718304101, associated( subset ) )
    call assert( 265671948, subset%size( ) .eq. 2 )
    d => subset%get( 1 )
    call assert( 442998693, d .eq. b )
    deallocate( d )
    d => subset%get( 2 )
    call assert( 272841789, d .eq. c )
    deallocate( d )
    deallocate( subset )

    ! applies-to sub set
    subset => set%subset( applies_to = prop_target )
    call assert( 909353061, associated( subset ) )
    call assert( 456720908, subset%size( ) .eq. 3 )
    d => subset%get( 1 )
    call assert( 286564004, d .eq. a )
    deallocate( d )
    d => subset%get( 2 )
    call assert( 181415500, d .eq. b )
    deallocate( d )
    d => subset%get( 3 )
    call assert( 628783346, d .eq. c )
    deallocate( d )
    deallocate( subset )

    ! defined-by sub set
    subset => set%subset( defined_by = my_name )
    call assert( 225366672, associated( subset ) )
    call assert( 402693417, subset%size( ) .eq. 2 )
    d => subset%get( 1 )
    call assert( 850061263, d .eq. a )
    deallocate( d )
    d => subset%get( 2 )
    call assert( 344854858, d .eq. b )
    deallocate( d )
    deallocate( subset )

    ! free memory
    deallocate( a )
    deallocate( b )
    deallocate( c )
    deallocate( set )

  end subroutine test_property_set_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_util_property_set
