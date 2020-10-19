! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!>\file
!> The musica_grid module

!> The grid_t type and related functions for building sets of gridded
!! properties.
module musica_grid

  use musica_config,                   only : config_t
  use musica_constants,                only : musica_dk, musica_ik
  use musica_file,                     only : file_t
  use musica_file_dimension,           only : file_dimension_t
  use musica_file_variable,            only : file_variable_ptr
  use musica_string,                   only : string_t

  implicit none
  private

  public :: grid_t

  !> @name Types of spacing for grid elements
  !> @{
  !> Unknown or custom spacing
  integer(kind=musica_ik), parameter :: kSpacingUnknown = 0
  !> Linear spacing
  integer(kind=musica_ik), parameter :: kSpacingLinear = 1
  !> Logarithmic spacing
  integer(kind=musica_ik), parameter :: kSpacingLog = 2
  !> @}

  !> Grid for use with sets of gridded properties
  !!
  !! \todo add examples for grid_t usage
  type :: grid_t
    !> Name of the grid
    type(string_t) :: name_
    !> Number of grid sections
    integer(kind=musica_ik) :: number_of_sections_ = 0
    !> Grid element mid-points
    real(kind=musica_dk), allocatable :: mid_points_(:)
    !> Grid element boundaries
    real(kind=musica_dk), allocatable :: boundaries_(:)
    !> Spacing
    integer(kind=musica_ik) :: spacing_ = kSpacingUnknown
    !> Input file
    class(file_t), pointer :: file_ => null( )
    !> File configuration
    type(config_t) :: file_config_
    !> File variables
    class(file_variable_ptr), allocatable :: variables_(:)
    !> Dimension for per-grid-section properties in input file
    class(file_dimension_t), pointer :: section_dimension_ => null( )
    !> Dimension for per-grid-boundary properties in input file
    class(file_dimension_t), pointer :: boundary_dimension_ => null( )
  contains
    !> Returns the number of grid sections
    procedure :: number_of_sections
    !> Allocates a real array for per-grid-section properties
    procedure :: per_section_property_array
    !> Allocates a real array for per-grid-boundary properties
    procedure :: per_boundary_property_array
    !> @name Attempts to load a set of parameters on the grid from the input
    !!       file for each grid section
    !! @{
    procedure, private :: load_section_properties_1D
    procedure, private :: load_section_properties_2D
    generic :: load_section_properties => load_section_properties_1D,         &
                                          load_section_properties_2D
    !>@}
    !> @name Attempts to load a set of parameters on the grid from the input
    !!       file for each grid boundary
    !! @{
    procedure, private :: load_boundary_properties_1D
    procedure, private :: load_boundary_properties_2D
    generic :: load_boundary_properties => load_boundary_properties_1D,       &
                                           load_boundary_properties_2D
    !>@}
    !> General 1D property loader
    procedure, private :: load_properties_1D
    !> General 2D property loader
    procedure, private :: load_properties_2D
    !> Creates a map to the grid based on a supplied set of mid-points
    procedure :: map_to_mid_points
    !> Creates a map to the grid based on a supplied set of boundaries
    procedure :: map_to_boundaries
    !> Allocates and fills an array with the grid mid-points
    procedure :: mid_points
    !> Allocates and fills an array with the grid boundaries
    procedure :: boundaries
    !> Private constructor
    !! (Should only be called by constructors of extending types)
    procedure :: private_constructor
    !> Load grid dimensions if specified in configuration
    procedure, private :: load_grid
    !> Calculate grid dimensions if specified in configuration or if
    !! enough information exists from loaded data
    procedure, private :: calculate_grid
    !> @name Grid comparison
    !> @{
    procedure, private :: grid_equals
    generic :: operator(==) => grid_equals
    procedure, private :: grid_not_equals
    generic :: operator(/=) => grid_not_equals
    !> @}
    !> Finalize the grid
    final :: finalize
  end type grid_t

  !> Constructor
  interface grid_t
    module procedure :: constructor
  end interface grid_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructor of grid_t objects
  !!
  !! Grids are defined with either specified minimum and maximum mid-points
  !! or boundaries and either linear or logarithmic spacing, or with
  !! mid-points and/or boundaries from an input file.
  !!
  !! If either mid-points or boundaries (but not both) are loaded from an
  !! input file and the spacing is specified as linear or logarithmic, the
  !! unknown values (mid-points or boundaries) are calculated. If the spacing
  !! is not specified, the unknown values are not calculated and any attempt
  !! to use them will return an error.
  !!
  function constructor( config, units ) result( new_obj )

    !> New grid
    class(grid_t), pointer :: new_obj
    !> Grid configuration
    type(config_t), intent(inout) :: config
    !> MUSICA units for the file variable
    character(len=*), intent(in) :: units

    allocate( new_obj )
    call new_obj%private_constructor( config, units )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the number of grid sections
  integer(kind=musica_ik) function number_of_sections( this )

    !> Grid
    class(grid_t), intent(in) :: this

    number_of_sections = this%number_of_sections_

  end function number_of_sections

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Allocates a real array for per-grid-section properties
  function per_section_property_array( this ) result( property_array )

    use musica_assert,                 only : assert

    !> Allocated array
    real(kind=musica_dk), allocatable :: property_array(:)
    !> Grid
    class(grid_t), intent(in) :: this

    call assert( 669175982, this%number_of_sections_ .gt. 0 )
    allocate( property_array( this%number_of_sections_ ) )

  end function per_section_property_array

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Allocates a real array for per-grid-boundary properties
  function per_boundary_property_array( this ) result( property_array )

    use musica_assert,                 only : assert

    !> Allocated array
    real(kind=musica_dk), allocatable :: property_array(:)
    !> Grid
    class(grid_t), intent(in) :: this

    call assert( 669175982, this%number_of_sections_ .gt. 0 )
    allocate( property_array( this%number_of_sections_ + 1 ) )

  end function per_boundary_property_array

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Attempts to load a set of per-grid-section properties on the grid from
  !! the input file
  subroutine load_section_properties_1D( this, variable_name, units,          &
      properties, found )

    use musica_assert,                 only : assert, assert_msg, die, die_msg
    use musica_file_dimension_range,   only : file_dimension_range_t
    use musica_file_variable,          only : file_variable_t
    use musica_file_variable_factory,  only : file_variable_builder
    use musica_string,                 only : to_char

    !> Grid
    class(grid_t), intent(inout) :: this
    !> MUSICA name for the file variable
    !! (Name matching will be based on the grid configuration data)
    character(len=*), intent(in) :: variable_name
    !> MUSICA units for the file variable
    character(len=*), intent(in) :: units
    !> Loaded property values
    real(kind=musica_dk), allocatable, intent(out) :: properties(:)
    !> Flag indicating whether the property was found
    !!
    !! If this flag is not included and the variable is not found an error is
    !! thrown.
    logical, intent(out), optional :: found

    call this%load_properties_1D( .true., variable_name, units, properties,   &
                                  found )

  end subroutine load_section_properties_1D

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Attempts to load a set of per-grid-section properties on the grid from
  !! the input file
  subroutine load_section_properties_2D( this, variable_name, units,          &
      properties, found )

    use musica_assert,                 only : assert, assert_msg, die, die_msg
    use musica_file_dimension_range,   only : file_dimension_range_t
    use musica_file_variable,          only : file_variable_t
    use musica_file_variable_factory,  only : file_variable_builder
    use musica_string,                 only : to_char

    !> Grid
    class(grid_t), intent(inout) :: this
    !> MUSICA name for the file variable
    !! (Name matching will be based on the grid configuration data)
    character(len=*), intent(in) :: variable_name
    !> MUSICA units for the file variable
    character(len=*), intent(in) :: units
    !> Loaded property values
    real(kind=musica_dk), allocatable, intent(out) :: properties(:,:)
    !> Flag indicating whether the property was found
    !!
    !! If this flag is not included and the variable is not found an error is
    !! thrown.
    logical, intent(out), optional :: found

    call this%load_properties_2D( .true., variable_name, units, properties,   &
                                  found )

  end subroutine load_section_properties_2D

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Attempts to load a set of per-grid-boundary properties on the grid from
  !! the input file
  subroutine load_boundary_properties_1D( this, variable_name, units,         &
      properties, found )

    use musica_assert,                 only : assert, assert_msg, die, die_msg
    use musica_file_dimension_range,   only : file_dimension_range_t
    use musica_file_variable,          only : file_variable_t
    use musica_file_variable_factory,  only : file_variable_builder
    use musica_string,                 only : to_char

    !> Grid
    class(grid_t), intent(inout) :: this
    !> MUSICA name for the file variable
    !! (Name matching will be based on the grid configuration data)
    character(len=*), intent(in) :: variable_name
    !> MUSICA units for the file variable
    character(len=*), intent(in) :: units
    !> Loaded property values
    real(kind=musica_dk), allocatable, intent(out) :: properties(:)
    !> Flag indicating whether the property was found
    !!
    !! If this flag is not included and the variable is not found an error is
    !! thrown.
    logical, intent(out), optional :: found

    call this%load_properties_1D( .false., variable_name, units, properties,  &
                                  found )

  end subroutine load_boundary_properties_1D

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Attempts to load a set of per-grid-boundary properties on the grid from
  !! the input file
  subroutine load_boundary_properties_2D( this, variable_name, units,         &
      properties, found )

    use musica_assert,                 only : assert, assert_msg, die, die_msg
    use musica_file_dimension_range,   only : file_dimension_range_t
    use musica_file_variable,          only : file_variable_t
    use musica_file_variable_factory,  only : file_variable_builder
    use musica_string,                 only : to_char

    !> Grid
    class(grid_t), intent(inout) :: this
    !> MUSICA name for the file variable
    !! (Name matching will be based on the grid configuration data)
    character(len=*), intent(in) :: variable_name
    !> MUSICA units for the file variable
    character(len=*), intent(in) :: units
    !> Loaded property values
    real(kind=musica_dk), allocatable, intent(out) :: properties(:,:)
    !> Flag indicating whether the property was found
    !!
    !! If this flag is not included and the variable is not found an error is
    !! thrown.
    logical, intent(out), optional :: found

    call this%load_properties_2D( .false., variable_name, units, properties,  &
                                  found )

  end subroutine load_boundary_properties_2D

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Attempts to load a set of properties on the grid from the input file
  subroutine load_properties_1D( this, per_section, variable_name, units,     &
      properties, found )

    use musica_assert,                 only : assert, assert_msg, die
    use musica_file_dimension_range,   only : file_dimension_range_t
    use musica_file_variable,          only : file_variable_t
    use musica_file_variable_factory,  only : file_variable_builder
    use musica_string,                 only : to_char

    !> Grid
    class(grid_t), intent(inout) :: this
    !> Flag indicating whether the property set is per-grid-section
    !! (if false the property set is assumed to be per-grid-boundary)
    logical, intent(in) :: per_section
    !> MUSICA name for the file variable
    !! (Name matching will be based on the grid configuration data)
    character(len=*), intent(in) :: variable_name
    !> MUSICA units for the file variable
    character(len=*), intent(in) :: units
    !> Loaded property values
    real(kind=musica_dk), allocatable, intent(out) :: properties(:)
    !> Flag indicating whether the property was found
    !!
    !! If this flag is not included and the variable is not found an error is
    !! thrown.
    logical, intent(out), optional :: found

    logical :: l_found
    type(string_t) :: file_name
    class(file_variable_t), pointer :: var
    type(file_dimension_range_t) :: grid_dim
    type(file_dimension_range_t), allocatable :: var_dims(:)

    call assert( 500998644, associated( this%file_ ) )
    var => file_variable_builder( this%file_config_, this%file_,              &
                                  variable_name = variable_name,              &
                                  found = l_found )
    if( present( found ) ) found = l_found
    if( .not. l_found ) then
      file_name = this%file_%name( )
      call assert_msg( 776027678, present( found ),                           &
                       "Could not file variable '"//variable_name//           &
                       "' in file '"//file_name%to_char( )//"'" )
    end if
    call var%set_musica_units( units )
    var_dims = var%get_dimensions( )
    if( per_section ) then
      call assert( 985533766, associated( this%section_dimension_ ) )
      grid_dim   = this%section_dimension_%get_range( )
      properties = this%per_section_property_array( )
    else
      call assert( 192587805, associated( this%boundary_dimension_ ) )
      grid_dim   = this%boundary_dimension_%get_range( )
      properties = this%per_boundary_property_array( )
    end if
    call assert_msg( 316464888, rank( var_dims ) .eq. 1,                      &
                     "Rank mismatch for gridded property '"//variable_name//  &
                     "' in file '"//file_name%to_char( )//                    &
                     ": Expected 1 got "//to_char( rank( var_dims ) ) )
    call assert( 899688087, var_dims(1) .eq. grid_dim )
    call var%get_data( this%file_, var_dims, properties )
    deallocate( var )

  end subroutine load_properties_1D

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Attempts to load a set of properties on the grid from the input file
  !!
  !! The grid dimension is always last
  !!
  subroutine load_properties_2D( this, per_section, variable_name, units,     &
      properties, found )

    use musica_assert,                 only : assert, assert_msg, die
    use musica_file_dimension_range,   only : file_dimension_range_t
    use musica_file_variable,          only : file_variable_t
    use musica_file_variable_factory,  only : file_variable_builder
    use musica_string,                 only : to_char

    !> Grid
    class(grid_t), intent(inout) :: this
    !> Flag indicating whether the property set is per-grid-section
    !! (if false the property set is assumed to be per-grid-boundary)
    logical, intent(in) :: per_section
    !> MUSICA name for the file variable
    !! (Name matching will be based on the grid configuration data)
    character(len=*), intent(in) :: variable_name
    !> MUSICA units for the file variable
    character(len=*), intent(in) :: units
    !> Loaded property values
    real(kind=musica_dk), allocatable, intent(out) :: properties(:,:)
    !> Flag indicating whether the property was found
    !!
    !! If this flag is not included and the variable is not found an error is
    !! thrown.
    logical, intent(out), optional :: found

    logical :: l_found
    integer(kind=musica_ik) :: non_grid_min, non_grid_max, non_grid_dim_id,   &
                               i_elem
    type(string_t) :: file_name
    class(file_variable_t), pointer :: var
    type(file_dimension_range_t) :: grid_dim
    type(file_dimension_range_t), allocatable :: var_dims(:)

    call assert( 500998644, associated( this%file_ ) )
    var => file_variable_builder( this%file_config_, this%file_,              &
                                  variable_name = variable_name,              &
                                  found = l_found )
    if( present( found ) ) found = l_found
    if( .not. l_found ) then
      file_name = this%file_%name( )
      call assert_msg( 776027678, present( found ),                           &
                       "Could not file variable '"//variable_name//           &
                       "' in file '"//file_name%to_char( )//"'" )
    end if
    call var%set_musica_units( units )
    var_dims = var%get_dimensions( )
    if( per_section ) then
      call assert( 565036104, associated( this%section_dimension_ ) )
      grid_dim = this%section_dimension_%get_range( )
    else
      call assert( 988892539, associated( this%boundary_dimension_ ) )
      grid_dim = this%boundary_dimension_%get_range( )
    end if
    call assert_msg( 469827927, size( var_dims ) .eq. 2,                      &
                     "Rank mismatch for gridded property '"//variable_name//  &
                     "' in file '"//file_name%to_char( )//                    &
                     ": Expected 2 got "//to_char( rank( var_dims ) ) )
    if( var_dims(1) .eq. grid_dim ) then
      non_grid_dim_id = 2
    else if( var_dims(2) .eq. grid_dim ) then
      non_grid_dim_id = 1
    else
      call die( 337809239 )
    end if
    non_grid_min = var_dims( non_grid_dim_id )%lower_bound( )
    non_grid_max = var_dims( non_grid_dim_id )%upper_bound( )
    allocate( properties( non_grid_min : non_grid_max,                        &
                          grid_dim%lower_bound( ) : grid_dim%upper_bound( ) ) )
    do i_elem = non_grid_min, non_grid_max
      call var_dims( non_grid_dim_id )%set( i_elem, i_elem )
      call var%get_data( this%file_, var_dims, properties( i_elem, : ) )
    end do
    deallocate( var )

  end subroutine load_properties_2D

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Creates a map to the grid based on a supplied set of mid-points
  function map_to_mid_points( this, mid_points )

    use musica_assert,                 only : assert
    use musica_map,                    only : map_t

    !> Map between grid and specified mid-points
    type(map_t) :: map_to_mid_points
    !> Grid
    class(grid_t), intent(in) :: this
    !> Mid points to map to
    real(kind=musica_dk), intent(in) :: mid_points(:)

    call assert( 250378770, allocated( this%mid_points_ ) )
    map_to_mid_points = map_t( this%mid_points_, mid_points )

  end function map_to_mid_points

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Creates a map to the grid based on a supplied set of boundaries
  function map_to_boundaries( this, boundaries )

    use musica_assert,                 only : assert
    use musica_map,                    only : map_t

    !> Map between grid and specified boundaries
    type(map_t) :: map_to_boundaries
    !> Grid
    class(grid_t), intent(in) :: this
    !> Boundaries to map to
    real(kind=musica_dk), intent(in) :: boundaries(:)

    call assert( 129437345, allocated( this%boundaries_ ) )
    map_to_boundaries = map_t( this%boundaries_, boundaries )

  end function map_to_boundaries

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Allocates and fills an array with the grid mid-points
  function mid_points( this )

    use musica_assert,                 only : assert

    !> New array with mid-points
    real(kind=musica_dk), allocatable :: mid_points(:)
    !> Grid
    class(grid_t), intent(in) :: this

    call assert( 126078572, allocated( this%mid_points_ ) )
    mid_points = this%mid_points_

  end function mid_points

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Allocates and fills an array with the grid boundaries
  function boundaries( this )

    use musica_assert,                 only : assert

    !> New array with boundaries
    real(kind=musica_dk), allocatable :: boundaries(:)
    !> Grid
    class(grid_t), intent(in) :: this

    call assert( 975409853, allocated( this%boundaries_ ) )
    boundaries = this%boundaries_

  end function boundaries

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Private constructor
  !! (Should only be called by constructors of extending types)
  subroutine private_constructor( this, config, units )

    use musica_assert,                 only : die_msg

    !> Grid
    class(grid_t), intent(inout) :: this
    !> Grid configuration
    type(config_t), intent(inout) :: config
    !> MUSICA units for the file variable
    character(len=*), intent(in) :: units

    character(len=*), parameter :: my_name = "Grid constructor"
    logical :: found
    type(string_t) :: spacing_type

    call config%get( "name", this%name_, my_name )

    call config%get( "spacing", spacing_type, my_name, found = found )
    if( found ) then
      spacing_type = spacing_type%to_lower( )
      if( spacing_type .eq. "linear" ) then
        this%spacing_ = kSpacingLinear
      else if( spacing_type .eq. "logarithmic" ) then
        this%spacing_ = kSpacingLog
      else
        call die_msg( 824945627, "Invalid spacing specified for grid '"//     &
                      this%name_%to_char( )//"' : '"//                        &
                      spacing_type%to_char( )//"'" )
      end if
    end if

    call this%load_grid( config, units )
    call this%calculate_grid( config )

    if( allocated( this%mid_points_ ) ) then
      this%number_of_sections_ = size( this%mid_points_ )
    else if( allocated( this%boundaries_ ) ) then
      this%number_of_sections_ = size( this%boundaries_ ) - 1
    else
      call die_msg( 445992531, "No dimensions found for grid '"//             &
                    this%name_%to_char( )//"'" )
    end if

  end subroutine private_constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Load the grid dimensions from a file, if specified in the configuration
  subroutine load_grid( this, config, units )

    use musica_assert,                 only : assert_msg
    use musica_file_dimension_factory, only : file_dimension_builder
    use musica_file_factory,           only : file_builder
    use musica_file_variable,          only : file_variable_t,                &
                                              find_variable_by_musica_name
    use musica_file_variable_factory,  only : file_variable_builder

    !> Grid
    class(grid_t), intent(inout) :: this
    !> Grid configuration
    type(config_t), intent(inout) :: config
    !> MUSICA units for the grid
    character(len=*), intent(in) :: units

    character(len=*), parameter :: my_name = "Grid loader"
    logical :: found
    type(config_t) :: file_config
    type(string_t) :: file_name, file_type, prop_name, dim_name
    type(string_t), allocatable :: file_split(:)
    integer(kind=musica_ik) :: i_var, n_var

    call config%get( "from file", file_config, my_name, found = found )
    if( .not. found ) return
    call file_config%get( "type", file_type, my_name, found = found )
    if( .not. found ) then
      call file_config%get( "file name", file_name, my_name )
      file_split = file_name%split(".")
      call assert_msg( 795304923, size( file_split ) .gt. 1,                  &
                      "Cannot guess file type for '"//file_name%to_char( )//  &
                      "'." )
      file_type = file_split( size( file_split ) )
      call file_config%add( "type", file_type, my_name )
    end if
    call file_config%add( "intent", "input", my_name )
    this%file_config_ = file_config
    this%file_ => file_builder( file_config )
    call this%file_%check_open( )
    n_var = this%file_%number_of_variables( )
    allocate( this%variables_( n_var ) )
    do i_var = 1, n_var
      this%variables_( i_var )%val_ =>                                        &
          file_variable_builder( file_config, this%file_, variable_id = i_var )
    end do
    prop_name = this%name_//"%midpoint"
    i_var = find_variable_by_musica_name( this%variables_, prop_name, found )
    if( found ) then
      call this%variables_( i_var )%val_%set_musica_units( units )
      this%section_dimension_ =>                                              &
          file_dimension_builder( file_config, this%file_,                    &
                                  this%variables_( i_var )%val_ )
      this%mid_points_ = this%section_dimension_%get_values( )
    else
      call file_config%get( "grid section dimension", dim_name, my_name,      &
                            found = found )
      this%section_dimension_ =>                                              &
          file_dimension_builder( file_config, this%file_,                    &
                                  dimension_name = dim_name%to_char( ) )
    end if
    prop_name = this%name_//"%boundary"
    i_var = find_variable_by_musica_name( this%variables_, prop_name, found )
    if( found ) then
      call this%variables_( i_var )%val_%set_musica_units( units )
      this%boundary_dimension_ =>                                             &
          file_dimension_builder( file_config, this%file_,                    &
                                  this%variables_( i_var )%val_ )
      this%boundaries_ = this%boundary_dimension_%get_values( )
    else
      call file_config%get( "grid section dimension", dim_name, my_name,      &
                            found = found )
      this%boundary_dimension_ =>                                             &
          file_dimension_builder( file_config, this%file_,                    &
                                  dimension_name = dim_name%to_char( ) )
    end if

  end subroutine load_grid

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate grid if specified in configuration or if enough information
  !! exists from loaded data
  subroutine calculate_grid( this, config )

    use musica_array,                  only : calculate_linear_array,         &
                                              calculate_logarithmic_array
    use musica_assert,                 only : assert_msg, die

    !> Grid
    class(grid_t), intent(inout) :: this
    !> Grid configuration
    type(config_t), intent(inout) :: config

    character(len=*), parameter :: my_name = "grid calculator"
    logical :: num_found, min_found, max_found
    integer(kind=musica_ik) :: num_elem
    real(kind=musica_dk) :: min_midpoint, max_midpoint, min_bound, max_bound, &
                            space

    ! calculate midpoints and boundaries specified in the configuration
    call config%get( "number of elements", num_elem, my_name,                 &
                     found = num_found )
    call config%get( "mid-point minimum", min_midpoint, my_name,              &
                     found = min_found )
    call config%get( "mid-point maximum", max_midpoint, my_name,              &
                     found = max_found )
    call assert_msg( 243175667, min_found .eqv. max_found,                    &
                     "Minimum and maximum mid-points must be specified "//    &
                     "together for grid '"//this%name_%to_char( )//"'" )
    if( min_found ) then
      call assert_msg( 517079030, .not. allocated( this%mid_points_ ),        &
                       "Mid-points over-defined for grid '"//                 &
                       this%name_%to_char( )//"'" )
      call assert_msg( 812224356, num_found, "The number of elements must "// &
                       "be specified for grid '"//this%name_%to_char( )//     &
                       "'" )
      call assert_msg( 418170180, this%spacing_ .ne. kSpacingUnknown,         &
                       "Cannot calculate grid '"//this%name_%to_char( )//     &
                       "' with unknown spacing type." )
      select case( this%spacing_ )
      case( kSpacingLinear )
        this%mid_points_ =                                                    &
            calculate_linear_array( min_midpoint, max_midpoint, num_elem )
      case( kSpacingLog )
        this%mid_points_ =                                                    &
            calculate_logarithmic_array( min_midpoint, max_midpoint, num_elem )
      case default
        call die( 734972577 )
      end select
    end if
    call config%get( "boundary minimum", min_bound, my_name,                  &
                     found = min_found )
    call config%get( "boundary maximum", max_bound, my_name,                  &
                     found = max_found )
    call assert_msg( 496900795, min_found .eqv. max_found,                    &
                     "Minimum and maximum boundaries must be specified "//    &
                     "together for grid '"//this%name_%to_char( )//"'" )
    if( min_found ) then
      call assert_msg( 403307446, .not. allocated( this%boundaries_ ),        &
                       "Boundaries over-defined for grid '"//                 &
                       this%name_%to_char( )//"'" )
      call assert_msg( 207708000, num_found, "The number of elements must "// &
                       "be specified for grid '"//this%name_%to_char( )//     &
                       "'" )
      call assert_msg( 264093320, this%spacing_ .ne. kSpacingUnknown,         &
                       "Cannot calculate grid '"//this%name_%to_char( )//     &
                       "' with unknown spacing type." )
      select case( this%spacing_ )
      case( kSpacingLinear )
        this%boundaries_ =                                                    &
            calculate_linear_array( min_bound, max_bound, num_elem + 1 )
      case( kSpacingLog )
        this%boundaries_ =                                                    &
            calculate_logarithmic_array( min_bound, max_bound, num_elem + 1 )
      case default
        call die( 512980839 )
      end select
    end if

    ! calculate mid-points and boundaries if enough information exists
    if( allocated( this%mid_points_ ) .and.                                   &
        .not. allocated( this%boundaries_ ) ) then
      select case( this%spacing_ )
      case( kSpacingLinear )
        space = ( this%mid_points_( 2 ) - this%mid_points_( 1 ) )             &
                / 2.0_musica_dk
        min_bound = this%mid_points_( 1 ) - space
        max_bound = this%mid_points_( size( this%mid_points_ ) ) + space
        this%boundaries_ =                                                    &
            calculate_linear_array( min_bound, max_bound,                     &
                                    size( this%mid_points_ ) + 1 )
      case( kSpacingLog )
        space = ( log( this%mid_points_( 2 ) ) -                              &
                  log( this%mid_points_( 1 ) ) ) / 2.0_musica_dk
        min_bound = exp( log( this%mid_points_( 1 ) ) - space )
        max_bound = exp( log( this%mid_points_(                               &
                                         size( this%mid_points_ ) ) ) + space )
        this%boundaries_ =                                                    &
            calculate_logarithmic_array( min_bound, max_bound,                &
                                         size( this%mid_points_ ) + 1 )
      end select
    else if( allocated( this%boundaries_ ) .and.                              &
             .not. allocated( this%mid_points_ ) ) then
      select case( this%spacing_ )
      case( kSpacingLinear )
        space = ( this%boundaries_( 2 ) - this%boundaries_( 1 ) )             &
                / 2.0_musica_dk
        min_midpoint = this%boundaries_( 1 ) + space
        max_midpoint = this%boundaries_( size( this%boundaries_ ) ) - space
        this%mid_points_ =                                                    &
            calculate_linear_array( min_midpoint, max_midpoint,               &
                                    size( this%boundaries_ ) - 1 )
      case( kSpacingLog )
        space = ( log( this%boundaries_( 2 ) ) -                              &
                  log( this%boundaries_( 1 ) ) ) / 2.0_musica_dk
        min_midpoint = exp( log( this%boundaries_( 1 ) ) + space )
        max_midpoint = exp( log( this%boundaries_(                            &
                                         size( this%boundaries_ ) ) ) - space )
        this%mid_points_ =                                                    &
            calculate_logarithmic_array( min_midpoint, max_midpoint,          &
                                         size( this%boundaries_ ) - 1 )
      end select
    end if

  end subroutine calculate_grid

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Equality comparison
  logical elemental function grid_equals( a, b ) result( equals )

    !> Grid a
    class(grid_t), intent(in) :: a
    !> Grid b
    class(grid_t), intent(in) :: b

    integer(kind=musica_ik) :: i_elem

    equals = .false.
    if( allocated( a%mid_points_ ) ) then
      if( .not. allocated( b%mid_points_ ) ) return
      if( size( a%mid_points_ ) .ne. size( b%mid_points_ ) ) return
      do i_elem = 1, size( a%mid_points_ )
        if( a%mid_points_( i_elem ) .ne. b%mid_points_( i_elem ) ) return
      end do
    end if
    if( allocated( a%boundaries_ ) ) then
      if( .not. allocated( b%boundaries_ ) ) return
      if( size( a%boundaries_ ) .ne. size( b%boundaries_ ) ) return
      do i_elem = 1, size( a%boundaries_ )
        if( a%boundaries_( i_elem ) .ne. b%boundaries_( i_elem ) ) return
      end do
    end if
    equals = .true.

  end function grid_equals

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Inequality comparison
  logical elemental function grid_not_equals( a, b ) result( not_equals )

    !> Grid a
    class(grid_t), intent(in) :: a
    !> Grid b
    class(grid_t), intent(in) :: b

    not_equals = .not. ( a .eq. b )

  end function grid_not_equals

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalize the grid
  subroutine finalize( this )

    !> Grid
    type(grid_t), intent(inout) :: this

    integer(kind=musica_ik) :: i_var

    if( associated( this%file_ ) ) deallocate( this%file_ )
    if( allocated( this%variables_ ) ) then
      do i_var = 1, size( this%variables_ )
        if( associated( this%variables_( i_var )%val_ ) ) then
          deallocate( this%variables_( i_var )%val_ )
        end if
      end do
      deallocate( this%variables_ )
    end if
    if( associated( this%section_dimension_ ) )                               &
        deallocate( this%section_dimension_ )
    if( associated( this%boundary_dimension_ ) )                              &
        deallocate( this%boundary_dimension_ )

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module musica_grid
