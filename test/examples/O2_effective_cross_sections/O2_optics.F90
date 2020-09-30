! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Example module to compute O2 effective cross-sections using gridded
!! properties

!> The O2_optics_t type and related functions
module musica_O2_optics

  use musica_constants,                only : musica_dk, musica_ik
  use musica_grid,                     only : grid_t

  implicit none
  private

  public :: O2_optics_t

  !> Minimum log( O2 slant column density ) [unitless]
  real(kind=musica_dk), parameter :: kMinLogO2Column = 38.0_musica_dk
  !> Maximum log( O2 slant column density ) [unitless]
  real(kind=musica_dk), parameter :: kMaxLogO2Column = 56.0_musica_dk

  !> O2 optical properies
  !!
  !! Effective absorption cross-sections for O2 are calculated as a function
  !! of wavelength (\f$\lambda\f$) as:
  !! \f[
  !!   \chi(\lambda) = e^{ a(\lambda) * ( T - 220 ) + b(\lambda) }
  !! \f]
  !! where \f$a(\lambda)\f$ and \f$b(\lambda)\f$ are Chebyshev polynomials.
  !!
  type, extends(grid_t) :: O2_optics_t
    !> Chebyshev parameters for \f$a(\lambda)\f$ (coefficient, wavelength)
    real(kind=musica_dk), allocatable :: chebyshev_ac_(:,:)
    !> Chebyshev parameters for \f$b(\lambda)\f$ (coefficient, wavelength)
    real(kind=musica_dk), allocatable :: chebyshev_bc_(:,:)
    !> Working array for \f$a(\lambda)\f$ [T-1]
    real(kind=musica_dk), allocatable :: a_(:)
    !> Working array for \f$b(\lambda)\f$ [unitless]
    real(kind=musica_dk), allocatable :: b_(:)
  contains
    !> Calculates the effective cross-section of O2 in the Schumann-Runge
    !! bands
    procedure :: effective_cross_sections
  end type O2_optics_t

  !> Constructor of O2_optics_t objects
  interface O2_optics_t
    module procedure :: constructor
  end interface O2_optics_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructor of O2_optics_t objects
  function constructor( config ) result( new_obj )

    use musica_config,                 only : config_t

    !> New O2_optics_t object
    type(O2_optics_t), pointer :: new_obj
    !> O2 optics configration data
    class(config_t), intent(inout) :: config

    allocate( new_obj )
    call new_obj%private_constructor( config, "m" )
    call new_obj%load_section_properties( "chebev_ac", "unitless",            &
                                          new_obj%chebyshev_ac_ )
    call new_obj%load_section_properties( "chebev_bc", "unitless",            &
                                          new_obj%chebyshev_bc_ )
    new_obj%a_ = new_obj%per_section_property_array( )
    new_obj%b_ = new_obj%per_section_property_array( )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculates the effective cross-section of O2 in the Schumann-Runge bands
  !!
  !! Uses the parameterization of G.A. Koppers, and D.P. Murtagh
  !! [ref. Ann.Geophys., 14 68-79, 1996]
  subroutine effective_cross_sections( this, temperature__K,                  &
      log_O2_slant_column, cross_sections )

    use musica_assert,                 only : assert
    use musica_math,                   only : chebyshev

    !> O2 optical properties
    class(O2_optics_t), intent(inout) :: this
    !> Temperature [K]
    real(kind=musica_dk), intent(in) :: temperature__K
    !> log(O2 slant column density) [unitless]
    !! \bug what are the units for O2 slant column density?
    real(kind=musica_dk), intent(in) :: log_O2_slant_column
    !> Effective O2 cross-section for each wavelength in the grid [unitless]
    real(kind=musica_dk), intent(out) :: cross_sections(:)

    integer(kind=musica_ik) :: i_wl

    call assert( 603277016, size( cross_sections )                            &
                 .eq. this%number_of_sections( ) )
    do i_wl = 1, size( cross_sections )
      this%a_( i_wl ) =                                                       &
          chebyshev( kMinLogO2Column, kMaxLogO2Column,                        &
                     this%chebyshev_ac_( :, i_wl ),                           &
                     size( this%chebyshev_ac_, 1 ), log_O2_slant_column )
      this%b_( i_wl ) =                                                       &
          chebyshev( kMinLogO2Column, kMaxLogO2Column,                        &
                     this%chebyshev_bc_( :, i_wl ),                           &
                     size( this%chebyshev_bc_, 1 ), log_O2_slant_column )
      cross_sections( i_wl ) =                                                &
          exp( this%a_( i_wl ) * ( temperature__K - 220.0_musica_dk ) +       &
               this%b_( i_wl ) )
    end do

  end subroutine effective_cross_sections

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module musica_O2_optics
