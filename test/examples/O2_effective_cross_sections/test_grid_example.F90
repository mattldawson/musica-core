! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests use of an extending type of grid_t for calculating O2 effective
!! absorption cross-sections

!> O2 effective absorption cross-section test
program test_O2_optics

  use musica_config,                   only : config_t
  use musica_constants,                only : musica_dk, musica_ik
  use musica_O2_optics,                only : O2_optics_t

  implicit none

  type(config_t) :: config
  type(O2_optics_t), pointer :: O2_optics
  real(kind=musica_dk), allocatable :: wavelength_boundaries(:)
  real(kind=musica_dk) :: log_O2_slant_column
  real(kind=musica_dk), allocatable :: effective_O2_cross_sections(:)
  integer(kind=musica_ik) :: i_wl

  call config%from_file( 'data/test_O2_optics_config.json' )
  O2_optics => O2_optics_t( config )
  call config%finalize( )

  effective_O2_cross_sections = O2_optics%per_section_property_array( )
  wavelength_boundaries       = O2_optics%boundaries( )

  log_O2_slant_column = 42.3_musica_dk

  call O2_optics%effective_cross_sections( 298.0_musica_dk,                   &
                                           log_O2_slant_column,               &
                                           effective_O2_cross_sections )

  write(*,*) "O2 effective cross sections"
  do i_wl = 1, size( wavelength_boundaries ) - 1
    write(*,*) "wavelength range ", wavelength_boundaries( i_wl ), " - ",     &
               wavelength_boundaries( i_wl ), " : ",                          &
               effective_O2_cross_sections( i_wl )
  end do

  deallocate( O2_optics                   )
  deallocate( effective_O2_cross_sections )
  deallocate( wavelength_boundaries       )

end program test_O2_optics
