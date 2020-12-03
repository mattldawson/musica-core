! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The musica_domain_column module

!> The domain_column_t type and related functions
module musica_domain_column

  use musica_constants,                only : musica_dk, musica_ik
  use musica_domain,                   only : domain_t, domain_state_t,      &
                                              domain_iterator_t,             &
                                              domain_state_mutator_t,        &
                                              domain_state_accessor_t
  use musica_string,                   only : string_t

  implicit none
  private

  public :: domain_column_t, domain_column_state_t


contains

end module musica_domain_column
