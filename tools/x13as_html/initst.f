C     Last change:  BCM  23 Mar 2005   10:26 am
      SUBROUTINE initst
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     Initial variables used to store SEATS diagnostics to NULL value
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'seatlg.cmn'
      INCLUDE 'seatmd.cmn'
      INCLUDE 'stcfcm.cmn'
c     ------------------------------------------------------------------
      Havetr = .false.
      Havesf = .false.
      Haveir = .false.
      Havesa = .false.
      Havecy = .false.
      Havftr = .false.
      Havfsf = .false.
      Havfir = .false.
      Havfsa = .false.
      Havfcy = .false.
      Hseftr = .false.
      Hsefsf = .false.
      Hsefor = .false.
      Hsefsa = .false.
      Hsefcy = .false.
      Hsrftr = .false.
      Hsrfsf = .false.
      Hsrfsa = .false.
      Hsrfcy = .false.
      Hvstsa = .false.
      Hvstir = .false.
      Hvstft = .false.
      Hvstfs = .false.
      Hvstfo = .false.
      Hvstfa = .false.
      Hvstfc = .false.
      Ntcnum = NOTSET
      Ntcden = NOTSET
      Nsnum = NOTSET
      Nsden = NOTSET
      Nsanum = NOTSET
      Nsaden = NOTSET
      Ntrnum = NOTSET
      Ntrden = NOTSET
      Ntcwkf = NOTSET
      Nsawkf = NOTSET
      Nswkf = NOTSET
      Ntrwkf = NOTSET
      Nirwkf = NOTSET
      Tcvar = DNOTST
      Svar = DNOTST
      Savar = DNOTST
      Trvar = DNOTST
      Irrvar = DNOTST
c     ------------------------------------------------------------------
      RETURN
      END
