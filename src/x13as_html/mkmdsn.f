      SUBROUTINE mkmdsn(Nrar,Nrdiff,Nrma,Nsar,Nsdiff,Nsma,Mdldsn,Nmddcr)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Set up model description text
c-----------------------------------------------------------------------
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      CHARACTER Mdldsn*(132)
      INTEGER Nrar,Nrdiff,Nrma,Nsar,Nsdiff,Nsma,Nmddcr
c-----------------------------------------------------------------------
      Mdldsn(1:1)="("
      Nmddcr=2
      CALL itoc(Nrar,Mdldsn,Nmddcr)
      IF(Lfatal)RETURN
      Mdldsn(Nmddcr:Nmddcr)=" "
      Nmddcr=Nmddcr+1
      CALL itoc(Nrdiff,Mdldsn,Nmddcr)
      IF(Lfatal)RETURN
      Mdldsn(Nmddcr:Nmddcr)=" "
      Nmddcr=Nmddcr+1
      CALL itoc(Nrma,Mdldsn,Nmddcr)
      IF(Lfatal)RETURN
      Mdldsn(Nmddcr:Nmddcr)=")"
      IF(Nsar.gt.0.or.Nsma.gt.0.or.Nsdiff.gt.0)THEN
       Nmddcr=Nmddcr+1
       Mdldsn(Nmddcr:Nmddcr)="("
       Nmddcr=Nmddcr+1
       CALL itoc(Nsar,Mdldsn,Nmddcr)
       IF(Lfatal)RETURN
       Mdldsn(Nmddcr:Nmddcr)=" "
       Nmddcr=Nmddcr+1
       CALL itoc(Nsdiff,Mdldsn,Nmddcr)
       IF(Lfatal)RETURN
       Mdldsn(Nmddcr:Nmddcr)=" "
       Nmddcr=Nmddcr+1
       CALL itoc(Nsma,Mdldsn,Nmddcr)
       IF(Lfatal)RETURN
       Mdldsn(Nmddcr:Nmddcr)=")"
      END IF
c     ------------------------------------------------------------------
      RETURN
      END

