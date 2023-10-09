C     Last change:  BCM  26 Jan 98   12:55 pm
      SUBROUTINE cvrerr(Srsttl,Begsrs,Nobs,Spnttl,Begspn,Nspobs,Sp)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     cvrerr.f, Release 1, Subroutine Version 1.6, Modified 30 Nov 1994.
c-----------------------------------------------------------------------
c     Check whether the span (begspn,nspobs) is covered by the
c series (begsrs,nobs)
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      CHARACTER chrdt1*(10),chrdt2*(10),Srsttl*(*),Spnttl*(*)
      INTEGER Begspn,Begsrs,idate,idif,nchr1,nchr2,Nobs,Nspobs,Sp
      DIMENSION Begspn(2),Begsrs(2),idate(2)
c     ------------------------------------------------------------------
      CALL dfdate(Begspn,Begsrs,Sp,idif)
      IF(idif.lt.0)THEN
       CALL wrtdat(Begspn,Sp,chrdt1,nchr1)
       IF(.not.Lfatal)CALL wrtdat(Begsrs,Sp,chrdt2,nchr2)
       IF(Lfatal)RETURN
       WRITE(STDERR,1010)Spnttl,chrdt1(1:nchr1),Srsttl,chrdt2(1:nchr2)
       WRITE(Mt2,1011)Spnttl,chrdt1(1:nchr1),Srsttl,chrdt2(1:nchr2)
 1010  FORMAT(' ERROR: ',a,' start date, ',a,
     &        ', must begin on or after ',/,'        ',a,
     &        ' start date, ',a,'.',/)
 1011  FORMAT(' <p><strong>ERROR:</strong> ',a,' start date, ',a,
     &        ', must begin on or after ',/,' ',a,
     &        ' start date, ',a,'.</p>',/)
      END IF
c     ------------------------------------------------------------------
      IF(Nobs-idif.lt.Nspobs)THEN
       CALL addate(Begspn,Sp,Nspobs-1,idate)
       CALL wrtdat(idate,Sp,chrdt1,nchr1)
       IF(Lfatal)RETURN
       CALL addate(Begsrs,Sp,Nobs-1,idate)
       CALL wrtdat(idate,Sp,chrdt2,nchr2)
       IF(Lfatal)RETURN
       WRITE(STDERR,1020)Spnttl,chrdt1(1:nchr1),Srsttl,chrdt2(1:nchr2)
       WRITE(Mt2,1021)Spnttl,chrdt1(1:nchr1),Srsttl,chrdt2(1:nchr2)
 1020  FORMAT(' ERROR: ',a,' end date, ',a,', must end on or before ',/,
     &        '        ',a,' end date, ',a,'.',/)
 1021  FORMAT(' <p><strong>ERROR:</strong> ',a,' end date, ',a,
     &        ', must end on or before ',/,' ',a,' end date, ',a,
     &        '.</p>',/)
      END IF
c     ------------------------------------------------------------------
      IF(Nspobs.le.0)THEN
       CALL addate(Begsrs,Sp,Nobs-1,idate)
       CALL wrtdat(idate,Sp,chrdt1,nchr1)
       IF(.not.Lfatal)CALL wrtdat(Begsrs,Sp,chrdt2,nchr2)
       IF(Lfatal)RETURN
       WRITE(STDERR,1030)Spnttl,chrdt1(1:nchr1),chrdt2(1:nchr2)
       WRITE(Mt2,1031)Spnttl,chrdt1(1:nchr1),chrdt2(1:nchr2)
 1030  FORMAT(' ERROR: ',a,' end date, ',a,', must end after ',/,
     &        '        its own start date, ',a,'.',/)
 1031  FORMAT(' <p><strong>ERROR:</strong> ',a,' end date, ',a,
     &        ', must end after ',/,' its own start date, ',a,
     &        '.</p>',/)
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
