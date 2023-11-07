      LOGICAL FUNCTION chkcvr(Begsrs,Nobs,Begspn,Nspobs,Sp)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     chkcvr.f, Release 1, Subroutine Version 1.4, Modified 30 Nov 1994.
c-----------------------------------------------------------------------
c     Check whether the span (begspn,nspobs) is covered by the
c series (begsrs,nobs).  Returns true if the span is covered.
c-----------------------------------------------------------------------
      INTEGER Begspn,Begsrs,idif,Nobs,Nspobs,Sp
      DIMENSION Begspn(2),Begsrs(2)
c     ------------------------------------------------------------------
      chkcvr=.true.
      CALL dfdate(Begspn,Begsrs,Sp,idif)
      IF(idif.lt.0.or.idif+Nspobs.gt.Nobs.or.Nspobs.le.0)chkcvr=.false.
c     ------------------------------------------------------------------
      RETURN
      END
