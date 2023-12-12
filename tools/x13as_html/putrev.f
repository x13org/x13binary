C     Last change:  BCM  25 Nov 97   10:36 am
      SUBROUTINE putrev(Inrev,Outrev,Outch,Outind,Iptr,Lrv,Lrvch,Muladd,
     &                  Itype,Rvdiff,Indrev)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     This subroutine puts the values into the proper vector or matrix
c     for the revisions history analysis
c-----------------------------------------------------------------------
      DOUBLE PRECISION PCT,ZERO
      LOGICAL F
      PARAMETER(PCT=100D0,ZERO=0D0,F=.false.)
c-----------------------------------------------------------------------
      INCLUDE 'agr.cmn'
c-----------------------------------------------------------------------
      LOGICAL Lrv,Lrvch
      DOUBLE PRECISION Inrev,Outrev,Outch,Outind
      INTEGER Iptr,Itype,Muladd,Rvdiff,Indrev
      DIMENSION Inrev(*)
c-----------------------------------------------------------------------
      IF(Lrv)THEN
       Outrev=Inrev(Iptr)
       IF(Itype.eq.0)THEN
        IF(Muladd.ne.1)Outrev=Outrev*PCT
       ELSE IF(Itype.eq.1)THEN
        IF(Iagr.eq.2.and.Iag.ge.0.and.Indrev.gt.0)THEN
         IF(Iag.eq.0)Outind=Outind+(Inrev(Iptr)*W)
         IF(Iag.eq.1)Outind=Outind-(Inrev(Iptr)*W)
         IF(Iag.eq.2)Outind=Outind*(Inrev(Iptr)*W)
         IF(Iag.eq.3)Outind=Outind/(Inrev(Iptr)*W)
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(Lrvch)THEN
       Outch=Inrev(Iptr)-Inrev(Iptr-1)
       IF(Muladd.eq.1.and.Rvdiff.eq.2)THEN
        IF(Inrev(Iptr-1).le.ZERO)THEN
         Lrvch=F
         Rvdiff=-1
        END IF
       END IF
       IF(Muladd.ne.1.or.Rvdiff.eq.2)Outch=(Outch/Inrev(Iptr-1))*PCT
      END IF
c-----------------------------------------------------------------------
      RETURN
      END

