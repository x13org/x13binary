      SUBROUTINE clrotl(Nrxy)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Remove automatically identified outlier regressors from model
c     This routine is done before redoing automatic outlier
c     identification in the automatic model identification procedure
c     (BCM April 2007)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      INTEGER icol,Nrxy
c-----------------------------------------------------------------------
	icol=Nb
      DO WHILE (icol.ge.1)
       IF(Rgvrtp(icol).eq.PRGTAA.or.Rgvrtp(icol).eq.PRGTAL.or.
*     &    Rgvrtp(icol).eq.PRGTAT.or.Rgvrtp(icol).eq.PRGTAS)THEN
     &    Rgvrtp(icol).eq.PRGTAT)THEN
        CALL dlrgef(icol,Nrxy,1)
        IF(Lfatal)RETURN
       END IF
       icol=icol-1
      END DO
      Natotl=0
c-----------------------------------------------------------------------
      RETURN
      END
