C     Last change:  SRD  18 Nov 99    6:29 am
      SUBROUTINE getstr(Chrvec,Ptrvec,Nstr,Istr,Str,Nchr)
      IMPLICIT NONE
c----------------------------------------------------------------------
c     Gets the istr string if possible
c----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      LOGICAL T
      PARAMETER(T=.true.)
c     -----------------------------------------------------------------
      CHARACTER Chrvec*(*),Str*(*)
      INTEGER begstr,Istr,Nchr,Nstr,Ptrvec
      DIMENSION Ptrvec(0:Nstr)
c     -----------------------------------------------------------------
      IF(Istr.gt.Nstr.or.Istr.lt.1)THEN
*       CALL writln('Index out of range vector',STDERR,Mt2,T,T)
       CALL writln('Index out of range vector (getstr)',STDERR,Mt2,T,T)
       CALL abend
       RETURN
      END IF
c     -----------------------------------------------------------------
      CALL eltlen(Istr,Ptrvec,Nstr,Nchr)
      IF(Lfatal)RETURN
      begstr=Ptrvec(Istr-1)
c     -----------------------------------------------------------------
      IF(Nchr.gt.len(Str))THEN
       CALL writln('Not able to get character string',STDERR,Mt2,T,T)
       CALL abend
       RETURN
c     -----------------------------------------------------------------
      ELSE IF(Nchr.gt.0)THEN
       Str(1:Nchr)=Chrvec(begstr:begstr+Nchr-1)
      END IF
c     -----------------------------------------------------------------
      RETURN
      END
