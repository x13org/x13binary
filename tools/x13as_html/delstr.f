**==delstr.f    processed by SPAG 4.03F  at 09:47 on  1 Mar 1994
      SUBROUTINE delstr(Istr,Chrvec,Ptrvec,Nstr,Nlim)
      IMPLICIT NONE
c----------------------------------------------------------------------
c     Deletes the istr string if possible
c----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c     -----------------------------------------------------------------
      LOGICAL T
      PARAMETER(T=.true.)
c     -----------------------------------------------------------------
      CHARACTER Chrvec*(*)
      INTEGER i,Istr,nchr,newbeg,nrest,Nstr,oldbeg,Ptrvec,Nlim
      DIMENSION Ptrvec(0:Nlim)
c     -----------------------------------------------------------------
      IF(Istr.gt.Nstr.or.Istr.lt.1)THEN
*       CALL writln('Index out of range vector',STDERR,Mt2,T,T)
       CALL writln('Index out of range vector (delstr)',STDERR,Mt2,T,T)
       CALL abend
       RETURN
      END IF
c     -----------------------------------------------------------------
      oldbeg=Ptrvec(Istr)
      newbeg=Ptrvec(Istr-1)
      nrest=Ptrvec(Nstr)-oldbeg-1
      IF(nrest.ge.0)Chrvec(newbeg:newbeg+nrest)
     &   =Chrvec(oldbeg:oldbeg+nrest)
c     -----------------------------------------------------------------
      CALL eltlen(Istr,Ptrvec,Nstr,nchr)
      IF(Lfatal)RETURN
c     -----------------------------------------------------------------
      DO i=Istr,Nstr-1
       Ptrvec(i)=Ptrvec(i+1)-nchr
      END DO
      Nstr=Nstr-1
c     -----------------------------------------------------------------
      RETURN
c     -----------------------------------------------------------------
      END
