C     Last change:  BCM   2 Apr 98   12:59 pm
**==insstr.f    processed by SPAG 4.03F  at 09:50 on  1 Mar 1994
      SUBROUTINE insstr(Str,Istr,Pstr,Chrvec,Ptrvec,Nstr)
c----------------------------------------------------------------------
      IMPLICIT NONE
c----------------------------------------------------------------------
      INCLUDE 'error.cmn'
c----------------------------------------------------------------------
      CHARACTER Chrvec*(*),Str*(*)
      INTEGER begchr,endchr,ichr,Istr,nwmold,Nstr,Pstr,Ptrvec
      DIMENSION Ptrvec(0:Pstr)
c-----------------------------------------------------------------------
c     Insert the pointers before copying the strings.
c-----------------------------------------------------------------------
      CALL insptr(.true.,len(Str),Istr,Pstr,len(Chrvec),Ptrvec,Nstr)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Copy from last character to the first since the substring is
c being push down and maybe copying over itself.
c-----------------------------------------------------------------------
      begchr=Ptrvec(Istr)
      endchr=Ptrvec(Nstr)-1
      nwmold=Ptrvec(Istr)-Ptrvec(Istr-1)
c----------------------------------------------------------------------
      DO ichr=endchr,begchr,-1
       Chrvec(ichr:ichr)=Chrvec(ichr-nwmold:ichr-nwmold)
      END DO
c-----------------------------------------------------------------------
c     Now there is room to insert the new substring.
c-----------------------------------------------------------------------
      Chrvec(Ptrvec(Istr-1):Ptrvec(Istr)-1)=Str
c----------------------------------------------------------------------
      RETURN
      END
