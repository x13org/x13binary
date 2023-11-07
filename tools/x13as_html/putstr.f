C     Last change:  BCM   2 Apr 98    1:01 pm
**==putstr.f    processed by SPAG 4.03F  at 09:52 on  1 Mar 1994
      SUBROUTINE putstr(Str,Pstr,Chrvec,Ptrvec,Nstr)
      IMPLICIT NONE
c     -----------------------------------------------------------------
      INCLUDE 'error.cmn'
c     -----------------------------------------------------------------
      CHARACTER Chrvec*(*),Str*(*)
      INTEGER Nstr,Pstr,Ptrvec
      DIMENSION Ptrvec(0:Pstr)
c     -----------------------------------------------------------------
      CALL insptr(.true.,len(Str),Nstr+1,Pstr,len(Chrvec),Ptrvec,Nstr)
c     -----------------------------------------------------------------
      IF(.not.Lfatal)Chrvec(Ptrvec(Nstr-1):Ptrvec(Nstr)-1)=Str
c     -----------------------------------------------------------------
      RETURN
c     -----------------------------------------------------------------
      END
