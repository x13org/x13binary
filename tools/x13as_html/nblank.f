C     Last change:  BCM  22 Dec 97    1:57 pm
**==nblank.f    processed by SPAG 3.10FA at 13:18 on  4 Aug 1992
      INTEGER FUNCTION nblank(Str)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Returns nblank, the index of the last non blank in the string
c-----------------------------------------------------------------------
c Input arguments
c Name  Type Description
c-----------------------------------------------------------------------
c str     c  String with 0 or more trailing blanks
c-----------------------------------------------------------------------
      CHARACTER Str*(*)
      INTEGER nb
c     ------------------------------------------------------------------
c     nblank=lnblnk(str)
c-----------------------------------------------------------------------
c     For non Sun systems, work backwards through the string until
c something other than a blank.
c-----------------------------------------------------------------------
      nblank=0
c      nb = LEN_TRIM(Str)
      nb = LEN(Str)
      if (nb.gt.0) then
       DO nblank = nb , 1 , -1
        IF(.NOT.(Str(nblank:nblank).eq.' '))GO TO 10
       END DO
      end if
c     ------------------------------------------------------------------
   10 RETURN
      END
