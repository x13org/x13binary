C     Last change:  BCM  23 Jul 1998    3:36 pm
**==dtoc.f    processed by SPAG 4.03F  at 09:48 on  1 Mar 1994
      SUBROUTINE dtoc(Dnum,Str,Ipos)
c     -----------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'stdio.i'
      INCLUDE 'savcmn.cmn'
      INCLUDE 'units.cmn'
c     -----------------------------------------------------------------
      LOGICAL T
      DOUBLE PRECISION ZERO
      PARAMETER(T=.true.,ZERO=0D0)
c     -----------------------------------------------------------------
      CHARACTER Str*(*),temp*22,tmp2*3
      DOUBLE PRECISION d10
      INTEGER Ipos,nleft
      DOUBLE PRECISION Dnum
c     -----------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c     -----------------------------------------------------------------
      nleft=len(Str(Ipos:))
      IF(dpeq(Dnum,ZERO))then
       d10=0
      ELSE
       d10=log10(abs(Dnum))
      END IF
c     -----------------------------------------------------------------
      IF(Svsize.gt.nleft)THEN
       WRITE(temp,Svfmt)Dnum
       WRITE(tmp2,1010)nleft
 1010  FORMAT(i3)
       CALL eWritln('Cannot write '//temp(1:Svsize)//' in '//tmp2//
     &              ' spaces.',STDERR,Mt2,T,T)
       CALL abend
       RETURN
c     -----------------------------------------------------------------
      ELSE
       IF(d10.gt.-100d0)THEN
        WRITE(Str(Ipos:),Svfmt)Dnum
       ELSE
        WRITE(Str(Ipos:),Svfmt)0D0
       END IF
       Ipos=Ipos+Svsize
      END IF
c     -----------------------------------------------------------------
      RETURN
      END


      