C     Last change:  BCM   1 Dec 1998   10:09 am
**==eltlen.f    processed by SPAG 4.03F  at 09:48 on  1 Mar 1994
      SUBROUTINE eltlen(Istr,Ptrvec,Nstr,Length)
      IMPLICIT NONE
c     -----------------------------------------------------------------
      INCLUDE 'stdio.i'
      INTEGER Istr,Length,Nstr,Ptrvec
      DIMENSION Ptrvec(0:Nstr)
      INCLUDE 'units.cmn'
c     -----------------------------------------------------------------
      IF(Istr.lt.1.or.Istr.gt.Nstr)THEN
       WRITE(STDERR,1010)Istr,Nstr
 1010  FORMAT(' ERROR: No position',i3,' in ',i3,
     &        ' long character vector.')
       CALL errhdr
       WRITE(Mt2,1011)Istr,Nstr
 1011  FORMAT('<p><strong>ERROR:</strong> No position',i3,' in ',i3,
     &        ' long character vector.</p>')
       CALL abend
       RETURN
c     -----------------------------------------------------------------
      ELSE
       Length=Ptrvec(Istr)-Ptrvec(Istr-1)
      END IF
c     -----------------------------------------------------------------
      RETURN
      END
