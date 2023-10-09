C     Last change:  BCM  29 Oct 97    7:11 am
**==fis.f    processed by SPAG 4.03F  at 09:48 on  1 Mar 1994
      DOUBLE PRECISION FUNCTION fis(Cs,N)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      DOUBLE PRECISION c,Cs
      INTEGER N
C*** End of declarations inserted by SPAG
C --- THIS FUNCTION ADJUSTS THE I/S RATIO FOR THE NUMBER OF YEARS
C
      DIMENSION c(8)
      DATA c/1.00000D0,1.02584D0,1.01779D0,1.01383D0,1.00000D0,
     &       3.00000D0,1.55291D0,1.30095D0/
      IF(N.lt.6)THEN
       fis=c(N-1)
       Cs=c(N+3)
       RETURN
      END IF
      Cs=dble(N)*1.732051D0/(8.485281D0+dble(N-6)*1.732051D0)
      fis=DBLE(N)*12.247449D0/(73.239334D0+dble(N-6)*12.247449D0)
      RETURN
      END
