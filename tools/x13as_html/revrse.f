**==revrse.f    processed by SPAG 4.03F  at 09:53 on  1 Mar 1994
      SUBROUTINE revrse(Frwd,Nr,Nc,Bkwd)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Reverses the rows of x, an nr by nc matrix.  Note that we go
c halfway through the series.
c-----------------------------------------------------------------------
      INTEGER Nr,Nc,hlf,i,j,backi
      DOUBLE PRECISION Frwd(Nc,Nr),Bkwd(Nc,Nr),tmp
c-----------------------------------------------------------------------
c     If the number of rows is even then exchange half the rows, hlf=nr/2.
c If the number of rows is odd make sure the middle row is written also,
c hlf=nr/2+1.
c-----------------------------------------------------------------------
      hlf=(Nr+1)/2
      DO i=1,hlf
       backi=Nr-i+1
c     ------------------------------------------------------------------
       DO j=1,Nc
        tmp=Frwd(j,i)
        Bkwd(j,i)=Frwd(j,backi)
        Bkwd(j,backi)=tmp
       END DO
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
