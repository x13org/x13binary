**==aggmea.f    processed by SPAG 4.03F  at 09:46 on  1 Mar 1994
      SUBROUTINE aggmea(A,B,W,X,Y,Z,I1,I2,Ma,Xa)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      DOUBLE PRECISION A,B,c,cbar,div,W,X,Y,Z
      INTEGER i,I1,I2,k,Ma
      LOGICAL Xa
C*** End of declarations inserted by SPAG
      INCLUDE 'srslen.prm'
C  THIS SUBROUTINE PRODUCES THE MEASURES OF ROUGHNESS R1 AND R2
C  (MEAN SQUARE ERROR AND ROOT MEAN SQUARE ERROR) OF DIRECT AND
C  INDIRECT SEASONAL ADJUSTMENT OF COMPOSITED SERIES
      DIMENSION A(PLEN),B(PLEN)
      k=I2-1
      W=0.D0
      X=0.D0
      Y=0.D0
      Z=0.D0
      DO i=I1,k
       W=W+(A(i+1)-A(i))*(A(i+1)-A(i))
      END DO
      div=I2-I1
      W=W/div
      X=sqrt(W)
      IF(.not.Xa)RETURN
      cbar=0.D0
      DO i=I1,I2
       IF(Ma.eq.0)THEN
        cbar=cbar+(A(i)/B(i))
       ELSE
        cbar=cbar+(A(i)-B(i))
       END IF
      END DO
      div=div+1.D0
      cbar=cbar/div
      DO i=I1,I2
       IF(Ma.eq.0)THEN
        c=(A(i)/B(i))-cbar
       ELSE
        c=(A(i)-B(i))-cbar
       END IF
       Y=Y+(c*c)
      END DO
      Y=Y/div
      Z=sqrt(Y)
      RETURN
      END
