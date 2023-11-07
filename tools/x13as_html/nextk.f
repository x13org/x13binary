C     Last change:  BCM  15 Apr 2005   12:12 pm
      SUBROUTINE nextk(Type)
      IMPLICIT NONE
C **********************************************************************
C *                                                                    *
C * COMPUTES THE NEXT K POLYNOMIALS USING SCALARS COMPUTED IN CALCSC   *
C *                                                                    *
C **********************************************************************
C-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'global.cmn'
C-----------------------------------------------------------------------
      DOUBLE PRECISION temp,dabs
      INTEGER i,Type
C-----------------------------------------------------------------------
      IF(Type.ne.3)THEN
       temp=A0
       IF(Type.eq.1)temp=B0
       IF(dabs(A1).gt.dabs(temp)*Eta*10D0)THEN
C-----------------------------------------------------------------------
C USE SCALED FORM OF THE RECURRENCE
C-----------------------------------------------------------------------
        A7=A7/A1
        A3=A3/A1
        K(1)=Qp(1)
        K(2)=Qp(2)-A7*Qp(1)
        DO i=3,N
         K(i)=A3*Qk(i-2)-A7*Qp(i-1)+Qp(i)
        END DO
        RETURN
       ELSE
C-----------------------------------------------------------------------
C IF A1 IS NEARLY ZERO THEN USE A SPECIAL FORM OF THE RECURRENCE
C-----------------------------------------------------------------------
        K(1)=0.D0
        K(2)=-A7*Qp(1)
        DO i=3,N
         K(i)=A3*Qk(i-2)-A7*Qp(i-1)
        END DO
        RETURN
       END IF
      END IF
C-----------------------------------------------------------------------
C USE UNSCALED FORM OF THE RECURRENCE IF TYPE IS 3
C-----------------------------------------------------------------------
      K(1)=0.D0
      K(2)=0.D0
      DO i=3,N
       K(i)=Qk(i-2)
      END DO
      RETURN
      END
