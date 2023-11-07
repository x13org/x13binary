**==meancra.f    processed by SPAG 6.05Fc at 12:31 on 12 Oct 2004
 
      SUBROUTINE MEANCRA(A1x,Aty,Rtz,Modlid,Mq,Ny)
      IMPLICIT NONE
**--MEANCRA5
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER Modlid,Mq,Ny
      REAL*8 A1x(*),Aty(*),Rtz(*)
C
C Local variables
C
      INTEGER i,j,k
      REAL*8 tt
C
C*** End of declarations rewritten by SPAG
C
 
C   -----------  ATY IS D11
 
c ****  Start of Executable Program                                     
 
      IF (Modlid.EQ.0) THEN
       DO i=1,Ny
        tt=A1x(i)/Aty(i)-1D0
        k=Mq*(i-1)
        DO j=1,Mq
         Rtz(k+j)=tt
        END DO
       END DO
      ELSE
       DO i=1,Ny
        tt=A1x(i)-Aty(i)
        k=Mq*(i-1)
 
        DO j=1,Mq
         Rtz(k+j)=tt/Mq
        END DO
       END DO
      END IF
      END
