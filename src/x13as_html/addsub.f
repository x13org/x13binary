**==addsub.f    processed by SPAG 6.05Fc at 12:31 on 12 Oct 2004
 
 
      SUBROUTINE ADD_SUB(A,B,C,N,M,Id,Ind)
      IMPLICIT NONE
**--ADDSUB6
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER Id,Ind,M,N
      REAL*8 A(Id,*),B(Id,*),C(Id,*)
C
C Local variables
C
      INTEGER i,j
C
C*** End of declarations rewritten by SPAG
C
c ****  Start of Executable Program                                     
C      INTEGER*4 N,M,ID,IND
 
      DO i=1,N
       DO j=1,M
        IF (Ind.GT.0) THEN
         C(i,j)=A(i,j)+B(i,j)
        ELSE
         C(i,j)=A(i,j)-B(i,j)
        END IF
       END DO
      END DO
      END
