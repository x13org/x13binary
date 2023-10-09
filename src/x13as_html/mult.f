**==mult.f    processed by SPAG 6.05Fc at 12:31 on 12 Oct 2004
 
      SUBROUTINE MATMLT(A,B,C,M,Ip,Iq,Ia,Ib,Ic)
      IMPLICIT NONE
**--MULT5
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      INTEGER*4 Ia,Ib,Ic,Ip,Iq,M
      REAL*8 A(Ia,*),B(Ib,*),C(Ic,*)
C
C Local variables
C
      INTEGER*4 i,ir,is
      REAL*8 sum
C
C*** End of declarations rewritten by SPAG
C
 
c ****  Start of Executable Program                                     
 
C      a(m,p)*b(p,q) = c(m,q)
 
      DO ir=1,M
       DO is=1,Iq
        sum=0.0D0
        DO i=1,Ip
         sum=sum+(A(ir,i)*B(i,is))
        END DO
        C(ir,is)=sum
       END DO
      END DO
      END
