**==hndend.f    processed by SPAG 4.03F  at 17:01 on 16 May 1994
      SUBROUTINE hndend(M,Nterm,W,Endwt,R)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     This subroutine uses the algorithm given in Doherty (1993) to
c     generate end filters of length m for an nterm Henderson filter.
c-----------------------------------------------------------------------
      INCLUDE 'hender.prm'
c-----------------------------------------------------------------------
      DOUBLE PRECISION ZERO,TWO
      PARAMETER(ZERO=0D0,TWO=2D0)
c-----------------------------------------------------------------------
      INTEGER M,Nterm,n,i,j
      DOUBLE PRECISION W(*),cw(PMXHND),Endwt(*),u1,u2,c1,c2,R
c-----------------------------------------------------------------------
      n=(Nterm+1)/2
      j=1
      DO i=n,2,-1
       cw(j)=W(i)
       j=j+1
      END DO
      j=1
      DO i=n,Nterm
       cw(i)=W(j)
       j=j+1
      END DO
c-----------------------------------------------------------------------
      u1=ZERO
      u2=ZERO
      DO j=M+1,Nterm
       u1=u1+cw(j)
       u2=u2+(j-dble(M+1)/TWO)*cw(j)
      END DO
c-----------------------------------------------------------------------
      DO i=1,M
       c1=(i-(dble(M+1)/TWO))*R
       c2=1+(dble(M*(M-1)*(M+1))/12D0)*R
       Endwt(i)=cw(i)+(u1/dble(M))+(u2*(c1/c2))
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
