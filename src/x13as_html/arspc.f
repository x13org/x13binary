C     Last change:  BCM  15 Jan 2008    12:40 pm
      SUBROUTINE arspc(Frq,Nfrq,Maxar,Bar,Var,Ldecbl,Sxx)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      DOUBLE PRECISION PI,ONE,ZERO,TEN
      PARAMETER(PI=3.14159265358979d0,ONE=1D0,ZERO=0D0,TEN=10D0)
c-----------------------------------------------------------------------
      LOGICAL Ldecbl
      INTEGER Maxar,Nfrq,i,j
      DOUBLE PRECISION Frq,Bar,Var,Sxx,c2,s2,dj
c-----------------------------------------------------------------------
      DIMENSION Bar(*),Frq(*),Sxx(*)
c-----------------------------------------------------------------------
      DOUBLE PRECISION decibl
      EXTERNAL decibl
c-----------------------------------------------------------------------
      DO i=1,Nfrq
       c2=ZERO
       DO j=1,Maxar
        dj=dble(2*j)*PI*Frq(i)
        c2=c2+(Bar(j)*cos(dj))
       END DO
       s2=ZERO
       DO j=1,Maxar
        dj=dble(2*j)*PI*Frq(i)
        s2=s2+(Bar(j)*sin(dj))
       END DO
       Sxx(i)=Var/((1-c2)*(1-c2) + s2*s2)
c-----------------------------------------------------------------------
       IF(Ldecbl)THEN
        IF(Sxx(i).lt.ZERO)Sxx(i)=-Sxx(i)
        Sxx(i)=decibl(Sxx(i))
       END IF
c-----------------------------------------------------------------------
      END DO
      RETURN
      END
       