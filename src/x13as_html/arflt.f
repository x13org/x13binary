**==arflt.f    processed by SPAG 4.03F  at 09:46 on  1 Mar 1994
      SUBROUTINE arflt(Nelta,Arimap,Arimal,Opr,Begopr,Endopr,C,Neltc)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INTEGER Arimal,beglag,Begopr,endlag,Endopr,i,ilag,iopr,mxlag,
     &        Nelta,Neltc,ntmpa,off,Opr
      DOUBLE PRECISION C,Arimap,tmp
      DIMENSION Arimal(*),Arimap(*),C(*),Opr(0:*)
c-----------------------------------------------------------------------
c     Note c is the a matrix on input.  First
c calculate the number of elements in the c/a matrices
c-----------------------------------------------------------------------
      ntmpa=Nelta
c-----------------------------------------------------------------------
      DO iopr=Begopr,Endopr
       CALL maxlag(Arimal,Opr,iopr,iopr,mxlag)
       ntmpa=ntmpa-mxlag
       beglag=Opr(iopr-1)
       endlag=Opr(iopr)-1
c-----------------------------------------------------------------------
c     Calculate the a(i)'s, i=lagb(nlag)+1,lag(nlag+1)
c-----------------------------------------------------------------------
       DO i=1,ntmpa
        off=i+mxlag
        tmp=C(off)
c-----------------------------------------------------------------------
c     Calculate c(i)
c-----------------------------------------------------------------------
        DO ilag=beglag,endlag
         tmp=tmp-Arimap(ilag)*C(off-Arimal(ilag))
        END DO
        C(i)=tmp
       END DO
c-----------------------------------------------------------------------
       Neltc=ntmpa
      END DO
c-----------------------------------------------------------------------
      RETURN
      END

