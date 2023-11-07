C     Last change:  BCM  25 Nov 97   12:17 pm
      SUBROUTINE mltpos(Nelta,Arimap,Arimal,Opr,Begopr,Endopr,Neltc,C)
      IMPLICIT NONE
c----------------------------------------------------------------------
c     Filters the matrix, a, using the Difference, AR, and MA operators
c                     a=[Diff(B)*phi(B)/th(B)]*z,
c where the MA is filtered exactly or conditionally depending on exctMA.
c-----------------------------------------------------------------------
c Name  Type Description
c-----------------------------------------------------------------------
      DOUBLE PRECISION ZERO
      PARAMETER(ZERO=0D0)
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
c     ------------------------------------------------------------------
      INTEGER PXA
      PARAMETER(PXA=(PB+1)*(PLEN+2*PORDER))
c     ------------------------------------------------------------------
      LOGICAL secpas
      INTEGER Arimal,beglag,Begopr,endlag,Endopr,i,ilag,iopr,itmp,Nelta,
     &        Neltc,Opr
      DOUBLE PRECISION C,Arimap,tmp,work
      DIMENSION Arimal(*),Arimap(*),C(*),Opr(0:*),work(PXA)
c-----------------------------------------------------------------------
c     Note c is the a matrix on input.  First
c calculate the number of elements in the c/a matrices
c-----------------------------------------------------------------------
      secpas=.false.
      DO iopr=Begopr,Endopr
       beglag=Opr(iopr-1)
       endlag=Opr(iopr)-1
c-----------------------------------------------------------------------
c     Calculate the c(i)'s, i=lagb(nlag)+1,lag(nlag+1)
c-----------------------------------------------------------------------
       DO i=1,Neltc
        IF(secpas.or.i.le.Nelta)THEN
         tmp=C(i)
c     ------------------------------------------------------------------
        ELSE
         tmp=ZERO
        END IF
c-----------------------------------------------------------------------
c     Calculate c(i)
c-----------------------------------------------------------------------
        DO ilag=beglag,endlag
         itmp=i-Arimal(ilag)
         IF(itmp.gt.0.and.(secpas.or.itmp.le.Nelta))tmp=tmp-Arimap(ilag)
     &      *C(itmp)
        END DO
c     ------------------------------------------------------------------
        work(i)=tmp
       END DO
c     ------------------------------------------------------------------
       secpas=.true.
       CALL copy(work,Neltc,1,C)
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
