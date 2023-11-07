C     Last Change: Mar. 2021
C     Last change:  BCM  17 Apr 2003   11:17 pm
      SUBROUTINE rndsa(Sa,Sarnd,L1,L2,Rndok)
      IMPLICIT NONE
c     -------------------------------------------------------------------
c     Round the seasonally adjusted values so that they sum to the same
c     value as the rounded annual total, as in the UK version of X-11.
c     Author: Brian C. Monsell - August 1995      
c     -------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'x11opt.cmn'
c     -------------------------------------------------------------------
*      DOUBLE PRECISION ILIM
      LOGICAL F,T
*      PARAMETER(ILIM=2147483647D0)
      PARAMETER(F=.false.,T=.true.)
c     -------------------------------------------------------------------
      CHARACTER thisvl*(21)
      DOUBLE PRECISION c,cindx,Sa,sumx,X,Sarnd,thisx
      LOGICAL Rndok
      INTEGER j,L1,L2,ll1,ll2,r,sumr,d,round,ij,nn
      DIMENSION Sa(*),Sarnd(*),X(PSP),c(PSP),r(PSP),cindx(PSP),
     &          thisx(PSP)
      EXTERNAL round
c     -------------------------------------------------------------------
c     initialize variables
c     -------------------------------------------------------------------
      Rndok=T
      ll1=L1
      IF((ll1/Ny)*Ny.eq.ll1)THEN
       x(1) = SA(ll1)*(10D0**Kdec)
       r(1) = round(x(1))
       Sarnd(ll1)= r(1)/(10D0**Kdec)
       ll1=ll1+1
      END IF
      DO WHILE (ll1.le.L2)
       sumx=0D0
       sumr=0
       ll2=((ll1/Ny)+1)*Ny
       IF(ll2.gt.L2)ll2=L2
c     -------------------------------------------------------------------
c     begin looping through every year of seasonally adjusted data.
c     -------------------------------------------------------------------
       DO ij=ll1,ll2
        j=ij-ll1+1
        cindx(j)=dble(j)
c     -------------------------------------------------------------------
c     multiply observations by 10^(# of significant digits in output) and
c     see if this number can be represented as an integer.  If not, exit
c     subroutine
c     -------------------------------------------------------------------
        x(j)=SA(ij)*(10D0**Kdec)
        IF(x(j).gt.1000D0)THEN
         write(thisvl,1000)x(j)
         thisvl(18:20)='000'
         read(thisvl,1000)thisx(j)
         x(j)=x(j)-thisx(j)
 1000    FORMAT(f21.0)
        ELSE
         thisx(j)=0D0
        END IF
*        IF(ABS(x(j)).gt.ILIM)THEN
*         CALL writln('ERROR: Cannot perform rounding on seasonally adjus
*     &ted series:',STDERR,Mt2,T)
*         CALL writln('       observation too large to be represented as 
*     &an integer.',STDERR,Mt2,F)
*         Rndok=F
*         RETURN
*        END IF
c     -------------------------------------------------------------------
c     round seasonally adjusted numbers to integers, and store the 
c     difference between the rounded and original figures.
c     ------------------------------------------------------------------
        r(j)=round(x(j))
        c(j)=dble(r(j))-x(j)
        sumx=sumx+x(j)
*        IF((ABS(sumx).gt.ILIM).OR.((ILIM-sumr).lt.r(j)))THEN
*         CALL writln('ERROR: Cannot perform rounding on seasonally adjus
*     &ted series:',STDERR,Mt2,T)
*         CALL writln('       yearly total too large to be represented as
*     & an integer.',STDERR,Mt2,F)
*         Rndok=F
*         RETURN
*        END IF
        sumr=sumr+r(j)
       END DO
c     ------------------------------------------------------------------
c     Determine the difference between the rounded yearly total and the
c     total of the rounded observations.
c     ------------------------------------------------------------------
       d=round(sumx)-sumr
c     -------------------------------------------------------------------
c     Sort the differences
c     -------------------------------------------------------------------
       nn=ll2-ll1+1
       CALL ssort(c,cindx,nn,2)
c     -------------------------------------------------------------------
c     if d is greater than zero, add 1 to the observations with the 
c     d largest differences in c.
c     -------------------------------------------------------------------
       IF(d.gt.0)THEN
        DO j=nn,nn-d+1
         ij=int(cindx(j))
         r(ij)=r(ij)+1
        END DO
c     -------------------------------------------------------------------
c     if d is less than zero, subract 1 from the observations with the 
c     d largest smallest in c.
c     -------------------------------------------------------------------
       ELSE IF(d.lt.0)THEN
        DO j=1,d
         ij=int(cindx(j))
         r(ij)=r(ij)-1
        END DO
       END IF
c     -------------------------------------------------------------------
c     Store result into Sarnd.
c     -------------------------------------------------------------------
       DO ij=ll1,ll2
        j=ij-ll1+1
        Sarnd(ij)=(thisx(j)+dble(r(j)))/(10D0**Kdec)
       END DO
       ll1=ll2+1
      END DO
c     -------------------------------------------------------------------
      RETURN
      END
