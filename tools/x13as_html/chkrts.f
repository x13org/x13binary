      LOGICAL FUNCTION chkrts(Begopr,Endopr)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     chkrts.f, Release 1, Subroutine Version 1.1, Modified 16 Feb 1995.
c-----------------------------------------------------------------------
c     Check the roots of theta(B)=0 and makes them invertible if
c their roots are inside the unit circle.
c-----------------------------------------------------------------------
c Changed:
c  To recompute the degree of the polynomial if the lag is not the
c    highest lag, by REG on 04 Feb 2004, revised by BCM on 06 Feb 2004.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
c     ------------------------------------------------------------------
      LOGICAL F,T
      DOUBLE PRECISION ONE
      PARAMETER(F=.false.,T=.true.,ONE=1D0)
c     ------------------------------------------------------------------
      LOGICAL allfix,allinv
      INTEGER beglag,Begopr,degree,endlag,Endopr,factor,i,ic,icc,ihlf,
     &        ilag,iopr,lagone
      DOUBLE PRECISION coef(PORDER+1),cfncsq,coefi,coefc,coefnc
c-----------------------------------------------------------------------
c     Check and possibly inverts the roots of theta(B)=0
c-----------------------------------------------------------------------
      chkrts=F
c     ------------------------------------------------------------------
      IF(Endopr.gt.0)THEN
       DO iopr=Begopr,Endopr
        beglag=Opr(iopr-1)
        endlag=Opr(iopr)-1
c     ------------------------------------------------------------------
        factor=Oprfac(iopr)
        lagone=Arimal(beglag)
        degree=lagone/factor
        DO ilag=beglag,endlag
         IF(lagone.lt.Arimal(ilag))THEN
          degree=Arimal(ilag)/factor
          lagone=Arimal(ilag)
         END IF
        END DO
        CALL setdp(0D0,degree,coef)
c     ------------------------------------------------------------------
        allfix=T
        DO ilag=beglag,endlag
         IF(.not.Arimaf(ilag))allfix=F
         coef(Arimal(ilag)/factor)=Arimap(ilag)
        END DO
c     ------------------------------------------------------------------
        IF(.not.allfix)THEN
         allinv=F
         DO ic=degree,1,-1
c-----------------------------------------------------------------------
c     \phi_{p+1}
c-----------------------------------------------------------------------
          coefnc=coef(ic)
c-----------------------------------------------------------------------
c     1=\phi^2_{p+1,p+1}
c-----------------------------------------------------------------------
          cfncsq=ONE-coefnc*coefnc
          IF(cfncsq.le.0)GO TO 20
c     ------------------------------------------------------------------
          IF(ic.eq.1)GO TO 10
          ihlf=ic/2
c     ------------------------------------------------------------------
          DO i=1,ihlf
c-----------------------------------------------------------------------
c     \phi_{p+1,j}
c-----------------------------------------------------------------------
           coefi=coef(i)
c-----------------------------------------------------------------------
c     p-j+1
c-----------------------------------------------------------------------
           icc=ic-i
c-----------------------------------------------------------------------
c     \phi_{p+1,p-j+1}
c-----------------------------------------------------------------------
           coefc=coef(icc)
c-----------------------------------------------------------------------
c     \phi_{p,j}=\phi_{p+1,j}+\phi_{p+1,p+1}\phi_{p+1,p-j+1}/
c (1=\phi^2_{p+1,p+1})
c-----------------------------------------------------------------------
           coef(i)=(coefi+coefnc*coefc)/cfncsq
c-----------------------------------------------------------------------
c     \phi_{p,p-j+1}=\phi_{p+1,p-j+1}+\phi_{p+1,p+1}\phi_{p+1,j}/
c (1=\phi^2_{p+1,p+1})
c-----------------------------------------------------------------------
           IF(icc.ne.ihlf)coef(icc)=(coefc+coefnc*coefi)/cfncsq
          END DO
         END DO
c     ------------------------------------------------------------------
   10    allinv=T
c-----------------------------------------------------------------------
c    Check invertibility, set the error flag, and print the error
c message if necessary.
c-----------------------------------------------------------------------
   20    IF(.not.allinv)THEN
          chkrts=T
          Prbfac=iopr
         END IF
        END IF
       END DO
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
