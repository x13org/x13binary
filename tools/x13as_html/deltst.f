C     Last change:  BCM   2 Apr 98   12:56 pm
      SUBROUTINE deltst(Nefobs,Begcol,Endcol,Mint,Mini,Minptr,Lauto,
     &                  Lxreg)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Finds the minimum absolute t statistic in the subset, subqrp, of
c regression parameters.
c-----------------------------------------------------------------------
c Name  Type Description
c-----------------------------------------------------------------------
c i       i  Local do loop index
c begcol    i  Local index to the first regression estimate to test
c ielt    i  Local index for an element of the xpx matrix
c endcol    i  Local index to the last (end) regression estimate to test
c mini    i  Output index to the outlier with the smallest t-value
c mint    d  Output smallest t-value of the identified outliers
c nefobs  i  Input number of effective observations or number of
c             observations to compute the likelihood
c nelt    i  Local number of elements in the packed form of
c             chol([X:y]'[X:y])
c rmse    d  Output root mean square error
c tmp     d  Local temporary scalar
c tval    d  Local t statistic*rmse for the current beta
c xpxinv  d  Local pb(pb+1)/2, (ncxy-1)ncxy/2 used vector to hold the
c             packed form of the inverse of X'X
c-----------------------------------------------------------------------
c     Variable typing and initialization
c-----------------------------------------------------------------------
      DOUBLE PRECISION ZERO
      LOGICAL T,F
      PARAMETER(ZERO=0.0D0,T=.true.,F=.false.)
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      CHARACTER tmpttl*(PCOLCR)
      LOGICAL locok,Lauto,Lxreg
      INTEGER i,i2,j,Begcol,ielt,Endcol,Mini,Nefobs,nelt,otltyp,ntmpcr,
     &        t0,itmp,Minptr,fh2
      DOUBLE PRECISION Mint,rmse,tmp,tval,xpxinv
      DIMENSION tmp(2),xpxinv(PB*(PB+1)/2),Mini(POTLR),Mint(POTLR),
     &          Minptr(POTLR)
C-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
c     Find the root mean square error, the first diagonal element
c of chol(X'X), and initialize the minimum values
c-----------------------------------------------------------------------
      nelt=Nb*Ncxy/2
      rmse=Chlxpx(nelt+Ncxy)
      IF(dpeq(rmse,ZERO))THEN
       fh2=0
       IF(.not.Lhiddn)THEN
        WRITE(STDERR,1010)
        fh2=Mt1
       END IF
       CALL eWritln('Cannot compute outlier t-statistic for '//
     &              'outlier backward deletion - ',fh2,Mt2,T,F)
       CALL writln(' the residual root mean square error is zero.',
     &             fh2,Mt2,F,T)
       IF(.not.Lauto)THEN
        IF(Lxreg)THEN
         IF(.not.Lhiddn)WRITE(STDERR,1020)'x11regression options'
         CALL writln(' Check the x11regression options specified'//
     &               ' in the input specification file.',fh2,Mt2,T,T)
        ELSE
         IF(.not.Lhiddn)WRITE(STDERR,1020)'regARIMA model'
         CALL writln(' Check the regARIMA model specified'//
     &               ' in the input specification file.',fh2,Mt2,T,T)
        END IF
       END IF
       CALL abend
       RETURN
      END IF
      rmse=rmse/sqrt(dble(Nefobs))
      CALL copy(Chlxpx,nelt,1,xpxinv)
      CALL dppdi(xpxinv,Nb,tmp,1)
      CALL setint(NOTSET,POTLR,Mini)
c-----------------------------------------------------------------------
c     Calculate b(i)/sqrt(X'X) for each regression effect in the subset
c-----------------------------------------------------------------------
      DO i=Begcol,Endcol
       CALL getstr(Colttl,Colptr,Ncoltl,i,tmpttl,ntmpcr)
       IF(.not.Lfatal)CALL rdotlr(tmpttl(1:ntmpcr),Begspn,Sp,otltyp,t0,
     &                            itmp,locok)
       IF((.not.locok).and.(.not.Lfatal))CALL abend()
       IF(Lfatal)RETURN
       IF(i.eq.Begcol)THEN
        ielt=Begcol*(Begcol+1)/2
       ELSE
        ielt=ielt+i
       END IF
       tval=B(i)/sqrt(xpxinv(ielt))/rmse
c-----------------------------------------------------------------------
c     Check to see if this outlier is the minimum for its type (AO or
c     LS)
c-----------------------------------------------------------------------
       IF(Mini(otltyp).eq.NOTSET)THEN
        Mini(otltyp)=i
        Mint(otltyp)=tval
       ELSE IF(abs(tval).le.abs(Mint(otltyp)))THEN
        Mini(otltyp)=i
        Mint(otltyp)=tval
       END IF
      END DO
c-----------------------------------------------------------------------
c     Create pointer that has rank of smallest t-stats of each type
c-----------------------------------------------------------------------
      CALL setint(NOTSET,POTLR,Minptr)
      i2=1
      DO i=1,POTLR
       IF(Mini(i).ne.NOTSET)THEN
        Minptr(i2)=i
        IF(i2.gt.1)THEN
         j=i2-1
         DO WHILE(j.gt.0)
          IF(abs(Mint(Minptr(j))).gt.abs(Mint(Minptr(j+1))))THEN
           Minptr(j+1)=Minptr(j)
           Minptr(j)=i
          END IF
          j=j-1
         END DO
        END IF
        i2=i2+1
       END IF
      END DO
c-----------------------------------------------------------------------      
 1010 FORMAT(/,' ERROR: Cannot compute outlier t-statistic for',
     &         ' outlier backward deletion - ',
     &       /,'        the residual root mean square error is zero.')
 1020 FORMAT(/,'        Check the ',a,' specified in the',
     &         ' input specification',/,'        file.',/)
c-----------------------------------------------------------------------      
      RETURN
      END
