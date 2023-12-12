C     Last change:  BCM  28 May 1998    9:26 am
      SUBROUTINE chitst(Xpxinv,Begcol,Endcol,Chi2vl,Pv,Regidx,Is1grp,
     &                  Info)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'mdldat.cmn'
c-----------------------------------------------------------------------
      INCLUDE 'units.cmn'
c-----------------------------------------------------------------------
c     replace dimension length for Xpxinv (BCM May 2007)
      LOGICAL Is1grp
      INTEGER Begcol,coldsp,Endcol,icol,ielt,Info,irow,ncol,Regidx,jcol,
     &        jelt,kelt,lstcol,nnfix,fstcol,kcol
      DOUBLE PRECISION chisq,Chi2vl,Pv,rhs,subinv,Xpxinv
      DIMENSION rhs(PB),subinv(PB*(PB+1)),Xpxinv(PXPX),Regidx(PB)
      EXTERNAL chisq
c-----------------------------------------------------------------------
c     Compute chi square values for variables [begcol:endcol]
c-----------------------------------------------------------------------
      ncol=0
      ielt=1
      jelt=NOTSET
c-----------------------------------------------------------------------
      DO icol=Begcol,Endcol
       IF(Regidx(icol).ne.NOTSET)lstcol=icol
      END DO
c-----------------------------------------------------------------------
      DO icol=Begcol,Endcol
       IF(Regidx(icol).ne.NOTSET)THEN
        ncol=ncol+1
        IF(jelt.eq.NOTSET)jelt=Regidx(icol)
        rhs(ncol)=B(icol)
        coldsp=(Regidx(icol)-1)*Regidx(icol)/2
        nnfix=0
c-----------------------------------------------------------------------
        DO irow=jelt,lstcol
         IF(Is1grp.or.Regidx(irow).ne.NOTSET)THEN
          nnfix=nnfix+1
          IF(nnfix.le.ncol)THEN
           subinv(ielt)=Xpxinv(coldsp+nnfix+jelt-1)
           ielt=ielt+1
          END IF
         END IF
        END DO
c-----------------------------------------------------------------------
       END IF
      END DO
c-----------------------------------------------------------------------
      CALL dppfa(subinv,ncol,Info)
      IF(Info.eq.0)THEN
       CALL dppsl(subinv,ncol,rhs,.true.)
       CALL yprmy(rhs,ncol,Chi2vl)
       Chi2vl=Chi2vl/Var
       Pv=chisq(Chi2vl,ncol)
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
