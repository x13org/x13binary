      SUBROUTINE rmlpyr(Trnsrs,Nobspf)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     remove prior effects like leap year adjustments when trading day
c     factors are removed from the regression matrix and Picktd = true.
c     (BCM May 2004).
c-----------------------------------------------------------------------
      LOGICAL F,T
      INTEGER DIV,MULT,PLOM,PLOQ
      PARAMETER(DIV=4,MULT=3,PLOM=2,PLOQ=3,F=.false.,T=.true.)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'inpt.cmn'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'priusr.cmn'
      INCLUDE 'adj.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION Trnsrs,lomeff
      INTEGER Nobspf
      LOGICAL lom,begrgm
      DIMENSION Trnsrs(*),lomeff(PLEN),begrgm(PLEN)
c-----------------------------------------------------------------------
c     Generate leap year effect 
c-----------------------------------------------------------------------
      IF(Priadj.eq.PLOM.or.Priadj.eq.PLOQ)THEN
       lom=T
      ELSE
       lom=F
      END IF
      IF(Lrgmtd.and.MOD(Tdzero,2).ne.0)THEN
       CALL gtrgpt(Begadj,Tddate,Tdzero,begrgm,Nadj)
      ELSE
       CALL setlg(T,PLEN,begrgm)
      END IF
      CALL td7var(Begadj,Sp,Nadj,1,1,lom,F,T,lomeff,begrgm)
c-----------------------------------------------------------------------
c     Adjust the series for the length of month effect
c-----------------------------------------------------------------------
      CALL eltfcn(DIV,Y(Frstsy),Adj(Adj1st),Nobspf,PLEN,Trnsrs)
      CALL eltfcn(MULT,Trnsrs,lomeff(Adj1st),Nobspf,PLEN,Trnsrs)
      IF(Lmvaft.or.Ln0aft)THEN
       CALL trnfcn(trnsrs,Nspobs,Fcntyp,Lam,trnsrs)
      ELSE
       CALL trnfcn(trnsrs,Nobspf,Fcntyp,Lam,trnsrs)
      END IF
      IF(Lfatal)RETURN
      CALL eltfcn(DIV,Adj(Adj1st),lomeff(Adj1st),Nobspf,PLEN,
     &            Adj(Adj1st))
c-----------------------------------------------------------------------
c     copy updated adjustment factors into Sprior
c-----------------------------------------------------------------------
      CALL copy(Adj,Nadj,-1,Sprior(Setpri))
c-----------------------------------------------------------------------
c     Update indicator variables
c-----------------------------------------------------------------------
      Priadj=0
      IF(Nustad.eq.0.and.Nuspad.eq.0)THEN
       Kfmt=0
       IF(Lpradj)Lpradj=F
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
      
