C     Last change:  BCM  14 May 1998    8:45 am
      SUBROUTINE regx11(A)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     This subroutine performs an OLS regression on the irregular 
c     component of an X-11 seasonal adjustment.  The regressors have
c     been previously chosen by the user.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
c-----------------------------------------------------------------------
      LOGICAL F,T
      DOUBLE PRECISION ONE,PI,TWO,ZERO,MONE
      PARAMETER(ONE=1D0,PI=3.14159265358979d0,TWO=2D0,ZERO=0D0,
     &          MONE=-1D0,F=.false.,T=.true.)
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'series.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'xclude.cmn'
      INCLUDE 'units.cmn'
c-----------------------------------------------------------------------
      INTEGER PA,PXA,PXY
      PARAMETER(PA=PLEN+2*PORDER,PXY=PLEN*(PB+1),PXA=PA*(PB+1))
c-----------------------------------------------------------------------
      CHARACTER cstr1*(6),cstr2*(6),cstr3*(6)
      DOUBLE PRECISION A,apa,txy
      INTEGER nrtxy,neltxy,nstr1,nstr2,nstr3
      DIMENSION A(PA),txy(PXA)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      DOUBLE PRECISION dpmpar
      EXTERNAL dpmpar,dpeq
c-----------------------------------------------------------------------
c     Check the work array size
c-----------------------------------------------------------------------
      Nfev=0
      IF(Nspobs*Ncxy.gt.PXY)THEN
       CALL itoc(Nspobs,cstr1,nstr1)
       IF(.not.Lfatal)CALL itoc(Ncxy,cstr2,nstr2)
       IF(.not.Lfatal)CALL itoc(PXA,cstr3,nstr3)
       IF(Lfatal)RETURN
       CALL errhdr
       CALL eWritln('Work array too small, '//cstr1(1:(nstr1-1))//
     &             ' * '//cstr2(1:(nstr2-1))//' > '//cstr3(1:(nstr3-1)),
     &             STDERR,Mt2,T,T)
       CALL abend
       RETURN
      END IF
c-----------------------------------------------------------------------
      Armaer=0
      Dnefob=dble(Nspobs-Nintvl)
      neltxy=Nspobs*Ncxy
      nrtxy=Nspobs
      CALL copy(Xy,neltxy,1,txy)
c-----------------------------------------------------------------------
c     If observations excluded from regression, delete the offending
c     rows of data from txy and adjust the row length variables.
c-----------------------------------------------------------------------
      IF(Nxcld.gt.0)THEN
       CALL dlrgrw(txy,Ncxy,Nspobs,Rgxcld)
       nrtxy=nrtxy-Nxcld
       neltxy=nrtxy*Ncxy
       Dnefob=Dnefob-Nxcld
      END IF
c-----------------------------------------------------------------------
c     Perform OLS regression
c-----------------------------------------------------------------------
      IF(Nb.le.0)THEN
       CALL yprmy(txy,nrtxy,apa)
       Chlxpx(1)=sqrt(apa)
      ELSE
       CALL olsreg(txy,nrtxy,Ncxy,Ncxy,B,Chlxpx,PXPX,Sngcol)
       IF(Lfatal)RETURN
       IF(Sngcol.gt.0)THEN
        Convrg=F
        Armaer=PSNGER
        RETURN
       END IF
       Nfev=Nfev+Ncxy+1
      END IF
c-----------------------------------------------------------------------
c     Calculate the objective function.
c-----------------------------------------------------------------------
      CALL resid(txy,nrtxy,Ncxy,Ncxy,1,Nb,MONE,B,A)
      IF(Lfatal)RETURN
      CALL yprmy(A,nrtxy,apa)
c-----------------------------------------------------------------------
c     Calculate the maximum likelihood variance and the likelihood
c-----------------------------------------------------------------------
      Var=apa/Dnefob
      IF(Var.lt.TWO*dpmpar(1))Var=ZERO
      IF(dpeq(Var,ZERO))THEN
       Lnlkhd=ZERO
      ELSE
       Lnlkhd=-(Dnefob*(log(TWO*PI*Var)+ONE))/TWO
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
