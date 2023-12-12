C     Last change:  BCM  14 Jul 1998    9:03 am
      SUBROUTINE fcstxy(Fctori,Nfcst,Fcst,Se,Rgvar)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     fcstxy.f, Release 1, Subroutine Version 1.5, Modified 03 Feb 1995.
c-----------------------------------------------------------------------
c     Computes forecasts for the data.  The data
c has been transformed by a Box-Cox transformation and adjusted
c for regression effects
c-----------------------------------------------------------------------
c     Include files
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'error.cmn'
c      INCLUDE 'units.cmn'
c-----------------------------------------------------------------------
c Input Arguments
c Name Type  Description
c-----------------------------------------------------------------------
c a       d  Nrsd long array of residuals
c nfcst   i  Number of forcasts to calculate
c fctori    i  Length of the undifferences series
c nrsd    i  Number of residuals to calculate. Nwp if conditional,
c             na if exact.  This number tell the maflt routine which
c             type of filtering to do.
c Xy    d  Vector of transformed, undifferenced series
c-----------------------------------------------------------------------
      INTEGER Nfcst,Fctori,nrsd
c-----------------------------------------------------------------------
c Name Type  Description
c-----------------------------------------------------------------------
c fcst    d  Ouput nfcst long vector of forecasts of the tranformed and
c             regression adjusted data (has mean though)
c se      d  Output nfcst long vector of standard errors
c-----------------------------------------------------------------------
      DOUBLE PRECISION Fcst,Rgvar,Se
      DIMENSION Fcst(Nfcst),Rgvar(Nfcst),Se(Nfcst)
c-----------------------------------------------------------------------
c Name Type  Description
c-----------------------------------------------------------------------
c aropr   i  Operator index vector (like opr) for the expanded AR
c             operator.  Used as input to ratpos.
c beglag  i  Index to the begining lag of an operator
c begopr  i  Index to the begining operator of the differencing, AR,
c             MA.
c i       i  Do loop index
c ia      i  Index to the current residual
c iopr    i  Index to the current lag operator
c ishft1  i  Index for the temporary forecast vector when calculating
c             the do index in terms of the data time index
c ishft2  i  Index for the exact residual vector when calculating the
c             do index in terms of the data time index
c j       i  Do loop index
c ndltar   i  Difference from the starting time point of the temporary
c             forecast vector to the starting time point of the data
c             vector
c ndltma   i  Difference from the starting time point of the exact
c             residuals to the starting time point of the data vector
c nlag    i  Number of lags in a lag operator
c nopr    i  Number of differenceing and AR or MA lag operators
c mxdfar  i  Length of the full AR operator (phi*differencing)
c one     d  PARAMETER for 1.0d0
c tfcst   d  Pfcst long work array to calculate the forecasts
c tmp     d  Temporary scalar to make intermediate calculations for
c             the regression adjustments and forecasts.
c work    d  Work vector used to compute forecasts and hold psi(B)
c             weights when computing the standard errors
c fularl    i  Work array for the lags in the full AR and
c             differencing operator (Nonseasonal x Seasonal for both).
c fularp    d  Work array porder long for the full AR and differencing
c             operators.
c fulmal    i  Work array for the lags in the full MA operator
c             associated with ma. (Nonseasonal x Seasonal)
c fulmap    d  Work array porder long for the full MA operator
c zero    d  PARAMETER for 0.0d0
c-----------------------------------------------------------------------
c     Variable typing and initialization
c-----------------------------------------------------------------------
      LOGICAL T,F
      DOUBLE PRECISION ZERO,ONE,MONE
      INTEGER PCXY,PAF,PARDOR
      PARAMETER(ZERO=0.0D0,ONE=1.0D0,T=.true.,F=.FALSE.,PCXY=PB+1,
     &          PAF=(2*PORDER+PLEN)*PCXY,PARDOR=PORDER+PDIFOR,
     &          MONE=-1.0D0)
c     ------------------------------------------------------------------
      INTEGER aropr,beglag,begopr,endopr,i,ia,ielt,ilag,info,iopr,
     &        ishft1,ishft2,j,mxdfar,ndltar,ndltma,neltf,neltxy,nlag,
     &        fularl,nfular,fulmal,nfulma,nb2
      DOUBLE PRECISION a,tfcst,tmp,fctssq,fularp,fulmap,piwght
      DIMENSION a(PAF),aropr(0:POPR),fularl(PARDOR),fularp(PARDOR),
     &          fulmal(PORDER),fulmap(PORDER),piwght(PARDOR+PFCST),
     &          tfcst((PFCST+PARDOR)*PCXY)
c-----------------------------------------------------------------------
c     Calculate the filtered Xy matrix
c-----------------------------------------------------------------------
      CALL copy(Xy,Fctori*Ncxy,1,a)
      CALL armafl(Fctori,Ncxy,F,F,a,nrsd,PAF,info)
c      CALL armafl(Fctori,Ncxy,T,F,a,nrsd,info)
c-----------------------------------------------------------------------
c     Calculate the full AR and Differencing operator, and its order
c-----------------------------------------------------------------------
      nfular=0
      begopr=Mdl(DIFF-1)
      endopr=Mdl(AR)-1
      DO iopr=begopr,endopr
       beglag=Opr(iopr-1)
       CALL eltlen(iopr,Opr,Nopr,nlag)
       IF(Lfatal)RETURN
       CALL polyml(Arimap(beglag),Arimal(beglag),nlag,fularp,fularl,
     &             nfular,PARDOR,fularp,fularl,nfular)
      END DO
c     ------------------------------------------------------------------
      mxdfar=Mxdflg+Mxarlg
c-----------------------------------------------------------------------
c     Calculate the full MA and its order.
c-----------------------------------------------------------------------
      nfulma=0
      begopr=Mdl(MA-1)
      endopr=Mdl(MA)-1
      DO iopr=begopr,endopr
       beglag=Opr(iopr-1)
       CALL eltlen(iopr,Opr,Nopr,nlag)
       IF(Lfatal)RETURN
       CALL polyml(Arimap(beglag),Arimal(beglag),nlag,fulmap,fulmal,
     &             nfulma,PORDER,fulmap,fulmal,nfulma)
      END DO
c-----------------------------------------------------------------------
c     Calculate the psi(B) weights for the standard errors
c-----------------------------------------------------------------------
      CALL setdp(ZERO,PARDOR+PFCST,piwght)
c     ------------------------------------------------------------------
      piwght(1)=ONE
      DO i=1,nfulma
       piwght(fulmal(i)+1)=-fulmap(i)
      END DO
c    ------------------------------------------------------------------
      aropr(0)=1
c I add the 1 because the pointers are to the first element of the
c of the next operator.
      aropr(1)=nfular+1
      CALL ratpos(Mxmalg+1,fularp,fularl,aropr,1,1,Nfcst,piwght)
c-----------------------------------------------------------------------
c     ndltar is the difference in indexing between the vector of
c observations and a temporary vector, tfcst, which contains only
c the values for time points needed to calculate the forecasts.
c     Ndltma is the difference in indexing between the vector of
c observations and the vector of residuals which maybe exact or
c conditional (corresponding to the different lengths of these vectors).
c     Multiply the series length and the lags by the number of columns
c so a matrix can be filtered the same as a vector
c-----------------------------------------------------------------------
      DO ilag=1,nfular
       fularl(ilag)=Ncxy*fularl(ilag)
      END DO
c     ------------------------------------------------------------------
      DO ilag=1,nfulma
       fulmal(ilag)=Ncxy*fulmal(ilag)
      END DO
c     ------------------------------------------------------------------
      ndltar=mxdfar*Ncxy
      ndltma=nrsd*Ncxy
      neltxy=Fctori*Ncxy
      neltf=Nfcst*Ncxy
c     ------------------------------------------------------------------
      CALL copy(Xy(neltxy-ndltar+1),ndltar+neltf,1,tfcst)
      DO ielt=ndltar+Ncxy,ndltar+neltf,Ncxy
       tfcst(ielt)=ZERO
      END DO
c-----------------------------------------------------------------------
c     Calculate the forecasts.
c-----------------------------------------------------------------------
      DO ielt=1,neltf
       ishft1=ielt+ndltar
       tmp=ZERO
c     ------------------------------------------------------------------
       DO j=1,nfular
        tmp=tmp+fularp(j)*tfcst(ishft1-fularl(j))
       END DO
c     ------------------------------------------------------------------
       ishft2=ielt+ndltma
       DO j=1,nfulma
        ia=ishft2-fulmal(j)
        IF(ia.le.ndltma.and.ia.gt.0)tmp=tmp-fulmap(j)*a(ia)
       END DO
c     ------------------------------------------------------------------
       tfcst(ielt)=tmp-tfcst(ishft1)
       IF(ielt.lt.ishft1)tfcst(ishft1)=tmp
      END DO
c-----------------------------------------------------------------------
c     Calculate Ay-(AX-X_f)*b
c-----------------------------------------------------------------------
      ielt=mxdfar*Ncxy+1
      CALL resid(tfcst,Nfcst,Ncxy,Ncxy,1,Nb,MONE,B,Fcst)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Compute the standard errors for the forecasts where
c se(lead)=sqrt(sum(psi(i)), i=0,lead-1).  Psi(B)=AR(B)/MA(B).
c First set up the expanded AR operator.
c-----------------------------------------------------------------------
c     Compute the standard errors using var where var=a'a/nwp, a(i),
c i=1,na.
c get the chlxpx and use to LX vector and then take the sum of squares`
c-----------------------------------------------------------------------
      fctssq=ZERO
      ielt=1
c     ------------------------------------------------------------------
c     Generate number of unfixed regressors
c     ------------------------------------------------------------------
      nb2=Nb
      IF(Iregfx.ge.2)THEN
       DO j=1,Nb
        IF(Regfx(j))nb2=nb2-1
       END DO
      END IF
c     ------------------------------------------------------------------
      DO i=1,Nfcst
       IF(nb2.gt.0)THEN
        CALL dppsl(Chlxpx,nb2,tfcst(ielt),T)
        CALL yprmy(tfcst(ielt),nb2,tmp)
c     ------------------------------------------------------------------
       ELSE
        tmp=ZERO
       END IF
c     ------------------------------------------------------------------
       Rgvar(i)=tmp*Var
       fctssq=fctssq+piwght(i)**2
       tmp=tmp+fctssq
       Se(i)=sqrt(tmp*Var)
       ielt=ielt+Ncxy
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
