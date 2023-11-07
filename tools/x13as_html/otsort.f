      SUBROUTINE otsort()
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Sort outliers just read into regARIMA data dictionary, 
c     while maintaining initial values read in from b argument. 
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      LOGICAL F
      PARAMETER(F=.false.)
c-----------------------------------------------------------------------
      CHARACTER atrttl*(PCOLCR*PB),str*(PCOLCR)
      DOUBLE PRECISION batr
      LOGICAL vfix
      INTEGER atrptr,j,jgrp,icol,itype,nreg,natrtl,vtype,nchr,iotlr
      DIMENSION atrptr(0:PB),batr(PB),vtype(PB),vfix(PB)
c-----------------------------------------------------------------------
c     check to see if there are outliers specified by the user.
c     if so, keep track of position of first outlier.  If not,
c     return.
c-----------------------------------------------------------------------
      j=0
	DO jgrp=1,Ngrp
       icol=Grp(jgrp-1)
       itype=Rgvrtp(icol)
       IF(itype.eq.PRGTAO.or.itype.eq.PRGTLS.or.itype.eq.PRGTRP.or.
     &    itype.eq.PRGTMV.or.itype.eq.PRGTTC.or.itype.eq.PRGTSO.or.
     &    itype.eq.PRGTTL.or.itype.eq.PRGTQI.or.itype.eq.PRGTQD.or.
     &    itype.eq.PRSQAO.or.itype.eq.PRSQLS)THEN
        j=icol
        GO TO 1
       END IF
      END DO
   1  IF(j.eq.0)RETURN
c-----------------------------------------------------------------------
c     delete all outliers but the first, keeping track of the variable
c     type, name, initial value, and fixed indicator
c-----------------------------------------------------------------------
      CALL intlst(PB,atrptr,natrtl)
      nreg=natrtl+1
      j=j+1
      DO icol=Nb,j,-1
       itype=Rgvrtp(icol)
       IF(itype.eq.PRGTAO.or.itype.eq.PRGTLS.or.itype.eq.PRGTRP.or.
     &    itype.eq.PRGTMV.or.itype.eq.PRGTTC.or.itype.eq.PRGTSO.or.
     &    itype.eq.PRGTTL.or.itype.eq.PRGTQI.or.itype.eq.PRGTQD.or.
     &    itype.eq.PRSQAO.or.itype.eq.PRSQLS)THEN
        CALL getstr(Colttl,Colptr,Ncoltl,icol,str,nchr)
        IF(.not.Lfatal)
     &     CALL insstr(str(1:nchr),nreg,PB,atrttl,atrptr,natrtl)
        IF(Lfatal)RETURN
        batr(natrtl)=B(icol)
        vtype(natrtl)=itype
        vfix(natrtl)=Regfx(icol)
        nreg=nreg+1
        iotlr=icol
        CALL dlrgef(iotlr,Nrxy,1)
        IF(Lfatal)RETURN
       END IF
      END DO
c-----------------------------------------------------------------------
c    if no outliers were deleted, return
c-----------------------------------------------------------------------
      IF(natrtl.eq.0)RETURN
c-----------------------------------------------------------------------
c    else, add the outliers one at a time, so that the program will
c    update the information appropriately.
c-----------------------------------------------------------------------
      DO icol=1,natrtl
       CALL getstr(atrttl,atrptr,natrtl,icol,str,nchr)
       IF(.not.Lfatal)CALL adrgef(batr(icol),str(1:nchr),str(1:nchr),
     &                            vtype(icol),vfix(icol),F)
       IF(Lfatal)RETURN
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
      
