C     Last change:  BCM  11 Jun 1998    4:04 pm
      SUBROUTINE chusrg(Upuser,Usfxtl,Nusfx,Nusftl,Usfptr)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     check user defined regressors
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'usrreg.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION ZERO,ONE
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.,ZERO=0D0,ONE=1D0)
c-----------------------------------------------------------------------
      CHARACTER str*(PCOLCR),Usfxtl*(PCOLCR*PUREG)
      LOGICAL Upuser,allfix
      INTEGER i,iuser,j,disp,uptr,nuser,nchr,Nusfx,Nusftl,Usfptr,k,rtype
      DOUBLE PRECISION fvec
      DIMENSION fvec(PLEN),Usfptr(0:PUREG)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      INTEGER strinx
      EXTERNAL dpeq,strinx 
c-----------------------------------------------------------------------
c     Initialize variables.
c-----------------------------------------------------------------------
      IF(Upuser)RETURN
      iuser=Ncusrx+1
c-----------------------------------------------------------------------
c     Compute difference between start of user-defined regressors and
c     beginning of model span
c-----------------------------------------------------------------------
      CALL dfdate(Begmdl,Bgusrx,Sp,disp)
c-----------------------------------------------------------------------
c     Find user-defined regression variables.
c-----------------------------------------------------------------------
      allfix=T
      DO i=Nb,1,-1
       rtype=Rgvrtp(i)
       IF((rtype.eq.PRGTUD.or.(rtype.ge.PRGTUH.and.rtype.le.PRGUH5).or.
     &     rtype.eq.PRGTUS).and.(.not.Regfx(i)))THEN
        iuser=iuser-1 
        nuser=Nspobs
c-----------------------------------------------------------------------
c     Find the first non-zero observation in the differenced user
c     defined regressor
c-----------------------------------------------------------------------
        CALL setdp(ZERO,PLEN,fvec)
        uptr=(Ncusrx*disp)+iuser
        CALL daxpy(nuser,ONE,Userx(uptr),Ncusrx,fvec,1)
        CALL arflt(nuser,Arimap,Arimal,Opr,Mdl(DIFF-1),Mdl(DIFF)-1,
     &             fvec,nuser)
        j=1
        DO WHILE(dpeq(fvec(j),ZERO).and.j.le.nuser)
         j=j+1
        END DO
c-----------------------------------------------------------------------
c     Use this value to check if the user-defined regressor should be
c     removed from the regression matrix
c-----------------------------------------------------------------------
        IF(j.gt.nuser)THEN
c-----------------------------------------------------------------------
c     If the user defined regressor is not defined for the period up
c     to the start of the sliding span, fix the regressor for this run.
c-----------------------------------------------------------------------
         Regfx(i)=T
         IF(.not.Upuser)Upuser=T
         CALL getstr(Colttl,Colptr,Ncoltl,i,str,nchr)
         IF(Lfatal)RETURN
         k=0
         IF(Nusftl.gt.0)k=strinx(F,Usfxtl,Usfptr,1,Nusftl,str(1:nchr))
         IF(k.eq.0)THEN
          CALL insstr(str(1:nchr),Nusfx,PUREG,Usfxtl,Usfptr,Nusftl)
          IF(Lfatal)RETURN
          Nusfx=Nusfx+1
         END IF
        END IF
       END IF
       IF(.not.Regfx(i).and.allfix)allfix=F
      END DO
c-----------------------------------------------------------------------
      IF(Upuser)THEN
       IF(allfix)THEN
        Iregfx=3
       ELSE
        Iregfx=2
       END IF
      END IF
      RETURN
      END
