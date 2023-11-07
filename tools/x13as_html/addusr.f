C     Last change:  BCM   7 May 1998    2:14 pm
      SUBROUTINE addusr(Rind,Fxindx)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'usrreg.cmn'
      INCLUDE 'urgbak.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'units.cmn'
c-----------------------------------------------------------------------
      LOGICAL F
      PARAMETER(F=.FALSE.)
c-----------------------------------------------------------------------
      CHARACTER effttl*(PCOLCR),thisu*(PCOLCR)
      INTEGER begcol,disp,ncol,igrp,i,nchr,Rind,icol,nusr,ucol,Fxindx,
     &        rtype
c-----------------------------------------------------------------------
      INTEGER strinx
      EXTERNAL strinx
c-----------------------------------------------------------------------
c     If there are user defined regressors left in the model, delete
c     them before adding all the user-defined regressors back in
c-----------------------------------------------------------------------
      IF(Ncusrx.gt.0)THEN
       igrp=Ngrp
       DO WHILE (igrp.ge.1)
        begcol=Grp(igrp-1)
        ncol=Grp(igrp)-begcol
        rtype=Rgvrtp(begcol)
        IF((rtype.ge.PRGTUH.and.rtype.le.PRGUH5).or.rtype.eq.PRGTUS.or.
     &      rtype.eq.PRGUTD.or.rtype.eq.PRGTUD.or.rtype.eq.PRGULM.or.
     &      rtype.eq.PRGULQ.or.rtype.eq.PRGULY.or.rtype.eq.PRGUAO.or.
     &      rtype.eq.PRGULS.or.rtype.eq.PRGUSO.or.rtype.eq.PRGUCN.or.
     &      rtype.eq.PRGUCY)THEN
         DO icol=begcol,begcol+ncol-1
          CALL getstr(Colttl,Colptr,Nb,icol,thisu,nusr)
          IF(Lfatal)RETURN
          ucol=strinx(F,Usrtt2(Rind),Usrpt2,1,Ncusx2(Rind),
     &                thisu(1:nusr))
          Buser(ucol)=B(icol)
         END DO
         CALL dlrgef(begcol,Nrxy,ncol)
         IF(Lfatal)RETURN
        END IF
        igrp=igrp-1
       END DO
      END IF
c-----------------------------------------------------------------------
c     Restore values of the user defined regression variables
c-----------------------------------------------------------------------
      disp=PUSERX*Rind+1
      CALL copy(Userx2(disp),PUSERX,1,Userx)
      disp=(PUREG+1)*Rind+1
      CALL cpyint(Usrpt2(disp),PUREG+1,1,Usrptr(0))
      disp=PUREG*Rind+1
      CALL cpyint(Usrty2(disp),PUREG,1,Usrtyp)
      Ncusrx=Ncusx2(Rind)
      Usrttl=Usrtt2(Rind)
c-----------------------------------------------------------------------
c     Restore user-defined regressors to the regression matrix
c-----------------------------------------------------------------------
      disp=PUREG*Rind
      DO i=1,Ncusrx
c       IF(.not.(Fxuser(disp+i).and.Fxindx.eq.2))THEN
        CALL getstr(Usrttl,Usrptr,Ncusrx,i,effttl,nchr)
        IF(Lfatal)RETURN
        IF(Usrtyp(i).eq.PRGTUS)THEN
         CALL adrgef(Buser(disp+i),effttl(1:nchr),
     &               'User-defined Seasonal',Usrtyp(i),Fxuser(disp+i),F)
        ELSE IF(Usrtyp(i).eq.PRGTUH)THEN
         CALL adrgef(Buser(disp+i),effttl(1:nchr),
     &               'User-defined Holiday',Usrtyp(i),Fxuser(disp+i),F)
        ELSE IF(Usrtyp(i).eq.PRGUH2)THEN
         CALL adrgef(Buser(disp+i),effttl(1:nchr),
     &               'User-defined Holiday Group 2',Usrtyp(i),
     &               Fxuser(disp+i),F)
        ELSE IF(Usrtyp(i).eq.PRGUH3)THEN
         CALL adrgef(Buser(disp+i),effttl(1:nchr),
     &               'User-defined Holiday Group 3',Usrtyp(i),
     &               Fxuser(disp+i),F)
        ELSE IF(Usrtyp(i).eq.PRGUH4)THEN
         CALL adrgef(Buser(disp+i),effttl(1:nchr),
     &               'User-defined Holiday Group 4',Usrtyp(i),
     &               Fxuser(disp+i),F)
        ELSE IF(Usrtyp(i).eq.PRGUH5)THEN
         CALL adrgef(Buser(disp+i),effttl(1:nchr),
     &               'User-defined Holiday Group 5',Usrtyp(i),
     &               Fxuser(disp+i),F)
        ELSE IF(Usrtyp(i).eq.PRGUTD)THEN
         CALL adrgef(Buser(disp+i),effttl(1:nchr),
     &               'User-defined Trading Day',Usrtyp(i),
     &               Fxuser(disp+i),F)
        ELSE IF(Usrtyp(i).eq.PRGULY)THEN
         CALL adrgef(Buser(disp+i),effttl(1:nchr),
     &               'User-defined Leap Year',Usrtyp(i),
     &               Fxuser(disp+i),F)
        ELSE IF(Usrtyp(i).eq.PRGULM)THEN
         CALL adrgef(Buser(disp+i),effttl(1:nchr),
     &               'User-defined LOM',Usrtyp(i),Fxuser(disp+i),F)
        ELSE IF(Usrtyp(i).eq.PRGULQ)THEN
         CALL adrgef(Buser(disp+i),effttl(1:nchr),'User-defined LOQ',
     &               Usrtyp(i),Fxuser(disp+i),F)
        ELSE IF(Usrtyp(i).eq.PRGUAO)THEN
         CALL adrgef(Buser(disp+i),effttl(1:nchr),'User-defined AO',
     &               Usrtyp(i),Fxuser(disp+i),F)
        ELSE IF(Usrtyp(i).eq.PRGULS)THEN
         CALL adrgef(Buser(disp+i),effttl(1:nchr),'User-defined LS',
     &               Usrtyp(i),Fxuser(disp+i),F)
        ELSE IF(Usrtyp(i).eq.PRGUSO)THEN
         CALL adrgef(Buser(disp+i),effttl(1:nchr),'User-defined SO',
     &               Usrtyp(i),Fxuser(disp+i),F)
        ELSE IF(Usrtyp(i).eq.PRGUCN)THEN
         CALL adrgef(Buser(disp+i),effttl(1:nchr),
     &               'User-defined Constant',Usrtyp(i),
     &               Fxuser(disp+i),F)
        ELSE IF(Usrtyp(i).eq.PRGUCY)THEN
         CALL adrgef(Buser(disp+i),effttl(1:nchr),
     &               'User-defined Cycle',Usrtyp(i),
     &               Fxuser(disp+i),F)
        ELSE
         CALL adrgef(Buser(disp+i),effttl(1:nchr),'User-defined',
     &               PRGTUD,Fxuser(disp+i),F)
        END IF
c       END IF
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
