      SUBROUTINE bakusr(Userx,Usrtyp,Usrptr,Ncusrx,Usrttl,Regfx,B,
     &                  Rgvrtp,Ngrp,Grpttl,Grp,Grpptr,Ngrptl,Rind,Is1st)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Making backup copy of user defined regressors for regARIMA, m.
c-----------------------------------------------------------------------
      LOGICAL T
      PARAMETER(T=.true.)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'urgbak.cmn'
c-----------------------------------------------------------------------
      LOGICAL Regfx,Is1st
      CHARACTER Grpttl*(PGRPCR*PGRP),Usrttl*(PCOLCR*PUREG)
      DOUBLE PRECISION B,Userx
      INTEGER Usrtyp,Ncusrx,Usrptr,Rgvrtp,Ngrp,Grpptr,Ngrptl,Grp,disp,i,
     &        iuser,igrp,begcol,endcol,Rind
      DIMENSION B(PB),Regfx(PB),Rgvrtp(PB),Userx(PUSERX),Usrtyp(PUREG),
     &          Grp(0:PGRP),Grpptr(0:PGRP),Usrptr(0:PUREG)
c-----------------------------------------------------------------------
      INTEGER strinx
      EXTERNAL strinx
c-----------------------------------------------------------------------
c     remove the user defined regressors from the regression matrix.
c-----------------------------------------------------------------------
      iuser=(PUREG*Rind)
      DO igrp=1,Ngrp
       begcol=Grp(igrp-1)
       endcol=Grp(igrp)-1
       IF((Rgvrtp(begcol).ge.PRGTUH.and.Rgvrtp(begcol).le.PRGUH5).or.
     &     Rgvrtp(begcol).eq.PRGTUS.or.Rgvrtp(begcol).eq.PRGUTD.or.
     &     Rgvrtp(begcol).eq.PRGTUD.or.Rgvrtp(begcol).eq.PRGULM.or.
     &     Rgvrtp(begcol).eq.PRGULQ.or.Rgvrtp(begcol).eq.PRGULY.or.
     &     Rgvrtp(begcol).eq.PRGUAO.or.Rgvrtp(begcol).eq.PRGULS.or.
     &     Rgvrtp(begcol).eq.PRGUSO.or.Rgvrtp(begcol).eq.PRGUCN.or.
     &     Rgvrtp(begcol).eq.PRGUCY)THEN
        DO i=begcol,endcol
         iuser=iuser+1
         Buser(iuser)=B(i)
         Fxuser(iuser)=Regfx(i)
        END DO
       END IF
      END DO
c-----------------------------------------------------------------------
c     Make backup copy of user defined regressors.
c-----------------------------------------------------------------------
      IF(.not.Is1st)RETURN
      disp=(PUSERX*Rind)+1
      CALL copy(Userx(disp),PUSERX,1,Userx2)
      disp=(PUREG*Rind)+1
      CALL cpyint(Usrtyp(disp),PUREG,1,Usrty2)
      disp=((PUREG+1)*Rind)+1
      CALL cpyint(Usrptr(0),PUREG+1,1,Usrpt2(disp))
      Ncusx2(Rind)=Ncusrx
      Usrtt2(Rind)=Usrttl
c-----------------------------------------------------------------------
      RETURN
      END
