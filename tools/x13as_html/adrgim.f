C     Last change:  BCM  28 Sep 1998    8:55 am
      SUBROUTINE adrgim(Begsrs,Nobs,Havesp,Grptxt,Vartyp,Vrtyp2,Zeroz,
     &                  Delreg,Lregim,Fullef,Locok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Add regression variables for a change in regime
c-----------------------------------------------------------------------
      INCLUDE 'lex.i'
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
c-----------------------------------------------------------------------
      CHARACTER Grptxt*(*),rdtstr*(10),colstr*(PCOLCR),igrptl*(PGRPCR),
     &          rgmtxt*(PGRPCR)
      LOGICAL argok,Delreg,Havesp,Locok,Lregim,Fullef
      INTEGER Vartyp,Vrtyp2,strgim,igrp,begcol,endcol,icol,ncolcr,ngtxt,
     &        Begsrs,Nobs,nchdat,dfrgim,Zeroz,varori,varor1,varor2,nchr,
     &        nrgm
      DIMENSION Begsrs(2),strgim(2)
c-----------------------------------------------------------------------
      INTEGER strinx
      EXTERNAL strinx
c-----------------------------------------------------------------------
c     Get the date of the change of regime.
c-----------------------------------------------------------------------
      CALL gtrgdt(Havesp,Sp,strgim,Zeroz,argok,Locok)
      IF(.not.argok.or.Lfatal)THEN
       Locok=F
       RETURN
      END IF
      CALL wrtdat(strgim,Sp,rdtstr,nchdat)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Test date of change of regime to see if it is valid
c-----------------------------------------------------------------------
      CALL dfdate(strgim,Begsrs,Sp,dfrgim)
      IF(dfrgim.le.0.or.dfrgim.ge.Nobs)THEN
       CALL inpter(PERROR,Lstpos,
     &       'Date given for change of regime not within the series.',T)
       Locok=F
       RETURN
      END IF
c-----------------------------------------------------------------------
c     Add change of regime regression variables.  First, determine which
c     group the full effect is stored in.
c-----------------------------------------------------------------------
      igrp=strinx(F,Grpttl,Grpptr,1,Ngrp,Grptxt)
      begcol=Grp(igrp-1)
      endcol=Grp(igrp)-1
c-----------------------------------------------------------------------
      IF(Zeroz.eq.0.and.(Vartyp.eq.PRRTTD.or.Vartyp.eq.PRR1TD).and.
     &   Picktd)THEN
       CALL adrgef(DNOTST,'Leap Year','Leap Year (after '//
     &             rdtstr(1:nchdat)//')',PRGTLY,F,T)
      END IF
c-----------------------------------------------------------------------
c     For each effect in the group, add a change of regime variable that
c     will be its analog.
c-----------------------------------------------------------------------
      ngtxt=len(Grptxt)
      DO icol=endcol,begcol,-1
       CALL getstr(Colttl,Colptr,Ncoltl,icol,colstr,ncolcr)
       IF(.not.Lfatal)THEN
        IF(Zeroz.eq.0.or.(Zeroz.eq.1.and.Fullef))THEN
         nrgm=ngtxt+nchdat+21
         rgmtxt(1:nrgm)=Grptxt(1:ngtxt)//' (change for before '//
     &                  rdtstr(1:nchdat)//')'
         CALL adrgef(DNOTST,colstr(1:ncolcr)//' I',rgmtxt(1:nrgm),
     &               Vartyp,F,T)
        ELSE IF(Zeroz.gt.0)THEN
         nrgm=ngtxt+nchdat+10
         rgmtxt(1:nrgm)=Grptxt(1:ngtxt)//' (before '//
     &                  rdtstr(1:nchdat)//')'
         CALL adrgef(DNOTST,colstr(1:ncolcr)//' I',rgmtxt(1:nrgm),
     &               Vartyp,F,T)
         IF(Zeroz.eq.2)THEN
          nrgm=ngtxt+nchdat+12
          rgmtxt(1:nrgm)=Grptxt(1:ngtxt)//' (starting '//
     &                   rdtstr(1:nchdat)//')'
          CALL adrgef(DNOTST,colstr(1:ncolcr)//' II',rgmtxt(1:nrgm),
     &                Vrtyp2,F,T)
         END IF
        ELSE IF(Fullef)THEN
         nrgm=ngtxt+nchdat+20
         rgmtxt(1:nrgm)=Grptxt(1:ngtxt)//' (change for after '//
     &                  rdtstr(1:nchdat)//')'
         CALL adrgef(DNOTST,colstr(1:ncolcr)//' II',rgmtxt(1:nrgm),
     &               Vrtyp2,F,T)
        ELSE
         nrgm=ngtxt+nchdat+12
         rgmtxt(1:nrgm)=Grptxt(1:ngtxt)//' (starting '//
     &                  rdtstr(1:nchdat)//')'
         CALL adrgef(DNOTST,colstr(1:ncolcr)//' II',rgmtxt(1:nrgm),
     &               Vrtyp2,F,T)
        END IF
       END IF
       IF(Lfatal)RETURN
      END DO
c-----------------------------------------------------------------------
c     Delete regular regressors if regime regressors are to be fit by 
c     themselves
c-----------------------------------------------------------------------
      IF(Delreg.AND.(Zeroz.ne.0))THEN
       CALL dlrgef(begcol,Nobs,endcol-begcol+1)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
      IF(Zeroz.eq.0.or.Fullef)THEN
       varor1=0
       varor2=0
       IF(Zeroz.eq.0)THEN
        varori=Rgvrtp(begcol)
        varor1=varori+1
        varor2=varori+3
       ELSE IF(Vartyp.eq.PRR1TD)THEN
        varori=PRG1TD
        varor1=varori+1
        varor2=varori+3
       ELSE IF(Vartyp.eq.PRG1ST)THEN
        varori=PRG1ST
       ELSE 
        varori=Vartyp-17
        varor1=varori+1
        varor2=varori+3
       END IF
       igrp=Ngrp
       DO WHILE (igrp.gt.0)
        icol=Grp(igrp-1)
        IF(Rgvrtp(icol).eq.varori)THEN
         CALL getstr(Grpttl,Grpptr,Ngrp,igrp,igrptl,nchr)
         IF(.not.Lfatal)CALL delstr(igrp,Grpttl,Grpptr,Ngrp,PGRP)
         IF(Lfatal)RETURN
         IF(Zeroz.ge.0)THEN
          CALL insstr(igrptl(1:nchr)//' (after '//rdtstr(1:nchdat)//')',
     &                igrp,PGRP,Grpttl,Grpptr,Ngrp)
         ELSE
          CALL insstr(igrptl(1:nchr)//' (before '//rdtstr(1:nchdat)//
     &                ')',igrp,PGRP,Grpttl,Grpptr,Ngrp)
         END IF
         IF(Lfatal)RETURN
c-----------------------------------------------------------------------
         IF(igrp.lt.Ngrp)THEN
          icol=Grp(igrp)
          IF(Rgvrtp(icol).ge.varor1.and.Rgvrtp(icol).le.varor2)THEN
           CALL getstr(Grpttl,Grpptr,Ngrp,igrp+1,igrptl,nchr)
           IF(Lfatal)RETURN
           CALL delstr(igrp+1,Grpttl,Grpptr,Ngrp,PGRP)
           IF(Zeroz.ge.0)THEN
            CALL insstr(
     &              igrptl(1:nchr)//' (after '//rdtstr(1:nchdat)//')',
     &              igrp+1,PGRP,Grpttl,Grpptr,Ngrp)
           ELSE
            CALL insstr(
     &              igrptl(1:nchr)//' (before '//rdtstr(1:nchdat)//')',
     &              igrp+1,PGRP,Grpttl,Grpptr,Ngrp)
           END IF
          END IF
         END IF
c-----------------------------------------------------------------------
         igrp=0
        ELSE
         igrp=igrp-1
        END IF
       END DO
c-----------------------------------------------------------------------
c  If trading day group name changed, change the length of month/
c  leap year group name
c-----------------------------------------------------------------------
      END IF
c-----------------------------------------------------------------------
c     If td option picked, also add lom or loq variable.
c-----------------------------------------------------------------------
      IF((Vartyp.eq.PRRTTD.or.Vartyp.eq.PRR1TD).and.Picktd)THEN
       IF(Zeroz.eq.0)THEN
        CALL adrgef(DNOTST,'Leap Year I',
     &          'Leap Year (change for before '//rdtstr(1:nchdat)//')',
     &              PRRTLY,F,T)
       ELSE IF(Zeroz.gt.0)THEN
        CALL adrgef(DNOTST,'Leap Year I',
     &           'Leap Year (before '//rdtstr(1:nchdat)//')',PRRTLY,F,T)
        IF(Zeroz.eq.2)
     &     CALL adrgef(DNOTST,'Leap Year II','Leap Year (starting '//
     &                 rdtstr(1:nchdat)//')',PRATLY,F,T)
       ELSE IF(Fullef)THEN
        CALL adrgef(DNOTST,'Leap Year II','Leap Year (change for after '
     &              //rdtstr(1:nchdat)//')',PRATLY,F,T)
       ELSE
        CALL adrgef(DNOTST,'Leap Year II',
     &         'Leap Year (starting '//rdtstr(1:nchdat)//')',PRATLY,F,T)
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(.not.Lregim)Lregim=.true.
      IF(Vartyp.eq.PRRTTD.or.Vartyp.eq.PRRTST.or.Vartyp.eq.PRR1TD.or.
     &   Vartyp.eq.PRR1ST)THEN
       CALL cpyint(strgim,2,1,Tddate)
       IF((Vartyp.eq.PRRTTD.or.Vartyp.eq.PRR1TD).and.Picktd)
     &     CALL cpyint(strgim,2,1,Lndate)
      END IF
      IF(Vartyp.eq.PRRTLM.or.Vartyp.eq.PRRTLQ.or.Vartyp.eq.PRRTLY)
     &   CALL cpyint(strgim,2,1,Lndate)
c-----------------------------------------------------------------------
      RETURN
      END
