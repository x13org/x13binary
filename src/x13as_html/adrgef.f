C     Last change:  BCM  21 Sep 1998    9:20 am
      SUBROUTINE adrgef(Initvl,Effttl,Igrptl,Vartyp,Varfix,Userin)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Add a column to the igrptl group and create the
c regression group if it doesn't exist.  Also, does the checking a
c bookkeeping needed to add the effect.
c-----------------------------------------------------------------------
c Name   Type Description
c-----------------------------------------------------------------------
c begcol  i  Local begining column of the regression group the new
c             columns are to be placed
c blkttl  c  Local pgrpcr characters of blanks
c effttl  c  Input title of the effect
c             headings for this regression group
c i       i  Local do loop index
c icol    i  Local index for the current column in colttl or effttl
c igrp    i  Local the current regression group
c igrptl  c  Input character title of this regression group.
c initvl  d  Input initial value.
c nchr    i  Input number of characters in the group title
c tmpttl  c  Local character to hold the group title padded with blanks
c vartyp  i  Input type of regression variable (see model.prm).
c-----------------------------------------------------------------------
c     Specify parameters
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
c     Type and dimension variables
c-----------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
c-----------------------------------------------------------------------
      CHARACTER Effttl*(*),Igrptl*(*),tmpttl*(PGRPCR),numstr*(5),
     &          ceff*(PCOLCR)
      LOGICAL addcat,locok,havreg,Varfix,ltmp,Userin
      INTEGER begotl,bgotl2,endotl,icol,igrp,nchr,otlidx,otlid2,Vartyp,
     &        jgrp,itype,dspzro,dsptyp,zeroz,zero2,rgmidx,rgmid2,begcol,
     &        endcol,jcol,itmp,newreg,ireg,ipos,neff
      DOUBLE PRECISION Initvl,dptmp
      DIMENSION dspzro(-1:1),dptmp(PB),itmp(PB),ltmp(PB)
c-----------------------------------------------------------------------
      INTEGER ctoi,strinx
      EXTERNAL ctoi,strinx
c-----------------------------------------------------------------------
      CHARACTER DAYDIC*18
      INTEGER dayptr,PDAY
      PARAMETER(PDAY=6)
      DIMENSION dayptr(0:PDAY)
      PARAMETER(DAYDIC='montuewedthufrisat')
c     ------------------------------------------------------------------
      CHARACTER MONDIC*33
      INTEGER monptr,PMON
      PARAMETER(PMON=11)
      DIMENSION monptr(0:PMON)
      PARAMETER(MONDIC='janfebmaraprmayjunjulaugsepoctnov')
c     ------------------------------------------------------------------
      DATA dspzro/PRATSE,PRGTSE,PRRTSE/
      DATA dayptr/1,4,7,10,13,16,19/
      DATA monptr/1,4,7,10,13,16,19,22,25,28,31,34/
c-----------------------------------------------------------------------
c     Find the group.  If the group doesn't exist, create it
c-----------------------------------------------------------------------
      igrp=strinx(F,Grpttl,Grpptr,1,Ngrp,Igrptl)
c-----------------------------------------------------------------------
      IF(igrp.eq.0)THEN
       addcat=T
       igrp=1
       IF(Ngrp.gt.0)THEN
c     ------------------------------------------------------------------
c     IF this is an outlier supplied by user, sort by date
c     ------------------------------------------------------------------
        IF((Vartyp.eq.PRGTAO.or.Vartyp.eq.PRGTLS.or.Vartyp.eq.PRGTRP.or.
     &      Vartyp.eq.PRGTMV.or.Vartyp.eq.PRGTTC.or.Vartyp.eq.PRGTTL.or.
     &      Vartyp.eq.PRGTSO.or.Vartyp.eq.PRGTQI.or.Vartyp.eq.PRGTQD.or.
     &      Vartyp.eq.PRSQAO.or.Vartyp.eq.PRSQLS)
     &      .and.(.not.Userin))THEN
         CALL rdotlr(Effttl,Begspn,Sp,otlidx,begotl,endotl,locok)
         IF(.not.locok)THEN
          CALL abend
          RETURN
         END IF
         havreg=.false.
         DO jgrp=1,Ngrp
          icol=Grp(jgrp-1)
          itype=Rgvrtp(icol)
          IF(itype.eq.PRGTAO.or.itype.eq.PRGTLS.or.itype.eq.PRGTRP.or.
     &       itype.eq.PRGTMV.or.itype.eq.PRGTTC.or.itype.eq.PRGTTL.or.
     &       itype.eq.PRGTSO.or.itype.eq.PRGTQI.or.itype.eq.PRGTQD.or.
     &       itype.eq.PRSQAO.or.itype.eq.PRSQLS)THEN
           CALL getstr(Colttl,Colptr,Nb,icol,tmpttl,nchr)
           IF(Lfatal)RETURN
           CALL rdotlr(tmpttl(1:nchr),Begspn,Sp,otlid2,bgotl2,endotl,
     &                 locok)
           IF(.not.locok)THEN
            CALL abend
            RETURN
           END IF
           havreg=.true.
c     ------------------------------------------------------------------
           IF(begotl.eq.bgotl2)THEN
            IF(otlidx.eq.otlid2)THEN
             neff=len(Effttl)
             ceff(1:neff)=Effttl(1:neff)
             CALL eWritln(ceff(1:neff)//' already exists.',
     &                    STDERR,Mt2,T,T)
             CALL abend
             RETURN
c     ------------------------------------------------------------------
            ELSE IF(otlidx.lt.otlid2)THEN
             GO TO 1
            END IF
c     ------------------------------------------------------------------
           ELSE IF(begotl.lt.bgotl2)THEN
            GO TO 1
           END IF
c     ------------------------------------------------------------------
c     If no more outliers, insert outlier group here
c     ------------------------------------------------------------------
          ELSE IF(havreg)THEN
           GO TO 1
          END IF
         END DO
   1     igrp=jgrp
c     ------------------------------------------------------------------
c     If this is an lom/loq/lpyear regressor, try to find a
c     corresponding set of trading day regressors - otherwise, add to
c     end of group
c     ------------------------------------------------------------------
        ELSE IF(Vartyp.eq.PRGTLM.or.Vartyp.eq.PRGTLQ.or.Vartyp.eq.PRGTLY
     &      .or.Vartyp.eq.PRRTLM.or.Vartyp.eq.PRRTLQ.or.Vartyp.eq.PRRTLY
     &      .or.Vartyp.eq.PRATLM.or.Vartyp.eq.PRATLQ.or.Vartyp.eq.PRATLY
     &      )THEN
         DO jgrp=1,Ngrp
          icol=Grp(jgrp-1)
          itype=Rgvrtp(icol)
          IF((itype.eq.PRGTTD.or.itype.eq.PRG1TD).and.(Vartyp.eq.PRGTLM
     &        .or.Vartyp.eq.PRGTLQ.or.Vartyp.eq.PRGTLY))THEN
           igrp=jgrp+1
          ELSE IF((itype.eq.PRRTTD.or.itype.eq.PRR1TD).and.
     &            (Vartyp.eq.PRRTLM.or.Vartyp.eq.PRRTLQ.or.
     &             Vartyp.eq.PRRTLY))THEN 
           igrp=jgrp+1
          ELSE IF((itype.eq.PRATTD.or.itype.eq.PRA1TD).and.
     &            (Vartyp.eq.PRATLM.or.Vartyp.eq.PRATLQ.or.
     &             Vartyp.eq.PRATLY))THEN 
           igrp=jgrp+1
          END IF
         END DO
         IF(igrp.eq.1)igrp=Ngrp+1
c     ------------------------------------------------------------------
c     IF this is not an outlier supplied by user, check to see if
c     it is a "change of regime" regressors (or a parent of a change
c     of regime regressor).  If so, keep those groups together and
c     sort by type of regressor, date of regime change.
c     ------------------------------------------------------------------
        ELSE IF(Vartyp.eq.PRGTSE.or.Vartyp.eq.PRGTTS.or.Vartyp.eq.PRGTTD
     &      .or.Vartyp.eq.PRGTST.or.Vartyp.eq.PRGTSL.or.Vartyp.eq.PRRTSE
     &      .or.Vartyp.eq.PRRTTS.or.Vartyp.eq.PRRTTD.or.Vartyp.eq.PRRTST
     &      .or.Vartyp.eq.PRRTSL.or.Vartyp.eq.PRATSE.or.Vartyp.eq.PRATTS
     &      .or.Vartyp.eq.PRATTD.or.Vartyp.eq.PRATST.or.Vartyp.eq.PRATSL
     &      .or.Vartyp.eq.PRG1TD.or.Vartyp.eq.PRR1TD.or.Vartyp.eq.PRA1TD
     &      .or.Vartyp.eq.PRG1ST.or.Vartyp.eq.PRR1ST.or.Vartyp.eq.PRA1ST
     &      )THEN
         CALL rdregm(Igrptl,Begspn,Sp,zeroz,rgmidx,locok)
         IF(.not.locok)THEN
          CALL abend
          RETURN
         END IF
         havreg=.false.
         dsptyp=Vartyp-dspzro(zeroz)
         DO jgrp=1,Ngrp
          icol=Grp(jgrp-1)
          itype=Rgvrtp(icol)
          CALL getstr(Grpttl,Grpptr,Ngrp,jgrp,tmpttl,nchr)
          IF(Lfatal)RETURN
          CALL rdregm(tmpttl(1:nchr),Begspn,Sp,zero2,rgmid2,locok)
          IF(.not.locok)THEN
           CALL abend
           RETURN
          END IF
          IF(itype.eq.dsptyp+dspzro(zero2))THEN
           havreg=.true.
c     ------------------------------------------------------------------
c     If they are the same type of regressor, print error message and
c     leave routine
c     ------------------------------------------------------------------
           IF(zeroz.eq.zero2)THEN
c            IF(zeroz.eq.0)RETURN
c            IF(rgmidx.lt.rgmid2)GO TO 2
            neff=len(Effttl)
            ceff(1:neff)=Effttl(1:neff)
            CALL eWritln(ceff(1:neff)//
     &                   ' is already in the regression.',
     &                   STDERR,Mt2,T,T)
            CALL abend
            RETURN
c     ------------------------------------------------------------------
c       Else, check to see if this regressor type should come before
c       the current regressor.
c     ------------------------------------------------------------------
           ELSE IF(zeroz.eq.0.OR.(zeroz.EQ.1.and.zero2.eq.-1))THEN
            GO TO 2
           END IF
          ELSE IF(havreg)THEN
           GO TO 2
          END IF
         END DO
   2     igrp=jgrp
c     ------------------------------------------------------------------
        ELSE
         igrp=Ngrp+1
        END IF
       END IF
       IF(igrp.eq.Ngrp+1)THEN
        CALL putstr(Igrptl,PGRP,Grpttl,Grpptr,Ngrptl)
        IF(Lfatal)RETURN
       ELSE
        CALL insstr(Igrptl,igrp,PGRP,Grpttl,Grpptr,Ngrptl)
        IF(Lfatal)RETURN
       END IF
c     ------------------------------------------------------------------
      ELSE
       addcat=F
      END IF
c     ------------------------------------------------------------------
      IF(Nb.ge.PB)THEN
       ipos=1
       CALL itoc(PB,numstr,ipos)
       neff=len(Effttl)
       ceff(1:neff)=Effttl(1:neff)
       CALL eWritln('Adding '//ceff(1:neff)//' exceeds the number '//
     &              'of regression effects allowed',STDERR,Mt2,T,F)
       CALL writln('        in the model ('//numstr(1:(ipos-1))//').',
     &             STDERR,Mt2,F,T)
       CALL writln('        Check the regression model, change the '//
     &             'automatic outlier options,',STDERR,Mt2,T,F)
       CALL writln('        (e.g. method to ADDONE, raise the '//
     &             'critical value, or change types',STDERR,Mt2,T,F)
       CALL writln('        to identify AOs only), or change the '//
     &             'program limits (see '//LIMSEC,STDERR,Mt2,T,F)
       CALL writln('        of the '//PRGNAM//' '//DOCNAM//').',
     &             STDERR,Mt2,T,F)
       CALL abend
       RETURN
      END IF
c     ------------------------------------------------------------------
      CALL insptr(addcat,1,igrp,PGRP,PB,Grp,Ngrp)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Find out if the column titles already exist
c-----------------------------------------------------------------------
      icol=strinx(F,Colttl,Colptr,1,Ncoltl,Effttl)
c     ------------------------------------------------------------------
      IF(icol.gt.0)THEN
       neff=len(Effttl)
       ceff(1:neff)=Effttl(1:neff)
       CALL eWritln(ceff(1:neff)//' is already in the regression.',
     &              STDERR,Mt2,T,T)
       CALL abend
       RETURN
      END IF
c-----------------------------------------------------------------------
c     Sort the automatically identified outliers by date
c-----------------------------------------------------------------------
      nchr=len(Igrptl)
*      IF(Vartyp.eq.PRGTAA.or.Vartyp.eq.PRGTAL.or.Vartyp.eq.PRGTAT.or.
*     &   Vartyp.eq.PRGTAS)THEN
      IF(Vartyp.eq.PRGTAA.or.Vartyp.eq.PRGTAL.or.Vartyp.eq.PRGTAT)THEN
       CALL rdotlr(Effttl,Begspn,Sp,otlidx,begotl,endotl,locok)
       IF(.not.locok)THEN
        CALL abend
        RETURN
       END IF
c     ------------------------------------------------------------------
c Note grp is updated for the new outlier so use -2 instead of -1
c     ------------------------------------------------------------------
       DO icol=Grp(igrp-1),Grp(igrp)-2
        CALL getstr(Colttl,Colptr,Nb,icol,tmpttl,nchr)
        IF(Lfatal)RETURN
        CALL rdotlr(tmpttl(1:nchr),Begspn,Sp,otlid2,bgotl2,endotl,locok)
        IF(.not.locok)THEN
         CALL abend
         RETURN
        END IF
c     ------------------------------------------------------------------
        IF(begotl.eq.bgotl2)THEN
         IF(otlidx.eq.otlid2)THEN
          neff=len(Effttl)
          ceff(1:neff)=Effttl(1:neff)
          CALL eWritln(ceff(1:neff)//' already exists.',
     &                 STDERR,Mt2,T,T)
          CALL abend
          RETURN
c     ------------------------------------------------------------------
         ELSE IF(otlidx.lt.otlid2)THEN
          GO TO 10
         END IF
c     ------------------------------------------------------------------
        ELSE IF(begotl.lt.bgotl2)THEN
         GO TO 10
        END IF
       END DO
       icol=Grp(igrp)-1
c     ------------------------------------------------------------------
c Need to handle trading day sometime
c     ------------------------------------------------------------------
      ELSE IF(Vartyp.eq.PRGTTD.or.Vartyp.eq.PRGTST.or.Vartyp.eq.PRRTTD
     &    .or.Vartyp.eq.PRRTST.or.Vartyp.eq.PRATTD.or.Vartyp.eq.PRATST)
     &        THEN
       newreg=strinx(F,DAYDIC,dayptr,1,PDAY,Effttl(1:3))
       DO icol=Grp(igrp-1),Grp(igrp)-2
        CALL getstr(Colttl,Colptr,Nb,icol,tmpttl,nchr)
        IF(Lfatal)RETURN
        ireg=strinx(F,DAYDIC,dayptr,1,PDAY,tmpttl(1:3))
        IF(newreg.eq.ireg)THEN
         neff=len(Effttl)
         ceff(1:neff)=Effttl(1:neff)
         CALL eWritln(ceff(1:neff)//' already exists.',
     &                STDERR,Mt2,T,T)
         CALL abend
         RETURN
        ELSE IF(newreg.lt.ireg)THEN
         GO TO 10
        END IF
       END DO
       icol=Grp(igrp)-1
c     ------------------------------------------------------------------
c Need to handle seasonal
c     ------------------------------------------------------------------
      ELSE IF(Vartyp.eq.PRGTSE.or.Vartyp.eq.PRRTSE.or.Vartyp.eq.PRATSE)
     &        THEN
       ipos=1
       IF(Sp.eq.12)THEN
        newreg=strinx(F,MONDIC,monptr,1,PMON,Effttl(1:3))
       ELSE
        newreg=ctoi(Effttl,ipos)
       END IF
       DO icol=Grp(igrp-1),Grp(igrp)-2
        ipos=1
        CALL getstr(Colttl,Colptr,Nb,icol,tmpttl,nchr)
        IF(Lfatal)RETURN
        IF(Sp.eq.12)THEN
         ireg=strinx(F,MONDIC,monptr,1,PMON,tmpttl(1:3))
        ELSE
         ireg=ctoi(tmpttl(1:nchr),ipos)
        END IF
        IF(newreg.eq.ireg)THEN
         neff=len(Effttl)
         ceff(1:neff)=Effttl(1:neff)
         CALL eWritln(ceff(1:neff)//' already exists.',
     &                STDERR,Mt2,T,T)
         CALL abend
         RETURN
        ELSE IF(newreg.lt.ireg)THEN
         GO TO 10
        END IF
       END DO
       icol=Grp(igrp)-1
c     ------------------------------------------------------------------
      ELSE
       icol=Grp(igrp)-1
      END IF
c     ------------------------------------------------------------------
   10 CALL insstr(Effttl,icol,PB,Colttl,Colptr,Ncoltl)
      IF(Lfatal)RETURN
      Nb=Ncoltl
      Ncxy=Nb+1
      IF(addcat)THEN
       begcol=Grp(igrp-1)
       endcol=Grp(igrp)-1
       DO jcol=begcol,endcol
        dptmp(jcol-begcol+1)=Initvl
        itmp(jcol-begcol+1)=Vartyp
        ltmp(jcol-begcol+1)=Varfix
       END DO
       CALL insdbl(dptmp,igrp,Grp,Ngrp,B)
       IF(.not.Lfatal)CALL insint(itmp,igrp,Grp,Ngrp,Rgvrtp)
       IF(.not.Lfatal)CALL inslg(ltmp,igrp,Grp,Ngrp,Regfx)
       IF(Lfatal)RETURN
      ELSE
       IF(Nb.gt.icol)THEN
        CALL copy(B(icol),Nb-icol,-1,B(icol+1))
        CALL cpyint(Rgvrtp(icol),Nb-icol,-1,Rgvrtp(icol+1))
        CALL copylg(Regfx(icol),Nb-icol,-1,Regfx(icol+1))
       END IF
       B(icol)=Initvl
       Rgvrtp(icol)=Vartyp
       Regfx(icol)=Varfix
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
