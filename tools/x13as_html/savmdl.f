C     Last change:  BCM   3 Sep 2003    2:21 pm
      SUBROUTINE savmdl(Begxy,Nrxy,Elong)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Prints out input file with regression, ARIMA specs
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'usrreg.cmn'
      INCLUDE 'mdltbl.i'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'savcmn.cmn'
      INCLUDE 'error.cmn'
c      INCLUDE 'units.cmn'
c     ------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      CHARACTER icoltl*(PCOLCR),igrptl*(PGRPCR),outstr*(PGRPCR)
      CHARACTER fmtusr*(20)
      LOGICAL locok,nxtreg,prvreg,Elong
      INTEGER begcol,beglag,begopr,begusr,Begxy,chrlen,endchr,endcol,
     &        endlag,endopr,fh,ibeg,icol,iend,ielt,iflt,igrp,ilag,iopr,
     &        ipos,nchr,nigrpc,noutcr,Nrxy,nusr,i,idtbeg,idtend,nigrp2,
     &        ncol
      DIMENSION Begxy(2)
c     ------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c     ------------------------------------------------------------------
      CHARACTER URGDIC*184
      INTEGER urgptr,PURG,urgidx
      PARAMETER(PURG=65)
      DIMENSION urgptr(0:PURG)
      PARAMETER(URGDIC='constanttdlomloqlpyeartdstocklomstockeasterlabor
     &thanksaolsrpusereasterstocksceasterseasonaltcsoholidayholiday2holi
     &day3holiday4holiday5transitorysocycletdlomloqleapyraolssoconstantc
     &ycle')
c     ------------------------------------------------------------------
      DATA urgptr/1,9,9,9,11,14,17,23,30,38,44,49,55,57,59,61,61,61,65,
     &            65,65,65,65,65,65,65,65,76,76,84,84,84,84,84,84,84,84,
     &            84,92,94,94,94,94,94,96,96,96,96,96,103,111,119,127,
     &            135,145,147,152,154,157,160,166,168,170,172,180,185/
c-----------------------------------------------------------------------
c     Open file with the an extension which depends on the type of the
c series.
c-----------------------------------------------------------------------
      begusr=0
      CALL opnfil(T,F,LESTMD,fh,locok)
      IF(.not.locok)THEN
       CALL abend
       RETURN
      END IF
c-----------------------------------------------------------------------
c     Print regression spec
c-----------------------------------------------------------------------
      IF(Nb.gt.0)THEN
       WRITE(fh,1010)
 1010  FORMAT(' regression{',/,'  variables=(')
c-----------------------------------------------------------------------
c     Print the regression variables
c-----------------------------------------------------------------------
       DO igrp=1,Ngrp
        CALL getstr(Grpttl,Grpptr,Ngrp,igrp,igrptl,nchr)
        IF(Lfatal)RETURN
        nigrpc=index(igrptl(1:nchr),'[')-1
        IF(nigrpc.eq.-1)nigrpc=nchr
        IF(igrptl(1:2).eq.'AO'.or.igrptl(1:2).eq.'LS'.or.igrptl(1:2)
     &     .eq.'Rp'.or.igrptl(1:2).eq.'TC'.or.igrptl(1:2).eq.'SO'.or.
     &     igrptl(1:2).eq.'TL')nigrpc=2
c-----------------------------------------------------------------------
c     Determine the beginning and ending columns in the group
c-----------------------------------------------------------------------
        begcol=Grp(igrp-1)
        endcol=Grp(igrp)-1
c-----------------------------------------------------------------------
c     check to see if next group is a set of change-of-regime regression
c     variables
c-----------------------------------------------------------------------
        nxtreg=F
        IF(igrp.lt.Ngrp)THEN
         IF(((Rgvrtp(endcol+1).gt.PRGTUD.and.Rgvrtp(endcol+1).lt.PRGTMV)
     &      .AND.((Rgvrtp(endcol+1)-Rgvrtp(endcol)).eq.17)).or.
     &      (Rgvrtp(endcol+1).eq.PRG1ST.AND.
     &      ((Rgvrtp(endcol+1)-Rgvrtp(endcol)).eq.1)))nxtreg=T
        END IF
        prvreg=F
        IF(igrp.gt.1)THEN
         IF(((Rgvrtp(begcol).gt.PRGTUD.and.Rgvrtp(begcol).lt.PRGTMV)
     &     .AND.((Rgvrtp(begcol)-Rgvrtp(begcol-1)).eq.17)).or.
     &      (Rgvrtp(begcol).eq.PRG1ST.AND.
     &      ((Rgvrtp(begcol)-Rgvrtp(begcol-1)).eq.1)))prvreg=T
        END IF
c-----------------------------------------------------------------------
c     Determine the type of regression variable
c-----------------------------------------------------------------------
        GO TO(10,20,30,40,50,60,70,80,90,100,
     &        110,120,130,140,150,160,160,170,20,30,
     &        40,50,60,70,80,90,105,131,190,20,
     &        30,40,50,60,70,80,90,170,210,160,
     &        40,40,40,155,160,80,80,80,170,170,
     &        170,170,170,145,155,170,170,170,170,170,
     &        170,170,170,170,170),Rgvrtp(begcol)
c-----------------------------------------------------------------------
c     Constant is a column of ones filtered by 1/Diff(B).
c-----------------------------------------------------------------------
   10   noutcr=5
        outstr='const'
        GO TO 220
c-----------------------------------------------------------------------
c     Seasonal effects
c-----------------------------------------------------------------------
   20   IF(prvreg)GO TO 180
        noutcr=8
        outstr='seasonal'
        IF(Rgvrtp(begcol).eq.PRRTSE)GO TO 180
        IF(Rgvrtp(begcol).eq.PRATSE)GO TO 200
        GO TO 220
c-----------------------------------------------------------------------
c     Trigonometric Seasonal effects
c-----------------------------------------------------------------------
   30   IF(prvreg)GO TO 180
        outstr='sincos['
        ipos=8
        DO icol=endcol,begcol,-2
         CALL getstr(Colttl,Colptr,Ncoltl,icol,icoltl,ncol)
         IF(Lfatal)RETURN
         endchr=index(icoltl(1:ncol),'t/')-1
         chrlen=endchr-8
         outstr(ipos:(ipos+chrlen))=icoltl(9:endchr)//','
         ipos=ipos+chrlen+1
        END DO
c     ------------------------------------------------------------------
        noutcr=ipos-1
        outstr(noutcr:noutcr)=']'
        IF(Rgvrtp(begcol).eq.PRRTTS)GO TO 180
        IF(Rgvrtp(begcol).eq.PRATTS)GO TO 200
        GO TO 220
c-----------------------------------------------------------------------
c     Trading Day effects
c-----------------------------------------------------------------------
   40   IF(prvreg)GO TO 180
        IF(Picktd)THEN
         IF(begcol.eq.endcol)THEN
          noutcr=7
          outstr='td1coef'
         ELSE
          noutcr=2
          outstr='td'
         END IF
        ELSE
         IF(begcol.eq.endcol)THEN
          noutcr=11
          outstr='td1nolpyear'
         ELSE
          noutcr=10
          outstr='tdnolpyear'
         END IF
        END IF
        IF(Rgvrtp(begcol).eq.PRRTTD.or.Rgvrtp(begcol).eq.PRR1TD)
     &     GO TO 180
        IF(Rgvrtp(begcol).eq.PRATTD.or.Rgvrtp(begcol).eq.PRA1TD)
     &     GO TO 200
        GO TO 220
c-----------------------------------------------------------------------
c     Length-of-Month and Length-of-Quarter effects.  Only include
c if the trading day was not specified by td.
c-----------------------------------------------------------------------
   50   IF(prvreg)GO TO 180
        IF(Picktd)THEN
         noutcr=0
c     ------------------------------------------------------------------
        ELSE
         noutcr=3
         outstr='lom'
         IF(Rgvrtp(begcol).eq.PRRTLM)GO TO 180
         IF(Rgvrtp(begcol).eq.PRATLM)GO TO 200
        END IF
        GO TO 220
c-----------------------------------------------------------------------
c     Length-of-Quarter effects.  Only include
c if the trading day was not specified by td.
c-----------------------------------------------------------------------
   60   IF(prvreg)GO TO 180
        IF(Picktd)THEN
         noutcr=0
c     ------------------------------------------------------------------
        ELSE
         noutcr=3
         outstr='loq'
         IF(Rgvrtp(begcol).eq.PRRTLQ)GO TO 180
         IF(Rgvrtp(begcol).eq.PRATLQ)GO TO 200
        END IF
        GO TO 220
c-----------------------------------------------------------------------
c     Leap Year effect
c-----------------------------------------------------------------------
   70   IF(prvreg)GO TO 180
        IF(Picktd)THEN
         noutcr=0
c     ------------------------------------------------------------------
        ELSE
         noutcr=6
         outstr='lpyear'
         IF(Rgvrtp(begcol).eq.PRRTLY)GO TO 180
         IF(Rgvrtp(begcol).eq.PRATLY)GO TO 200
        END IF
        GO TO 220
c-----------------------------------------------------------------------
c     Stock Trading Day effects
c-----------------------------------------------------------------------
   80   IF(prvreg)GO TO 180
        nigrp2=index(igrptl(1:nchr),']')
        IF(begcol.eq.endcol)THEN
         noutcr=12+nigrp2-nigrpc
         outstr='tdstock1coef'//igrptl((nigrpc+1):nigrp2)
        ELSE
         noutcr=7+nigrp2-nigrpc
         outstr='tdstock'//igrptl((nigrpc+1):nigrp2)
        END IF
        IF(Rgvrtp(begcol).eq.PRRTST.or.Rgvrtp(begcol).eq.PRR1ST)
     &     GO TO 180
        IF(Rgvrtp(begcol).eq.PRATST.or.Rgvrtp(begcol).eq.PRA1ST)
     &     GO TO 200
        GO TO 220
c-----------------------------------------------------------------------
c     Stock Length-of-Month effect
c-----------------------------------------------------------------------
   90   IF(prvreg)GO TO 180
        noutcr=8
        outstr='lomstock'
        IF(Rgvrtp(begcol).eq.PRRTSL)GO TO 180
        IF(Rgvrtp(begcol).eq.PRATSL)GO TO 200
        GO TO 220
c-----------------------------------------------------------------------
c     Easter holiday effect
c-----------------------------------------------------------------------
  100   DO icol=begcol,endcol
         CALL getstr(Colttl,Colptr,Nb,icol,igrptl,nchr)
         IF(Lfatal)RETURN
         nigrpc=index(igrptl(1:nchr),'[')-1
         noutcr=6+nchr-nigrpc
         outstr='easter'//igrptl((nigrpc+1):nchr)
         IF(icol.lt.endcol)WRITE(fh,1020)outstr(1:noutcr)
        END DO
        GO TO 220
c-----------------------------------------------------------------------
c     Stock Easter holiday effect
c-----------------------------------------------------------------------
  105   DO icol=begcol,endcol
         CALL getstr(Colttl,Colptr,Nb,icol,igrptl,nchr)
         IF(Lfatal)RETURN
         nigrpc=index(igrptl(1:nchr),'[')-1
         noutcr=11+nchr-nigrpc
         outstr='easterstock'//igrptl((nigrpc+1):nchr)
         IF(icol.lt.endcol)WRITE(fh,1020)outstr(1:noutcr)
        END DO
        GO TO 220
c-----------------------------------------------------------------------
c     Labor day holiday effect
c-----------------------------------------------------------------------
  110   noutcr=5+nchr-nigrpc
        outstr='labor'//igrptl((nigrpc+1):nchr)
        GO TO 220
c-----------------------------------------------------------------------
c     Thanksgiving-Christmas holiday effect
c-----------------------------------------------------------------------
  120   noutcr=5+nchr-nigrpc
        outstr='thank'//igrptl((nigrpc+1):nchr)
        GO TO 220
c-----------------------------------------------------------------------
c     AOs
c-----------------------------------------------------------------------
  130   noutcr=2+nchr-nigrpc
        outstr='ao'//igrptl((nigrpc+1):nchr)
        GO TO 220
c-----------------------------------------------------------------------
c     MVs - skip over
c-----------------------------------------------------------------------
  131   noutcr=0
        GO TO 220
c-----------------------------------------------------------------------
c     LSs
c-----------------------------------------------------------------------
  140   noutcr=2+nchr-nigrpc
        outstr='ls'//igrptl((nigrpc+1):nchr)
        GO TO 220
c-----------------------------------------------------------------------
c     TLSs
c-----------------------------------------------------------------------
  145   noutcr=2+nchr-nigrpc
        outstr='tl'//igrptl((nigrpc+1):nchr)
        GO TO 220
c-----------------------------------------------------------------------
c     Ramps
c-----------------------------------------------------------------------
  150   noutcr=2+nchr-nigrpc
        outstr='rp'//igrptl((nigrpc+1):nchr)
        GO TO 220
c-----------------------------------------------------------------------
c     SOs
c-----------------------------------------------------------------------
  155   noutcr=2+nchr-nigrpc
        outstr='so'//igrptl((nigrpc+1):nchr)
        GO TO 220
c-----------------------------------------------------------------------
c     Automatically Identified Outliers
c-----------------------------------------------------------------------
  160   DO icol=begcol,endcol
         CALL getstr(Colttl,Colptr,Ncoltl,icol,icoltl,nchr)
         IF(Lfatal)RETURN
c     ------------------------------------------------------------------
         IF(icoltl(1:2).eq.'AO')THEN
          icoltl(1:2)='ao'
         ELSE IF(icoltl(1:2).eq.'TC')THEN
          icoltl(1:2)='tc'
         ELSE IF(icoltl(1:2).eq.'LS')THEN
          icoltl(1:2)='ls'
*         ELSE IF(icoltl(1:2).eq.'SO')THEN
*          icoltl(1:2)='so'
         END IF
c     ------------------------------------------------------------------
         WRITE(fh,1020)icoltl(1:nchr)
        END DO
 1020   FORMAT('   ',a)
 1080   FORMAT('    ',e24.10,a)
c     ------------------------------------------------------------------
        noutcr=0
        GO TO 220
c-----------------------------------------------------------------------
c     User-defined regression variables.  First check the dates or the
c variables.
c-----------------------------------------------------------------------
  170   IF(begusr.eq.0)begusr=igrp
        noutcr=0
        GO TO 220
c-----------------------------------------------------------------------
c     Change of regime regression variables.  First, get the date of
c     the change-of-regime from the group title.
c-----------------------------------------------------------------------
  180   idtbeg=index(igrptl(1:nchr),'(before ')+8
        IF(idtbeg.eq.8)
     &     idtbeg=index(igrptl(1:nchr),'(change for before ')+19
        idtend=index(igrptl(idtbeg:nchr),')')+idtbeg-2
        IF(prvreg)THEN
         outstr(noutcr+1:)='/'//igrptl(idtbeg:idtend)//'/'
         noutcr=noutcr+idtend-idtbeg+3
        ELSE
         outstr(noutcr+1:)='/'//igrptl(idtbeg:idtend)//'//'
         noutcr=noutcr+idtend-idtbeg+4
        END IF
        GO TO 220
c-----------------------------------------------------------------------
c     statistics canada Easter holiday effect
c-----------------------------------------------------------------------
  190   DO icol=begcol,endcol
         CALL getstr(Colttl,Colptr,Nb,icol,igrptl,nchr)
         IF(Lfatal)RETURN
         nigrpc=index(igrptl(1:nchr),'[')-1
         noutcr=8+nchr-nigrpc
         outstr='sceaster'//igrptl((nigrpc+1):nchr)
         IF(icol.lt.endcol)WRITE(fh,1020)outstr(1:noutcr)
        END DO
        GO TO 220
c-----------------------------------------------------------------------
c     Change of regime regression variables.  First, get the date of
c     the change-of-regime from the group title.
c-----------------------------------------------------------------------
  200   idtbeg=index(igrptl(1:nchr),'(starting ')+10
        IF(idtbeg.eq.10)
     &     idtbeg=index(igrptl(1:nchr),'(change for after ')+18
        idtend=index(igrptl(idtbeg:nchr),')')+idtbeg-2
        outstr(noutcr+1:)='//'//igrptl(idtbeg:idtend)//'/'
        noutcr=noutcr+idtend-idtbeg+4
        GO TO 220
c-----------------------------------------------------------------------
c     TCs
c-----------------------------------------------------------------------
  210   noutcr=2+nchr-nigrpc
        outstr='tc'//igrptl((nigrpc+1):nchr)
c-----------------------------------------------------------------------
c     Write out the regression term
c-----------------------------------------------------------------------
  220   IF(.not.nxtreg.and.noutcr.gt.0)WRITE(fh,1020)outstr(1:noutcr)
       END DO
c-----------------------------------------------------------------------
c     Write out the closing parentheses for the variables argument.
c-----------------------------------------------------------------------
       WRITE(fh,1020)'  )'
c-----------------------------------------------------------------------
c     User-defined regression variables.  Add the effect names, start
c date and the data.
c-----------------------------------------------------------------------
       IF(begusr.gt.0)THEN
        WRITE(fh,1030)
 1030   FORMAT('  user=(')
        DO igrp=begusr,Ngrp
         begcol=Grp(igrp-1)
         IF((Rgvrtp(begcol).ge.PRGTUH.and.Rgvrtp(begcol).le.PRGUH5).or.
     &      Rgvrtp(begcol).eq.PRGTUD.or.Rgvrtp(begcol).eq.PRGTUS.or.
     &      Rgvrtp(begcol).eq.PRGUTD.or.Rgvrtp(begcol).eq.PRGULM.or.
     &      Rgvrtp(begcol).eq.PRGULQ.or.Rgvrtp(begcol).eq.PRGULY.or.
     &      Rgvrtp(begcol).eq.PRGUAO.or.Rgvrtp(begcol).eq.PRGULS.or.
     &      Rgvrtp(begcol).eq.PRGUSO.or.Rgvrtp(begcol).eq.PRGUCN.or.
     &      Rgvrtp(begcol).eq.PRGUCY)THEN
          endcol=Grp(igrp)-1
          DO icol=begcol,endcol
           CALL getstr(Colttl,Colptr,Ncoltl,icol,icoltl,nchr)
           IF(Lfatal)RETURN
           WRITE(fh,1020)icoltl(1:nchr)
          END DO
         END IF
        END DO
c     ------------------------------------------------------------------
        WRITE(fh,1020)'  )'
c     ------------------------------------------------------------------
        CALL wrtdat(Begxy,Sp,outstr,nchr)
        IF(Lfatal)RETURN
        WRITE(fh,1021)'start='//outstr(1:nchr)
 1021   FORMAT('  ',a)
c-----------------------------------------------------------------------
c     The data start at the span and go until the end of the
c forecasts.
c-----------------------------------------------------------------------
        WRITE(fh,1021)'data=('
        nusr=endcol-begusr+1
        WRITE(fmtusr,1040)Svsize+1,Svprec
 1040   FORMAT('(t5,4e',i2.2,'.',i2.2,')')
        DO iend=endcol,Ncxy*Nrxy,Ncxy
         ibeg=iend-nusr+1
         WRITE(fh,fmtusr)(Xy(ielt),ielt=ibeg,iend)
c         WRITE(fh,1040)(Xy(ielt),ielt=ibeg,iend)
c 1040    FORMAT(t5,4g16.6)
        END DO
        WRITE(fh,1020)'  )'
        IF(Nusrrg.gt.0)THEN
         WRITE(fh,1021)'usertype=('
         DO urgidx=1,Nusrrg
          CALL getstr(URGDIC,urgptr,PURG,Usrtyp(urgidx),outstr,nchr)
          IF(Lfatal)RETURN
          WRITE(fh,1020)'     '//outstr(1:nchr)
         END DO
         WRITE(fh,1020)'  )'
        END IF
       END IF
c-----------------------------------------------------------------------
c     Printout the noapply option
c-----------------------------------------------------------------------
       IF(Adjtd.eq.-1.or.Adjao.eq.-1.or.Adjls.eq.-1.or.Adjtc.eq.-1.or.
     &    Adjso.eq.-1.or.Adjhol.eq.-1.or.Adjsea.eq.-1.or.Adjusr.eq.-1)
     &    THEN
        WRITE(fh,1021)'noapply=('
        IF(Adjtd.eq.-1)WRITE(fh,1020)'     td'
        IF(Adjao.eq.-1)WRITE(fh,1020)'     ao'
        IF(Adjls.eq.-1)WRITE(fh,1020)'     ls'
        IF(Adjtc.eq.-1)WRITE(fh,1020)'     tc'
        IF(Adjso.eq.-1)WRITE(fh,1020)'     so'
        IF(Adjhol.eq.-1)WRITE(fh,1020)'     holiday'
        IF(Adjsea.eq.-1)WRITE(fh,1020)'     userseasonal'
        IF(Adjusr.eq.-1)WRITE(fh,1020)'     user'
        WRITE(fh,1020)'  )'
       END IF
c-----------------------------------------------------------------------
c     Printout the regression coeffficients 
c-----------------------------------------------------------------------
       WRITE(fh,1021)'b=('
       DO i=1,Nb
        IF(Rgvrtp(i).ne.PRGTUD)THEN
         IF(Regfx(i))THEN
          WRITE(fh,1080)B(i),'f'
         ELSE
          WRITE(fh,1080)B(i)
         END IF
        END IF
       END DO
       IF(begusr.gt.0)THEN
        DO i=1,Nb
         IF(Rgvrtp(i).eq.PRGTUD)THEN
          IF(Regfx(i))THEN
           WRITE(fh,1080)B(i),'f'
          ELSE
           WRITE(fh,1080)B(i)
          END IF
         END IF
        END DO
       END IF
       WRITE(fh,1020)'  )'
c-----------------------------------------------------------------------
c     Print out miscellaneous additional options if they are different
c     than the default.
c-----------------------------------------------------------------------
       IF(.not.Elong)WRITE(fh,1021)'eastermeans=no'
       IF(.not.dpeq(Tcalfa,0.7D0))WRITE(fh,2020)'  tcrate=',Tcalfa
 2020  FORMAT(a,f14.6)
       WRITE(fh,1050)
 1050  FORMAT(' }')
      END IF
c-----------------------------------------------------------------------
c     Add the ARIMA model if there is any
c-----------------------------------------------------------------------
      WRITE(fh,1060)Mdldsn(1:Nmddcr)
 1060 FORMAT(/,' arima{model=',/,'  ',a)
c     ------------------------------------------------------------------
      CALL prARMA(fh,F)
c     ------------------------------------------------------------------
      WRITE(fh,1100)
 1100 FORMAT(' }')
c     ------------------------------------------------------------------
      IF(locok)CALL fclose(fh)
c     ------------------------------------------------------------------
      RETURN
      END
