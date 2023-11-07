C     Last change:  BCM  11 Jun 1998    4:07 pm
      SUBROUTINE makttl(Ttldic,Ttlptr,Pttl,Tblptr,Dsptr,Tblttl,Ntbttl,
     &                  Label,Fcst)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Create table title used in TABLE subroutine
c     ------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'fctlbl.prm'
      INCLUDE 'tbllbl.prm'
      INCLUDE 'x11tbl.i'
      INCLUDE 'cmptbl.i'
      INCLUDE 'frctbl.i'
      INCLUDE 'error.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'tbltitle.prm'
      INCLUDE 'force.cmn'
      INCLUDE 'mq3.cmn'
c-----------------------------------------------------------------------
      LOGICAL Label,Fcst
      CHARACTER Ttldic*(*),Tblttl*(PTTLEN),upper*(1),clbl*(10),ctbl*(3),
     &          tmpttl*(PTTLEN)
      INTEGER Ttlptr,Pttl,Tblptr,Ntbttl,nclbl,Dsptr,i1,i2,ntmp,ntmp2,n2,
     &        n3,ipos
      DIMENSION Ttlptr(0:Pttl)
c-----------------------------------------------------------------------
      INTEGER nblank
      EXTERNAL nblank
c-----------------------------------------------------------------------
      INCLUDE 'fctlbl.var'
      INCLUDE 'tbllbl.var'
c-----------------------------------------------------------------------
c     Get the table description from one of the data dictionaries
C-----------------------------------------------------------------------
      CALL getstr(Ttldic,Ttlptr,Pttl,Tblptr-Dsptr,Tblttl,Ntbttl)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Convert codes to correct labels and make the first character
c     upper case, if labels are produced
c-----------------------------------------------------------------------
      IF(Ntbttl.eq.0)THEN
       ipos=1
       CALL itoc(Tblptr,ctbl,ipos) 
       IF(Lfatal)RETURN
       CALL eWritln(PRGNAM//' could not generate a table title '//
     &              '(internal code #'//ctbl(1:(ipos-1))//').',
     &              STDERR,Mt2,T,F)
       CALL writln('       Please send the input spec file that '//
     &             'generated this error message,',STDERR,Mt2,F,F)
       CALL writln('       along with any data files used, to '//
     &             'x12@census.gov.',STDERR,Mt2,F,T)
       CALL abend
       RETURN
      ELSE
       i1=index(Tblttl(1:Ntbttl),'%')
       upper=Tblttl(1:1)
       IF(i1.gt.0)THEN
        ntmp=nblank(Moqu)
        ntmp2=nblank(Pcdif)
        IF(i1.eq.1)upper=Moqu(1:1)
        IF(Label)THEN
         i2=ichar(upper)
         IF(i2.ge.97.and.i2.le.122)upper=char(i2-32)
        END IF
        n2=Ntbttl+2*ntmp+ntmp2+4
        IF(i1.eq.1)THEN
         n3=Ntbttl-1
         tmpttl(1:n3)=Tblttl(2:Ntbttl)
         Tblttl(1:n2)=upper//Moqu(2:ntmp)//'-to-'//Moqu(1:ntmp)//' '//
     &                Pcdif(1:ntmp2)//tmpttl(1:n3)
        ELSE
         n3=Ntbttl-i1
         tmpttl(1:n3)=Tblttl((i1+1):Ntbttl)
         Tblttl(1:n2)=upper//Tblttl(2:(i1-1))//Moqu(1:ntmp)//'-to-'//
     &            Moqu(1:ntmp)//' '//Pcdif(1:ntmp2)//tmpttl(1:n3)
        END IF
        Ntbttl=n2
       ELSE
        i1=index(Tblttl(1:Ntbttl),'^')
        IF(i1.gt.0)THEN
         IF(Tblptr.eq.LXERA1.or.Tblptr.eq.LXERA2.or.Tblptr.eq.LCMPR1.or.
     &      Tblptr.eq.LCMPR2)THEN
          ntmp=nblank(Moqu)
          upper=char(ichar(Moqu(1:1))-32)
          ntmp2=nblank(Rad)
          IF(ntmp2.gt.6)ntmp2=4
          n2=Ntbttl+2*ntmp+ntmp2+4
          n3=Ntbttl-i1
          tmpttl(1:n3)=Tblttl((i1+1):Ntbttl)
          Tblttl(1:n2)=upper//Moqu(2:ntmp)//'-to-'//Moqu(1:ntmp)//' '//
     &           Rad(1:ntmp2)//tmpttl(1:n3)
          Ntbttl=n2
         ELSE
          ntmp=nblank(Rad)
          IF(i1.eq.1)upper=Rad(1:1)
          IF(Label)THEN
           i2=ichar(upper)
           IF(i2.ge.97.and.i2.le.122)upper=char(i2-32)
          END IF
          n2=Ntbttl+ntmp-1
          IF(i1.eq.1)THEN
           n3=Ntbttl-1
           tmpttl(1:n3)=Tblttl(2:Ntbttl)
           Tblttl(1:n2)=upper//Rad(2:ntmp)//tmpttl(1:n3)
          ELSE
           n3=Ntbttl-i1
           tmpttl(1:n3)=Tblttl((i1+1):Ntbttl)
           Tblttl(1:n2)=upper//Tblttl(2:(i1-1))//Rad(1:ntmp)//
     &                  tmpttl(1:n3)
          END IF
          Ntbttl=n2
         END IF
        ELSE IF(Label)THEN
         i2=ichar(Tblttl(1:1))
         IF(i2.ge.97.and.i2.le.122)upper=char(i2-32)
         n3=Ntbttl-1
         tmpttl(1:n3)=Tblttl(2:Ntbttl)
         Tblttl(1:Ntbttl)=upper//tmpttl(1:n3)
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
c     If table label produced as well, get label
c-----------------------------------------------------------------------
      IF(Label)THEN
       IF(Fcst)THEN
        CALL getstr(LBFDIC,lbfptr,PLBF,Tblptr,clbl,nclbl)
       ELSE
        CALL getstr(LBLDIC,lblptr,PLBL,Tblptr,clbl,nclbl)
       END IF
       IF(Lfatal)RETURN
       IF(nclbl.gt.0)THEN
        n2=Ntbttl+nclbl+2
        tmpttl(1:Ntbttl)=Tblttl(1:Ntbttl)
        Tblttl(1:n2)=clbl(1:nclbl)//'  '//tmpttl(1:Ntbttl)
        Ntbttl=n2
       END IF
      END IF
c-----------------------------------------------------------------------
      IF((Tblptr.eq.LFCRND.or.Tblptr.eq.LCPRND).and.Iyrt.gt.0)THEN
       Tblttl((Ntbttl+1):(Ntbttl+25))='with forced yearly totals'
       Ntbttl=Ntbttl+25
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
