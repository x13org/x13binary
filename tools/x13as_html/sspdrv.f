C     Last change:  BCM  23 Mar 2005    3:38 pm
      SUBROUTINE sspdrv(Ltmax,Lmodel,Lx11,X11agr,Lseats,Lcomp,Lgraf,
     &                  Iagr,Ncomp)
      IMPLICIT NONE
C-----------------------------------------------------------------------
c     Driver routine for the sliding spans analysis procedure.
C-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'ssap.prm'
      INCLUDE 'ssap.cmn'
      INCLUDE 'sspinp.cmn'
      INCLUDE 'sspdat.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'svllog.prm'
      INCLUDE 'svllog.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'xrgtbl.i' 
      INCLUDE 'mdltbl.i'   
      INCLUDE 'revtbl.i'
      INCLUDE 'ssptbl.i'
      INCLUDE 'dgnsvl.i'
      INCLUDE 'title.cmn'
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'otlrev.cmn'
      INCLUDE 'usrreg.cmn'
      INCLUDE 'usrxrg.cmn'
      INCLUDE 'xrgmdl.cmn'
      INCLUDE 'x11opt.cmn'
C-----------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
C-----------------------------------------------------------------------
      CHARACTER otlstr*(PCOLCR),usfxtl*(PCOLCR*PUREG),outstr*(PCOLCR),
     &          numstr*(10)
      LOGICAL Lmodel,Lx11,X11agr,Lseats,Lcomp,Lgraf,lyy,lyy2,lncset,
     &        lnlset,otlfix,upuser,lastfx,bfx2,upusrx,lstxfx,ssdbak,
     &        ssidbk
      INTEGER otl,Iagr,notstr,strinx,i,j,msrtmp,Ltmax,nusfx,nusftl,
     &        usfptr,igrp,Ncomp,ipos,ipos2
C-----------------------------------------------------------------------
      DIMENSION bfx2(PB),usfptr(0:PUREG)
C-----------------------------------------------------------------------
      EXTERNAL strinx
      REAL ticks
C-----------------------------------------------------------------------
      CALL intlst(PUREG,usfptr,nusftl)
      nusfx=nusftl+1
C-----------------------------------------------------------------------
c     Initialize variables
C-----------------------------------------------------------------------
      lncset=Ncol.gt.0
      lnlset=Nlen.GT.0
      IF(Iagr.eq.5)THEN
       IF((.not.lncset).and.(.not.lnlset).and.(.not.Lcomp))THEN
        Ncol=Indcol
        Nlen=Indlen
       END IF
      END IF
      CALL setssp(Issap,Begspn,Pos1ob,Posfob,Ltmax,Lmodel,Lseats,lncset,
     &            lnlset,otlfix)
      IF(Lfatal.or.Issap.eq.0)RETURN
      Issap=2
c-----------------------------------------------------------------------
C     Activate no-print option and print no plots
c-----------------------------------------------------------------------
      IF(Sstran)Lhiddn=T
      DO i=1,NTBL
       IF(i.lt.LRVHDR.or.i.gt.LSSTDS)THEN
        IF(Sstran)THEN
         Prttab(i)=F
         Savtab(i)=F
        ELSE
         IF(i.ne.LESTES.and.i.ne.LXRXRG.and.i.ne.(LXRXRG+1))Savtab(i)=F
        END IF
       END IF
      END DO
      DO i=1,NSVLOG
       IF(i.lt.LSLASA.or.i.gt.(LSLPCT+1))Svltab(i)=F
      END DO
      IF(Svltab(LSLPCT+1))THEN
       IF(.not.Svltab(LSLPCT))Svltab(LSLPCT)=T
      END IF
C-----------------------------------------------------------------------
c     Check options for indirect sliding spans analysis,
C-----------------------------------------------------------------------
      IF(Iagr.eq.5)THEN
       IF(Indssp.eq.NOTSET)THEN
        Indssp=-3
       ELSE IF(Indssp.gt.0)THEN
        IF(Nscomp.lt.Ncomp)THEN
         Indssp=-4
        ELSE IF(Indlen.ne.Nlen)THEN
         Indssp=-5
        ELSE IF(Indcol.ne.Ncol)THEN
         Indssp=-6
        END IF
        IF(Lcomp)Indssp=0
       END IF
      END IF
C-----------------------------------------------------------------------
c     Set up and perform transparent seasonal adjustments for sliding
c     spans diagonstics
C-----------------------------------------------------------------------
      msrtmp=Lmsr
      ssdbak=Ssdiff
      ssidbk=Ssidif
      Ierhdr=NOTSET
      DO j=1,Ncol
       CALL x11int
*       IF(Lsumm.gt.0)THEN
*        CALL cpu_time(ticks)
*        WRITE(*,9000) 'bssx11a',j,':',ticks
*        WRITE(Nform,9000) 'bssx11a',j,':',ticks
*       END IF
       CALL ssx11a(j,Lmodel,Lx11,Lseats,msrtmp,Ncol,Nlen,Ixreg,otlfix,
     &             Ssinit,Ssxotl,Ssxint)
*       IF(Lsumm.gt.0)THEN
*        CALL cpu_time(ticks)
*        WRITE(*,9000) 'essx11a',j,':',ticks
*        WRITE(Nform,9000) 'essx11a',j,':',ticks
*       END IF
       IF(Lfatal)RETURN
       IF(Ixreg.eq.3)Ixreg=2
C-----------------------------------------------------------------------
c     Reset model parameters to original values.
C-----------------------------------------------------------------------
       IF(Ssinit.eq.2)THEN
        DO i=1,PARIMA
         IF(.not.Arimaf(i))Arimap(i)=DNOTST
        END DO
        DO i=1,PB
         IF(Iregfx.eq.0)B(i)=DNOTST
        END DO
        IF(Ixreg.gt.0)THEN
         DO i=1,PB
          IF(Irgxfx.eq.0)Bx(i)=DNOTST
         END DO
        END IF
       END IF
c----------------------------------------------------------------------
c     check user defined regressors to see if they are well-defined
c     for this span.
c----------------------------------------------------------------------
       upuser=F
       upusrx=F
       lastfx=Userfx
       lstxfx=Usrxfx
       IF(Nusxrg.gt.0)THEN
        CALL copylg(Regfxx,Nbx,1,bfx2)
        CALL chusrg(upusrx,usfxtl,nusfx,nusftl,usfptr)
        IF(Lfatal)RETURN
        IF(upusrx)THEN
         IF(.not.Usrxfx)Usrxfx=T
         CALL bakusr(Xuserx,Usxtyp,Usrxpt,Nusxrg,Usrxtt,Regfxx,Bx,
     &               Rgxvtp,Nxgrp,Grpttx,Grpx,Gpxptr,Ngrptx,1,
     &               .not.lstxfx)
        END IF
       END IF
       IF(Ncusrx.gt.0)THEN
        CALL copylg(Regfx,Nb,1,bfx2)
        CALL chusrg(upuser,usfxtl,nusfx,nusftl,usfptr)
        IF(Lfatal)RETURN
        IF(upuser)THEN
         IF(.not.Userfx)Userfx=T
         CALL bakusr(Userx,Usrtyp,Usrptr,Ncusrx,Usrttl,Regfx,B,Rgvrtp,
     &               Ngrp,Grpttl,Grp,Grpptr,Ngrp,0,.not.lastfx)
         CALL ssprep(T,F,F)
        END IF
       END IF
C-----------------------------------------------------------------------
*       IF(Lsumm.gt.0)THEN
*        CALL cpu_time(ticks)
*        WRITE(*,9000) 'bx11ari',j,':',ticks
*        WRITE(Nform,9000) 'bx11ari',j,':',ticks
*       END IF
       CALL x11ari(Lmodel,Lx11,X11agr,Lseats,Lcomp,Issap,Irev,Irevsa,
     &             Ixreg,0,F,F)
*       IF(Lsumm.gt.0)THEN
*        CALL cpu_time(ticks)
*        WRITE(*,9000) 'ex11ari',j,':',ticks
*        WRITE(Nform,9000) 'ex11ari',j,':',ticks
*       END IF
C-----------------------------------------------------------------------
c     If there was an error in the ARIMA model estimation, print out
c     the error message here.
C-----------------------------------------------------------------------
       IF(Armaer.eq.PMXIER)THEN
        CALL abend
        RETURN
       ELSE IF(Armaer.ne.0)THEN
        Armaer=0
       END IF
       IF(Lfatal)RETURN
C-----------------------------------------------------------------------
C     If Seats seasonal adjustment for span is unadmissable, print
C     message and leave routine.
C-----------------------------------------------------------------------
       IF(Issap.lt.0)THEN
        Issap=0
        RETURN
       END IF
C-----------------------------------------------------------------------
c     Remove outliers added to regression variables
C-----------------------------------------------------------------------
       IF(Notrtl.gt.0)THEN
        DO i=1,Notrtl
         CALL getstr(Otrttl,Otrptr,Notrtl,i,otlstr,notstr)
         IF(Lfatal)RETURN
         otl=strinx(T,Colttl,Colptr,1,Nb,otlstr(1:notstr))
         IF(otl.gt.0)THEN
          CALL dlrgef(otl,Nrxy,1)
          IF(Lfatal)RETURN
         END IF
        END DO
        CALL ssprep(T,F,F)
       END IF
C-----------------------------------------------------------------------
C     Add user-defined regressors deleted back into regression matrix
C-----------------------------------------------------------------------
       IF(upuser)THEN
        CALL copylg(bfx2,Nb,1,Regfx)
        Userfx=lastfx
        CALL ssprep(T,F,F)
       END IF
       IF(upusrx)THEN
        CALL copylg(bfx2,Nb,1,Regfxx)
        Usrxfx=lstxfx
       END IF
      END DO
C-----------------------------------------------------------------------
c     Reset sliding span indicator, add to header if Ierhdr has changed
C-----------------------------------------------------------------------
      Issap=3
      IF(Ierhdr.ne.NOTSET)CALL errhdr
C-----------------------------------------------------------------------
      IF(Ssdiff.and.(.not.ssdbak))THEN
       CALL nWritln('Seasonally adjusted values for at least one of'//
     &              'the spans was',Mt1,Mt2,T,F)
       CALL writln(' less than or equal to zero.',Mt1,Mt2,F,F)
       CALL writln(' The sliding spans analysis will be calculated '//
     &              'from the maximum ',Mt1,Mt2,F,F)
       CALL writln(' differences of the seasonally adjusted series '//
     &             'rather than ',Mt1,Mt2,F,F)
       CALL writln(' the implied adjustment factors.',Mt1,Mt2,F,T)
      END IF
C-----------------------------------------------------------------------
      IF(nusftl.gt.0)THEN
       CALL nWritln('The user defined regressors listed below were '//
     &              'held fixed',Mt1,Mt2,T,F)
       CALL writln(' for at least one span during the sliding '//
     &             'spans analysis:',Mt1,Mt2,F,T)
       CALL writTagClass(Mt1,'ul','indent')
       CALL writTagClass(Mt2,'ul','indent')
       DO igrp=1,nusftl
        CALL getstr(usfxtl,usfptr,nusfx,igrp,outstr,ipos)
        IF(Lfatal)RETURN
        CALL writTagOneLine(Mt1,'li','@',outstr(1:ipos))
        CALL writTagOneLine(Mt2,'li','@',outstr(1:ipos))
       END DO
       CALL writTag(Mt1,'</ul>')
       CALL writTag(Mt2,'</ul>')
      END IF
C-----------------------------------------------------------------------
c     If summary measures run, return
C-----------------------------------------------------------------------
      IF(Kfulsm.eq.1.or.(Kfulsm.eq.2.and.(Itd.eq.0.and.Ihol.eq.0)))
     &   RETURN
c-----------------------------------------------------------------------
c     Determine if year-to-year changes are to be printed out
c-----------------------------------------------------------------------
      lyy=Prttab(LSSYPC).or.Prttab(LSSYSM)
      lyy2=(Prttab(LSSYPC+1).or.Prttab(LSSYSM+1)).and.Iagr.eq.5
C-----------------------------------------------------------------------
c     Print out sliding spans header and F-tests for each span
c-----------------------------------------------------------------------
*      IF((Prttab(LSSSHD).or.Prttab(LSSFTS)).and.Lpage)THEN
*       WRITE(Mt1,Ttlfmt)Newpg,Title(1:Ntitle),Kpage,Serno(1:Nser)
*       Kpage=Kpage+1
*      END IF
      IF(Prttab(LSSSHD).or.Savtab(LSSSHD))
     &   CALL ssphdr(Iagr,Ncol,Nlen,Ssfxrg,Nssfxr,lyy,lyy2,Ssinit,
     &               Ssdiff,lncset,lnlset,Prttab(LSSSHD),Savtab(LSSSHD))
      IF((Prttab(LSSFTS).or.Savtab(LSSFTS)).and.(.not.Lseats))
     &   CALL ssftst(Ncol,Prttab(LSSFTS),Savtab(LSSFTS))
c-----------------------------------------------------------------------
c     Determine if year-to-year changes are to be computed
c-----------------------------------------------------------------------
      lyy=Prttab(LSSYPC).or.Prttab(LSSYSM).or.Prttab(LSSYCS).or.
     &    Savtab(LSSYPC).or.Savtab(LSSYSM).or.Savtab(LSSYCS).or.Lgraf
      lyy2=(Prttab(LSSYPC+1).or.Prttab(LSSYSM+1).or.Prttab(LSSYCS+1).or.
     &      Savtab(LSSYPC+1).or.Savtab(LSSYSM+1).or.Savtab(LSSYCS+1).or.
     &      Lgraf).and.Iagr.eq.5
C-----------------------------------------------------------------------
c     Perform sliding spans analysis
C-----------------------------------------------------------------------
      IF(Ltimer)THEN
       CALL cpu_time(ticks)
       WRITE(Nform,9001) 'bssap:',ticks
      END IF
      CALL ssap(S,Sa,Td,Isfadd,Iagr,Ncol,Nlen,Lsumm,lyy,Ssdiff,Lgraf)
      IF(Lfatal)RETURN
      IF(Ltimer)THEN
       CALL cpu_time(ticks)
       WRITE(Nform,9001) 'essap:',ticks
      END IF
c-----------------------------------------------------------------------
c     If this is a composite run, perform sliding spans for indirect 
c     adjustment
C-----------------------------------------------------------------------
      IF(Iagr.eq.5)THEN
       IF(Indssp.gt.0)THEN
        Iagr=6
*        IF(Lpage)THEN
*         WRITE(Mt1,Ttlfmt)Newpg,Title(1:Ntitle),Kpage,Serno(1:Nser)
*         Kpage=Kpage+1
*        END IF
        CALL mkPOneLine(Mt1,'@',
     &     'Sliding spans analysis: Indirect seasonal adjustment')
C-----------------------------------------------------------------------
        IF(Lsumm.gt.0)WRITE(Nform,1090)'yes'
C-----------------------------------------------------------------------
        IF(Ssidif.and.(.not.ssidbk))THEN
         CALL nWritln('The indirect seasonal adjustment for at least'//
     &                ' one of the spans',Mt1,Mt2,T,F)
         CALL writln(' was less than or equal to zero.',Mt1,Mt2,F,F)
         CALL writln(' The sliding spans analysis will be calculated'//
     &               ' from the maximum',Mt1,Mt2,F,F)
         CALL writln(' differences of the indirect seasonally '//
     &               'adjusted series rather',Mt1,Mt2,F,F)
         CALL writln(' than the implied adjustment factors.',
     &               Mt1,Mt2,F,T)
        END IF
        CALL ssap(Sfind,Saind,Td,Sfinda,Iagr,Ncol,Nlen,Lsumm,lyy2,
     &            Ssidif,Lgraf)
       ELSE
        IF(Lsumm.gt.0)WRITE(Nform,1090)'no'
        IF(Indssp.eq.-1)THEN
c  insert error message for different span length or number of spans
c  found for component
         CALL nWritln('Different span lengths were used for the '//
     &                'sliding spans analysis ',Mt1,Mt2,T,F)
         CALL writln('of the component seasonal adjustments.',
     &               Mt1,Mt2,F,T)
         CALL writln('Sliding spans analysis of the indirect '//
     &               'seasonal adjustments will not',
     &               Mt1,Mt2,T,F)
         CALL writln('be produced.  Use the length argument of the '//
     &               'slidingspans spec ',
     &               Mt1,Mt2,F,F)
         CALL writln('to ensure an appropriate span length is '//
     &               'specified for each ',
     &               Mt1,Mt2,F,F)
         CALL writln('of the component spec files.',Mt1,Mt2,F,T)
        ELSE IF(Indssp.eq.-2)THEN
c  insert error message for different span length or number of spans
c  found for component
         CALL nWritln('The number of sliding spans used for the '//
     &                'sliding spans analysis ',Mt1,Mt2,T,F)
         CALL writln('has changed for one of the components in the '//
     &               'composite seasonal adjustment.',Mt1,Mt2,F,T)
         CALL writln('Sliding spans analysis of the indirect '//
     &               'seasonal adjustments will not',Mt1,Mt2,T,F)
         CALL writln('be produced.  Check the numspan argument of '//
     &               'the slidingspans spec',Mt1,Mt2,F,F)
         CALL writln('to ensure the same number of sliding spans '//
     &               'is specified for each',Mt1,Mt2,F,F)
         CALL writln('of the component spec files.',Mt1,Mt2,F,T)
        ELSE IF(Indssp.eq.-3)THEN
c  insert error message for no sliding spans analysis of component
         ipos=1
         CALL itoc(Ncomp,numstr,ipos)
         CALL nWritln('Composite seasonal adjustment performed with '//
     &                numstr(1:(ipos-1))//' components,',Mt1,Mt2,T,F)
         CALL writln('but the indirect seasonal adjustment for the '//
     &               'sliding spans',Mt1,Mt2,F,F)
         CALL writln('was updated for none of the components.',
     &               Mt1,Mt2,F,T)
         CALL writln('Sliding spans analysis of the indirect '//
     &               'seasonal adjustments will not',Mt1,Mt2,T,F)
         CALL writln('be produced.  Ensure that a slidingspans spec'//
     &               ' is present in the',Mt1,Mt2,F,F)
         CALL writln('       spec files of all the components.',
     &               Mt1,Mt2,F,T)
        ELSE IF(Indssp.eq.-4)THEN
         ipos=1
         CALL itoc(Ncomp,numstr,ipos)
         ipos2=ipos
         CALL itoc(Nscomp,numstr,ipos2)
         CALL nWritln('Composite seasonal adjustment performed with '//
     &                numstr(1:(ipos-1))//' components,',Mt1,Mt2,T,F)
         CALL writln('but the indirect seasonal adjustment for the '//
     &               ' sliding spans',Mt1,Mt2,F,F)
         CALL writln('was updated for only '//numstr(ipos:(ipos2-1))//
     &               ' components.',Mt1,Mt2,F,T)
         CALL writln('Sliding spans analysis of the indirect '//
     &               'seasonal adjustments will not',Mt1,Mt2,T,F)
         CALL writln('be produced.  Check for errors in the '//
     &               'sliding spans analysis of the',Mt1,Mt2,F,F)
         CALL writln('components, and ensure that a slidingspans spec'//
     &               ' is present in the',Mt1,Mt2,F,F)
         CALL writln('       spec files of all the components.',
     &               Mt1,Mt2,F,F)
        ELSE IF(Indssp.eq.-5)THEN
c  insert error message for different span length for direct
         CALL nWritln('A different span length was specified for '//
     &                'the sliding spans analysis',Mt1,Mt2,T,F)
         CALL writln('of the direct seasonal adjustment of the '//
     &               'composite than was used for',Mt1,Mt2,F,F)
         CALL writln('the component seasonal adjustments.',Mt1,Mt2,F,T)
         CALL writln('Sliding spans analysis of the indirect '//
     &               'seasonal adjustments will not',Mt1,Mt2,T,F)
         CALL writln('be produced.  Use the length argument of the '//
     &               'slidingspans spec ',Mt1,Mt2,T,F)
         CALL writln('to ensure the same span length is used for '//
     &               ' sliding spans analysis ',Mt1,Mt2,F,F)
         CALL writln('of the direct seasonal adjustment and the '//
     &               'component adjustments.',Mt1,Mt2,F,T)
        ELSE IF(Indssp.eq.-6)THEN
c  insert error message for different number of spans for direct
         CALL nWritln('A different number of sliding spans was '//
     &                'specified for the',Mt1,Mt2,T,F)
         CALL writln('sliding spans analysis of the direct seasonal '//
     &               'adjustment of the',Mt1,Mt2,F,F)
         CALL writln('composite than was used for the component '//
     &               'seasonal adjustments.',Mt1,Mt2,F,T)
         CALL writln('Sliding spans analysis of the indirect '//
     &               'seasonal adjustments will not',Mt1,Mt2,T,F)
         CALL writln('be produced.  Use the length argument of the '//
     &               'slidingspans spec ',Mt1,Mt2,F,F)
         CALL writln('to ensure the same span length is used for '//
     &               'sliding spans analysis ',Mt1,Mt2,F,F)
         CALL writln('of the direct seasonal adjustment and the '//
     &               'component adjustments.',Mt1,Mt2,F,T)
        END IF
       END IF
      END IF
C-----------------------------------------------------------------------
      IF(.not.Lfatal)Lhiddn=F
C-----------------------------------------------------------------------
      RETURN
C-----------------------------------------------------------------------
* 1010 FORMAT(80A1)
 1090 FORMAT('indsspans: ',a)
* 9000 FORMAT(a,i1,a,e15.8)
 9001 FORMAT(a,e15.8)
      END
