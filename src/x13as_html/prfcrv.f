C     Last change:  BCM  16 Feb 1999   11:17 am
**==prfcrv.f    processed by SPAG 4.03F  at 16:46 on 14 Nov 1994
      SUBROUTINE prfcrv(Orig,Endall,Ny,Lam,Fcntyp,Nptr,Nsvptr,Lgraf,
     &                  Lsumm)
C-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Print revisions history of forecast errors for all lags
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'rev.prm'
      INCLUDE 'rev.cmn'
      INCLUDE 'revsrs.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'htmlout.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'svllog.prm'
      INCLUDE 'svllog.cmn'
      INCLUDE 'cchars.i'
c-----------------------------------------------------------------------
      CHARACTER fctfmt*60,str*10,str2*10,outstr*(10+PFCLAG*72),
     &          tbllbl*3,fctlbl*15,clslbl*2,thisId*6,thisLag*7,
     &          thisHdr*20,thisVal*17
      LOGICAL locok,Lgraf
      DOUBLE PRECISION Orig,Lam,fcter,fctss,fcttrn,tmp1,tmp2
      INTEGER begfct,fh,fh2,i,j,k,k2,ndef,Ny,Nptr,ndtc,ndtc2,ipos,
     &        Endall,idate,rdbdat,Nsvptr,Fcntyp,Lsumm,ifctl,iclsl
      DIMENSION begfct(2),Endall(2),fcter(PFCLAG),fctss(PFCLAG),
     &          idate(2),Orig(PLEN),fcttrn(PFCLAG)
c-----------------------------------------------------------------------
      LOGICAL T,F
      INTEGER MO,YR
      DOUBLE PRECISION ONE,ZERO
      PARAMETER(MO=2,YR=1,T=.true.,F=.false.,ZERO=0D0,ONE=1D0)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
C-----------------------------------------------------------------------
      CALL setdp(ZERO,PFCLAG,fcter)
      CALL setdp(ZERO,PFCLAG,fctss)
c-----------------------------------------------------------------------
c     Set date for first n-step ahead forecast
c-----------------------------------------------------------------------
      CALL addate(Rvstrt,Ny,Rfctlg(1),begfct)
c-----------------------------------------------------------------------
c     If forecast errors being saved, open file
c-----------------------------------------------------------------------
      IF(Savtab(Nptr).or.Lgraf)THEN
       locok=T
       IF(Savtab(Nptr))CALL opnfil(T,F,Nptr,fh,locok)
       IF(locok.and.Lgraf)CALL opnfil(T,T,Nptr,fh2,locok)
       IF(.not.locok)THEN
        CALL abend
        RETURN
       END IF
c-----------------------------------------------------------------------
c     Print header for forecast history
c-----------------------------------------------------------------------
       WRITE(fctfmt,1010)Nfctlg
       IF(Savtab(Nptr))WRITE(fh,fctfmt)'date',
     &               (TABCHR,'SumSqFcstError(',Rfctlg(k),')',k=1,Nfctlg)
       IF(Lgraf)WRITE(fh2,fctfmt)'date',
     &               (TABCHR,'SumSqFcstError(',Rfctlg(k),')',k=1,Nfctlg)
       fctfmt=' '
       WRITE(fctfmt,1030)Nfctlg
       IF(Savtab(Nptr))
     &    WRITE(fh,fctfmt)'------',
     &                    (TABCHR,'----------------------',k=1,Nfctlg)
       IF(Lgraf)
     &    WRITE(fh2,fctfmt)'------',
     &                     (TABCHR,'----------------------',k=1,Nfctlg)
      END IF
c-----------------------------------------------------------------------
c     Print header for forecast errors
c-----------------------------------------------------------------------
      IF(Prttab(Nptr).or.Prttab(Nptr+1))tbllbl='R 8'
      IF(Prttab(Nptr))THEN
       CALL genSkip(Nptr)
       CALL writTag(Mt1,'<h3>')
       CALL writln(tbllbl//'.  Evolving Sum of Squared Forecast '//
     &     ' Errors and evolving Mean Square Error',Mt1,0,F,F)
       CALL writln(' of forecasts of the original data adjusted'//
     &     ' for any AO, LS, TC outliers',Mt1,0,F,F)
       CALL writln(' or ramps at specified leads from the end of'//
     &     ' each data span.',Mt1,0,F,F)
       CALL writTag(Mt1,'</h3>')
       IF(Revfix)THEN
        CALL mkPOneLine(MT1,'@',
     &          'The regARIMA model is not reestimated for each span.')
       ELSE
        CALL mkPOneLine(MT1,'@',
     &          'The regARIMA model is reestimated for each span.')
       END IF
c-----------------------------------------------------------------------
       CALL wrtdat(begfct,Ny,str,ndtc)
       IF(.not.Lfatal)CALL wrtdat(Endall,Ny,str2,ndtc2)
       IF(Lfatal)RETURN
       CALL mkPOneLine(Mt1,'@',
     &         'Forecast dates vary from '//str(1:ndtc)//' to '//
     &         str2(1:ndtc2)//'.')
c-----------------------------------------------------------------------
       Inrv=Inrv+1
       WRITE(Mt1,1000)Inrv
       CALL mkTableTag(Mt1,'w50','@')
       CALL mkCaption(Mt1,
     &                'Evolving Sum of Squared Forecast Error Table')
       CALL writTag(Mt1,'<tr>')
       CALL mkTableCellSpan(Mt1,'row',2,'head','&nbsp;')
       DO k=1,Nfctlg
        WRITE(thisLag,1080)Rfctlg(k)
        WRITE(thisId,1090)'r8l',k
        CALL mkHeaderCellId(Mt1,0,2,thisId,'@','@',thisLag)
       END DO
       CALL writTag(Mt1,'</tr>')
       CALL writTag(Mt1,'<tr>')
       DO k=1,Nfctlg
        WRITE(thisId,1090)'r8s',k
        CALL mkHeaderCellId(Mt1,0,0,thisId,'@',
     &                      'Sum of Squared Forecast Error',
     &                      'SS Fct. Err.')
        WRITE(thisId,1090)'r8m',k
        CALL mkHeaderCellId(Mt1,0,0,thisId,'@','Mean Standard Error',
     &                      'Mean S.E.')
       END DO
       CALL writTag(Mt1,'</tr>')
      END IF
c-----------------------------------------------------------------------
c     Start loop to print/save forecast error information.
c-----------------------------------------------------------------------
      j=0
      DO i=Begrev+Rfctlg(1),Endrev
       Revptr=i-Begrev+1
       j=j+1
c-----------------------------------------------------------------------
c     Calculate forcast errors, accumulated sum of squares.
c-----------------------------------------------------------------------
       ndef=0
       DO k=1,Nfctlg
        IF(Nfctlg.eq.1.or.(Nfctlg.gt.1.and.Rfctlg(k).le.j))THEN
         IF(Rvtrfc.and.(.not.dpeq(Lam,ONE)))THEN
          IF(dpeq(Lam,ZERO))THEN
           fcter(k)=log(Orig(i))-log(Cncfct(k,Revptr))
          ELSE IF(Fcntyp.eq.3)THEN
           tmp1=Orig(i)
           tmp2=Cncfct(k,Revptr)
           fcter(k)=log(tmp1/(ONE-tmp1))-log(tmp2/(ONE-tmp2))
          ELSE
           tmp1=Lam**2+(Orig(i)**Lam-ONE)/Lam
           tmp2=Lam**2+(Cncfct(k,Revptr)**Lam-ONE)/Lam
           fcter(k)=tmp1-tmp2
          END IF
         ELSE
          fcter(k)=Orig(i)-Cncfct(k,Revptr)
         END IF
         fctss(k)=fctss(k)+(fcter(k)*fcter(k))
c         fcter2(k)=Oriwlc(i)-Cncfct(k,Revptr)
c         fctss2(k)=fctss2(k)+(fcter2(k)*fcter2(k))
c         fcter3(k)=Oriwlc(i)-Finfct(k,Revptr)
c         fctss3(k)=fctss3(k)+(fcter3(k)*fcter3(k))
         ndef=ndef+1
        END IF
       END DO
c-----------------------------------------------------------------------
c     Print out forecast errors
c-----------------------------------------------------------------------
       CALL addate(begfct,Ny,(j-1),idate)
       CALL wrtdat(idate,Ny,str,ndtc)
       IF(Lfatal)RETURN
       IF(Prttab(Nptr))THEN
        CALL writTag(Mt1,'<tr>')
        WRITE(thisId,1090)'r8r',j
        CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@',str(1:ndtc))
        DO k=1,Nfctlg
         IF(k.gt.ndef)THEN
          CALL mkTableCell(Mt1,'@','&nbsp;')
          CALL mkTableCell(Mt1,'@','&nbsp;')
         ELSE
          WRITE(thisHdr,1110)'r8r',j,'r8l',k,'r8s',k
          WRITE(thisVal,1120)fctss(k)
          CALL mkTableCellHeader(Mt1,thisHdr,'@',thisVal)
          WRITE(thisHdr,1110)'r8r',j,'r8l',k,'r8m',k
          WRITE(thisVal,1120)fctss(k)/(Revptr-Rfctlg(k))
          CALL mkTableCellHeader(Mt1,thisHdr,'@',thisVal)
         END IF
        END DO
        CALL writTag(Mt1,'</tr>')
       END IF
c-----------------------------------------------------------------------
       IF(Savtab(Nptr).or.Lgraf)THEN
c-----------------------------------------------------------------------
c     Set date of revision for observation Revptr
c-----------------------------------------------------------------------
        rdbdat=100*idate(YR)+idate(MO)
c-----------------------------------------------------------------------
c     Save forecast error revisions with date
c-----------------------------------------------------------------------
        ipos=1
        CALL itoc(rdbdat,outstr,ipos)
        IF(Lfatal)RETURN
c-----------------------------------------------------------------------
        DO k=1,Nfctlg
         outstr(ipos:ipos)=TABCHR
         ipos=ipos+1
         IF(k.le.ndef)THEN
          CALL dtoc(fctss(k),outstr,ipos)
         ELSE
          CALL dtoc(ZERO,outstr,ipos)
         END IF
         IF(Lfatal)RETURN
        END DO
        IF(Savtab(Nptr))WRITE(fh,1130)outstr(1:ipos-1)
        IF(Lgraf)WRITE(fh2,1130)outstr(1:ipos-1)
       END IF
       IF (i.eq.Endrev) THEN
        IF (Lsumm.gt.0) THEN
         IF(Rvtrfc)THEN
          WRITE(Nform,1130)'transformfcst: yes'
         ELSE
          WRITE(Nform,1130)'transformfcst: no'
         END IF
         WRITE(Nform,1140)(Rfctlg(k),k=1,Nfctlg)
         WRITE(Nform,1150)(fctss(k)/(Revptr-Rfctlg(k)),k=1,Nfctlg)
        END IF
        IF(Svltab(Nsvptr))THEN
         Inlgfl=Inlgfl+1
         WRITE(Ng,1001)Inlgfl
         CALL mkTableTag(Ng,'w60',
     &                   'Average of Squared History Forecast Errors')
         CALL mkCaption(Ng,'Average of Squared History Forecast Errors')
         DO k=1,Nfctlg
          CALL writTag(Ng,'<tr>')
          WRITE(Ng,1160)Rfctlg(k),Fctss(k)/(Revptr-Rfctlg(k))
          CALL writTag(Ng,'</tr>')
         END DO
         CALL writTag(Ng,'</table></div>')
         CALL mkPOneLine(Ng,'@','&nbsp;')
        END IF
       END IF
      END DO
      IF(Prttab(Nptr))THEN
       CALL writTag(Mt1,'</table></div>')
       CALL mkPOneLine(Mt1,'@','&nbsp;')
      END IF
      IF(Savtab(Nptr))CALL fclose(fh)
      IF(Lgraf)CALL fclose(fh2)
c-----------------------------------------------------------------------
c     If forecast history being saved, open file
c-----------------------------------------------------------------------
      IF(Savtab(Nptr+1).or.Lgraf)THEN
       locok=T
       IF(Savtab(Nptr+1))CALL opnfil(T,F,Nptr+1,fh,locok)
       IF(locok.and.Lgraf)CALL opnfil(T,T,Nptr+1,fh2,locok)
       IF(.not.locok)THEN
        CALL abend
        RETURN
       END IF
c-----------------------------------------------------------------------
c     Print header for forecast history
c-----------------------------------------------------------------------
       WRITE(fctfmt,1020)Nfctlg
       IF(Rvtrfc.and.(.not.dpeq(Lam,ONE)))THEN
        fctlbl='Trans(Forecast('
        ifctl=15
        clslbl='))'
        iclsl=2
       ELSE
        fctlbl='Forecast('
        ifctl=9
        clslbl=')'
        iclsl=2
       END IF
       IF(Savtab(Nptr+1))
     &    WRITE(fh,fctfmt)'date',(TABCHR,fctlbl(1:ifctl),Rfctlg(k),
     &                    clslbl(1:iclsl),TABCHR,'FcstError(',
     &                    Rfctlg(k),')',k=1,Nfctlg)
       IF(Lgraf)
     &    WRITE(fh2,fctfmt)'date',(TABCHR,fctlbl(1:ifctl),Rfctlg(k),
     &                     clslbl(1:iclsl),TABCHR,'FcstError(',
     &                     Rfctlg(k),')',k=1,Nfctlg)
       fctfmt=' '
       WRITE(fctfmt,1040)Nfctlg
       IF(Savtab(Nptr+1))
     &    WRITE(fh,fctfmt)'------',(TABCHR,'----------------------',
     &                    TABCHR,'----------------------',k=1,Nfctlg)
       IF(Lgraf)
     &    WRITE(fh2,fctfmt)'------',(TABCHR,'----------------------',
     &                     TABCHR,'----------------------',k=1,Nfctlg)
      END IF
c-----------------------------------------------------------------------
c     Start loop to print concurrent forecast information.
c-----------------------------------------------------------------------
      IF(Prttab(Nptr+1).or.Savtab(Nptr+1).or.Lgraf)THEN
       j=0
c-----------------------------------------------------------------------
c     Print header for forecast errors
c-----------------------------------------------------------------------
       IF(Prttab(Nptr+1))THEN
        CALL genSkip(Nptr+1)
        CALL writTag(Mt1,'<h3>')
        CALL writln(tbllbl//'.A  Forecasts of the outlier adjusted '//
     &     'data (Table B1) at specified leads',Mt1,0,F,F)
        CALL writln(' from the end of each data span and associated'//
     &     ' forecast errors.',Mt1,0,F,F)
        CALL writTag(Mt1,'</h3>')
        IF(Revfix)THEN
         CALL mkPOneLine(MT1,'@',
     &          'The regARIMA model is not reestimated for each span.')
        ELSE
        CALL mkPOneLine(MT1,'@',
     &          'The regARIMA model is reestimated for each span.')
        END IF
        CALL mkPOneLine(Mt1,'@',
     &         'Forecast dates vary from '//str(1:ndtc)//' to '//
     &         str2(1:ndtc2)//'.')
c-----------------------------------------------------------------------
        Inrv=Inrv+1
        WRITE(Mt1,1000)Inrv
        CALL mkTableTag(Mt1,'w50','@')
        CALL mkCaption(Mt1,'Forecast and Forecast Error History Table')
        CALL writTag(Mt1,'<tr>')
        CALL mkTableCellSpan(Mt1,'row',2,'head','&nbsp;')
        DO k=1,Nfctlg
         WRITE(thisLag,1080)Rfctlg(k)
         WRITE(thisId,1090)'r8l',k+Nfctlg
         CALL mkHeaderCellId(Mt1,0,2,thisId,'@','@',thisLag)
        END DO
        CALL writTag(Mt1,'</tr>')
        CALL writTag(Mt1,'<tr>')
        DO k=1,Nfctlg
         WRITE(thisId,1090)'r8f',k
         IF(Rvtrfc.and.(.not.dpeq(Lam,ONE)))THEN
          CALL mkHeaderCellId(Mt1,0,0,thisId,'@','Transformed Forecast',
     &                        'Trns(Fcst)')
         ELSE
          CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@','Forecast')
         END IF
         WRITE(thisId,1090)'r8e',k
         CALL mkHeaderCellId(Mt1,0,0,thisId,'@','Forecast Error',
     &                      'Fcst. Error')
        END DO
        CALL writTag(Mt1,'</tr>')
       END IF
c-----------------------------------------------------------------------
       DO i=Begrev+Rfctlg(1),Endrev
        Revptr=i-Begrev+1
        j=j+1
c-----------------------------------------------------------------------
c     Calculate forcast errors, accumulated sum of squares.
c-----------------------------------------------------------------------
        ndef=0
        DO k=1,Nfctlg
         IF(Nfctlg.eq.1.or.(Nfctlg.gt.1.and.Rfctlg(k).le.j))THEN
          IF(Rvtrfc.and.(.not.dpeq(Lam,ONE)))THEN
           IF(dpeq(Lam,ZERO))THEN
            fcter(k)=log(Orig(i))-log(Cncfct(k,Revptr))
            fcttrn(k)=log(Cncfct(k,Revptr))
           ELSE IF(Fcntyp.eq.3)THEN
            tmp1=Orig(i)
            tmp2=Cncfct(k,Revptr)
            fcter(k)=log(tmp1/(ONE-tmp1))-log(tmp2/(ONE-tmp2))
            fcttrn(k)=log(tmp2/(ONE-tmp2))
           ELSE
            tmp1=Lam**2+(Orig(i)**Lam-ONE)/Lam
            tmp2=Lam**2+(Cncfct(k,Revptr)**Lam-ONE)/Lam
            fcter(k)=tmp1-tmp2
            fcttrn(k)=tmp2
           END IF
          ELSE
           fcter(k)=Orig(i)-Cncfct(k,Revptr)
           fcttrn(k)=Cncfct(k,Revptr)
          END IF
          ndef=ndef+1
         END IF
        END DO
c-----------------------------------------------------------------------
c     Print out concurrent forecast
c-----------------------------------------------------------------------
        CALL addate(begfct,Ny,(j-1),idate)
        CALL wrtdat(idate,Ny,str,ndtc)
        IF(Lfatal)RETURN
        IF(Savtab(Nptr+1).or.Lgraf)THEN
         ipos=1
         rdbdat=100*idate(YR)+idate(MO)
         CALL itoc(rdbdat,outstr,ipos)
         IF(Lfatal)RETURN
        END IF
        IF(Nfctlg.eq.ndef)THEN
         IF(Prttab(Nptr+1))THEN
          CALL writTag(Mt1,'<tr>')
          WRITE(thisId,1090)'r8d',j
          CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@',str(1:ndtc))
          DO k=1,Nfctlg
           WRITE(thisHdr,1110)'r8d',j,'r8l',k+Nfctlg,'r8f',k
           WRITE(thisVal,1120)fcttrn(k)
           CALL mkTableCellHeader(Mt1,thisHdr,'@',thisVal)
           WRITE(thisHdr,1110)'r8r',j,'r8l',k+Nfctlg,'r8e',k
           WRITE(thisVal,1120)fcter(k)
           CALL mkTableCellHeader(Mt1,thisHdr,'@',thisVal)
          END DO
          CALL writTag(Mt1,'</tr>')
         END IF
         IF(Savtab(Nptr+1).or.Lgraf)THEN
          DO k=1,Nfctlg
           outstr(ipos:ipos)=TABCHR
           ipos=ipos+1
           CALL dtoc(fcttrn(k),outstr,ipos)
           IF(Lfatal)RETURN
           outstr(ipos:ipos)=TABCHR
           ipos=ipos+1
           CALL dtoc(fcter(k),outstr,ipos)
           IF(Lfatal)RETURN
          END DO
         END IF
        ELSE
         IF(Prttab(Nptr+1))THEN
          CALL writTag(Mt1,'<tr>')
          WRITE(thisId,1090)'r8d',j
          CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@',str(1:ndtc))
          DO k=1,Nfctlg
           IF(k.gt.ndef)THEN
            CALL mkTableCell(Mt1,'@','&nbsp;')
            CALL mkTableCell(Mt1,'@','&nbsp;')
           ELSE
            WRITE(thisHdr,1110)'r8d',j,'r8l',k+Nfctlg,'r8f',k
            WRITE(thisVal,1120)fcttrn(k)
            CALL mkTableCellHeader(Mt1,thisHdr,'@',thisVal)
            WRITE(thisHdr,1110)'r8r',j,'r8l',k+Nfctlg,'r8e',k
            WRITE(thisVal,1120)fcter(k)
            CALL mkTableCellHeader(Mt1,thisHdr,'@',thisVal)
           END IF
          END DO
          CALL writTag(Mt1,'</tr>')
         END IF
         IF(Savtab(Nptr+1).or.Lgraf)THEN
          DO k=1,Nfctlg
           outstr(ipos:ipos)=TABCHR
           ipos=ipos+1
           IF(k.le.ndef)THEN
            CALL dtoc(fcttrn(k),outstr,ipos)
           ELSE
            CALL dtoc(ZERO,outstr,ipos)
           END IF
           IF(Lfatal)RETURN
           outstr(ipos:ipos)=TABCHR
           ipos=ipos+1
           IF(k.le.ndef)THEN
            CALL dtoc(fcter(k),outstr,ipos)
           ELSE
            CALL dtoc(ZERO,outstr,ipos)
           END IF
           IF(Lfatal)RETURN
          END DO
         END IF
        END IF
        IF(Savtab(Nptr+1))WRITE(fh,1130)outstr(1:ipos-1)
        IF(Lgraf)WRITE(fh2,1130)outstr(1:ipos-1)
       END DO
      END IF
      IF(Savtab(Nptr+1))CALL fclose(fh)
      IF(Lgraf)CALL fclose(fh2)
      IF(Prttab(Nptr+1))THEN
       CALL writTag(Mt1,'</table></div>')
       CALL mkPOneLine(Mt1,'@','&nbsp;')
      END IF
c-----------------------------------------------------------------------
      RETURN
c-----------------------------------------------------------------------
 1000 FORMAT('<div id="rv',i3.3,'">')
 1001 FORMAT('<div id="lgrv',i6.6,'">')
 1010 FORMAT('(a,',i1,'(a,a,i2.2,a))')
 1020 FORMAT('(a,',i1,'(a,a,i2.2,a,a,a,i2.2,a))')
 1030 FORMAT('(a,',i1,'(a,a))')
 1040 FORMAT('(a,',i1,'(a,a,a,a))')
 1080 FORMAT('Lead ',i2)
 1090 FORMAT(a,i3.3)
 1110 FORMAT(a,i3.3,2(' ',a,i3.3))
 1120 FORMAT(e17.10)
 1130 FORMAT(a)
 1140 FORMAT('rvfcstlag: ',6i3)
 1150 FORMAT('meanssfe:',6(2x,E17.10))
 1160 FORMAT('<th>Lead ',i3,' forecasts</th><td>',E17.10,'</td>')
c-----------------------------------------------------------------------
      END
