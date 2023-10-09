C     Last change:  Mar. 2021, add Sliding span: in the header at li-48
C     previous change:  BCM  15 Oct 1998    1:08 pm
      SUBROUTINE pctrit(Ex,Tagr,Muladd,Nsea,Eststr,Nstr,Ntot,Itot,Cut,
     &                  Mqq,Nmqq,Chrarg,Ncarg,Lprt,Lsav,Lprtyy,Lsavyy)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Print out percent of observations flagged as extremes 
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'ssap.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'htmlout.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'svllog.prm'
      INCLUDE 'svllog.cmn'
      INCLUDE 'dgnsvl.i'
c-----------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c-----------------------------------------------------------------------
      CHARACTER Ex*(2),Eststr*(45),Mqq*(7),Chrarg*(31)
      LOGICAL Lprt,Lsav,lhdr,Lprtyy,Lsavyy
      DOUBLE PRECISION fper,Cut
      INTEGER i,Ntot,Itot,iext,Nstr,Tagr,Nsea,Muladd,Nmqq,Ncarg
      DIMENSION Ntot(NEST),Ex(2*NEST),Itot(NEST),Eststr(NEST),
     &          Nstr(NEST),Cut(NEST,4)
c-----------------------------------------------------------------------
      LOGICAL istrue
      EXTERNAL istrue
c-----------------------------------------------------------------------
c     For each estimate, check to see if the number of months flagged
c     has been reset.
c-----------------------------------------------------------------------
      IF(.NOT.(Lprt.or.Lsav.or.Lprtyy.or.Lsavyy.or.Svltab(LSLPCT)))
     &   RETURN
      lhdr=.true.
      IF(Lprt.or.Lprtyy)THEN
       Inspn=Inspn+1
       WRITE(Mt1,1010)Inspn
       CALL mkTableTag(Mt1,'w60','@')
       CALL mkCaption(Mt1,'Percentage of '//Mqq(1:Nmqq)//
     &                's flagged as unstable'//Chrarg(1:Ncarg))
      END IF
      IF(Svltab(LSLPCT))THEN
       Inlgfl=Inlgfl+1
       WRITE(Ng,1020)Inlgfl
       CALL mkTableTag(Ng,'w60','@')
       CALL mkCaption(Ng,'Sliding Spans: Percentage of '//Mqq(1:Nmqq)//
     &                's flagged as unstable'//Chrarg(1:Ncarg))
      END IF
c-----------------------------------------------------------------------
      DO i=1,NEST-1
       IF(Ntot(i).ne.NOTSET)THEN
c-----------------------------------------------------------------------
c     compute percent of months flagged
c-----------------------------------------------------------------------
        fper=(dble(Ntot(i))/dble(Itot(i)))*100D0
c-----------------------------------------------------------------------
c     Print out percentage for a given estimate, if requested
c-----------------------------------------------------------------------
        IF(Lprt)THEN
         CALL mkSSPctFlag(Mt1,Eststr(i)(1:Nstr(i)),Ntot(i),Itot(i),
     &                    fper,T)
        END IF
c-----------------------------------------------------------------------
c     Save percentage of months flagged
c-----------------------------------------------------------------------
        IF(Lsav)THEN
         iext=Tagr+(2*i)-1
         WRITE(Nform,1040)Ex(iext)(1:(Tagr+1)),Ntot(i),Itot(i),fper
        END IF
c-----------------------------------------------------------------------
c     Save percent flagged in log, if requested
c-----------------------------------------------------------------------
        IF(Svltab(LSLPCT))THEN
         CALL mkSSPctFlag(Ng,Eststr(i)(1:Nstr(i)),Ntot(i),Itot(i),
     &                    fper,T)
        END IF
       END IF
      END DO
c-----------------------------------------------------------------------
c     compute percent of months flagged for year-to-year changes
c-----------------------------------------------------------------------
      IF(Lprtyy.or.Lsavyy)THEN
       fper=(dble(Ntot(i))/dble(Itot(i)))*100D0
c-----------------------------------------------------------------------
c     Print out percentage for year-to-year changes, if requested
c-----------------------------------------------------------------------
       IF(Lprtyy)
     &   CALL mkSSPctFlag(Mt1,Eststr(i)(1:Nstr(i)),Ntot(i),Itot(i),
     &                    fper,T)
c-----------------------------------------------------------------------
c     Save percentage of months flagged for year-to-year changes
c-----------------------------------------------------------------------
       IF(Lsavyy)THEN
        iext=Tagr+(2*i)-1
        WRITE(Nform,1040)Ex(iext)(1:(Tagr+1)),Ntot(i),Itot(i),fper
       END IF
c-----------------------------------------------------------------------
c     Save percent flagged in log for year-to-year changes, if requested
c-----------------------------------------------------------------------
       IF(Svltab(LSLPCT))THEN
        CALL mkSSPctFlag(Ng,Eststr(NEST)(1:Nstr(NEST)),Ntot(NEST),
     &                   Itot(NEST),fper,T)
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(Lprt.or.Lprtyy)THEN
       CALL writTag(Mt1,'</table></div>')
       CALL mkPOneLine(Mt1,'@','&nbsp;')
      END IF
      IF(Svltab(LSLPCT))THEN
       CALL writTag(Ng,'</table></div>')
       CALL mkPOneLine(Ng,'@','&nbsp;')
      END IF
c-----------------------------------------------------------------------
c     Print message on suggested threshold values
c-----------------------------------------------------------------------
      IF(.not.Lprt)RETURN
      Inspn=Inspn+1
      WRITE(Mt1,1010)Inspn
      CALL mkTableTag(Mt1,'w65','@')
      CALL mkCaption(Mt1,'Recommended limits for percentages')
      IF(Muladd.eq.0)THEN
       IF(Ntot(1).eq.NOTSET)THEN
        CALL mkSSPctOneLine(Mt1,Eststr(4),'35% is too high',
     &       '40% is much too high')
       ELSE
        CALL mkSSPctOneLine(Mt1,Eststr(1),'15% is too high',
     &       '25% is much too high')
        CALL mkSSPctOneLine(Mt1,Eststr(4),'35% is too high',
     &       '40% is much too high')
       END IF
       IF(Lprtyy)
     &    CALL mkSSPctOneLine(Mt1,Eststr(5),'10% is too high','@')
      ELSE
        CALL mkSSPctOneLine(Mt1,Eststr(1),'15% is too high',
     &       '25% is much too high')
        CALL mkSSPctOneLine(Mt1,Eststr(4),'35% is too high',
     &       '40% is much too high')
       IF(Lprtyy)
     &    CALL mkSSPctOneLine(Mt1,Eststr(5),'10% is too high','@')
      END IF
      CALL writTag(Mt1,'</table></div>')
      CALL mkPOneLine(Mt1,'@','&nbsp;')
c-----------------------------------------------------------------------
c     Print message on threshold values used in the analysis
c-----------------------------------------------------------------------
      Inspn=Inspn+1
      WRITE(Mt1,1010)Inspn
      CALL mkTableTag(Mt1,'w65','@')
      IF(Nsea.eq.12)THEN
       CALL mkCaption(Mt1,
     &                'Threshold values used for Maximum Percent '//
     &                'Differences to flag months as unstable')
      ELSE
       CALL mkCaption(Mt1,
     &                'Threshold values used for Maximum Percent '//
     &                'Differences to flag quarters as unstable')
      END IF
      DO i=1,NEST-1
       IF(Ntot(i).ne.NOTSET)
     &   CALL mkSSPctFlag(Mt1,Eststr(i)(1:Nstr(i)),0,0,Cut(i,1),F)
      END DO
      IF(Ntot(i).ne.NOTSET.and.Lprtyy)
     &   CALL mkSSPctFlag(Mt1,Eststr(NEST)(1:Nstr(NEST)),0,0,
     &                    Cut(NEST,1),F)
c-----------------------------------------------------------------------
      CALL writTag(Mt1,'</table></div>')
      CALL mkPOneLine(Mt1,'@','&nbsp;')
c-----------------------------------------------------------------------
 1010 FORMAT('<div id="ssp',i3.3,'">')
 1020 FORMAT('<div id="lgssp',i6.6,'">')
 1040 FORMAT('s2.',a,'.per: ',i3,2x,i3,2x,f7.3)
c-----------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE mkSSPctOneLine(Fh,thisLbl,thisText1,thisText2)
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
      CHARACTER thisLbl*(*),thisText1*(*),thisText2*(*)
      INTEGER Fh
c-----------------------------------------------------------------------
      CALL writTag(Fh,'<tr>')
      CALL mkHeaderCellScope(fh,0,0,'row','@',thisLbl)
      IF(thisText2.eq.'@')THEN
       CALL mkTableCellSpan(Fh,'col',2,'center',thisText1)
      ELSE
       CALL mkTableCell(Fh,'center',thisText1)
       CALL mkTableCell(Fh,'center',thisText2)
      END IF
      CALL writTag(Fh,'</tr>')
c-----------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE mkSSPctFlag(Fh,thisLbl,i1,i2,thisReal,isPct)
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
      CHARACTER thisLbl*(*)
      DOUBLE PRECISION thisReal
      INTEGER Fh,i1,i2
      LOGICAL isPct
c-----------------------------------------------------------------------
      CALL writTag(Fh,'<tr>')
      CALL mkHeaderCellScope(fh,0,0,'row','@',thisLbl)
      IF(isPct)THEN
       write(Fh,1010)i1,i2,thisReal
      ELSE
       write(Fh,1020)thisReal
      END IF
      CALL writTag(Fh,'</tr>')
c-----------------------------------------------------------------------
 1010 FORMAT('<td>',i3,' out of ',i3,' (',f5.1,' %)</td>')
 1020 FORMAT('<td>Threshold = ',f5.1,' %</td>')
c-----------------------------------------------------------------------
      RETURN
      END
