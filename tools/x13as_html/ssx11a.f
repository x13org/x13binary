C     Last change:  BCM  16 Feb 1999    3:56 pm
**==ssx11a.f    processed by SPAG 4.03F  at 10:07 on  4 Oct 1994
      SUBROUTINE ssx11a(Ijk,Lmodel,Lx11,Lseats,Msr,Ncol,Nlen,Ixreg,
     &                  Otlfix,Ssinit,Ssxotl,Ssxint)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C     This subroutine sets up arrays and variables for the sliding
c     spans adjustment, if such an analysis is requested.  Options
c     are preserved where necessary. The transparent (no print out)
c     seasonal adjustment runs needed to perform the sliding spans
c     analysis are performed, and the sliding spans subroutines are
c     called.  If a sliding spans analysis is not selected by the
c     user, then the series is seasonally adjusted.
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'ssap.prm'
      INCLUDE 'ssap.cmn'
      INCLUDE 'ssft.cmn'
      INCLUDE 'lzero.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'otlrev.cmn'
      INCLUDE 'otxrev.cmn'
      INCLUDE 'missng.cmn'
      INCLUDE 'x11reg.cmn'
      INCLUDE 'xrgmdl.cmn'
      INCLUDE 'inpt.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'xeastr.cmn'
      INCLUDE 'orisrs.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
c-----------------------------------------------------------------------
      CHARACTER str*(PCOLCR)
      INTEGER i,Ijk,Ncol,Msr,Nlen,igrp,begcol,i1,i2,nchr,otltyp,begotl,
     &        endotl,lastsy,endcol,Ixreg,oldfx,oldxfx,ircol,obeg,oend,
     &        Ssinit
      LOGICAL Lmodel,locok,Lx11,Lseats,lidotl,Otlfix,Ssxotl,Ssxint
C-----------------------------------------------------------------------
      Icol=Ijk
      Nfcst=Posffc-Posfob
      IF(Ijk.eq.1)THEN
       oldfx=Iregfx
       oldxfx=Irgxfx
      ELSE
       Iregfx=oldfx
       Irgxfx=oldxfx
      END IF
c-----------------------------------------------------------------------
c     Reset length of series, series + forecasts, turn off identify
c     option
c-----------------------------------------------------------------------
      Nspobs=Nlen
      Nofpob=Nspobs+Nfcst
      Nbfpob=Nspobs+Nfcst+Nbcst
c-----------------------------------------------------------------------
C     SET UP BEGINNING, ENDING DATES and X11 pointers FOR SEASONAL
C     ADJUSTMENT OF SPAN IJK.
c-----------------------------------------------------------------------
      Lsp=L0+(Ijk-1)*Ny+Im-Nbcst2-1
      CALL setxpt(Nfcst,Lx11.or.Lseats,Fctdrp)
      Ly0=Lyr+(Pos1ob/Ny)
      IF(mod(Pos1ob,Ny).eq.0)Ly0=Ly0-1
      Lstyr=Lyr+(Posfob/Ny)
      IF(mod(Posfob,Ny).eq.0)Lstyr=Lstyr-1
c-----------------------------------------------------------------------
c     Set Span Beginning, Ending Dates for modeling routines
c-----------------------------------------------------------------------
      Begspn(MO)=Im
      Begspn(YR)=Ly0
      Endspn(MO)=mod(Posfob,Ny)
      IF(Endspn(MO).eq.0)Endspn(MO)=Ny
      Endspn(YR)=Lstyr
      CALL dfdate(Begspn,Begsrs,Sp,Frstsy)
      Frstsy=Frstsy+1
      Nomnfy=Nobs-Frstsy+1
      Nobspf=min(Nspobs+max(Nfcst-Fctdrp,0),Nomnfy)
      CALL dfdate(Endspn,Begsrs,Sp,lastsy)
      lastsy=lastsy+1
      CALL cpyint(Begspn,2,1,Begmdl)
      CALL cpyint(Endspn,2,1,Endmdl)
c-----------------------------------------------------------------------
c    Set variables for irregular regression.
c-----------------------------------------------------------------------
      IF(Ixreg.gt.0)THEN
       Ixreg=1
       IF(Lmodel)Ixreg=2
       CALL cpyint(Begspn,2,1,Begxrg)
       CALL cpyint(Endspn,2,1,Endxrg)
c-----------------------------------------------------------------------
       CALL loadxr(F)
c-----------------------------------------------------------------------
c    If automatic AO outlier identification is redone for each span,
c    then delete automatic outliers from irregular regression model
c-----------------------------------------------------------------------
       IF(Ssxotl)THEN
        CALL cpyint(Begspn,2,1,Begxot)
        CALL cpyint(Endspn,2,1,Endxot) 
        DO igrp=Ngrp,1,-1
         begcol=Grp(igrp-1)
         endcol=Grp(igrp)-1
         IF(Rgvrtp(begcol).eq.PRGTAA)THEN
          ircol=endcol
          DO WHILE (ircol.ge.begcol)
           CALL dlrgef(ircol,Nrxy,1)
           IF(Lfatal)RETURN
           ircol=ircol-1
          END DO
         END IF
        END DO
       ELSE IF(Notxtl.gt.0)THEN
        CALL dfdate(Begspn,Begxrg,Sp,obeg)
        obeg=obeg+1
        CALL dfdate(Endspn,Begxrg,Sp,oend)
        oend=oend+1
        DO igrp=Ngrp,1,-1
         begcol=Grp(igrp-1)
         endcol=Grp(igrp)-1
         IF(Rgvrtp(begcol).eq.PRGTAA.or.Rgvrtp(begcol).eq.PRGTAO)THEN
          ircol=endcol
          DO WHILE (ircol.ge.begcol)
           CALL getstr(Colttl,Colptr,Ncoltl,ircol,str,nchr)
           IF(Lfatal)RETURN
           CALL rdotlr(str(1:nchr),Begxrg,Sp,otltyp,begotl,endotl,locok)
           IF(.not.locok)THEN
            CALL abend
            RETURN
           END IF
           IF(begotl.lt.obeg.or.begotl.gt.oend)THEN
            CALL dlrgef(ircol,Nrxy,1)
            IF(Lfatal)RETURN
           END IF
           ircol=ircol-1
          END DO
         END IF
        END DO
c-----------------------------------------------------------------------
c    See if automatic outliers identified from full series can be
c    added to irregular regression model.
c-----------------------------------------------------------------------
        CALL adotss(Botx,Otxptr,Notxtl,Fixotx,Otxttl,lastsy,
     &              Otlfix.or.Ssxint)
       END IF
       CALL loadxr(T)
c-----------------------------------------------------------------------
      END IF
c-----------------------------------------------------------------------
      IF(Msr.eq.5)THEN
       Lmsr=Lterm
       Lterm=6
      END IF
      CALL restor(Lmodel,Lx11,Ixreg.gt.0)
c-----------------------------------------------------------------------
c     Reset missing value code.
c-----------------------------------------------------------------------
      IF(Missng)Missng=F
c-----------------------------------------------------------------------
C     RESET VALUES FOR ORIGINAL SERIES.
c-----------------------------------------------------------------------
      CALL copy(Orig(Pos1ob),Nspobs,-1,Stcsi(Pos1ob))
      CALL copy(Orig(Pos1ob),Nspobs,-1,Series(Pos1ob))
c-----------------------------------------------------------------------
c     Set logical variable that generates X-11 holiday date indicator
c     variable
c-----------------------------------------------------------------------
      IF(Lgenx)THEN
       Lgenx=F
c-----------------------------------------------------------------------
c     Check to see if easter adjustment can be done in all spans,
c     if specified.
c-----------------------------------------------------------------------
       IF(Keastr.eq.1)THEN
        i1=(Pos1bk/12)*12+3
        IF(i1.lt.Pos1bk)i1=i1+12
        DO i=1,Ncol
         IF(i.gt.1)i1=i1+12
         i2=(Pos1bk+Nlen-1)+(i-1)*12
         CALL chkeas(i1,i2)
c-----------------------------------------------------------------------
c     Print error messages and turn off easter adjustment if X-11
c     easter adjustment cannot be done.
c-----------------------------------------------------------------------
         IF((Ieast(1)*Ieast(2)*Ieast(3)*Ieast(4)).eq.0.and.(Keastr.eq.1)
     &      )THEN
          CALL errhdr
          WRITE(Mt2,1030)' due to:'
          WRITE(STDERR,1030)'.'
          Keastr=0
         END IF
         IF(Ieast(1).eq.0)WRITE(Mt2,1040)Cbr,i
         IF(Ieast(2).eq.0)WRITE(Mt2,1050)Cbr,i
         IF(Ieast(3).eq.0)WRITE(Mt2,1060)Cbr,i
         IF(Ieast(4).eq.0)WRITE(Mt2,1070)Cbr,i
        END DO
        CALL writTag(Mt2,'</p>')
        IF(Keastr.eq.0)THEN
         CALL writln('     Either choose a longer span for the '//
     &               'sliding spans analysis or',Mt2,STDERR,T,F)
         CALL writln('     preadjust the series using Easter '//
     &               'effects estimated from a regARIMA model.',
     &               Mt2,STDERR,F,T)
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
c     Check to see if X-11 holiday adjustment can still be done.
c-----------------------------------------------------------------------
      IF(Khol.eq.2)Khol=Keastr
c-----------------------------------------------------------------------
c      ilyr=Iyr+Ijk-1
c      Lcyr=ilyr
c      Lfdc=Pos1bk
c      Layr=ilyr
c      Lfdr=Pos1bk
c-----------------------------------------------------------------------
c     Check to see if outliers in model are no longer in the span.
c-----------------------------------------------------------------------
      IF(Lmodel)THEN
*       lidotl=Ltstao.or.Ltstls.or.Ltsttc.or.Ltstso
       lidotl=Ltstao.or.Ltstls.or.Ltsttc
       IF(lidotl)THEN
        CALL cpyint(Begspn,2,1,Begtst)
        CALL cpyint(Endspn,2,1,Endtst)
       END IF
       IF(Ngrp.gt.0)THEN
        CALL dfdate(Begspn,Begxy,Sp,obeg)
        obeg=obeg+1
        CALL dfdate(Endspn,Begxy,Sp,oend)
        oend=oend+1
        DO igrp=Ngrp,1,-1
         begcol=Grp(igrp-1)
         endcol=Grp(igrp)-1
         IF(Rgvrtp(begcol).eq.PRGTAA.or.Rgvrtp(begcol).eq.PRGTAO.or.
     &      Rgvrtp(begcol).eq.PRGTAL.or.Rgvrtp(begcol).eq.PRGTLS.or.
     &      Rgvrtp(begcol).eq.PRGTAT.or.Rgvrtp(begcol).eq.PRGTTC.or.
     &      Rgvrtp(begcol).eq.PRGTQI.or.Rgvrtp(begcol).eq.PRGTQD.or.     
     &      Rgvrtp(begcol).eq.PRGTSO.or.     
     &      Rgvrtp(begcol).eq.PRGTRP.or.Rgvrtp(begcol).eq.PRGTTL)THEN
          ircol=endcol
          DO WHILE (ircol.ge.begcol)
           CALL getstr(Colttl,Colptr,Ncoltl,ircol,str,nchr)
           IF(Lfatal)RETURN
           CALL rdotlr(str(1:nchr),Begxy,Sp,otltyp,begotl,endotl,locok)
           IF(.not.locok)THEN
            CALL abend
            RETURN
           END IF
           IF(((otltyp.eq.RP.or.otltyp.eq.TLS.or.otltyp.eq.QI.or.
     &          otltyp.eq.QD).and.(begotl.ge.obeg.or.endotl.le.oend))
     &    .or.((otltyp.ne.RP.and.otltyp.ne.TLS.and.otltyp.ne.QI.and.
     &          otltyp.ne.QD).and.(begotl.lt.obeg.or.
     &          begotl.gt.oend)))THEN
            CALL dlrgef(ircol,Nrxy,1)
            IF(Lfatal)RETURN
           END IF
           ircol=ircol-1
          END DO
         END IF
        END DO
       END IF
c-----------------------------------------------------------------------
c     Check to see if outliers stored previously can be put back into
c     the regression matrix.
c-----------------------------------------------------------------------
       CALL adotss(Botr,Otrptr,Notrtl,Fixotr,Otrttl,lastsy,
     &             Otlfix.or.Ssinit.eq.1)
      END IF
c-----------------------------------------------------------------------
 1030 FORMAT(/,'<p>Easter adjustment cannot be performed during the ',
     &       'sliding spans analysis',a)
 1040 FORMAT(a,'No years with Easter before April 1st in span ',i1,
     &       '.')
 1050 FORMAT(a,'No years with Easter after April 16th in span ',i1,
     &       '.')
 1060 FORMAT(a,'No years with Easter between April 2nd and April ',
     &       '8th in span ',i1,'.')
 1070 FORMAT(a,'No years with Easter between April 8th and April ',
     &       '15th in span ',i1,'.')
 1080 FORMAT(a,'Either choose a longer span for the sliding spans ',
     &       'analysis or',/,'preadjust the series using Easter ',
     &       'effects estimated from a regARIMA model.')
c-----------------------------------------------------------------------
      RETURN
      END
