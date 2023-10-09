      SUBROUTINE prtukp(Mt,Iagr,Ny,Indiv,Lsvlg)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'arima.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'tukey.cmn'
      INCLUDE 'rho.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'htmlout.prm'
      INCLUDE 'htmlout.cmn'
      INCLUDE 'spctbl.i'
c-----------------------------------------------------------------------
      LOGICAL Lsvlg
      CHARACTER thisLb*(36),cPeak*(6),begstr*(10),endstr*(10),ich*(2)
      INTEGER Mt,Iagr,Indiv,i,k,nLb,nchr1,nchr2,Ny,ipos,iPeak
      DOUBLE PRECISION thisPk
      DIMENSION thisPk(7),cPeak(7),iPeak(7)
c-----------------------------------------------------------------------
      IF(Iagr.eq.4)THEN
       IF(.not.Lsvlg)CALL genSkip(LSPCTP+4000)
       CALL writTagOneLine(Mt,'h3','@','  Peak probabilities for '//
     &                 'Tukey spectrum estimator: Indirect adjustments')
      ELSE
       IF(.not.Lsvlg)CALL genSkip(LSPCTP)
       CALL writTagOneLine(Mt,'h3','@',
     &              '  Peak probabilities for Tukey spectrum estimator')
      END IF
c-----------------------------------------------------------------------
      CALL wrtdat(Bgspec,Ny,begstr,nchr1)
      IF(.not.Lfatal)CALL wrtdat(Endspn,Ny,endstr,nchr2)
      IF(Lfatal)RETURN
      CALL mkPOneLine(Mt,'@','Tukey spectrum estimated from '//
     &                begstr(1:nchr1)//' to '//endstr(1:nchr2)//'.')
c-----------------------------------------------------------------------
      Indiv=Indiv+1
      IF(Lsvlg)THEN
       WRITE(Mt,1030)'lgspc',Indiv
      ELSE
       WRITE(Mt,1030)'spc',Indiv
      END IF
c-----------------------------------------------------------------------
      IF(Iagr.eq.4)THEN
       CALL mkTableTag(Mt,'w60','Peak probabilities for '//
     &                 'Tukey spectrum estimator: Indirect adjustments')
       CALL mkCaption(Mt,'Peak probabilities for '//
     &                'Tukey spectrum estimator: Indirect adjustments')
      ELSE
       CALL mkTableTag(Mt,'w60','Peak probabilities for '//
     &                 'Tukey spectrum estimator')
       CALL mkCaption(Mt,'Peak probabilities for '//
     &                'Tukey spectrum estimator')
        END IF
      CALL writTag(Mt,'<tr>')
      CALL writTagOneLine(Mt,'td','head','&nbsp;')
        DO i=1,6
         write(ich,1050)i
         CALL mkHeaderCellScope(Mt,0,0,'col','Seasonal Frequency '//
     &                        ich(2:2),ich)
        END DO
      CALL mkHeaderCellScope(Mt,0,0,'col','Trading Day','TD')
      CALL writTag(Mt,'</tr>')
c-----------------------------------------------------------------------
      DO i=1,Ntukey
       CALL writTag(Mt,'<tr>')
       thisLb=' '
c-----------------------------------------------------------------------
c   Set up labels, peak vectors
c-----------------------------------------------------------------------
       IF(Itukey(i).eq.LSPCRS)THEN
        DO k=1,6
         thisPk(k)=Ptsr(k)
        END DO
        thisPk(7)=Pttdr
        nLb=16
        thisLb(1:nLb)=' Model Residuals'
       ELSE IF(Itukey(i).eq.LSPTS0.or.Itukey(i).eq.LSPT0C)THEN
        DO k=1,6
         thisPk(k)=Ptso(k)
        END DO
        thisPk(7)=Pttdo
        CALL mkspst(Spcsrs,thisLb,nLb,k,.true.)
       ELSE IF(Itukey(i).eq.LSPTS1.or.Itukey(i).eq.LSPT1I.or.
     &         Itukey(i).eq.LSPT1S)THEN
        DO k=1,6
         thisPk(k)=Ptsa(k)
        END DO
        thisPk(7)=Pttda
        IF(Itukey(i).eq.LSPTS1)THEN
         IF(Lrbstsa)THEN
          nLb=32
          thisLb(1:nLb)=' Seasonally adjusted series (E2)'
         ELSE
          nLb=33
          thisLb(1:nLb)=' Seasonally adjusted series (D11)'
         END IF
        ELSE IF(Itukey(i).eq.LSPT1I)THEN
         IF(Lrbstsa)THEN
          nLb=33
          thisLb(1:nLb)=' Ind. Seasonally adj. series (E2)'
         ELSE
          nLb=34
          thisLb(1:nLb)=' Ind. Seasonally adj. series (D11)'
         END IF
        ELSE IF(Itukey(i).eq.LSPT1S)THEN
         nLb=35
         thisLb(1:nLb)=' Seasonally adjusted series (SEATS)'
        END IF
       ELSE IF(Itukey(i).eq.LSPTS2.or.Itukey(i).eq.LSPT2I.or.
     &         Itukey(i).eq.LSPT2S)THEN
        DO k=1,6
         thisPk(k)=Ptsi(k)
        END DO
        thisPk(7)=Pttdi
        IF(Itukey(i).eq.LSPTS2)THEN
         IF(Lrbstsa)THEN
          nLb=24
          thisLb(1:nLb)=' Modified Irregular (E3)'
         ELSE
          nLb=16
          thisLb(1:nLb)=' Irregular (D13)'
         END IF
        ELSE IF(Itukey(i).eq.LSPT2I)THEN
         IF(Lrbstsa)THEN
          nLb=33
          thisLb(1:nLb)=' Indirect Modified Irregular (E3)'
         ELSE
          nLb=25
          thisLb(1:nLb)=' Indirect Irregular (D13)'
         END IF
        ELSE IF(Itukey(i).eq.LSPT2S)THEN
         IF(Lrbstsa)THEN
          nLb=29
          thisLb(1:nLb)=' Stochastic Irregular (SEATS)'
         ELSE
          nLb=18
          thisLb(1:nLb)=' Irregular (SEATS)'
         END IF
        END IF
       END IF
c-----------------------------------------------------------------------
c   Set up 
c-----------------------------------------------------------------------
       DO k=1,7
        IF(thisPk(k).gt.0.99D0)THEN
         cPeak(k)='  **  '
         iPeak(k)=PSTR2TP
        ELSE IF(thisPk(k).gt.0.90D0)THEN
         cPeak(k)='  *   '
         iPeak(k)=PSTR1TP
        ELSE
         cPeak(k)='&nbsp;'
         iPeak(k)=0
        END IF
       END DO
c-----------------------------------------------------------------------
c    Write out probabilities and peak labels for each seasonal freq,
c    trading day
c-----------------------------------------------------------------------
       CALL mkHeaderCellScope(Mt,0,0,'row','@',thisLb(1:nLb))
       IF(Lsvlg)then
        DO k=1,7
         CALL mkTableCell(Mt,'center',cPeak(k))
        END DO
       ELSE
        DO k=1,7
         IF(iPeak(k).eq.0)THEN
          WRITE(Mt,1010)thisPk(k)
         ELSE
          Infoot=Infoot+1
          Vfoot(Infoot)=iPeak(k)
          WRITE(Mt,1020)thisPk(k),Infoot,cPeak(k),Infoot,cPeak(k)
         END IF
        END DO
*        WRITE(Mt,1010)thisLb,(thisPk(k),k=1,6),thisTD
*        WRITE(Mt,1020)(cPeak(k),k=1,6),cPeak(7)
       END IF
       CALL writTag(Mt,'</tr>')
      END DO
      CALL writTag(Mt,'</table></div>')
      CALL mkPOneLine(Mt,'@','&nbsp;')
c-----------------------------------------------------------------------
c      CALL writTag(Mt,Charhr)
      CALL mkPOneLine(Mt,'@','&nbsp; ----------'//Cbr//
     &       '&nbsp; ** - Peak Probability > 0.99,'//Cbr//
     &       '&nbsp; * - 0.90 < Peak Probability < 0.99')
c-----------------------------------------------------------------------
 1010 FORMAT('<td class="center">',F6.3,'</td>')
 1020 FORMAT('<td class="center">',f6.3,/,'<a href="#footnote',i4.4,
     &       '" class="longdesc">','Link to definition of ',a,'</a>',/
     &       '<a name="foot',i4.4,'"></a>',a,'</td>')
 1030 FORMAT('<div id="',a,i4.4,'">')
 1050 FORMAT('S',i1)
c-----------------------------------------------------------------------
      RETURN
      END
