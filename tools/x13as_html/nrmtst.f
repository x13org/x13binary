C     Last change:  BCM   2 Oct 1998    8:58 am
      SUBROUTINE nrmtst(Y,Nobs,Lprt,Lsav,Lsavlg)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     This routine generates the kurtosis and skewness of the residuals
c     to test the normality of the residuals.
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'units.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      LOGICAL T,F
      DOUBLE PRECISION ZERO,SIX
      PARAMETER(ZERO=0D0,SIX=6D0,T=.TRUE.,F=.false.)
c-----------------------------------------------------------------------
      DOUBLE PRECISION Y,ga,m2,m3,m4,ymu,ymymu,ppu,ppl,ykurt,dnobs,
     &                 yskew,seskew
      INTEGER i,ppi,Nobs
      LOGICAL Lprt,Lsav,Lsavlg,sig,sigskw
      DIMENSION Y(*)
c-----------------------------------------------------------------------
      DOUBLE PRECISION intrpp,totals
      EXTERNAL intrpp,totals
c-----------------------------------------------------------------------
      INCLUDE 'nrmtst.var'
c-----------------------------------------------------------------------
c     Compute Geary's a, Kurtosis statistic
c-----------------------------------------------------------------------
      IF(Lprt)CALL mkPClass(Mt1,'indent')
      IF(Lsavlg)THEN
       Inlgfl=Inlgfl+1
       WRITE(Ng,1000)Inlgfl
       CALL mkTableTag(Ng,'w60','regARIMA Normality Statistics')
       CALL mkCaption(Ng,'regARIMA Normality Statistics')
      END IF
      sig=F
      sigskw=F
      ymu = totals(Y,1,Nobs,1,1)
      ga=ZERO
      m2=ZERO
      m3=ZERO
      m4=ZERO
      dnobs=DBLE(NOBS)
      ppu=ZERO
      DO i = 1, Nobs
       ymymu = (Y(i) - ymu)
       ga = ga + (ABS(ymymu) / dnobs)
       m2 = m2 + ((ymymu * ymymu) / dnobs)
       m3 = m3 + ((ymymu * ymymu* ymymu) / dnobs)
       m4 = m4 + ((ymymu * ymymu * ymymu * ymymu) / dnobs)
      END DO
      ga = ga / SQRT(m2)
      ykurt = (m4 / (m2*m2))
      yskew = m3 / (m2 * SQRT(m2))
c-----------------------------------------------------------------------
c     Begin print out of normality diagnostics
c-----------------------------------------------------------------------
      IF(Lprt)WRITE(Mt1,1010)Nobs
c-----------------------------------------------------------------------
c     Compute percentage points for skewness statistic
c-----------------------------------------------------------------------
      IF(Nobs.lt.25)THEN
       IF(Lprt)CALL writTag(Mt1,'</p>')
       IF(Lsavlg)THEN
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellScope(Ng,0,0,'row','@','Skewness coefficient')
        CALL mkTableCell(Ng,'@','Cannot compute the significance of '//
     &                   'the skewness statistic on less than 25 '//
     &                   'observations.')
        CALL writTag(Ng,'</tr>')
        CALL writTag(Ng,'</table></div>')
        CALL mkPOneLine(Ng,'@','&nbsp;')
       END IF
       CALL nWritln('The program cannot compute the significance of '//
     &              'the skewness statistic on less than 25 '//
     &              'observations.',Mt1,Mt2,T,F)
       RETURN
      ELSE IF(Nobs.ge.25.and.Nobs.lt.50)THEN
       ppi = ((Nobs - 25) / 5) + 1
       ppu = intrpp(spp1,ns1,Nobs,ppi,PSKW1,F)
      ELSE IF(Nobs.ge.50.and.Nobs.lt.100)THEN
       ppi = ((Nobs - 50) / 10) + 1
       ppu = intrpp(spp2,ns2,Nobs,ppi,PSKW2,F)
      ELSE IF(Nobs.ge.100.and.Nobs.lt.200)THEN
       ppi = ((Nobs - 100) / 25) + 1
       ppu = intrpp(spp3,ns3,Nobs,ppi,PSKW3,F)
      ELSE IF(Nobs.ge.200.and.Nobs.lt.500)THEN
       ppi = ((Nobs - 200) / 50) + 1
       ppu = intrpp(spp4,ns4,Nobs,ppi,PSKW4,F)
      ELSE IF(Nobs.ge.500)THEN
       seskew = sqrt(SIX/dnobs)
       ppu = 2.326D0 * seskew
      END IF
      ppl = ZERO-ppu
c-----------------------------------------------------------------------
c     Print out and save (if necessary) skewness 
c-----------------------------------------------------------------------
      IF(yskew.lt.ppl)THEN
       sigskw=T
       IF(Lprt)WRITE(Mt1,1020)Cbr//'Skewness coefficient',yskew,
     &           '(significant negative skewness at one percent level)'
       IF(Lsav)WRITE(Nform,1030)'skewness',yskew,'-'
       IF(Lsavlg)THEN
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellScope(Ng,0,0,'row','@',' Skewness coefficient')
        WRITE(Ng,1040)yskew,Cbr//'(significant negative skewness)'
        CALL writTag(Ng,'</tr>')
       END IF
      ELSE IF(yskew.gt.ppu)THEN
       sigskw=T
       IF(Lprt)WRITE(Mt1,1020)Cbr//'Skewness coefficient',yskew,
     &           '(significant positive skewness at one percent level)'
       IF(Lsav)WRITE(Nform,1030)'skewness',yskew,'+'
       IF(Lsavlg)THEN
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellScope(Ng,0,0,'row','@',' Skewness coefficient')
        WRITE(Ng,1040)yskew,Cbr//'(significant positive skewness)'
        CALL writTag(Ng,'</tr>')
       END IF
      ELSE
       IF(Lprt)WRITE(Mt1,1020)Cbr//'Skewness coefficient',yskew,' '
       IF(Lsav)WRITE(Nform,1030)'skewness',yskew,' '
       IF(Lsavlg)THEN
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellScope(Ng,0,0,'row','@',' Skewness coefficient')
        WRITE(Ng,1040)yskew,' '
        CALL writTag(Ng,'</tr>')
       END IF
      END IF
c-----------------------------------------------------------------------
c     Compute percentage points for Geary's a statistic
c-----------------------------------------------------------------------
      IF(Nobs.lt.11)THEN
       IF(Lprt)CALL writTag(Mt1,'</p>')
       IF(Lsavlg)THEN
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellScope(Ng,0,0,'row','@',' Geary''s a')
        CALL mkTableCell(Ng,'@','Cannot compute the significance of '//
     &             'Geary''s a statistic on less than 11 observations.')
        CALL writTag(Ng,'</tr>')
        CALL writTag(Ng,'</table></div>')
        CALL mkPOneLine(Ng,'@','&nbsp;')
       END IF
       CALL nWritln('The program cannot compute the significance of '//
     &              'Geary''s a statistic on less than 11 '//
     &              'observations.',Mt1,Mt2,T,F)
       RETURN
      ELSE IF(Nobs.ge.11.and.Nobs.lt.41)THEN
       ppi = ((Nobs - 11) / 5) + 1
       ppu = intrpp(app1u,na1,Nobs,ppi,PAPP1,T)
       ppl = intrpp(app1l,na1,Nobs,ppi,PAPP1,T)
      ELSE IF(Nobs.ge.41.and.Nobs.lt.101)THEN
       IF(Nobs.eq.46)THEN
        ppu = app1u(8)
        ppl = app1l(8)
       ELSE
        ppi = ((Nobs - 41) / 10) + 1
        ppu = intrpp(app2u,na2,Nobs,ppi,PAPP2,Nobs.lt.81)
        ppl = intrpp(app2l,na2,Nobs,ppi,PAPP2,Nobs.lt.81)
       END IF
      ELSE IF(Nobs.ge.101.and.Nobs.le.1001)THEN
       ppi = ((Nobs - 101) / 100) + 1
       ppu = intrpp(app3u,na3,Nobs,ppi,PAPP3,Nobs.lt.801)
       ppl = intrpp(app3l,na3,Nobs,ppi,PAPP3,Nobs.lt.801)
      ELSE IF(Nobs.gt.1001)THEN
       IF(Lprt)CALL writTag(Mt1,'</p>')
       IF(Lsavlg)THEN
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellScope(Ng,0,0,'row','@',' Geary''s a')
        CALL mkTableCell(Ng,'@','Cannot compute the significance of '//
     &           'Geary''s a statistic on more than 1001 observations.')
        CALL writTag(Ng,'</tr>')
        CALL writTag(Ng,'</table></div>')
        CALL mkPOneLine(Ng,'@','&nbsp;')
       END IF
       CALL nWritln('The program cannot compute the significance of '//
     &              'Geary''s a statistic on more than 1001 '//
     &              'observations.',Mt1,Mt2,F,T)
       RETURN
      END IF
c-----------------------------------------------------------------------
c     Print out and save (if necessary) Geary's a statistic
c-----------------------------------------------------------------------
      IF(ga.lt.ppl.or.ga.gt.ppu)THEN
       sig=T
       IF(Lprt)WRITE(Mt1,1020)Cbr//'Geary''s a',ga,
     &                 '(significant at one percent level)'//Cbr
       IF(Lsav)WRITE(Nform,1030)'a',ga,'*'
       IF(Lsavlg)THEN
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellScope(Ng,0,0,'row','@',' Geary''s a statistic')
        WRITE(Ng,1040)ga,Cbr//'(significant)'
        CALL writTag(Ng,'</tr>')
       END IF
      ELSE
       IF(Lprt)WRITE(Mt1,1020)Cbr//'Geary''s a',ga,' '
       IF(Lsav)WRITE(Nform,1030)'a',ga,' '
       IF(Lsavlg)THEN
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellScope(Ng,0,0,'row','@',' Geary''s a statistic')
        WRITE(Ng,1040)ga,' '
        CALL writTag(Ng,'</tr>')
       END IF
      END IF
c-----------------------------------------------------------------------
c     Compute percentage points for Kurtosis statistic
c-----------------------------------------------------------------------
      IF(Nobs.lt.50)THEN
       IF(Lprt)CALL writTag(Mt1,'</p>')
       IF(Lsavlg)THEN
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellScope(Ng,0,0,'row','@',' Kurtosis')
        CALL mkTableCell(Ng,'@','Cannot perform hypothesis tests '//
     &                'for kurtosis on less than 50 observations.')
        CALL writTag(Ng,'</tr>')
        CALL writTag(Ng,'</table></div>')
        CALL mkPOneLine(Ng,'@','&nbsp;')
       END IF
       CALL nWritln('The program cannot perform hypothesis tests for '//
     &              'kurtosis on less than 50 observations.',
     &              Mt1,Mt2,F,T)
       RETURN
      ELSE IF(Nobs.ge.50.and.Nobs.lt.100)THEN
       ppi = ((Nobs - 50) / 25) + 1
       ppu = intrpp(kpp1u,nk1,Nobs,ppi,PKPP1,T)
       ppl = intrpp(kpp1l,nk1,Nobs,ppi,PKPP1,T)
      ELSE IF(Nobs.ge.100.and.Nobs.le.1000)THEN
       IF(Nobs.eq.125)THEN
        ppu = kpp1u(5)
        ppl = kpp1l(5)
       ELSE
        ppi = ((Nobs - 100) / 50) + 1
        ppu = intrpp(kpp2u,nk2,Nobs,ppi,PKPP2,Nobs.lt.900)
        ppl = intrpp(kpp2l,nk2,Nobs,ppi,PKPP2,Nobs.lt.900)
       END IF
      ELSE IF(Nobs.ge.1001)THEN
       IF(Lprt)CALL writTag(Mt1,'</p>')
       IF(Lsavlg)THEN
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellScope(Ng,0,0,'row','@',' Kurtosis')
        CALL mkTableCell(Ng,'@','Cannot perform hypothesis tests '//
     &                'for kurtosis on more than 1000 observations.')
        CALL writTag(Ng,'</tr>')
        CALL writTag(Ng,'</table></div>')
        CALL mkPOneLine(Ng,'@','&nbsp;')
       END IF
       CALL nWritln('The program cannot perform hypothesis tests for '//
     &              'kurtosis on more than 1000 observations.',
     &              Mt1,Mt2,F,T)
       RETURN
      END IF
c-----------------------------------------------------------------------
c     Print out and save (if necessary) Kurtosis
c-----------------------------------------------------------------------
      IF(ykurt.lt.ppl.or.ykurt.gt.ppu)THEN
       IF(.not.sig)sig=T
       IF(Lprt)WRITE(Mt1,1020)Cbr//'Kurtosis',ykurt,
     &                        '(significant at one percent level)'
       IF(Lsav)WRITE(Nform,1030)'kurtosis',ykurt,'*'
       IF(Lsavlg)THEN
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellScope(Ng,0,0,'row','@','Kurtosis')
        WRITE(Ng,1040)ykurt,Cbr//'(significant)'
        CALL writTag(Ng,'</tr>')
       END IF
      ELSE
       IF(Lprt)WRITE(Mt1,1020)Cbr//'Kurtosis',ykurt,' '
       IF(Lsav)WRITE(Nform,1030)'kurtosis',ykurt,' '
       IF(Lsavlg)THEN
        CALL writTag(Ng,'<tr>')
        CALL mkHeaderCellScope(Ng,0,0,'row','@','Kurtosis')
        WRITE(Ng,1040)ykurt,' '
        CALL writTag(Ng,'</tr>')
       END IF
      END IF
c-----------------------------------------------------------------------
c     Print out normality diagnostics
c-----------------------------------------------------------------------
      IF(Lprt)THEN
       CALL writTag(Mt1,'</p>')
       IF(sigskw)THEN
        CALL writln('Significant skewness has been detected in the '//
     &              'model residuals;',Mt1,0,T,F)
        CALL writln('  this makes the Geary''s a and Kurtosis '//
     &              'statistics unreliable indicators',Mt1,0,F,F)
        CALL writln('  of the normality of the residuals.',Mt1,0,F,T)
       ELSE
        IF(sig)THEN
         CALL writln('A significant value of one of these statistics '//
     &         'indicates that the',Mt1,0,T,F)
         CALL writln('  standardized residuals do not follow a '//
     &         'standard normal distribution.',Mt1,0,F,F)
         CALL writln('  If the regARIMA model fits the data well, '//
     &         'such lack of normality',Mt1,0,F,F)
         CALL writln('  ordinarily causes no problems.',Mt1,0,F,T)
         CALL writln('However, a significant value can occur because '//
     &         'certain data effects are',Mt1,0,T,F)
         CALL writln('  not captured well by the model. Sometimes '//
     &         'these effects can be captured',Mt1,0,F,F)
         CALL writln('  by additional or different regressors (e.g. '//
     &         'trading day, holiday or ',Mt1,0,F,F)
         CALL writln('  outlier regressors).',Mt1,0,F,T)
         CALL writln('There are other important effects that can '//
     &         'cause a significant value,',Mt1,0,T,F)
         CALL writln('  such as random variation of the coefficients '//
     &         'or time-varying conditional',Mt1,0,F,F)
         CALL writln('  variances, which cannot be represented by '//
     &         'regARIMA models. These other',Mt1,0,F,F)
         CALL writln('  effects cause the t-tests, AIC''s and '//
     &         'forecast coverage intervals of',Mt1,0,F,F)
         CALL writln('  '//PRGNAM//' to have reduced reliability. '//
     &         'Their presence is often',Mt1,0,F,F)
         CALL writln('  indicated by significant (high) values of '//
     &         'the Ljung-Box Q-statistics of',Mt1,0,F,F)
         CALL writln('  the squared residuals.',Mt1,0,F,T)
        ELSE
         CALL mkPOneLine(Mt1,'center',
     &           '<strong>No indication of lack of normality.</strong>')
        END IF
       END IF
      END IF
      IF(Lsavlg)THEN
       CALL writTag(Ng,'</table></div>')
       CALL mkPOneLine(Ng,'@','&nbsp;')
      END IF
c-----------------------------------------------------------------------
 1000 FORMAT('<div id="lgnrm',i6.6,'">')
 1010 FORMAT('    Number of residuals  :  ',i5,a)
 1020 FORMAT('    ',a,t26,':',f10.4,t40,a)
 1030 FORMAT(a,':',f10.4,1x,a)
 1040 FORMAT('<td>',f10.4,1x,a,'</td>')
c-----------------------------------------------------------------------
      RETURN
      END

