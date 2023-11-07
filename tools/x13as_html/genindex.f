C     Last change: Nov. 2022, reomove histogram links in sliding spans
c     and correct sliding spans links' labels
C     previous change:Jan. 2021, set variable eststr to the correct
c     length
      SUBROUTINE genIndex(Fh,Indx,IndxCode)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      LOGICAL T,F
      INTEGER NLVL
      PARAMETER (T=.true.,F=.false.,NLVL=5)
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'ssap.prm'
      INCLUDE 'tbltitle.prm'
      INCLUDE 'tbllog.prm'
      INCLUDE 'level.prm'
      INCLUDE 'htmlfile.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'spctbl.i'
c      INCLUDE 'units.cmn'
c-----------------------------------------------------------------------
      CHARACTER tblttl*(PTTLEN),eststr*(100),ex*(2),savstr*(100)
      INTEGER Fh,Indx,IndxCode,n1,ntbttl,i2,iss,nstr,npass,ipos,i,indxbk
      LOGICAL lerrbk
      DIMENSION ex(2*NEST)
c-----------------------------------------------------------------------
      CHARACTER SSEDIC*174
      INTEGER sseptr,PSSE
      PARAMETER(PSSE=6)
      DIMENSION sseptr(0:PSSE)
      PARAMETER(SSEDIC=
     &'Seasonal FactorsTrading Day FactorsFinal Seasonally Adjusted Seri
     &esMonth-to-Month Changes in SA SeriesYear-to-Year Changes in SA Se
     &riesQuarter-to-Quarter Changes in SA Series')
c-----------------------------------------------------------------------
      CHARACTER SETDIC*126
      INTEGER setptr,PSET
      PARAMETER(PSET=12)
      DIMENSION setptr(0:PSET)
      PARAMETER(SETDIC='StochasticRegressionTotalRevision error ofStocha
     &sticRegressionTotalRevision error ofStochasticRegressionTotalRevis
     &ion error of')
c-----------------------------------------------------------------------
      CHARACTER SERDIC*568
      INTEGER serptr,PSER
      PARAMETER(PSER=8)
      DIMENSION serptr(0:PSER)
      PARAMETER(SERDIC='Standard error of revision in trend-cycle estima
     &tor (last 5 years)Finite sample standard error of revision in tren
     &d-cycle estimator (last 5 years)Standard error of revision in tren
     &d-cycle estimator (last years)Finite sample standard error of revi
     &sion in trend-cycle estimator (last years)Standard error of revisi
     &on in SA series estimator (last 5 years)Finite sample standard err
     &or of revision in SA series estimator (last 5 years)Standard error
     & of revision in SA series estimator (last years)Finite sample stan
     &dard error of revision in SA series estimator (last years)')
c-----------------------------------------------------------------------
      CHARACTER REGDIC*389
      INTEGER regptr,PREG
      PARAMETER(PREG=14)
      DIMENSION regptr(0:PREG)
      PARAMETER(REGDIC='Level shiftTransitory outliersSeasonal outliersE
     &aster effectDeterministic trading day effectDeterministic seasonal
     & componentCalendar regression effectTrend-cycle regression effectB
     &usiness cycle regression effectIrregular regression effectTransito
     &ry component regression effectOther regression effect in seasonall
     &y adjusted seriesSeasonal regression effectSeparate regression eff
     &ect factors')
c-----------------------------------------------------------------------
      CHARACTER FINDIC*210
      INTEGER finptr,PFIN
      PARAMETER(PFIN=8)
      DIMENSION finptr(0:PFIN)
      PARAMETER(FINDIC='Final componentFinal seasonally adjusted seriesF
     &inal seasonally adjusted series with revised yearlyFinal trend-cyc
     &leFinal seasonalFinal td componentFinal transitory componentFinal 
     &transitory-irregular component')
c-----------------------------------------------------------------------
      CHARACTER RATDIC*132
      INTEGER ratptr,PRAT
      PARAMETER(PRAT=6)
      DIMENSION ratptr(0:PRAT)
      PARAMETER(RATDIC='Original series (from regARIMA)Original seriesFi
     &nal seasonally adjusted seriesSeasonally adjusted seriesFinal tren
     &d-cycleTrend-cycle')
c-----------------------------------------------------------------------
      CHARACTER OTLDIC*44
      INTEGER otlptr,POTL
      PARAMETER(POTL=4)
      DIMENSION otlptr(0:POTL)
      PARAMETER(OTLDIC='AdditiveLevel ChangeTemporary ChangeSeasonal')
c-----------------------------------------------------------------------
      CHARACTER PREDIC*381
      INTEGER preptr,PPRE
      PARAMETER(PPRE=13)
      DIMENSION preptr(0:PPRE)
      PARAMETER(PREDIC='Seasonal componentSeasonal factors (x 100)Stocha
     &stic trading day factor (x 100)Transitory factors (x 100)Transitor
     &y componentStochastic trading day componentFinal trend-cycleReal-t
     &ime estimators of trend-cycleSeasonally adjusted seriesReal-time e
     &stimators of seasonally adjusted seriesIrregular componentIrregula
     &r factors (x 100)Final seasonally adjusted series with revised yea
     &rly')
c-----------------------------------------------------------------------
      CHARACTER RSEDIC*320
      INTEGER rseptr,PRSE
      PARAMETER(PRSE=7)
      DIMENSION rseptr(0:PRSE)
      PARAMETER(RSEDIC='Standard error of seasonal factorsStandard error
     & of stochastic trading day componentStandard error of transitory c
     &omponentStandard error of trend-cycleRevision from updating real-t
     &ime trend-cycle estimatorsStandard error of seasonally adjusted se
     &riesRevision from updating real-time seasonally adjusted series es
     &timators')
c-----------------------------------------------------------------------
      DATA preptr/1,19,43,80,106,126,158,175,210,236,286,305,330,382/
      DATA rseptr/1,35,85,123,152,207,251,321/
      DATA otlptr/1,9,21,37,45/
      DATA ratptr/1,32,47,79,105,122,133/
      DATA finptr/1,16,48,100,117,131,149,175,211/
      DATA regptr/1,12,31,48,61,93,125,151,180,212,239,277,330,356,390/
      DATA setptr/1,11,21,26,43,53,63,68,85,95,105,110,127/
      DATA sseptr/1,17,36,68,103,136,175/
      DATA serptr/1,67,147,211,289,353,431,493,569/
      DATA ex/'a ','ai','b ','bi','c ','ci','d ','di','e ','ei'/
c-----------------------------------------------------------------------
      INCLUDE 'level.var'
c-----------------------------------------------------------------------
      lerrbk=Lfatal
      Lfatal=F
      indxbk=Indx
c-----------------------------------------------------------------------
      n1=Ncslast+1
      IF(IndxCode.le.NTBL)THEN
       tblttl=' '
       CALL getdes(IndxCode,tblttl,ntbttl,T)
       IF(level(IndxCode,4))THEN
        CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                     tblttl(1:ntbttl),T,F)
       ELSE
        CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                     'Plot of '//tblttl(1:ntbttl),T,F)
       END IF
       RETURN
      END IF
c-----------------------------------------------------------------------
      IF(IndxCode.eq.1000)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Over/Under Estimation Test',T,F)
      ELSE IF(IndxCode.eq.1001)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'SEATS Part 1 : ARIMA estimation',T,F)
      ELSE IF(IndxCode.eq.1002)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'SEATS input parameters',T,F)
      ELSE IF(IndxCode.eq.1003)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Wiener-Kolmogorov filters (one side)',T,F)
      ELSE IF(IndxCode.eq.1004)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Contribution of original series and '//
     $                    'of its innovations to the estimator '//
     $                    'of the components',T,F)
      ELSE IF(IndxCode.eq.1005)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Autocorrelation function of components '//
     $                    '(stationary transformation)',T,F)
      ELSE IF(IndxCode.eq.1006)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Forecast of stochastic series and '//
     $                    'components (levels)',T,F)
      ELSE IF(IndxCode.eq.1007)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Forecast of Seasonal Factors',T,F)
      ELSE IF(IndxCode.eq.1008)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Deterministic Component (from regARIMA)',T,F)
      ELSE IF(IndxCode.eq.1009)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Final Decomposition',T,F)
      ELSE IF(IndxCode.eq.1010)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Program Header',T,F)
      ELSE IF(IndxCode.eq.1011)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Forecast of Final Component',T,F)
      ELSE IF(IndxCode.eq.1012)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Decomposition of VARIANCE',T,F)
      ELSE IF(IndxCode.eq.1013)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Model Identification',T,F)
      ELSE IF(IndxCode.eq.1014)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Model Definition',T,F)
      ELSE IF(IndxCode.eq.1015)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Model Estimation/Evaluation',T,F)
      ELSE IF(IndxCode.eq.1016)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Diagnostic Checking',T,F)
      ELSE IF(IndxCode.eq.1017)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                   '<abbr title="Akaike information criterion">'//
     &                    'AIC</abbr> test for trading day regressor',
     &                    T,F)
      ELSE IF(IndxCode.eq.1018)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                   '<abbr title="Akaike information criterion">'//
     &                    'AIC</abbr> test for Easter regressor',T,F)
      ELSE IF(IndxCode.eq.1019)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                   '<abbr title="Akaike information criterion">'//
     &                    'AIC</abbr> test for user-defined regressor',
     &                    T,F)
      ELSE IF(IndxCode.eq.1020)THEN
       IF(Ny.eq.4)THEN
        CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                   '<abbr title="Akaike information criterion">'//
     &                     'AIC</abbr> test for length-of-quarter or '//
     &                     'leap year regressor',T,F)
       ELSE
        CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                   '<abbr title="Akaike information criterion">'//
     &                   'AIC</abbr> test for length-of-month or '//
     &                   'leap year regressor',T,F)
       END IF
c-----------------------------------------------------------------------
      ELSE IF(IndxCode.ge.1021.and.IndxCode.le.1030)THEN
       iss=IndxCode-1020
       i2=iss/2 + 1
       IF((iss.eq.7.or.iss.eq.8).and.Ny.eq.4)THEN
        CALL getstr(SSEDIC,sseptr,PSSE,PSSE,eststr,nstr)
       ELSE IF (mod(iss,2).eq.0) THEN
        CALL getstr(SSEDIC,sseptr,PSSE,i2-1,eststr,nstr)
       ELSE
        CALL getstr(SSEDIC,sseptr,PSSE,i2,eststr,nstr)
       END IF
       i2=mod(iss,2)
       IF(I2.eq.1)THEN
        CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                     'S 3.'//ex(iss)//' Sliding spans '//
     &                     'breakdown table for '//eststr(1:nstr),T,F)
       ELSE
        CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                     'S 3.'//ex(iss)//' Sliding spans '//
     &                     'breakdown table for '//eststr(1:nstr)//
     &                     ' (indirect)',T,F)
       END IF
c-----------------------------------------------------------------------
      ELSE IF(IndxCode.eq.1031)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Links to other HTML files',T,F)
      ELSE IF(IndxCode.eq.1032)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Content of input specification file',T,F)
      ELSE IF(IndxCode.eq.1033)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Content of saved model file',T,F)
      ELSE IF(IndxCode.eq.1034)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Tabular Histogram of the Residuals',T,F)
c-----------------------------------------------------------------------
c      ELSE IF(IndxCode.ge.1035.and.IndxCode.le.1044)THEN
c       iss=IndxCode-1034
c       i2=iss/2 + 1
c       IF(i2.eq.4.and.Ny.eq.4)THEN
c        CALL getstr(SSEDIC,sseptr,PSSE,PSSE,eststr,nstr)
c       ELSE
c        CALL getstr(SSEDIC,sseptr,PSSE,i2,eststr,nstr)
c       END IF
c       i2=mod(iss,2)
c       IF(i2.eq.1)THEN
c        CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
c     &                     'Sliding spans Histogram for '//
c     &                     eststr(1:nstr),T,F)
c       ELSE
c        CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
c     &                     'Sliding spans Histogram for '//
c     &                     eststr(1:nstr)//' (indirect)',T,F)
c       END IF
c-----------------------------------------------------------------------
      ELSE IF(IndxCode.eq.1045)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Spectral Diagnostics',T,F)
      ELSE IF(IndxCode.eq.1046)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'SEATS Part 2: Models for the components',T,F)
      ELSE IF(IndxCode.eq.1047)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Autocorrelations of extended residuals',T,F)
      ELSE IF(IndxCode.eq.1048)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &             'Autocorrelations of squared extended residuals',T,F)
      ELSE IF(IndxCode.eq.1049)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Autocorrelations of stationary series',T,F)
c-----------------------------------------------------------------------
      ELSE IF(IndxCode.eq.1050)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'SEATS Part 5 : Rates of growth',T,F)
      ELSE IF(IndxCode.eq.1051)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Table 5.1',T,F)
      ELSE IF(IndxCode.eq.1052)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Table 5.2',T,F)
      ELSE IF(IndxCode.eq.1053)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Table 5.3',T,F)
      ELSE IF(IndxCode.eq.1054)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Table 5.4',T,F)
      ELSE IF(IndxCode.eq.1055)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Table 5.5',T,F)
      ELSE IF(IndxCode.eq.1056)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Table 5.6',T,F)
      ELSE IF(IndxCode.eq.1057)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Table 5.7',T,F)
c-----------------------------------------------------------------------
      ELSE IF(IndxCode.eq.1058)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Test statistics',T,F)
      ELSE IF(IndxCode.eq.1059)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                 'Residual seasonality (non-parametric test)',T,F)
      ELSE IF(IndxCode.eq.1060)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'SEATS Part 6 : Estimation of the Cycle',T,F)
      ELSE IF(IndxCode.eq.1061)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Model fitted',T,F)
      ELSE IF(IndxCode.eq.1062)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &              'Partial Autocorrelations of stationary series',T,F)
      ELSE IF(IndxCode.eq.1063)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Differences',T,F)
      ELSE IF(IndxCode.eq.1064)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Parameter Estimates',T,F)
      ELSE IF(IndxCode.eq.1065)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'ARIMA model for estimators',T,F)
      ELSE IF(IndxCode.eq.1066)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Backcasting',T,F)
      ELSE IF(IndxCode.eq.1067)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'File Save Requests',T,F)
      ELSE IF(IndxCode.eq.1068)THEN
       IF(Muladd.ne.1)THEN
        CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                     'F 4. Multiplicative Trading Day Component'//
     &                     ' Factors',T,F)
       ELSE
        CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'F 4. Additive Trading Day Component Factors',
     &                     T,F)
       END IF
      ELSE IF(IndxCode.eq.1069)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Forecasting',T,F)
      ELSE IF(IndxCode.eq.1070)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'F Tests for Seasonal Regressors',T,F)
      ELSE IF(IndxCode.eq.1071)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'F Tests for Trading Day Regressors',T,F)
      ELSE IF(IndxCode.eq.1072)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                  '<abbr title="autoregressive moving average">'//
     &                    'ARMA</abbr> Iterations',T,F)
      ELSE IF(IndxCode.eq.1073)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'regARIMA Iterations',T,F)
      ELSE IF(IndxCode.eq.1074)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Regression model',T,F)
      ELSE IF(IndxCode.eq.1075)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'ARIMA model',T,F)
      ELSE IF(IndxCode.eq.1076)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Roots of '//Mdlttl(1:Nmdlcr),T,F)
      ELSE IF(IndxCode.eq.1077)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Irregular Regression Model',T,F)
      ELSE IF(IndxCode.eq.1078)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Spectrum of cycle',T,F)
      ELSE IF(IndxCode.eq.1079)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Sliding Spans Analysis',T,F)
      ELSE IF(IndxCode.eq.1080)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Sliding Spans Analysis: Direct '//
     &                    'seasonal adjustment ',T,F)
      ELSE IF(IndxCode.eq.1081)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Outlier detection',T,F)
      ELSE IF(IndxCode.ge.1082.and.IndxCode.le.1085)THEN
       iss=IndxCode-1081
       CALL getstr(OTLDIC,otlptr,POTL,iss,eststr,nstr)
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    eststr(1:nstr)//' Outlier t-values',T,F)
      ELSE IF(IndxCode.ge.1086.and.IndxCode.le.1089)THEN
       iss=IndxCode-1085
       CALL getstr(OTLDIC,otlptr,POTL,iss,eststr,nstr)
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                'Final '//eststr(1:nstr)//' Outlier t-values',T,F)
      ELSE IF(IndxCode.eq.1090)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'S 1 Sliding Spans Means',T,F)
      ELSE IF(IndxCode.eq.1091)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'S 1 Sliding Spans Means (Indirect)',T,F)
      ELSE IF(IndxCode.eq.1092)THEN
       IF(Ny.eq.12)THEN
        CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                     'S 2 Percentage of Months Flagged',T,F)
       ELSE
        CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                     'S 2 Percentage of Quarters Flagged',T,F)
       END IF
      ELSE IF(IndxCode.eq.1093)THEN
       IF(Ny.eq.12)THEN
        CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                'S 2 Percentage of Months Flagged (Indirect)',T,F)
       ELSE
        CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &              'S 2 Percentage of Quarters Flagged (Indirect)',T,F)
       END IF
      ELSE IF(IndxCode.eq.1094)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Summary statistics for mean',T,F)
      ELSE IF(IndxCode.eq.1095)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Summary statistics for mean (Indirect)',T,F)
      ELSE IF(IndxCode.eq.1096)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                   '<abbr title="Akaike information criterion">'//
     &                    'AIC</abbr> test for transformation',T,F)
      ELSE IF(IndxCode.eq.1097)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                   '<abbr title="Akaike information criterion">'//
     &                    'AIC</abbr> test for trading day '//
     &                    '(irregular regression)',T,F)
      ELSE IF(IndxCode.eq.1098)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                   '<abbr title="Akaike information criterion">'//
     &                    'AIC</abbr> test for Easter (irregular '//
     &                    'regression)',T,F)
      ELSE IF(IndxCode.eq.1099)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                   '<abbr title="Akaike information criterion">'//
     &                    'AIC</abbr> test for user-defined '//
     &                    'regressors (irregular regression)',T,F)
c-----------------------------------------------------------------------
      ELSE IF(IndxCode.eq.1100)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Original uncorrected series (from regARIMA)',
     &                    T,F)
      ELSE IF(IndxCode.eq.1101)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     $                    'Preadjustment factors outliers and ' //
     $                    'other deterministic effects',T,F)
      ELSE IF(IndxCode.eq.1102)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     $                    'Original series',T,F)
      ELSE IF(IndxCode.eq.1103)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     $                    'ARIMA series (corrected by regARIMA)',T,F)
c-----------------------------------------------------------------------
      ELSE IF(IndxCode.ge.1104.and.IndxCode.le.1107)THEN
       iss=IndxCode-1103
       CALL getstr(SETDIC,setptr,PSET,iss,eststr,nstr)
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     $                    eststr(1:nstr)//' cyclical component',T,F)
      ELSE IF(IndxCode.ge.1108.and.IndxCode.le.1111)THEN
       iss=IndxCode-1107
       CALL getstr(SETDIC,setptr,PSET,iss,eststr,nstr)
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     $                    eststr(1:nstr)//' cyclical factor',T,F)
      ELSE IF(IndxCode.eq.1112)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     $                    'Cyclical component',T,F)
      ELSE IF(IndxCode.eq.1113)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     $                    'Cyclical factors',T,F)
      ELSE IF(IndxCode.eq.1114)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     $                    'Revision error of cyclical factors',T,F)
c-----------------------------------------------------------------------
      ELSE IF(IndxCode.ge.1115.and.IndxCode.le.1122)THEN
       iss=IndxCode-1114
       CALL getstr(SERDIC,serptr,PSER,iss,eststr,nstr)
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     $                    eststr(1:nstr),T,F)
      ELSE IF(IndxCode.ge.1123.and.IndxCode.le.1136)THEN
       iss=IndxCode-1122
       CALL getstr(REGDIC,regptr,PREG,iss,eststr,nstr)
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     $                    eststr(1:nstr),T,F)
      ELSE IF(IndxCode.ge.2135.and.IndxCode.le.2142)THEN
       iss=IndxCode-2134
       CALL getstr(FINDIC,finptr,PFIN,iss,eststr,nstr)
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     $                    eststr(1:nstr),T,F)
      ELSE IF(IndxCode.ge.1143.and.IndxCode.le.1156)THEN
       iss=IndxCode-1142
       CALL getstr(REGDIC,regptr,PREG,iss,eststr,nstr)
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     $                    eststr(1:nstr)//' (X100)',T,F)
      ELSE IF(IndxCode.ge.1157.and.IndxCode.le.1162)THEN
       iss=IndxCode-1156
       CALL getstr(RATDIC,ratptr,PRAT,iss,eststr,nstr)
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     $                    'Rates for '//eststr(1:nstr),T,F)
c-----------------------------------------------------------------------
      ELSE IF(IndxCode.eq.1163)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Innovations',T,F)
      ELSE IF(IndxCode.eq.1164)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'BACKWARD RESIDUALS',T,F)
      ELSE IF(IndxCode.eq.1165)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Transformed Series',T,F)
      ELSE IF(IndxCode.eq.1166)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Differenced Series',T,F)
      ELSE IF(IndxCode.eq.1167)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Differenced and Centered Series',T,F)
      ELSE IF(IndxCode.eq.1168)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Differenced and Centered Transformed Series',
     &                    T,F)
      ELSE IF(IndxCode.eq.1169)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Preadjustment Component',T,F)
c-----------------------------------------------------------------------
      ELSE IF(IndxCode.ge.1170.and.IndxCode.le.1182)THEN
       iss=IndxCode-1169
       CALL getstr(PREDIC,preptr,PPRE,iss,eststr,nstr)
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     $                    eststr(1:nstr),T,F)
      ELSE IF(IndxCode.ge.1183.and.IndxCode.le.1189)THEN
       iss=IndxCode-1182
       CALL getstr(RSEDIC,rseptr,PRSE,iss,eststr,nstr)
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     $                    eststr(1:nstr),T,F)
c-----------------------------------------------------------------------
      ELSE IF(IndxCode.eq.1190)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Contribution of original series and '//
     $                    'of its innovations to the estimator '//
     $                    'of the components',T,F)
      ELSE IF(IndxCode.eq.1191)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Studentized residuals',T,F)
      ELSE IF(IndxCode.eq.1192)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Studentized extended residuals',T,F)
      ELSE IF(IndxCode.eq.1193)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    '<abbr title="moving average">MA</abbr>'//
     &                    ' representation of estimators',T,F)
      ELSE IF(IndxCode.eq.1194)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Deterministic component from regARIMA',T,F)
      ELSE IF(IndxCode.eq.1195)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Weights for asymmetric filter',T,F)
      ELSE IF(IndxCode.eq.1196)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'SEATS Part 4 : Estimates of the components',
     &                    T,F)
      ELSE IF(IndxCode.eq.1197)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Cross-Correlation',T,F)
      ELSE IF(IndxCode.eq.1198)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Cross-Covariance',T,F)
      ELSE IF(IndxCode.eq.1199)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'SEATS Part 3 : Error analysis',T,F)
      ELSE IF(IndxCode.eq.1200)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Recent Estimates',T,F)
      ELSE IF(IndxCode.eq.1201)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Forecast Stochastic Components',T,F)
      ELSE IF(IndxCode.eq.1202)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Significance of Seasonality',T,F)
      ELSE IF(IndxCode.eq.1203)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    '<abbr title="moving average">MA</abbr> '//
     $                    'approximate model',T,F)
      ELSE IF(IndxCode.eq.1204)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Models for the components',T,F)
      ELSE IF(IndxCode.eq.1205)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Factorization of the <abbr title="'//
     $                    'moving average">MA</abbr> polynomial',T,F)
      ELSE IF(IndxCode.eq.1208)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Tests for Cancellation of Level Shifts',T,F)
c-----------------------------------------------------------------------
      ELSE IF(IndxCode.eq.1500)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &    'Chi-squared Tests for Groups of User-defined Holiday '//
     &    'Regressors',T,F)
      ELSE IF(IndxCode.eq.1501)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Chi-squared Tests for Groups of Regressors',
     &                    T,F)
c-----------------------------------------------------------------------
      ELSE IF(IndxCode.gt.2000.and.IndxCode.lt.2500)THEN
       ipos=1
       npass=IndxCode-2000
       CALL itoc(npass,savstr,ipos)
c       write(Mtprof,*)' IndxCode, npass, savstr',IndxCode, npass, 
c     &                savstr(1:ipos-1)
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Outlier identification: Forward addition '//
     &                    'pass '//savstr(1:ipos-1),T,F)
c-----------------------------------------------------------------------
      ELSE IF(IndxCode.ge.2500.and.IndxCode.le.2513)THEN
       iss=IndxCode-2500
       IF(iss.eq.0)THEN
        CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                     'Long term trend',T,F)
       ELSE IF(iss.eq.13)THEN
        CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                     'Long term trend Component',T,F)
       ELSE
        CALL getstr(SETDIC,setptr,PSET,iss,eststr,nstr)
        IF(iss.gt.8)THEN
         CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                 eststr(1:nstr)//' long term trend factors',T,F)
        ELSE IF(iss.gt.4)THEN
         CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                   eststr(1:nstr)//' long term trend component',
     &                      T,F)
        ELSE
         CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                      eststr(1:nstr)//' long term trend',T,F)
        END IF
       END IF
      ELSE IF(IndxCode.ge.2600.and.IndxCode.le.2609)THEN
       iss=IndxCode-2600
       IF(iss.eq.0)THEN
        CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                     'SA series without Business Cycle',T,F)
       ELSE IF(iss.eq.13)THEN
        CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                     'SA series without Business Cycle Component',
     &                     T,F)
       ELSE
        CALL getstr(SETDIC,setptr,PSET,iss,eststr,nstr)
        IF(iss.gt.8)THEN
         CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                      eststr(1:nstr)//
     &                      ' SA series without Business Cycle factors',
     &                      T,F)
        ELSE IF(iss.gt.4)THEN
         CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                      eststr(1:nstr)//
     &                    ' SA series without Business Cycle component',
     &                      T,F)
        ELSE
         CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                      eststr(1:nstr)//
     &                      ' SA series without Business Cycle',T,F)
        END IF
       END IF
      ELSE IF(IndxCode.eq.2613)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Series without Business Cycle Component',
     &                    T,F)
      ELSE IF(IndxCode.ge.2700.and.IndxCode.le.2709)THEN
       iss=IndxCode-2700
       IF(iss.eq.0)THEN
        CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                     'Series without Business Cycle',T,F)
       ELSE
        CALL getstr(SETDIC,setptr,PSET,iss,eststr,nstr)
        IF(iss.gt.8)THEN
         CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                      eststr(1:nstr)//
     &                      ' Series without Business Cycle factors',
     &                      T,F)
        ELSE IF(iss.gt.4)THEN
         CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                      eststr(1:nstr)//
     &                      ' series without Business Cycle component',
     &                      T,F)
        ELSE
         CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                      eststr(1:nstr)//
     &                      ' series without Business Cycle',T,F)
        END IF
       END IF
      ELSE IF(IndxCode.eq.2713)THEN
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Series without Business Cycle Component',
     &                    T,F)
c-----------------------------------------------------------------------
      ELSE IF(IndxCode.gt.4000)THEN
       i2=IndxCode-4000
       IF(i2.eq.LSPCQS)THEN
        CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                  'QS Statistic for regARIMA Model Residuals',T,F)
       ELSE IF(i2.eq.LSPCTP)THEN
        CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                     'Peak probabilities for Tukey spectrum '//
     &                     'estimator: Indirect adjustments',T,F)
       END IF
c-----------------------------------------------------------------------
      ELSE IF(IndxCode.gt.3000)THEN
       ipos=1
       npass=IndxCode-3000
       CALL itoc(npass,savstr,ipos)
       CALL makeIndexLink(Fh,Indx,CsrsHTML(n1:NcsHTML)//'.html',
     &                    'Outlier identification: Backward deletion '//
     &                    'pass '//savstr(1:ipos-1),T,F)
      END IF
c-----------------------------------------------------------------------
      Lfatal=lerrbk
c-----------------------------------------------------------------------
      IF(Indx.eq.indxbk)THEN
       WRITE(STDERR,*)' '
       WRITE(STDERR,*)'  Cannot create index entry for code = ',IndxCode
       WRITE(STDERR,*)'  Send this code to x12@census.gov'
       Indx=Indx+1
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
