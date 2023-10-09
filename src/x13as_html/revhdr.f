C     Last change:  BCM  29 Sep 1998   10:48 am
      SUBROUTINE revhdr(Lr1y2y,Revsa,Revmdl,Lmodel,Ny,Endspn,Iagr)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C  *****  PRINTS HEADING THAT IDENTIFIES WHICH OPTIONS ARE BEING USED
C  *****  IN A GIVEN Revisions history ANALYSIS.
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'rev.prm'
      INCLUDE 'rev.cmn'
      INCLUDE 'revtrg.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      CHARACTER laglbl*(45),str*(10),cregfx*(12)
      LOGICAL Lr1y2y,Revsa,Revmdl,Lmodel
      INTEGER Ny,nlglbl,nchr,i,Endspn,nyrev,Iagr,nregfx
      DIMENSION Endspn(2),cregfx(4),nregfx(4)
c-----------------------------------------------------------------------
      DATA cregfx/'Trading Day ','Holiday     ','User-defined',
     &            'Outlier     '/
      DATA nregfx/11,7,12,7/
c-----------------------------------------------------------------------
      CALL writTagOneLine(Mt1,'h2','@','Revisions history analysis')
      CALL writTagOneLine(Mt1,'h3','@','R 0   Summary of options '//
     &                    'selected for revisions history analysis.')
      CALL mkPOneLine(Mt1,'@',
     &                'History analysis performed for the following:')
      CALL writTagClass(Mt1,'ul','indent')
      IF(Lrvsa)THEN
       IF(Iagr.eq.5)THEN
        IF(Indrev.gt.0)THEN
         WRITE(Mt1,1020)'Direct and Indirect Seasonally Adjusted Series'
        ELSE
         WRITE(Mt1,1020)'Direct Seasonally Adjusted Series'
        END IF
       ELSE
        WRITE(Mt1,1020)'Final Seasonally Adjusted Series'
       END IF
      END IF
      IF(Lrvch)
     &   WRITE(Mt1,1020)'Changes in Final Seasonally Adjusted Series'
      IF(Lrvtrn)WRITE(Mt1,1020)'Final Trend-Cycle Component'
      IF(Lrvtch)WRITE(Mt1,1020)'Changes in Final Trend Cycle Component'
      IF(Lrvaic)WRITE(Mt1,1020)'<abbr title="Akaike information '//
     &                         'criterion">AIC</abbr>'
      IF(Lrvfct)WRITE(Mt1,1020)'Forecast Errors'
      IF(Lrvarma)WRITE(Mt1,1020)'<abbr title="autoregressive moving '//
     &                          'average">ARMA</abbr> coefficients'
      IF(Lrvtdrg)WRITE(Mt1,1020)'Trading Day Coefficients'
 1020 FORMAT('<li>',a,'</li>')
      CALL writTag(Mt1,'</ul>')
c-----------------------------------------------------------------------
      IF(Lrvsa.and.Iagr.eq.5.and.Indrev.eq.0)THEN
       CALL mkPOneLine(Mt1,'@',
     &                 'History analysis was not performed on the '//
     &                 'Indirect Seasonally Adjusted Series '//
     &                 'for one of the following reasons:')
       CALL writTagClass(Mt1,'ul','indent')
       WRITE(Mt1,1020)'Identical starting dates not provided for the'//
     &    ' history analysis of all the components;'
       WRITE(Mt1,1020)
     &    'History analysis of seasonal adjustments not specified '//
     &    'for all the components;'
       WRITE(Mt1,1020)
     &    'Starting date specified for total series doesn''t match '//
     &    'starting date for the component series;'
       WRITE(Mt1,1020)
     &    'Starting date not specified for total series.'
       CALL writTag(Mt1,'</ul>')
       CALL mkPOneLine(Mt1,'@',
     &        'Revise the input specification files for the '//
     &        'components and total series'//
     &        ' accordingly and rerun the metafile to generate '//
     &        'revisions history analysis for'//
     &        ' the indirect seasonally adjusted series.')
      END IF
c-----------------------------------------------------------------------
      IF(Ntarsa.gt.0)THEN
       IF(Lrvsa.and.Lrvch)THEN
        laglbl='Seasonally Adjusted Series and Changes:'
        nlglbl=39
       ELSE IF(Lrvsa)THEN
        laglbl='Seasonally Adjusted Series:'
        nlglbl=27
       ELSE
        laglbl='Changes in the Seasonally Adjusted Series:'
        nlglbl=42
       END IF
       WRITE(Mt1,1040)laglbl(1:nlglbl)
       WRITE(Mt1,1050)(Targsa(i),i=1,Ntarsa)
       IF(Lr1y2y)WRITE(Mt1,1050)Ny,Ny*2
       CALL writTag(Mt1,'</p>')
      END IF
      IF(Ntartr.gt.0)THEN
       IF(Lrvtrn.and.Lrvtch)THEN
        laglbl='Trend-Cycle Component and Changes:'
        nlglbl=34
       ELSE IF(Lrvsa)THEN
        laglbl='Trend-Cycle Component:'
        nlglbl=22
       ELSE
        laglbl='Changes in the Trend Cycle Component:'
        nlglbl=37
       END IF
       WRITE(Mt1,1040)laglbl(1:nlglbl)
       WRITE(Mt1,1050)(Targtr(i),i=1,Ntartr)
       CALL writTag(Mt1,'</p>')
      END IF
 1040 FORMAT('<p>Lags from Concurrent Analyzed for ',a)
 1050 FORMAT(5x,5i5)
* 1060 FORMAT('  Difference between Lag ',i2,' and ',i2,' Analyzed.')
c-----------------------------------------------------------------------
      IF(Lrvfct.and.Nfctlg.gt.0)THEN
       WRITE(Mt1,1070)
 1070  FORMAT('<p>Forecast Lags Analyzed for Forecast Error History',
     &        ' Analysis:')
       WRITE(Mt1,1050)(Rfctlg(i),i=1,Nfctlg)
       CALL writTag(Mt1,'</p>')
      END IF
c-----------------------------------------------------------------------
      CALL wrtdat(Rvstrt,Ny,str,nchr)
      IF(Lfatal)RETURN
      CALL writTag(Mt1,'<p>')
      WRITE(Mt1,1060)'Starting date for history analysis: '//
     &                str(1:nchr),Cbr
      CALL wrtdat(Rvend,Ny,str,nchr)
      IF(Lfatal)RETURN
      IF(Revsa.and.Revmdl)THEN
       CALL dfdate(Rvend,Endspn,Ny,nyrev)
       IF(nyrev.eq.0)THEN
        WRITE(Mt1,1060)'Ending date for history analysis: '//
     &                  str(1:nchr),'</p>'
       ELSE
        WRITE(Mt1,1060)'Ending date for seasonal adjustment history '//
     &                 'analysis: '//str(1:nchr),Cbr
        CALL wrtdat(Endspn,Ny,str,nchr)
        IF(Lfatal)RETURN
        WRITE(Mt1,1060)'Ending date for regARIMA history analysis: '//
     &                  str(1:nchr),'</p>'
       END IF
      ELSE
       WRITE(Mt1,1060)'Ending date for history analysis: '//
     &                 str(1:nchr),'</p>'
      END IF
 1060 FORMAT(a,a)
c-----------------------------------------------------------------------
      IF(Revsa)THEN
       IF(Cnctar)THEN
        CALL mkPOneLine(Mt1,'@',
     &        'Seasonal Adjustment Revisions Computed Using '//
     &        'Concurrent as Target.')
       ELSE
        CALL mkPOneLine(Mt1,'@',
     &        'Seasonal Adjustment Revisions Computed Using '//
     &        'Final as Target.')
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(Lmodel)THEN
       IF(Revfix)THEN
        CALL mkPOneLine(Mt1,'@',
     &        'regARIMA coefficient estimates are fixed during '//
     &        'the history analysis.')
       ELSE
        CALL mkPOneLine(Mt1,'@',
     &        'regARIMA coefficient estimates are estimated during '//
     &        'the history analysis.')
        IF(Lrfrsh)CALL mkPOneLine(Mt1,'@',
     &        'Starting values are reset to the values estimated for '//
     &        'the full span of data.')
        IF(Nrvfxr.gt.0)THEN
         CALL mkPOneLine(Mt1,'@',
     &        'The following regressors are held fixed during the '//
     &        'history analysis:')
         CALL writTagClass(Mt1,'ul','indent')
         DO i=1,Nrvfxr
          WRITE(Mt1,1020)cregfx(Rvfxrg(i))(1:nregfx(Rvfxrg(i)))
         END DO
         CALL writTag(Mt1,'</ul>')
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
      RETURN
      END

