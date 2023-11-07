      SUBROUTINE svoudg(Lsav,Lsumm,Ny)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     save variables created by REG for over/under estimation
c     diagnostics into log file and/or diagonstic output -
c     Originally created by BCM - September 2005
c     ------------------------------------------------------------------
      INCLUDE 'acfast.i'
      INCLUDE 'across.i'
      INCLUDE 'htmlout.cmn'
      INCLUDE 'units.cmn'
c     ------------------------------------------------------------------
      LOGICAL Lsav
      INTEGER Lsumm,nsigv,nsiga1,nsigas,nsigcc,Ny
c     ------------------------------------------------------------------
c     Determine how many of the tests are significant
c     ------------------------------------------------------------------
      nsigv=0
      IF(.not.(FACFPDC(0).eq.'??'.or.FACFPDC(0).eq.'OK'))nsigv=nsigv+1
      IF(.not.(FACFADC(0).eq.'??'.or.FACFADC(0).eq.'OK'))nsigv=nsigv+1
      IF(.not.(FACFSDC(0).eq.'??'.or.FACFSDC(0).eq.'OK'))nsigv=nsigv+1
      IF(.not.(FACFIDC(0).eq.'??'.or.FACFIDC(0).eq.'OK'))nsigv=nsigv+1
      IF(.not.(NACFPDC(0).eq.'??'.or.NACFPDC(0).eq.'OK'))nsigv=nsigv+1
      IF(.not.(NACFADC(0).eq.'??'.or.NACFADC(0).eq.'OK'))nsigv=nsigv+1
      IF(.not.(NACFSDC(0).eq.'??'.or.NACFSDC(0).eq.'OK'))nsigv=nsigv+1
      IF(.not.(NACFIDC(0).eq.'??'.or.NACFIDC(0).eq.'OK'))nsigv=nsigv+1
      IF(.not.(WACFPDC(0).eq.'??'.or.WACFPDC(0).eq.'OK'))nsigv=nsigv+1
      IF(.not.(WACFADC(0).eq.'??'.or.WACFADC(0).eq.'OK'))nsigv=nsigv+1
      IF(.not.(WACFSDC(0).eq.'??'.or.WACFSDC(0).eq.'OK'))nsigv=nsigv+1
      IF(.not.(WACFIDC(0).eq.'??'.or.WACFIDC(0).eq.'OK'))nsigv=nsigv+1
      nsiga1=0
      IF(.not.(FACFPDC(1).eq.'??'.or.FACFPDC(1).eq.'OK'))nsiga1=nsiga1+1
      IF(.not.(FACFADC(1).eq.'??'.or.FACFADC(1).eq.'OK'))nsiga1=nsiga1+1
      IF(.not.(FACFSDC(1).eq.'??'.or.FACFSDC(1).eq.'OK'))nsiga1=nsiga1+1
      IF(.not.(FACFIDC(1).eq.'??'.or.FACFIDC(1).eq.'OK'))nsiga1=nsiga1+1
      IF(.not.(NACFPDC(1).eq.'??'.or.NACFPDC(1).eq.'OK'))nsiga1=nsiga1+1
      IF(.not.(NACFADC(1).eq.'??'.or.NACFADC(1).eq.'OK'))nsiga1=nsiga1+1
      IF(.not.(NACFSDC(1).eq.'??'.or.NACFSDC(1).eq.'OK'))nsiga1=nsiga1+1
      IF(.not.(NACFIDC(1).eq.'??'.or.NACFIDC(1).eq.'OK'))nsiga1=nsiga1+1
      IF(.not.(WACFPDC(1).eq.'??'.or.WACFPDC(1).eq.'OK'))nsiga1=nsiga1+1
      IF(.not.(WACFADC(1).eq.'??'.or.WACFADC(1).eq.'OK'))nsiga1=nsiga1+1
      IF(.not.(WACFSDC(1).eq.'??'.or.WACFSDC(1).eq.'OK'))nsiga1=nsiga1+1
      IF(.not.(WACFIDC(1).eq.'??'.or.WACFIDC(1).eq.'OK'))nsiga1=nsiga1+1
      nsigas=0
      IF(.not.(FACFPDC(Ny).eq.'??'.or.FACFPDC(Ny).eq.'OK'))
     &   nsigas=nsigas+1
      IF(.not.(FACFADC(Ny).eq.'??'.or.FACFADC(Ny).eq.'OK'))
     &   nsigas=nsigas+1
      IF(.not.(FACFSDC(Ny).eq.'??'.or.FACFSDC(Ny).eq.'OK'))
     &   nsigas=nsigas+1
      IF(.not.(FACFIDC(Ny).eq.'??'.or.FACFIDC(Ny).eq.'OK'))
     &   nsigas=nsigas+1
      IF(.not.(NACFPDC(Ny).eq.'??'.or.NACFPDC(Ny).eq.'OK'))
     &   nsigas=nsigas+1
      IF(.not.(NACFADC(Ny).eq.'??'.or.NACFADC(Ny).eq.'OK'))
     &   nsigas=nsigas+1
      IF(.not.(NACFSDC(Ny).eq.'??'.or.NACFSDC(Ny).eq.'OK'))
     &   nsigas=nsigas+1
      IF(.not.(NACFIDC(Ny).eq.'??'.or.NACFIDC(Ny).eq.'OK'))
     &   nsigas=nsigas+1
      IF(.not.(WACFPDC(Ny).eq.'??'.or.WACFPDC(Ny).eq.'OK'))
     &   nsigas=nsigas+1
      IF(.not.(WACFADC(Ny).eq.'??'.or.WACFADC(Ny).eq.'OK'))
     &   nsigas=nsigas+1
      IF(.not.(WACFSDC(Ny).eq.'??'.or.WACFSDC(Ny).eq.'OK'))
     &   nsigas=nsigas+1
      IF(.not.(WACFIDC(Ny).eq.'??'.or.WACFIDC(Ny).eq.'OK'))
     &   nsigas=nsigas+1
      nsigcc=0
      IF(.not.(seaIrrDgC.eq.'??'.or.seaIrrDgC.eq.'OK'))nsigcc=nsigcc+1
      IF(.not.(seaTreDgC.eq.'??'.or.seaTreDgC.eq.'OK'))nsigcc=nsigcc+1
      IF(.not.(treIrrDgC.eq.'??'.or.treIrrDgC.eq.'OK'))nsigcc=nsigcc+1
c     ------------------------------------------------------------------
c     Save significant over/under estimation tests to log file
c     ------------------------------------------------------------------
      IF (Lsav) THEN
       CALL mkPOneLine(Ng,'@','&nbsp;')
       IF(nsigv.eq.0)THEN
        CALL mkPOneLine(Ng,'center','None of the over/under '//
     &                  'estimation tests for Variance is significant')
       ELSE
        Inlgfl=Inlgfl+1
        WRITE(Ng,1000)Inlgfl
        CALL mkTableTag(Ng,'w60','Components for which the over/under'//
     &                  ' estimation tests of Variance are significant')
        CALL mkCaption(Ng,'Significant over/under estimation tests of'//
     &                 ' Variance')
        CALL writTag(Mt1,'<tr>')
        CALL mkTableCell(Ng,'head','&nbsp;')
        CALL mkHeaderCellScope(Ng,0,0,'col','@','Type')
        CALL mkHeaderCellScope(Ng,0,0,'col','@','p-value')
        CALL writTag(Mt1,'</tr>')
c        CALL writTagClass(Ng,'ul','indent')
        IF(.not.(FACFPDC(0).eq.'??'.or.FACFPDC(0).eq.'OK'))THEN
         IF(FACFPDC(0).eq.'++'.or.FACFPDC(0).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Trend-Cycle (Full Series)','oversmoothing',
     &                   FACFPDP(0))
         ELSE
          CALL mkOudgRow(Ng,'Trend-Cycle (Full Series)',
     &                   'undersmoothing',FACFPDP(0))
         END IF
        END IF
        IF(.not.(FACFADC(0).eq.'??'.or.FACFADC(0).eq.'OK'))THEN
         IF(FACFADC(0).eq.'++'.or.FACFADC(0).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Adjustment (Full Series)','oversmoothing',
     &                  FACFADP(0))
         ELSE
          CALL mkOudgRow(Ng,'Adjustment (Full Series)','undersmoothing',
     &                  FACFADP(0))
         END IF
        END IF
        IF(.not.(FACFSDC(0).eq.'??'.or.FACFSDC(0).eq.'OK'))THEN
         IF(FACFSDC(0).eq.'++'.or.FACFSDC(0).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Seasonal (Full Series)','oversmoothing',
     &                   FACFSDP(0))
         ELSE
          CALL mkOudgRow(Ng,'Seasonal (Full Series)','undersmoothing',
     &                   FACFSDP(0))
         END IF
        END IF
        IF(.not.(FACFIDC(0).eq.'??'.or.FACFIDC(0).eq.'OK'))THEN
         IF(FACFIDC(0).eq.'++'.or.FACFIDC(0).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Irregular (Full Series)','oversmoothing',
     &                   FACFIDP(0))
         ELSE
          CALL mkOudgRow(Ng,'Irregular (Full Series)','undersmoothing',
     &                   FACFIDP(0))
         END IF
        END IF
        IF(.not.(NACFPDC(0).eq.'??'.or.NACFPDC(0).eq.'OK'))THEN
         IF(NACFPDC(0).eq.'++'.or.NACFPDC(0).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Trend-Cycle (Trimmed Series)',
     &                   'oversmoothing',NACFPDP(0))
         ELSE
          CALL mkOudgRow(Ng,'Trend-Cycle (Trimmed Series)',
     &                   'undersmoothing',NACFPDP(0))
         END IF
        END IF
        IF(.not.(NACFADC(0).eq.'??'.or.NACFADC(0).eq.'OK'))THEN
         IF(NACFADC(0).eq.'++'.or.NACFADC(0).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Adjustment (Trimmed Series)',
     &                   'oversmoothing',NACFADP(0))
         ELSE
          CALL mkOudgRow(Ng,'Adjustment (Trimmed Series)',
     &                   'undersmoothing',NACFADP(0))
         END IF
        END IF
        IF(.not.(NACFSDC(0).eq.'??'.or.NACFSDC(0).eq.'OK'))THEN
         IF(NACFSDC(0).eq.'++'.or.NACFSDC(0).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Seasonal (Trimmed Series)','oversmoothing',
     &                   NACFSDP(0))
         ELSE
          CALL mkOudgRow(Ng,'Seasonal (Trimmed Series)',
     &                   'undersmoothing',NACFSDP(0))
         END IF
        END IF
        IF(.not.(NACFIDC(0).eq.'??'.or.NACFIDC(0).eq.'OK'))THEN
         IF(NACFIDC(0).eq.'++'.or.NACFIDC(0).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Irregular (Trimmed Series)',
     &                   'oversmoothing',NACFIDP(0))
         ELSE
          CALL mkOudgRow(Ng,'Irregular (Trimmed Series)',
     &                   'undersmoothing',NACFIDP(0))
         END IF
        END IF
        IF(.not.(WACFPDC(0).eq.'??'.or.WACFPDC(0).eq.'OK'))THEN
         IF(WACFPDC(0).eq.'++'.or.WACFPDC(0).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Trend-Cycle (Weighted)','oversmoothing',
     &                  WACFPDP(0))
         ELSE
          CALL mkOudgRow(Ng,'Trend-Cycle (Weighted)','undersmoothing',
     &                  WACFPDP(0))
         END IF
        END IF
        IF(.not.(WACFADC(0).eq.'??'.or.WACFADC(0).eq.'OK'))THEN
         IF(WACFADC(0).eq.'++'.or.WACFADC(0).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Adjustment (Weighted)','oversmoothing',
     &                  WACFADP(0))
         ELSE
          CALL mkOudgRow(Ng,'Adjustment (Weighted)','undersmoothing',
     &                  WACFADP(0))
         END IF
        END IF
        IF(.not.(WACFSDC(0).eq.'??'.or.WACFSDC(0).eq.'OK'))THEN
         IF(WACFSDC(0).eq.'++'.or.WACFSDC(0).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Seasonal (Weighted)','oversmoothing',
     &                  WACFSDP(0))
         ELSE
          CALL mkOudgRow(Ng,'Seasonal (Weighted)','undersmoothing',
     &                  WACFSDP(0))
         END IF
        END IF
        IF(.not.(WACFIDC(0).eq.'??'.or.WACFIDC(0).eq.'OK'))THEN
         IF(WACFIDC(0).eq.'++'.or.WACFIDC(0).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Irregular (Weighted)','oversmoothing',
     &                  WACFIDP(0))
         ELSE
          CALL mkOudgRow(Ng,'Irregular (Weighted)','undersmoothing',
     &                  WACFIDP(0))
         END IF
        END IF
        CALL writTag(Ng,'</table></div>')
        CALL mkPOneLine(Ng,'@','&nbsp;')
       END IF
c     ------------------------------------------------------------------
       IF(nsiga1.eq.0)THEN
        CALL mkPOneLine(Ng,'center','None of the over/under '//
     &                  'estimation tests for First Order '//
     &                  'Autocovariance is significant')
       ELSE
        Inlgfl=Inlgfl+1
        WRITE(Ng,1000)Inlgfl
        CALL mkTableTag(Ng,'w60','Components for which the over/under'//
     &                  ' estimation tests of First Order '//
     &                  'Autocovariance are significant')
        CALL mkCaption(Ng,'Significant over/under estimation tests of'//
     &                 ' First Order Autocovariance')
        CALL writTag(Mt1,'<tr>')
        CALL mkTableCell(Ng,'head','&nbsp;')
        CALL mkHeaderCellScope(Ng,0,0,'col','@','Type')
        CALL mkHeaderCellScope(Ng,0,0,'col','@','p-value')
        CALL writTag(Mt1,'</tr>')
        IF(.not.(FACFPDC(1).eq.'??'.or.FACFPDC(1).eq.'OK'))THEN
         IF(FACFPDC(1).eq.'++'.or.FACFPDC(1).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Trend-Cycle (Full Series)','oversmoothing',
     &                  FACFPDP(1))
         ELSE
          CALL mkOudgRow(Ng,'Trend-Cycle (Full Series)',
     &                   'undersmoothing',FACFPDP(1))
         END IF
        END IF
        IF(.not.(FACFADC(1).eq.'??'.or.FACFADC(1).eq.'OK'))THEN
         IF(FACFADC(1).eq.'++'.or.FACFADC(1).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Adjustment (Full Series)','oversmoothing',
     &                  FACFADP(1))
         ELSE
          CALL mkOudgRow(Ng,'Adjustment (Full Series)','undersmoothing',
     &                  FACFADP(1))
         END IF
        END IF
        IF(.not.(FACFSDC(1).eq.'??'.or.FACFSDC(1).eq.'OK'))THEN
         IF(FACFSDC(1).eq.'++'.or.FACFSDC(1).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Seasonal (Full Series)','oversmoothing',
     &                  FACFSDP(1))
         ELSE
          CALL mkOudgRow(Ng,'Seasonal (Full Series)','undersmoothing',
     &                  FACFSDP(1))
         END IF
        END IF
        IF(.not.(FACFIDC(1).eq.'??'.or.FACFIDC(1).eq.'OK'))THEN
         IF(FACFIDC(1).eq.'++'.or.FACFIDC(1).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Irregular (Full Series)','oversmoothing',
     &                  FACFIDP(1))
         ELSE
          CALL mkOudgRow(Ng,'Irregular (Full Series)','undersmoothing',
     &                  FACFIDP(1))
         END IF
        END IF
        IF(.not.(NACFPDC(1).eq.'??'.or.NACFPDC(1).eq.'OK'))THEN
         IF(NACFPDC(1).eq.'++'.or.NACFPDC(1).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Trend-Cycle (Trimmed Series)',
     &                   'oversmoothing',NACFPDP(1))
         ELSE
          CALL mkOudgRow(Ng,'Trend-Cycle (Trimmed Series)',
     &                   'undersmoothing',NACFPDP(1))
         END IF
        END IF
        IF(.not.(NACFADC(1).eq.'??'.or.NACFADC(1).eq.'OK'))THEN
         IF(NACFADC(1).eq.'++'.or.NACFADC(1).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Adjustment (Trimmed Series)',
     &                   'oversmoothing',NACFADP(1))
         ELSE
          CALL mkOudgRow(Ng,'Adjustment (Trimmed Series)',
     &                   'undersmoothing',NACFADP(1))
         END IF
        END IF
        IF(.not.(NACFSDC(1).eq.'??'.or.NACFSDC(1).eq.'OK'))THEN
         IF(NACFSDC(1).eq.'++'.or.NACFSDC(1).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Seasonal (Trimmed Series)',
     &                   'oversmoothing',NACFSDP(1))
         ELSE
          CALL mkOudgRow(Ng,'Seasonal (Trimmed Series)',
     &                   'undersmoothing',NACFSDP(1))
         END IF
        END IF
        IF(.not.(NACFIDC(1).eq.'??'.or.NACFIDC(1).eq.'OK'))THEN
         IF(NACFIDC(1).eq.'++'.or.NACFIDC(1).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Irregular (Trimmed Series)',
     &                   'oversmoothing',NACFIDP(1))
         ELSE
          CALL mkOudgRow(Ng,'Irregular (Trimmed Series)',
     &                   'undersmoothing',NACFIDP(1))
         END IF
        END IF
        IF(.not.(WACFPDC(1).eq.'??'.or.WACFPDC(1).eq.'OK'))THEN
         IF(WACFPDC(1).eq.'++'.or.WACFPDC(1).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Trend-Cycle (Weighted)','oversmoothing',
     &                  WACFPDP(1))
         ELSE
          CALL mkOudgRow(Ng,'Trend-Cycle (Weighted)','undersmoothing',
     &                  WACFPDP(1))
         END IF
        END IF
        IF(.not.(WACFADC(1).eq.'??'.or.WACFADC(1).eq.'OK'))THEN
         IF(WACFADC(1).eq.'++'.or.WACFADC(1).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Adjustment (Weighted)','oversmoothing',
     &                  WACFADP(1))
         ELSE
          CALL mkOudgRow(Ng,'Adjustment (Weighted)','undersmoothing',
     &                  WACFADP(1))
         END IF
        END IF
        IF(.not.(WACFSDC(1).eq.'??'.or.WACFSDC(1).eq.'OK'))THEN
         IF(WACFSDC(1).eq.'++'.or.WACFSDC(1).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Seasonal (Weighted)','oversmoothing',
     &                  WACFSDP(1))
         ELSE
          CALL mkOudgRow(Ng,'Seasonal (Weighted)','undersmoothing',
     &                  WACFSDP(1))
         END IF
        END IF
        IF(.not.(WACFIDC(1).eq.'??'.or.WACFIDC(1).eq.'OK'))THEN
         IF(WACFIDC(1).eq.'++'.or.WACFIDC(1).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Irregular (Weighted)','oversmoothing',
     &                  WACFIDP(1))
         ELSE
          CALL mkOudgRow(Ng,'Irregular (Weighted)','undersmoothing',
     &                  WACFIDP(1))
         END IF
        END IF
        CALL writTag(Ng,'</table></div>')
        CALL mkPOneLine(Ng,'@','&nbsp;')
       END IF
c     ------------------------------------------------------------------
       IF(nsigas.eq.0)THEN
        CALL mkPOneLine(Ng,'center','None of the over/under '//
     &                     'estimation tests for Seasonal Order '//
     &                     'Autocovariance is significant')
       ELSE
        Inlgfl=Inlgfl+1
        WRITE(Ng,1000)Inlgfl
        CALL mkTableTag(Ng,'w60','Components for which the over/under'//
     &                  ' estimation tests of Seasonal Order '//
     &                  'Autocovariance are significant')
        CALL mkCaption(Ng,'Significant over/under estimation tests of'//
     &                 ' Seasonal Order Autocovariance')
        CALL writTag(Mt1,'<tr>')
        CALL mkTableCell(Ng,'head','&nbsp;')
        CALL mkHeaderCellScope(Ng,0,0,'col','@','Type')
        CALL mkHeaderCellScope(Ng,0,0,'col','@','p-value')
        CALL writTag(Mt1,'</tr>')
        IF(.not.(FACFPDC(Ny).eq.'??'.or.FACFPDC(Ny).eq.'OK'))THEN
         IF(FACFPDC(Ny).eq.'++'.or.FACFPDC(Ny).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Trend-Cycle (Full Series)','oversmoothing',
     &                  FACFPDP(Ny))
         ELSE
          CALL mkOudgRow(Ng,'Trend-Cycle (Full Series)',
     &                   'undersmoothing',FACFPDP(Ny))
         END IF
        END IF
        IF(.not.(FACFADC(Ny).eq.'??'.or.FACFADC(Ny).eq.'OK'))THEN
         IF(FACFADC(Ny).eq.'++'.or.FACFADC(Ny).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Adjustment (Full Series)','oversmoothing',
     &                  FACFADP(Ny))
         ELSE
          CALL mkOudgRow(Ng,'Adjustment (Full Series)','undersmoothing',
     &                  FACFADP(Ny))
         END IF
        END IF
        IF(.not.(FACFSDC(Ny).eq.'??'.or.FACFSDC(Ny).eq.'OK'))THEN
         IF(FACFSDC(Ny).eq.'++'.or.FACFSDC(Ny).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Seasonal (Full Series)','oversmoothing',
     &                  FACFSDP(Ny))
         ELSE
          CALL mkOudgRow(Ng,'Seasonal (Full Series)','undersmoothing',
     &                  FACFSDP(Ny))
         END IF
        END IF
        IF(.not.(FACFIDC(Ny).eq.'??'.or.FACFIDC(Ny).eq.'OK'))THEN
         IF(FACFIDC(Ny).eq.'++'.or.FACFIDC(Ny).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Irregular (Full Series)','oversmoothing',
     &                  FACFIDP(Ny))
         ELSE
          CALL mkOudgRow(Ng,'Irregular (Full Series)','undersmoothing',
     &                  FACFIDP(Ny))
         END IF
        END IF
        IF(.not.(NACFPDC(Ny).eq.'??'.or.NACFPDC(Ny).eq.'OK'))THEN
         IF(NACFPDC(Ny).eq.'++'.or.NACFPDC(Ny).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Trend-Cycle (Trimmed Series)',
     &                   'oversmoothing',NACFPDP(Ny))
         ELSE
          CALL mkOudgRow(Ng,'Trend-Cycle (Trimmed Series)',
     &                   'undersmoothing',NACFPDP(Ny))
         END IF
        END IF
        IF(.not.(NACFADC(Ny).eq.'??'.or.NACFADC(Ny).eq.'OK'))THEN
         IF(NACFADC(Ny).eq.'++'.or.NACFADC(Ny).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Adjustment (Trimmed Series)',
     &                   'oversmoothing',NACFADP(Ny))
         ELSE
          CALL mkOudgRow(Ng,'Adjustment (Trimmed Series)',
     &                   'undersmoothing',NACFADP(Ny))
         END IF
        END IF
        IF(.not.(NACFSDC(Ny).eq.'??'.or.NACFSDC(Ny).eq.'OK'))THEN
         IF(NACFSDC(Ny).eq.'++'.or.NACFSDC(Ny).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Seasonal (Trimmed Series)','oversmoothing',
     &                  NACFSDP(Ny))
         ELSE
          CALL mkOudgRow(Ng,'Seasonal (Trimmed Series)',
     &                   'undersmoothing',NACFSDP(Ny))
         END IF
        END IF
        IF(.not.(NACFIDC(Ny).eq.'??'.or.NACFIDC(Ny).eq.'OK'))THEN
         IF(NACFIDC(Ny).eq.'++'.or.NACFIDC(Ny).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Irregular (Trimmed Series)',
     &                   'oversmoothing',NACFIDP(Ny))
         ELSE
          CALL mkOudgRow(Ng,'Irregular (Trimmed Series)',
     &                   'undersmoothing',NACFIDP(Ny))
         END IF
        END IF
        IF(.not.(WACFPDC(Ny).eq.'??'.or.WACFPDC(Ny).eq.'OK'))THEN
         IF(WACFPDC(Ny).eq.'++'.or.WACFPDC(Ny).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Trend-Cycle (Weighted)','oversmoothing',
     &                  WACFPDP(Ny))
         ELSE
          CALL mkOudgRow(Ng,'Trend-Cycle (Weighted)','undersmoothing',
     &                  WACFPDP(Ny))
         END IF
        END IF
        IF(.not.(WACFADC(Ny).eq.'??'.or.WACFADC(Ny).eq.'OK'))THEN
         IF(WACFADC(Ny).eq.'++'.or.WACFADC(Ny).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Adjustment (Weighted)','oversmoothing',
     &                  WACFADP(Ny))
         ELSE
          CALL mkOudgRow(Ng,'Adjustment (Weighted)','undersmoothing',
     &                  WACFADP(Ny))
         END IF
        END IF
        IF(.not.(WACFSDC(Ny).eq.'??'.or.WACFSDC(Ny).eq.'OK'))THEN
         IF(WACFSDC(Ny).eq.'++'.or.WACFSDC(Ny).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Seasonal (Weighted)','oversmoothing',
     &                  WACFSDP(Ny))
         ELSE
          CALL mkOudgRow(Ng,'Seasonal (Weighted)','undersmoothing',
     &                  WACFSDP(Ny))
         END IF
        END IF
        IF(.not.(WACFIDC(Ny).eq.'??'.or.WACFIDC(Ny).eq.'OK'))THEN
         IF(WACFIDC(Ny).eq.'++'.or.WACFIDC(Ny).eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Irregular (Weighted)','oversmoothing',
     &                  WACFIDP(Ny))
         ELSE
          CALL mkOudgRow(Ng,'Irregular (Weighted)','undersmoothing',
     &                  WACFIDP(Ny))
         END IF
        END IF
        CALL writTag(Ng,'</table></div>')
        CALL mkPOneLine(Ng,'@','&nbsp;')
       END IF
c     ------------------------------------------------------------------
       IF(nsigcc.eq.0)THEN
        CALL mkPOneLine(Ng,'center','None of the over/under '//
     &                  'estimation tests for Crosscovariance is '//
     &                  'significant')
       ELSE
        Inlgfl=Inlgfl+1
        WRITE(Ng,1000)Inlgfl
        CALL mkTableTag(Ng,'w60','Components for which the over/under'//
     &                  ' estimation tests of Crosscovariance are'//
     &                  ' significant')
        CALL mkCaption(Ng,'Significant over/under estimation tests of'//
     &                 ' Crosscovariance')
        CALL writTag(Mt1,'<tr>')
        CALL mkTableCell(Ng,'head','&nbsp;')
        CALL mkHeaderCellScope(Ng,0,0,'col','@','Type')
        CALL mkHeaderCellScope(Ng,0,0,'col','@','p-value')
        CALL writTag(Mt1,'</tr>')
        IF(.not.(seaIrrDgC.eq.'??'.or.seaIrrDgC.eq.'OK'))THEN
         IF(seaIrrDgC.eq.'++'.or.seaIrrDgC.eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Seasonal/Irregular',
     &                   'positive crosscovariance',seaIrrDgP)
         ELSE
          CALL mkOudgRow(Ng,'Seasonal/Irregular',
     &                   'negative crosscovariance',seaIrrDgP)
         END IF
        END IF
        IF(.not.(seaTreDgC.eq.'??'.or.seaTreDgC.eq.'OK'))THEN
        IF(seaTreDgC.eq.'++'.or.seaTreDgC.eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Seasonal/Trend-Cycle',
     &                  'positive crosscovariance',seaTreDgP)
         ELSE
          CALL mkOudgRow(Ng,'Seasonal/Trend-Cycle',
     &                  'negative crosscovariance',seaTreDgp)
         END IF
        END IF
        IF(.not.(treIrrDgC.eq.'??'.or.treIrrDgC.eq.'OK'))THEN
         IF(treIrrDgC.eq.'++'.or.treIrrDgC.eq.'+ ')THEN
          CALL mkOudgRow(Ng,'Trend-Cycle/Irregular',
     &                  'positive crosscovariance',treIrrDgp)
         ELSE
          CALL mkOudgRow(Ng,'Trend-Cycle/Irregular',
     &                  'negative crosscovariance',treIrrDgp)
         END IF
        END IF
        CALL writTag(Ng,'</table></div>')
        CALL mkPOneLine(Ng,'@','&nbsp;')
       END IF
      END IF
      
c     ------------------------------------------------------------------
c     Save significant over/under estimation tests to log file
c     ------------------------------------------------------------------
      IF (Lsumm.gt.0) THEN
       WRITE(Nform,1010)'nsigoustatvar: ',nsigv
       WRITE(Nform,1010)'nsigoustat1auto: ',nsiga1
       WRITE(Nform,1010)'nsigoustatsauto: ',nsigas
       WRITE(Nform,1010)'nsigoustatcrosscov: ',nsigcc
       IF(.not.(FACFPDC(0).eq.'??'))
     &    WRITE(Nform,1020)'oustatvartcfull: ',FACFPDG(0),FACFPDP(0)
       IF(.not.(FACFADC(0).eq.'??'))
     &    WRITE(Nform,1020)'oustatvarsafull: ',FACFADG(0),FACFADP(0)
       IF(.not.(FACFSDC(0).eq.'??'))
     &    WRITE(Nform,1020)'oustatvarsffull: ',FACFSDG(0),FACFSDP(0)
       IF(.not.(FACFIDC(0).eq.'??'))
     &    WRITE(Nform,1020)'oustatvarirfull: ',FACFIDG(0),FACFIDP(0)
       IF(.not.(NACFPDC(0).eq.'??'))
     &    WRITE(Nform,1020)'oustatvartctrim: ',NACFPDG(0),NACFPDP(0)
       IF(.not.(NACFADC(0).eq.'??'))
     &    WRITE(Nform,1020)'oustatvarsatrim: ',NACFADG(0),NACFADP(0)
       IF(.not.(NACFSDC(0).eq.'??'))
     &    WRITE(Nform,1020)'oustatvarsftrim: ',NACFSDG(0),NACFSDP(0)
       IF(.not.(NACFIDC(0).eq.'??'))
     &    WRITE(Nform,1020)'oustatvarirtrim: ',NACFIDG(0),NACFIDP(0)
       IF(.not.(WACFPDC(1).eq.'??'))
     &    WRITE(Nform,1020)'oustatvartcwt: ',WACFPDG(0),WACFPDP(0)
       IF(.not.(WACFADC(0).eq.'??'))
     &    WRITE(Nform,1020)'oustatvarsawt: ',WACFADG(0),WACFADP(0)
       IF(.not.(WACFSDC(0).eq.'??'))
     &    WRITE(Nform,1020)'oustatvarsfwt: ',WACFSDG(0),WACFSDP(0)
       IF(.not.(WACFIDC(0).eq.'??'))
     &    WRITE(Nform,1020)'oustatvarirwt: ',WACFIDG(0),WACFIDP(0)
       IF(.not.(FACFPDC(1).eq.'??'))
     &    WRITE(Nform,1020)'oustat1autotcfull: ',FACFPDG(1),FACFPDP(1)
       IF(.not.(FACFADC(1).eq.'??'))
     &    WRITE(Nform,1020)'oustat1autosafull: ',FACFADG(1),FACFADP(1)
       IF(.not.(FACFSDC(1).eq.'??'))
     &    WRITE(Nform,1020)'oustat1autosffull: ',FACFSDG(1),FACFSDP(1)
       IF(.not.(FACFIDC(1).eq.'??'))
     &    WRITE(Nform,1020)'oustat1autoirfull: ',FACFIDG(1),FACFIDP(1)
       IF(.not.(NACFPDC(1).eq.'??'))
     &    WRITE(Nform,1020)'oustat1autotctrim: ',NACFPDG(1),NACFPDP(1)
       IF(.not.(NACFADC(1).eq.'??'))
     &    WRITE(Nform,1020)'oustat1autosatrim: ',NACFADG(1),NACFADP(1)
       IF(.not.(NACFSDC(1).eq.'??'))
     &    WRITE(Nform,1020)'oustat1autosftrim: ',NACFSDG(1),NACFSDP(1)
       IF(.not.(NACFIDC(1).eq.'??'))
     &    WRITE(Nform,1020)'oustat1autoirtrim: ',NACFIDG(1),NACFIDP(1)
       IF(.not.(WACFPDC(1).eq.'??'))
     &    WRITE(Nform,1020)'oustat1autotcwt: ',WACFPDG(1),WACFPDP(1)
       IF(.not.(WACFADC(1).eq.'??'))
     &    WRITE(Nform,1020)'oustat1autosawt: ',WACFADG(1),WACFADP(1)
       IF(.not.(WACFSDC(1).eq.'??'))
     &    WRITE(Nform,1020)'oustat1autosfwt: ',WACFSDG(1),WACFSDP(1)
       IF(.not.(WACFIDC(1).eq.'??'))
     &    WRITE(Nform,1020)'oustat1autoirwt: ',WACFIDG(1),WACFIDP(1)
       IF(.not.(FACFPDC(Ny).eq.'??'))
     &    WRITE(Nform,1020)'oustatsautotcfull: ',FACFPDG(Ny),FACFPDP(Ny)
       IF(.not.(FACFADC(Ny).eq.'??'))
     &    WRITE(Nform,1020)'oustatsautosafull: ',FACFADG(Ny),FACFADP(Ny)
       IF(.not.(FACFSDC(Ny).eq.'??'))
     &    WRITE(Nform,1020)'oustatsautosffull: ',FACFSDG(Ny),FACFSDP(Ny)
       IF(.not.(FACFIDC(Ny).eq.'??'))
     &    WRITE(Nform,1020)'oustatsautoirfull: ',FACFIDG(Ny),FACFIDP(Ny)
       IF(.not.(NACFPDC(Ny).eq.'??'))
     &    WRITE(Nform,1020)'oustatsautotctrim: ',NACFPDG(Ny),NACFPDP(Ny)
       IF(.not.(NACFADC(Ny).eq.'??'))
     &    WRITE(Nform,1020)'oustatsautosatrim: ',NACFADG(Ny),NACFADP(Ny)
       IF(.not.(NACFSDC(Ny).eq.'??'))
     &    WRITE(Nform,1020)'oustatsautosftrim: ',NACFSDG(Ny),NACFSDP(Ny)
       IF(.not.(NACFIDC(Ny).eq.'??'))
     &    WRITE(Nform,1020)'oustatsautoirtrim: ',NACFIDG(Ny),NACFIDP(Ny)
       IF(.not.(WACFPDC(Ny).eq.'??'))
     &    WRITE(Nform,1020)'oustatsautotcwt: ',WACFPDG(Ny),WACFPDP(Ny)
       IF(.not.(WACFADC(Ny).eq.'??'))
     &    WRITE(Nform,1020)'oustatsautosawt: ',WACFADG(Ny),WACFADP(Ny)
       IF(.not.(WACFSDC(Ny).eq.'??'))
     &    WRITE(Nform,1020)'oustatsautosfwt: ',WACFSDG(Ny),WACFSDP(Ny)
       IF(.not.(WACFIDC(Ny).eq.'??'))
     &    WRITE(Nform,1020)'oustatsautoirwt: ',WACFIDG(Ny),WACFIDP(Ny)
       IF(.not.(seaIrrDgC.eq.'??'))
     &    WRITE(Nform,1020)'oustatccorsfir: ',seaIrrDia,seaIrrDgP
       IF(.not.(seaTreDgC.eq.'??'))
     &    WRITE(Nform,1020)'oustatccorsftc: ',seaTreDia,seaTreDgP
       IF(.not.(treIrrDgC.eq.'??'))
     &    WRITE(Nform,1020)'oustatccortcir: ',treIrrDia,treIrrDgP
      END IF
c     ------------------------------------------------------------------
 1000 FORMAT('<div id="lgoustat',i6.6,'">')
 1010 FORMAT(a,i3)
 1020 FORMAT(a,2e21.14)
c     ------------------------------------------------------------------
      RETURN
      END
c     ------------------------------------------------------------------
      SUBROUTINE mkOudgRow(Fh,thisHeader,thisText,thisReal)
      IMPLICIT NONE
c     ------------------------------------------------------------------
      DOUBLE PRECISION thisReal
      CHARACTER thisHeader*(*),thisText*(*)
      INTEGER Fh
c     ------------------------------------------------------------------
      CALL writTag(Fh,'<tr>')
      CALL mkHeaderCellScope(Fh,0,0,'row','@',thisHeader)
      CALL mkTableCell(Fh,'center',thisText)
      WRITE(Fh,1010)thisReal
      CALL writTag(Fh,'</tr>')
c     ------------------------------------------------------------------
 1010 FORMAT('<td class="center">',f10.4,'</td>')
c     ------------------------------------------------------------------
      RETURN
      END
c     ------------------------------------------------------------------
