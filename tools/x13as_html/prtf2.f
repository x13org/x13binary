C     Last change:  BCM  16 Feb 1999    3:48 pm
      SUBROUTINE prtf2(Nw,Mqf2,Khcfm)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C --- THIS SUBROUTINE GENERATES THE F2 TABLE for standard printouts.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'title.cmn'
      INCLUDE 'inpt2.cmn'
      INCLUDE 'work2.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'tests.cmn'
      INCLUDE 'mq3.cmn'
      INCLUDE 'agr.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      CHARACTER ctmp*(3),Mqf2*(7),aorb*(1),thisHdr1*(20),thisHdr2*(20),
     &          thisLag*(7),thisId*(6)
      INTEGER i,Khcfm,n,n1,Nw,itmp,imu,istd
      DIMENSION aorb(2)
c-----------------------------------------------------------------------
      INTEGER nblank
      EXTERNAL nblank
c-----------------------------------------------------------------------
      DATA aorb/'B','A'/
c-----------------------------------------------------------------------
      CALL writTagOneLine(Nw,'h3','@',
     &                    'F 2.A: Average '//Pcdif(1:nblank(Pcdif))//
     &                    ' without regard to sign over the indicated'//
     &                    ' span')
      CALL mkTableTag(Nw,'w70','Average '//Pcdif(1:nblank(Pcdif))//
     &                ' without regard to sign over the indicated span')
      CALL mkCaption(Nw,'Table F 2.A')
c-----------------------------------------------------------------------
      CALL writTag(Nw,'<tr>')
      CALL mkTableCell(Nw,'head','&nbsp;')
      CALL mkHeaderCellScope(Nw,0,0,'col','Original',
     &                       aorb(Khcfm)//'1'//Cbr//'O')
      CALL mkHeaderCellScope(Nw,0,0,'col','Seasonally Adjusted series',
     &                       'D11'//Cbr//'CI')
      CALL mkHeaderCellScope(Nw,0,0,'col','Irregular','D13'//Cbr//'I')
      CALL mkHeaderCellScope(Nw,0,0,'col','Trend-Cycle',
     &                       'D12'//Cbr//'C')
      CALL mkHeaderCellScope(Nw,0,0,'col','Seasonal','D10'//Cbr//'S')
      CALL mkHeaderCellScope(Nw,0,0,'col','Prior','A2'//Cbr//'P')
      CALL mkHeaderCellScope(Nw,0,0,'col','Trading Day and Holiday',
     &                       'D18'//Cbr//'TD&amp;H')
      CALL mkHeaderCellScope(Nw,0,0,'col',Moqu(1:nblank(Moqu))//
     &                       's for cyclical dominance',
     &                       'F1'//Cbr//Mqcd)
      CALL mkHeaderCellScope(Nw,0,0,'col',
     &                       'Original Series Modified for Extremes',
     &                       'E1'//Cbr//'Mod.O')
      CALL mkHeaderCellScope(Nw,0,0,'col',
     &               'Seasonally Adjusted Series Modified for Extremes',
     &                       'E2'//Cbr//'Mod.CI')
      CALL mkHeaderCellScope(Nw,0,0,'col',
     &                       'Irregular Modified for Extremes',
     &                       'E3'//Cbr//'Mod.I')
      CALL writTag(Nw,'</tr>')
c-----------------------------------------------------------------------
      DO i=1,Ny
       CALL writTag(Nw,'<tr>')
       WRITE(Nw,1010)i
 1010  FORMAT('<th scope="row"> Span ',I6,'</th>')
       IF(dabs(Obar(i)).gt.9999)THEN
        WRITE(Nw,1021)Obar(i),Cibar(i),Ibar(i),Cbar(i),Sbar(i),Pbar(i)
 1021   FORMAT(3X,6('<td>',G15.8,'</td>'))
        WRITE(Nw,1031)Tdbar(i),Smbar(i),Ombar(i),Cimbar(i),Imbar(i)
 1031   FORMAT(3X,5('<td>',G15.8,'</td>'))
       ELSE
        WRITE(Nw,1020)Obar(i),Cibar(i),Ibar(i),Cbar(i),Sbar(i),Pbar(i)
 1020   FORMAT(3X,6('<td>',F8.2,'</td>'))
        WRITE(Nw,1030)Tdbar(i),Smbar(i),Ombar(i),Cimbar(i),Imbar(i)
 1030   FORMAT(3X,5('<td>',F8.2,'</td>'))
       END IF
       CALL writTag(Nw,'</tr>')
      END DO
c-----------------------------------------------------------------------
      CALL writTag(Nw,'</table>')
      CALL mkPOneLine(Nw,'@','&nbsp;')
c-----------------------------------------------------------------------
      CALL writTagOneLine(Nw,'h3','@',
     &         'F 2.B: Relative contributions to the variance of the '//
     &                    Pcdif(1:nblank(Pcdif))//
     &                    ' in the components of the original series')
      CALL mkTableTag(Nw,'w70',
     &                'Relative contributions to the variance of the '//
     &                Pcdif(1:nblank(Pcdif))//
     &                ' in the components of the original series')
      CALL mkCaption(Nw,'Table F 2.B')
c-----------------------------------------------------------------------
      CALL writTag(Nw,'<tr>')
      CALL mkTableCell(Nw,'head','&nbsp;')
      CALL mkHeaderCellScope(Nw,0,0,'col',
     &                       'Irregular Modified for Extremes',
     &                       'E3'//Cbr//'I')
      CALL mkHeaderCellScope(Nw,0,0,'col','Trend-Cycle',
     &                       'D12'//Cbr//'C')
      CALL mkHeaderCellScope(Nw,0,0,'col','Seasonal','D10'//Cbr//'S')
      CALL mkHeaderCellScope(Nw,0,0,'col','Prior','A2'//Cbr//'P')
      CALL mkHeaderCellScope(Nw,0,0,'col','Trading Day and Holiday',
     &                       'D18'//Cbr//'TD&amp;H')
      CALL mkHeaderCellScope(Nw,0,0,'col','@','TOTAL')
      CALL mkHeaderCellScope(Nw,0,0,'col','Ratio times 100',
     &                       'Ratio'//Cbr//'(X100)')
      CALL writTag(Nw,'</tr>')
c-----------------------------------------------------------------------
      DO i=1,Ny
      CALL writTag(Nw,'<tr>')
       WRITE(Nw,1010)i
       WRITE(Nw,1060)Isq(i),Csq(i),Ssq(i),Psq(i),Tdsq(i),Osq2(i)
 1060  FORMAT(3X,5('<td>',2PF8.2,'</td>'),'<td>100.00</td><td>',2PF8.2,
     &        '</td>')
      CALL writTag(Nw,'</tr>')
      END DO
c-----------------------------------------------------------------------
      CALL writTag(Nw,'</table>')
      CALL mkPOneLine(Nw,'@','&nbsp;')
c-----------------------------------------------------------------------
      CALL writTagOneLine(Nw,'h3','@','F 2.C: Average '//
     &                    Pcdif(1:nblank(Pcdif))//' with regard to '//
     &                'sign and standard deviation over indicated span')
      CALL mkTableTag(Nw,'w70','Average '//Pcdif(1:nblank(Pcdif))//
     &                ' with regard to sign and standard deviation '//
     &                'over indicated span')
      CALL mkCaption(Nw,'Table F 2.C')
c-----------------------------------------------------------------------
      CALL writTag(Nw,'<tr>')
      CALL mkTableCellSpan(Nw,'row',2,'head','&nbsp;')
      
      WRITE(thisId,1050)'or',Iagr
      CALL mkHeaderCellId(Nw,0,2,thisId,'@','Original',
     &                    aorb(Khcfm)//'1'//Cbr//'O')
      WRITE(thisId,1050)'ir',Iagr
      CALL mkHeaderCellId(Nw,0,2,thisId,'@','Irregular',
     &                    'D13'//Cbr//'I')
      WRITE(thisId,1050)'tc',Iagr
      CALL mkHeaderCellId(Nw,0,2,thisId,'@','Trend-Cycle',
     &                    'D12'//Cbr//'C')
      WRITE(thisId,1050)'sf',Iagr
      CALL mkHeaderCellId(Nw,0,2,thisId,'@','Seasonal','D10'//Cbr//'S')
      WRITE(thisId,1050)'ci',Iagr
      CALL mkHeaderCellId(Nw,0,2,thisId,'@',
     &                    'Seasonally Adjusted series',
     &                    'D11'//Cbr//'CI')
      WRITE(thisId,1050)'cd',Iagr
      CALL mkHeaderCellId(Nw,0,2,thisId,'@',Moqu(1:nblank(Moqu))//
     &                    's for cyclical dominance',
     &                    'F1'//Cbr//Mqcd)
      CALL writTag(Nw,'</tr>')

      CALL writTag(Nw,'<tr>')

      imu=Inmu
      istd=Insd
      DO i = 1, 6
       Inmu=Inmu+1
       WRITE(thisId,1050)'mu',Inmu
       CALL mkHeaderCellId(Nw,0,0,thisId,'@','Average','Avg.')
       Insd=Insd+1
       WRITE(thisId,1050)'sd',Insd
       CALL mkHeaderCellId(Nw,0,0,thisId,'@','Standard Deviation',
     &                     'S.D.')
      END DO
      CALL writTag(Nw,'</tr>')
c-----------------------------------------------------------------------
      DO i=1,Ny
       CALL writTag(Nw,'<tr>')
       WRITE(thisLag,1070)i
       WRITE(thisId,1050)'sp',i+(Iagr*Ny)
       CALL mkHeaderCellId(Nw,0,0,thisId,'@','@',thisLag)
       
       WRITE(thisHdr1,1080)i+(Iagr*Ny),'or',Iagr,'mu',imu+1
       WRITE(thisHdr2,1080)i+(Iagr*Ny),'or',Iagr,'sd',istd+1
       CALL prf2cr(Nw,Obar2(i),Osd(i),thisHdr1,thisHdr2)
       
       WRITE(thisHdr1,1080)i+(Iagr*Ny),'ir',Iagr,'mu',imu+2
       WRITE(thisHdr2,1080)i+(Iagr*Ny),'ir',Iagr,'sd',istd+2
       CALL prf2cr(Nw,Ibar2(i),Isd(i),thisHdr1,thisHdr2)
       
       WRITE(thisHdr1,1080)i+(Iagr*Ny),'tc',Iagr,'mu',imu+3
       WRITE(thisHdr2,1080)i+(Iagr*Ny),'tc',Iagr,'sd',istd+3
       CALL prf2cr(Nw,Cbar2(i),Csd(i),thisHdr1,thisHdr2)
       
       WRITE(thisHdr1,1080)i+(Iagr*Ny),'sf',Iagr,'mu',imu+4
       WRITE(thisHdr2,1080)i+(Iagr*Ny),'sf',Iagr,'sd',istd+4
       CALL prf2cr(Nw,Sbar2(i),Ssd(i),thisHdr1,thisHdr2)
       
       WRITE(thisHdr1,1080)i+(Iagr*Ny),'ci',Iagr,'mu',imu+5
       WRITE(thisHdr2,1080)i+(Iagr*Ny),'ci',Iagr,'sd',istd+5
       CALL prf2cr(Nw,Cibar2(i),Cisd(i),thisHdr1,thisHdr2)
       
       WRITE(thisHdr1,1080)i+(Iagr*Ny),'cd',Iagr,'mu',imu+6
       WRITE(thisHdr2,1080)i+(Iagr*Ny),'cd',Iagr,'sd',istd+6
       CALL prf2cr(Nw,Smbar2(i),Smsd(i),thisHdr1,thisHdr2)

       CALL writTag(Nw,'</tr>')
      END DO
 1050 FORMAT(a2,i4.4)
 1070 FORMAT('Span',i3)
 1080 FORMAT('sp',i4.4,2(' ',a2,i4.4))
c-----------------------------------------------------------------------
      CALL writTag(Nw,'</table>')
      CALL mkPOneLine(Nw,'@','&nbsp;')
c-----------------------------------------------------------------------
      CALL writTagOneLine(Nw,'h3','@','F 2.D: Average duration of run')
      CALL mkTableTag(Nw,'w50','Average duration of run')
      CALL mkCaption(Nw,'Table F 2.D')
c-----------------------------------------------------------------------
      CALL writTag(Nw,'<tr>')
      CALL mkHeaderCellScope(Nw,0,0,'col','Seasonally Adjusted series',
     &                       'CI')
      CALL mkHeaderCellScope(Nw,0,0,'col','Irregular','I')
      CALL mkHeaderCellScope(Nw,0,0,'col','Trend-Cycle','C')
      CALL mkHeaderCellScope(Nw,0,0,'col',Moqu(1:nblank(Moqu))//
     &                       's for cyclical dominance',Mqcd)
      CALL writTag(Nw,'</tr>')
c-----------------------------------------------------------------------
      WRITE(Nw,1100)Adrci,Adri,Adrc,Adrmcd
 1100 FORMAT('<tr>',4('<td class="center">',F8.2,'</td>'),'</tr>')
c-----------------------------------------------------------------------
      CALL writTag(Nw,'</table>')
      CALL mkPOneLine(Nw,'@','&nbsp;')
c-----------------------------------------------------------------------
*      IF(Ny.eq.12)Kpage=Kpage+1
c-----------------------------------------------------------------------
      CALL writTagOneLine(Nw,'h3','@','F 2.E: I/C Ratio for '//
     &                    Moqu(1:nblank(Moqu))//'s span')
      CALL mkTableTag(Nw,'w70','I/C Ratio for '//
     &                    Moqu(1:nblank(Moqu))//'s span')
      CALL mkCaption(Nw,'Table F 2.E')
c-----------------------------------------------------------------------
      CALL writTag(Nw,'<tr>')
      CALL mkTableCell(Nw,'head','&nbsp;')
      DO i=1,Ny
       WRITE(Nw,1011)i
 1011  FORMAT('<th scope="col"> Span ',I6,'</th>')
      END DO
      CALL writTag(Nw,'</tr>')
      CALL writTag(Nw,'<tr>')
      CALL mkHeaderCellScope(Nw,0,0,'row','I over C ratio','I/C')
      DO i=1,Ny
       WRITE(Nw,1130)Smic(i)
 1130  FORMAT(3x,'<td class="center">',F8.2,'</td>')
      END DO
      CALL writTag(Nw,'</tr>')
c-----------------------------------------------------------------------
      CALL writTag(Nw,'</table>')
      CALL mkPOneLine(Nw,'@','&nbsp;')
c-----------------------------------------------------------------------
      WRITE(Nw,1140)Moqu(1:nblank(Moqu)),Mcd
 1140 FORMAT(/,'<p class="center"><strong>',a7,
     &         's for cyclical dominance:</strong>',i8,'</p>')
      CALL mkPOneLine(Nw,'@','&nbsp;')
c-----------------------------------------------------------------------
      CALL writTagOneLine(Nw,'h3','@',
     &                    'Relative contribution of the components '//
     &                    'to the stationary portion of the variance '//
     &                    'in the original series')
      CALL mkTableTag(Nw,'w70',
     &                'Relative contribution of the components to '//
     &                'the stationary portion of the variance in the '//
     &                'original series')
      CALL mkCaption(Nw,'Table F 2.F')
c-----------------------------------------------------------------------
      CALL writTag(Nw,'<tr>')
      CALL mkHeaderCellScope(Nw,0,0,'col','Irregular','I')
      CALL mkHeaderCellScope(Nw,0,0,'col','Trend-Cycle','C')
      CALL mkHeaderCellScope(Nw,0,0,'col','Seasonal','S')
      CALL mkHeaderCellScope(Nw,0,0,'col','Prior','P')
      CALL mkHeaderCellScope(Nw,0,0,'col','Trading Day and Holiday',
     &                       'TD&amp;H')
      CALL mkHeaderCellScope(Nw,0,0,'col','@','Total')
      CALL writTag(Nw,'</tr>')
c-----------------------------------------------------------------------
      WRITE(Nw,1150)Vi,Vc,Vs,Vp,Vtd,Rv
 1150 FORMAT('<tr>',6('<td class="center">',F8.2,'</td>'),'</tr>')
c-----------------------------------------------------------------------
      CALL writTag(Nw,'</table>')
      CALL mkPOneLine(Nw,'@','&nbsp;')
c-----------------------------------------------------------------------
      n=Ny+2
      itmp=1
      CALL itoc(n,ctmp,itmp)
      CALL writTagOneLine(Nw,'h3','@','F 2.G: The autocorrelation of '//
     &                    'the irregulars for spans 1 to '//
     &                    ctmp(1:(itmp-1)))
      CALL mkTableTag(Nw,'w80','The autocorrelation of the '//
     &                'irregulars for spans 1 to '//ctmp(1:(itmp-1)))
      CALL mkCaption(Nw,'Table F 2.G')
c-----------------------------------------------------------------------
      CALL writTag(Nw,'<tr>')
      CALL mkTableCell(Nw,'head','&nbsp;')
      DO i=1,n
       WRITE(Nw,1011)i
      END DO
      CALL writTag(Nw,'</tr>')
      CALL writTag(Nw,'<tr>')
      CALL mkHeaderCellScope(Nw,0,0,'row','autocorrelation function',
     &                       'ACF')
      DO i=1,n
       IF(Autoc(i).lt.0D0)THEN
        WRITE(Nw,1160)Autoc(i)
       ELSE
        WRITE(Nw,1170)Autoc(i)
       END IF
      END DO
      CALL writTag(Nw,'</tr>')
 1160 FORMAT(3x,'<td class="nowrapcenter">',f8.2,'</td>')
 1170 FORMAT(3x,'<td class="center">',f8.2,'</td>')
c-----------------------------------------------------------------------
      CALL writTag(Nw,'</table>')
      CALL mkPOneLine(Nw,'@','&nbsp;')
c-----------------------------------------------------------------------
      WRITE(Nw,1180)Cbr,Ratic,Cbr
 1180 FORMAT(/,3x,'<p><strong>F 2.H:</strong>',a,'&nbsp;&nbsp;&nbsp;',
     &            '<strong>The final <abbr title="I over C">I/C</abbr>',
     &            ' Ratio from Table D12:</strong>',f12.2,a)
      IF(Kfulsm.lt.2)WRITE(Nw,1181)Ratis
 1181 FORMAT(' &nbsp;&nbsp;&nbsp;<strong>The final <abbr title=',
     &       '"I over S">I/S</abbr> Ratio from Table D10:</strong>',
     &       F12.2)
      CALL writTag(Nw,'</p>')
      CALL mkPOneLine(Nw,'@','&nbsp;')
c-----------------------------------------------------------------------
      CALL writTagOneLine(Nw,'h3','@','F 2.I: Seasonality tests')
      CALL mkTableTag(Nw,'w80','Seasonality tests')
      CALL mkCaption(Nw,'Table F 2.I')
c-----------------------------------------------------------------------
      CALL writTag(Nw,'<tr>')
      CALL mkTableCell(Nw,'head','&nbsp;')
      CALL mkHeaderCellScope(Nw,0,0,'col','@','Statistic')
      CALL mkHeaderCellScope(Nw,0,0,'col','@',
     &                       'Probability'//Cbr//'level')
      CALL writTag(Nw,'</tr>')
c-----------------------------------------------------------------------
      CALL writTag(Nw,'<tr>')
      CALL mkHeaderCellScope(Nw,0,0,'row','@',
     &                  'F-test for stable seasonality from Table B 1.')
      WRITE(Nw,1190)Fpres,P3
      CALL writTag(Nw,'</tr>')
 1190 FORMAT(3X,'<td> ',F11.3,' </td><td> ',F8.2,'% </td>')

      CALL writTag(Nw,'<tr>')
      CALL mkHeaderCellScope(Nw,0,0,'row','@',
     &                  'F-test for stable seasonality from Table D 8.')
      WRITE(Nw,1190)Fstabl,P1
      CALL writTag(Nw,'</tr>')

      CALL writTag(Nw,'<tr>')
      CALL mkHeaderCellScope(Nw,0,0,'row','@',
     &                       'Kruskal-Wallis Chi Squared test for '//
     &                       'stable seasonality from Table D 8.')
      WRITE(Nw,1190)Chikw,P5
      CALL writTag(Nw,'</tr>')

      CALL writTag(Nw,'<tr>')
      CALL mkHeaderCellScope(Nw,0,0,'row','@',
     &                  'F-test for moving seasonality from Table D 8.')
      WRITE(Nw,1190)Fmove,P2
      CALL writTag(Nw,'</tr>')
c-----------------------------------------------------------------------
      CALL writTag(Nw,'</table>')
      CALL mkPOneLine(Nw,'@','&nbsp;')
c-----------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE prf2cr(Nw,Xbar2,Xsd,thisHdr1,thisHdr2)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C --- THIS SUBROUTINE GENERATES THE rows for the F2.c TABLE.
c-----------------------------------------------------------------------
      DOUBLE PRECISION Xbar2,Xsd
      INTEGER Nw
      CHARACTER thisHdr1*(20),thisHdr2*(20)
c-----------------------------------------------------------------------
       IF (Xsd.gt.999999D0)THEN
        IF (Xbar2.lt.-99999D0)THEN
         WRITE(Nw,2091)thisHdr1,Xbar2,thisHdr2,Xsd
        ELSE
         WRITE(Nw,2090)thisHdr1,Xbar2,thisHdr2,Xsd
        END IF
       ELSE IF (Xbar2.lt.0D0)THEN
        IF (Xbar2.lt.-99999D0)THEN
         WRITE(Nw,2091)thisHdr1,Xbar2,thisHdr2,Xsd
        ELSE
         WRITE(Nw,1091)thisHdr1,Xbar2,thisHdr2,Xsd
        END IF
       ELSE
        IF (Xbar2.gt.999999D0)THEN
         WRITE(Nw,2090)thisHdr1,Xbar2,thisHdr2,Xsd
        ELSE
         WRITE(Nw,1090)thisHdr1,Xbar2,thisHdr2,Xsd
        END IF
       END IF
c-----------------------------------------------------------------------
 1090 FORMAT(3x,2('<td headers="',a,'">',F9.2,'</td> '))
 1091 FORMAT(3x,'<td class="nowrap" headers="',a,'">',F9.2,
     &       '</td> <td headers="',a,'">',F9.2,'</td>')
 2090 FORMAT(3x,2('<td headers="',a,'">',G15.8,'</td> '))
 2091 FORMAT(3x,'<td class="nowrap" headers="',a,'">',G15.8,
     &       '</td> <td headers="',a,'">',G15.8,'</td>')
c-----------------------------------------------------------------------
      RETURN
      END
