C     Last change:  BCM  16 Feb 1999    3:49 pm
      SUBROUTINE prtf2w(Nw,Mqf2,Khcfm)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C --- THIS SUBROUTINE GENERATES THE F2 TABLE for wide printouts.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'title.cmn'
      INCLUDE 'inpt2.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'work2.cmn'
      INCLUDE 'tests.cmn'
      INCLUDE 'mq3.cmn'
c-----------------------------------------------------------------------
      CHARACTER Mqf2*(7),aorb*(1)
      INTEGER i,Khcfm,n,Nw
      DIMENSION aorb(2)
c-----------------------------------------------------------------------
      INTEGER nblank
      EXTERNAL nblank
c-----------------------------------------------------------------------
      DATA aorb/'B','A'/
c-----------------------------------------------------------------------
      WRITE(Nw,1010)Pcdif(1:nblank(Pcdif)),aorb(Khcfm),Mqf2,Mqcd
 1010 FORMAT(6X,'F 2.A: Average ',a,
     &       ' without regard to sign over the indicated span',/,14X,
     &       'Span',/,15X,'in',6X,A1,'1',5X,'D11',5X,'D13',5X,'D12',5X,
     &       'D10',6X,'A2',5X,'D18',6X,'F1',13X,'E1',6X,'E2',6X,'E3',/,
     &       11X,A7,'s',4X,'O',6X,'CI',7X,'I',7X,'C',7X,'S',7X,'P',5X,
     &       'TD&H',5X,A3,11X,'Mod.O   Mod.CI  Mod.I')
      DO i=1,Ny
       WRITE(Nw,1020)i,Obar(i),Cibar(i),Ibar(i),Cbar(i),Sbar(i),Pbar(i),
     &               Tdbar(i),Smbar(i),Ombar(i),Cimbar(i),Imbar(i)
 1020  FORMAT(15X,I2,8F8.2,9X,3F8.2)
      END DO
      WRITE(Nw,1030)Pcdif(1:nblank(Pcdif)),Mqf2
 1030 FORMAT(/,6X,'F 2.B: Relative contributions to the variance of the'
     &       ,a15,' in the components of the original series',/,14x,
     &       'Span',/,15X,'in',5X,'E3',5X,'D12',5X,'D10',6X,'A2',5X,
     &       'D18',12X,'RATIO',/,11X,A7,'s',3X,'I',7X,'C',7X,'S',7X,'P',
     &       5X,'TD&H',4X,'TOTAL   (X100)')
      DO i=1,Ny
       WRITE(Nw,1040)i,Isq(i),Csq(i),Ssq(i),Psq(i),Tdsq(i),Osq2(i)
 1040  FORMAT(15X,I2,5(2PF8.2),'  100.00',2PF8.2)
      END DO
      WRITE(Nw,1050)Pcdif(1:nblank(Pcdif)),aorb(Khcfm),Mqcd,Mqf2
 1050 FORMAT(/,6X,'F 2.C: Average ',A,
     & ' with regard to sign and standard deviation over indicated span'
     & ,/,14X,'Span',8X,A1,'1',15X,'D13',14X,'D12',14X,'D10',14X,'D11',
     & 15X,'F1',/,15X,'IN',10X,'O',16X,'I',16X,'C',16X,'S',16X,'CI',14X,
     & A3,/,11X,A7,'s',6(3X,'Avg.',4X,'S.D.',2X))
      DO i=1,Ny
       WRITE(Nw,1060)i,Obar2(i),Osd(i),Ibar2(i),Isd(i),Cbar2(i),Csd(i),
     &               Sbar2(i),Ssd(i),Cibar2(i),Cisd(i),Smbar2(i),Smsd(i)
 1060  FORMAT(15X,I2,6(F9.2,F8.2))
      END DO
      WRITE(Nw,1070)Mqcd,Adrci,Adri,Adrc,Adrmcd
 1070 FORMAT(/,6X,'F 2.D: Average duration of run',8X,'CI',6X,'I',7X,
     &       'C',6X,A3,/,39X,4F8.2)
*      IF(Ny.eq.12)Kpage=Kpage+1
      WRITE(Nw,1080)Moqu(1:nblank(Moqu)),(i,i=1,Ny)
 1080 FORMAT(//,6X,'F 2.E: I/C Ratio for ',A,'s Span',/,18X,12I8)
      WRITE(Nw,1090)(Smic(i),i=1,Ny)
 1090 FORMAT(19X,14F8.2)
      WRITE(Nw,1100)Moqu(1:nblank(Moqu)),Mcd
 1100 FORMAT(/,7X,A7,'s for cyclical dominance:',i8)
      WRITE(Nw,1110)Vi,Vc,Vs,Vp,Vtd,Rv
 1110 FORMAT(//,6X,
     &'F 2.F: Relative contribution of the components to the stationary 
     &portion of the variance in the original series',/,24x,'i',7x,'C',
     &7X,'S',7X,'P',5X,'TD&H',3X,'Total',/,19X,6F8.2,/)
      n=Ny+2
      WRITE(Nw,1120)n,(i,i=1,n)
 1120 FORMAT(/,6X,
     &     'F 2.G: The autocorrelation of the irregulars for spans 1 to'
     &     ,I3,/,18X,14I8)
      WRITE(Nw,1090)(Autoc(i),i=1,n)
      WRITE(Nw,1130)Ratic
 1130 FORMAT(/,6X,'F 2.H: The final I/C Ratio from Table D12:',F12.2)
      IF(Kfulsm.lt.2)WRITE(Nw,1131)Ratis
 1131 FORMAT(12X,' The final I/S Ratio from Table D10:',F12.2)
      WRITE(Nw,1140)Fpres,P3
 1140 FORMAT(/,6X,'F 2.I:',75X,'Statistic Probability',/,100x,'level',/,
     &       13X,'F-test for stable seasonality from Table B 1.',26X,
     &       ':',F11.3,F8.2,'%')
c      IF(Kdwopt.ge.1.and.Kdwopt.lt.6)WRITE(Nw,1170)F(4),P(4)
c 1170 FORMAT(13X,'F-test for the trading day regession in Table C15.',
c     &       21X,':',F11.3,F8.2,'%')
      WRITE(Nw,1150)Fstabl,P1,Chikw,P5,Fmove,P2
 1150 FORMAT(13X,'F-test for stable seasonality from Table D 8.',26X,
     &       ':',F11.3,F8.2,'%',/,13X,
     &'Kruskal-Wallis Chi Squared test for stable seasonality from Table
     & D 8. :',F11.3,F8.2,'%',/,13X,
     &'F-test for moving seasonality from Table D 8.',26X,':',F11.3,
     &F8.2,'%')
c-----------------------------------------------------------------------
      RETURN
      END
