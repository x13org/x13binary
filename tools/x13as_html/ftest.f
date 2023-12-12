C     Last change:  BCM  16 Feb 1999   11:15 am
      SUBROUTINE ftest(X,Ib,Ie,Nyr,Ind,Lprt,Lsav)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C --- THIS ROUTINE COMPUTES A ONE-WAY ANALYSIS OF VARIANCE ON SERIES
C --- X. IF THE TREND HAS NOT BEEN REMOVED FROM X, IT IS ELIMINATED
C --- BY A FIRST DIFFERENCE.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'agr.cmn'
      INCLUDE 'ssap.prm'
      INCLUDE 'ssft.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'x11msc.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'tests.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      LOGICAL T
      DOUBLE PRECISION ZERO,BIG,ONE,ONEHND
      PARAMETER(T=.true.,ZERO=0.0D0,BIG=10.0D10,ONE=1.0D0,
     &         ONEHND=100.0D0)
c-----------------------------------------------------------------------
      CHARACTER blank*2,star*2,star2*2,fstar*2,thisId*(6),thisHdr*(13),
     &          thisVal*(19)
      DOUBLE PRECISION dfb,f,prob,Temp,X,c,dfr,fmsm,fmsr,ssm,ssqt,
     &                 ssr,st,summ,sumt
      INTEGER i,Ib,Ie,Ind,itype,j,ji,kb,kdfb,kdfr,kdft,l,nm,nt,Nyr
c      INTEGER id11f
      LOGICAL Lprt,Lsav
      DIMENSION X(Ie),Temp(PLEN)
c-----------------------------------------------------------------------
      DOUBLE PRECISION fvalue
      LOGICAL dpeq
      EXTERNAL dpeq,fvalue
c-----------------------------------------------------------------------
      COMMON /work  / Temp
c-----------------------------------------------------------------------
      DATA blank,star/'  ','* '/,star2/'**'/
c-----------------------------------------------------------------------
      c=ONE
      l=0
c      id11f=0
c-----------------------------------------------------------------------
      IF(Issap.eq.2.and.Ind.gt.0)RETURN
      IF(Muladd.eq.0)c=10000.0D0
      itype=0
      IF(Ind.eq.0.or.Ind.eq.2)THEN
       DO i=Ib,Ie
        Temp(i)=X(i)
       END DO
       kb=Ib
      ELSE
       IF(Same)THEN
        CALL wWritln('Program cannot perform F-test on first '//
     &               'differenced data.',Mt1,Mt2,T,T)
        RETURN
       END IF
       l=Nyr/4
       kb=Ib+l
       DO i=kb,Ie
        Temp(i)=X(i)-X(i-l)
       END DO
      END IF
      DO WHILE (.true.)
       nt=0
       sumt=ZERO
       ssqt=ZERO
       ssm=ZERO
       DO i=1,Nyr
        nm=0
        summ=ZERO
        ji=i+kb-1
        DO j=ji,Ie,Nyr
         nm=nm+1
         summ=summ+Temp(j)
         ssqt=ssqt+Temp(j)*Temp(j)
        END DO
        nt=nt+nm
        sumt=sumt+summ
        ssm=ssm+summ*summ/nm
       END DO
       st=nt
       ssqt=(ssqt-sumt*sumt/st)*c
       ssm=(ssm-sumt*sumt/st)*c
       ssr=ssqt-ssm
       kdfr=nt-Nyr
       kdft=nt-1
       kdfb=Nyr-1
       dfr=kdfr
       dfb=kdfb
       fmsm=ssm/dfb
       fmsr=ssr/dfr
       IF(dpeq(fmsr,ZERO))THEN
        CALL wWritln('Cannot compute F-statistic since residual '//
     &               'mean square error is equal to zero for this '//
     &               'series.',Mt1,Mt2,T,T)
        RETURN
       END IF
       f=fmsm/fmsr
       prob=fvalue(f,kdfb,kdfr)*ONEHND
       IF(Ind.eq.0)THEN
        Fstabl=f
        P1=prob
       ELSE IF(Ind.ne.1)THEN
        Fpres=f
        P3=prob
       END IF
       IF(prob.le.0.1D0)THEN
        fstar=star2
       ELSE IF(prob.gt.ONE)THEN
        fstar=blank
       ELSE
        fstar=star
       END IF
       IF((Lhiddn.and.Issap.lt.2).or.((Ixreg.eq.2.or.Khol.eq.1).and.
     &    (.not.Prt1ps)))RETURN
C --- IF THIS F TEST IS FOR TABLE D8 AND TABLE D8 IS NOT TO BE
C --- PRINTED, DON'T PRINT THE RESULTS OF THIS TEST
       IF(Ind.eq.0.or.Ind.eq.2)THEN
        IF(Issap.eq.2.and.Ind.eq.0)THEN
         Ssfts(Icol)=f
        ELSE IF(.not.Lhiddn.and.Lprt)THEN
         IF(.not.Lcmpaq)CALL mkPOneLine(Mt1,'@','&nbsp;')
         CALL mkTableTag(Mt1,'w70',
     &       'Test for the presence of seasonality assuming stability.')
         CALL mkCaption(Mt1,
     &       'Test for the presence of seasonality assuming stability.')
         CALL writTag(Mt1,'<tr>')
         CALL mkTableCell(Mt1,'head','&nbsp;')
         Inss=Inss+1
         WRITE(thisId,1010)'ss',Inss
         CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@',
     &                       'Sum of'//Cbr//'Squares')
         Indf=Indf+1
         WRITE(thisId,1010)'df',Indf
         CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@',
     &                       'Degrees of'//Cbr//'Freedom')
         Inms=Inms+1
         WRITE(thisId,1010)'ms',Inms
         CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@',
     &                       'Mean'//Cbr//'Square')
         Infv=Infv+1
         WRITE(thisId,1010)'fv',Infv
         CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@','F-value')
         CALL writTag(Mt1,'</tr>')

         CALL writTag(Mt1,'<tr>')
         Inbt=Inbt+1
         WRITE(thisId,1010)'bt',Inbt
         IF(Nyr.eq.12)THEN
          CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@','Between months')
         ELSE
          CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@','Between quarters')
         END IF
         WRITE(thisHdr,1020)'bt',Inbt,'ss',Inss
         IF(ssm.gt.BIG)THEN
          WRITE(thisVal,1200)ssm
         ELSE
          WRITE(thisVal,1030)ssm
         END IF
         CALL mkTableCellHeader(Mt1,thisHdr,'center',thisVal)
         WRITE(thisHdr,1020)'bt',Inbt,'df',Indf
         WRITE(thisVal,1060)kdfb
         CALL mkTableCellHeader(Mt1,thisHdr,'center',thisVal)
         WRITE(thisHdr,1020)'bt',Inbt,'ms',Inms
         IF(fmsm.gt.BIG)THEN
          WRITE(thisVal,1200)fmsm
         ELSE
          WRITE(thisVal,1050)fmsm
         END IF
         CALL mkTableCellHeader(Mt1,thisHdr,'center',thisVal)
         WRITE(thisHdr,1020)'bt',Inbt,'fv',Infv
         IF(f.gt.BIG)THEN
          WRITE(thisVal,1200)f
         ELSE
          WRITE(thisVal,1050)f
         END IF
         CALL mkTableCellHeader(Mt1,thisHdr,'center',thisVal//fstar)
         CALL writTag(Mt1,'</tr>')

         CALL writTag(Mt1,'<tr>')
         Inrs=Inrs+1
         WRITE(thisId,1010)'rs',Inrs
         CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@','Residual')
         WRITE(thisHdr,1020)'rs',Inrs,'ss',Inss
         IF(ssr.gt.BIG)THEN
          WRITE(thisVal,1200)ssr
         ELSE
          WRITE(thisVal,1040)ssr
         END IF
         CALL mkTableCellHeader(Mt1,thisHdr,'center',thisVal)
         WRITE(thisHdr,1020)'rs',Inrs,'df',Indf
         WRITE(thisVal,1060)kdfr
         CALL mkTableCellHeader(Mt1,thisHdr,'center',thisVal)
         WRITE(thisHdr,1020)'rs',Inrs,'ms',Inms
         IF(fmsr.gt.BIG)THEN
          WRITE(thisVal,1200)fmsr
         ELSE
          WRITE(thisVal,1050)fmsr
         END IF
         CALL mkTableCellHeader(Mt1,thisHdr,'center',thisVal)
         CALL mkTableCell(Mt1,'@','&nbsp;')
         CALL writTag(Mt1,'</tr>')

         CALL writTag(Mt1,'<tr>')
         Intl=Intl+1
         WRITE(thisId,1010)'tl',Intl
         CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@','Total')
         WRITE(thisHdr,1020)'tl',Intl,'ss',Inss
         IF(ssqt.gt.BIG)THEN
          WRITE(thisVal,1200)ssqt
         ELSE
          WRITE(thisVal,1040)ssqt
         END IF
         CALL mkTableCellHeader(Mt1,thisHdr,'center',thisVal)
         WRITE(thisHdr,1020)'tl',Intl,'df',Indf
         WRITE(thisVal,1060)kdft
         CALL mkTableCellHeader(Mt1,thisHdr,'center',thisVal)
         CALL mkTableCell(Mt1,'@','&nbsp;')
         CALL mkTableCell(Mt1,'@','&nbsp;')
         CALL writTag(Mt1,'</tr>')
         CALL writTag(Mt1,'</table>')
         IF(.not.Lcmpaq)CALL mkPOneLine(Mt1,'@','&nbsp;')

         IF(fstar.eq.star2)
     &      CALL mkPOneLine(Mt1,'center',fstar//
     &           'Seasonality present at the 0.1 per cent level.')
         IF(fstar.ne.star2)
     &      CALL mkPOneLine(Mt1,'center',fstar//
     &   'No evidence of stable seasonality at the 0.1 per cent level.')
         IF(.not.Lcmpaq)CALL mkPOneLine(Mt1,'@','&nbsp;')
        END IF
        IF(Muladd.ne.0.or.Lhiddn)RETURN
        IF(fmsr.gt.0.01D0)RETURN
        CALL mkPOneLine(Mt1,'center',fstar//
     &           'Seasonality present at the 0.1 per cent level.')
        WRITE(Mt1,1130)'<p class="center">','</p>'
        RETURN
       END IF
       IF(.not.Lprt.and..not.Lsav)RETURN
       IF(itype.eq.0)THEN
        IF(Lprt)
     &     CALL mkPOneLine(Mt1,'center',
     &                 'Test for the presence of residual seasonality.')
        IF(fstar.eq.blank)THEN
         IF(Lprt)THEN
           WRITE(Mt1,1071)'<p class="center">',fstar,f,'</p>'
         END IF
c         IF(Lsav)id11f=0
        ELSE
         IF(Lprt)THEN
           WRITE(Mt1,1061)'<p class="center">',fstar,f,'</p>'
         END IF
c         IF(Lsav)id11f=3
        END IF
        IF(Lsav)THEN
         IF(Iagr.lt.4)THEN
          WRITE(Nform,1140)'d11.f: ',f,prob
         ELSE
          WRITE(Nform,1140)'id11.f: ',f,prob
         END IF
        END IF
        kb=Ie-3*Nyr+1
        IF(kb.lt.(Ib+l))RETURN
        itype=1
        GO TO 10
       ELSE
        IF(fstar.ne.blank)THEN
         IF(Lprt)THEN
           WRITE(Mt1,1111)'<p class="center">',fstar,f,'</p>'
         END IF
        ELSE
         IF(Lprt)THEN
           WRITE(Mt1,1081)'<p class="center">',fstar,f,'</p>'
           IF(prob.le.5D0)THEN
            WRITE(Mt1,1091)'<p class="center">',fstar,'</p>'
           ELSE IF(prob.gt.5D0)THEN
            WRITE(Mt1,1101)'<p class="center">',fstar,'</p>'
           END IF
         END IF
        END IF
        IF(Lsav)THEN
         IF(Iagr.lt.4)THEN
          WRITE(Nform,1140)'d11.3y.f: ',f,prob
         ELSE
          WRITE(Nform,1140)'id11.3y.f: ',f,prob
         END IF
        END IF
       END IF
       IF(Lprt)THEN
        CALL mkPOneLine(Mt1,'center','<strong>NOTE:</strong> '//
     &          'sudden large changes in the level of the adjusted '//
     &          'series will invalidate the results of this test '//
     &          'for the last three year period.')
       END IF
       RETURN
   10  CONTINUE
      END DO
c-----------------------------------------------------------------------
 1010 FORMAT(a2,i4.4)
 1020 FORMAT(a2,i4.4,' ',a2,i4.4)
 1030 FORMAT(f19.4)
 1040 FORMAT(f19.5)
 1050 FORMAT(f19.3)
 1060 FORMAT(i9)
 1061 FORMAT(/,a,a2,'Residual seasonality present in the entire ',
     &       'series at the 1 per cent level.  F =',F10.2,a)
 1071 FORMAT(/,a,a2,'No evidence of residual seasonality in the ',
     &       'entire series at the 1 per cent level.  F =',F10.2,a)
 1081 FORMAT(/,a,a2,'No evidence of residual seasonality in the ',
     &       'last 3 years at the 1 per cent level.  F =',F10.2,a)
 1091 FORMAT(/,a,a2,'Residual seasonality present in the last 3 ',
     &       'years at the 5 per cent level.',a)
 1101 FORMAT(/,a,a2,'No evidence of residual seasonality in the ',
     &       'last 3 years at the 5 per cent level.',a)
 1111 FORMAT(/,a,a2,'Residual seasonality present in the last 3 ',
     &       'years at the 1 per cent level.  F =',F10.2,a)
 1130 FORMAT(/,a,'Due to the small residual mean square error all',
     &           ' the analysis',
     &       /,'of variance tests for this series may be invalid.',a)
 1140 FORMAT(a,2(2x,f12.5))
 1200 FORMAT(g19.12)
c-----------------------------------------------------------------------
      END
