C     Last change:  BCM  17 Apr 2003   11:09 pm
      SUBROUTINE mstest(Array,Jfda,Jlda,Nyr,Lprt)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C --- AN F TEST FOR MOVING SEASONALITY
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'ssap.prm'
      INCLUDE 'ssft.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'tests.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      LOGICAL T
      DOUBLE PRECISION ONE,ZERO,BIG
      PARAMETER(ONE=1D0,ZERO=0D0,BIG=10.0D10,T=.true.)
c-----------------------------------------------------------------------
      DOUBLE PRECISION Array,Temp,fvalue,suma1,xbar,colss,colmn,rowss,
     &                 rowmn,fnyr,totss,errss,degfre,fnoyrs,rowssn,
     &                 errssn,c
      CHARACTER bk*2,s1*2,s2*2,fstar*2,thisId*(6),thisHdr*(13),
     &          thisVal*(19)
      INTEGER i,i1,ifmo,j,j1,Jfda,Jlda,k,k1,l,l1,lmo,m,n,n1,ndgfre,
     &        nmin1,noyrs,Nyr,sp1
      LOGICAL Lprt
      DIMENSION Temp(PLEN)
c-----------------------------------------------------------------------
      COMMON /work  / Temp
c-----------------------------------------------------------------------
      DIMENSION Array(Jlda)
c  Bob Fay moved EXTERNAL up
      LOGICAL dpeq
      EXTERNAL dpeq,fvalue
c-----------------------------------------------------------------------
      DATA s1,s2,bk/'* ','**','  '/
c     ------------------------------------------------------------------
      c=ONE
      ifmo=(Jfda+Nyr-2)/Nyr*Nyr+1
      lmo=Jlda/Nyr*Nyr
      noyrs=(lmo-ifmo)/Nyr+1
      fnoyrs=noyrs
      IF(Muladd.eq.0)THEN
c-----------------------------------------------------------------------
C -- MULTIPLICATIVE MODEL
c-----------------------------------------------------------------------
       c=10000.0D0
       DO j=ifmo,lmo
        Temp(j)=abs(Array(j)-ONE)
       END DO
      ELSE
c-----------------------------------------------------------------------
C --- ADDITIVE MODEL
c-----------------------------------------------------------------------
       DO i=ifmo,lmo
        Temp(i)=abs(Array(i))
       END DO
      END IF
c-----------------------------------------------------------------------
C --- ANALYSIS OF VARIANCE TEST
c-----------------------------------------------------------------------
      suma1=ZERO
      DO k=ifmo,lmo
       suma1=suma1+Temp(k)
      END DO
      fnyr=Nyr
      xbar=suma1/(fnyr*fnoyrs)
      colss=ZERO
      DO l=1,Nyr
       colmn=ZERO
       k1=ifmo+l-1
       DO m=k1,lmo,Nyr
        colmn=colmn+Temp(m)
       END DO
       colmn=colmn/fnoyrs
       colss=colss+(colmn-xbar)*(colmn-xbar)
      END DO
      colss=colss*fnoyrs*c
      rowss=ZERO
      DO n=ifmo,lmo,Nyr
       rowmn=ZERO
       l1=n+Nyr-1
       DO i1=n,l1
        rowmn=rowmn+Temp(i1)
       END DO
       rowmn=rowmn/fnyr
       rowss=rowss+(rowmn-xbar)*(rowmn-xbar)
      END DO
      rowss=rowss*fnyr*c
      totss=ZERO
      DO j1=ifmo,lmo
       totss=totss+(Temp(j1)-xbar)*(Temp(j1)-xbar)
      END DO
      errss=totss*c-colss-rowss
      degfre=fnoyrs-ONE
      rowssn=rowss/degfre
      errssn=errss/((fnyr-ONE)*degfre)
      ndgfre=(Nyr-1)*(noyrs-1)
      IF(dpeq(errssn,ZERO))THEN
       CALL wWritln('Cannot compute moving F-statistic since residual'//
     &              ' mean square error is equal to zero for this '//
     &              'series.',Mt1,Mt2,T,T)
       RETURN
      END IF
      Fmove=rowssn/errssn
      n1=noyrs-1
      P2=fvalue(Fmove,n1,ndgfre)*100D0
      IF(Issap.eq.2)Ssmf(Icol)=Fmove
c-----------------------------------------------------------------------
      IF(.not.Lprt.or.Lhiddn)RETURN
c-----------------------------------------------------------------------
      IF(P2.le.0.1D0)THEN
       fstar=s2
      ELSE IF(P2.gt.ONE)THEN
       fstar=bk
      ELSE
       fstar=s1
      END IF
      nmin1=noyrs-1
      IF(.not.Lcmpaq)CALL mkPOneLine(Mt1,'@','&nbsp;')
      CALL mkTableTag(Mt1,'w70','Moving Seasonality Test')
      CALL mkCaption(Mt1,'Moving Seasonality Test')
      CALL writTag(Mt1,'<tr>')
      CALL mkTableCell(Mt1,'head','&nbsp;')
      Inss=Inss+1
      WRITE(thisId,1010)'ss',Inss
      CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@',
     &                    'Sum of'//Cbr//'Squares')
      Indf=Indf+1
      WRITE(thisId,1010)'df',Indf
      CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@',
     &                    'Degrees of'//Cbr//'Freedom')
      Inms=Inms+1
      WRITE(thisId,1010)'ms',Inms
      CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@','Mean'//Cbr//'Square')
      Infv=Infv+1
      WRITE(thisId,1010)'fv',Infv
      CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@','F-value')
      CALL writTag(Mt1,'</tr>')

      CALL writTag(Mt1,'<tr>')
      Inbt=Inbt+1
      WRITE(thisId,1010)'bt',Inbt
      CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@','Between Years')
      WRITE(thisHdr,1020)'bt',Inbt,'ss',Inss
      IF (rowss.gt.BIG) THEN
       WRITE(thisVal,1200)rowss
      ELSE
       WRITE(thisVal,1030)rowss
      END IF
      CALL mkTableCellHeader(Mt1,thisHdr,'center',thisVal)
      WRITE(thisHdr,1020)'bt',Inbt,'df',Indf
      WRITE(thisVal,1060)nmin1
      CALL mkTableCellHeader(Mt1,thisHdr,'center',thisVal)
      WRITE(thisHdr,1020)'bt',Inbt,'ms',Inms
      IF (rowssn.gt.BIG) THEN
       WRITE(thisVal,1200)rowssn
      ELSE
       WRITE(thisVal,1050)rowssn
      END IF
      CALL mkTableCellHeader(Mt1,thisHdr,'center',thisVal)
      WRITE(thisHdr,1020)'bt',Inbt,'fv',Infv
      IF (Fmove.gt.BIG) THEN
       WRITE(thisVal,1200)Fmove
      ELSE
       WRITE(thisVal,1050)Fmove
      END IF
      CALL mkTableCellHeader(Mt1,thisHdr,'center',thisVal//fstar)
      CALL writTag(Mt1,'</tr>')

      CALL writTag(Mt1,'<tr>')
      Inrs=Inrs+1
      WRITE(thisId,1010)'rs',Inrs
      CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@','Error')
      WRITE(thisHdr,1020)'rs',Inrs,'ss',Inss
      IF (errss.gt.BIG) THEN
       WRITE(thisVal,1200)errss
      ELSE
       WRITE(thisVal,1040)errss
      END IF
      CALL mkTableCellHeader(Mt1,thisHdr,'center',thisVal)
      WRITE(thisHdr,1020)'rs',Inrs,'df',Indf
      WRITE(thisVal,1060)ndgfre
      CALL mkTableCellHeader(Mt1,thisHdr,'center',thisVal)
      WRITE(thisHdr,1020)'rs',Inrs,'ms',Inms
      IF (errssn.gt.BIG) THEN
       WRITE(thisVal,1200)errssn
      ELSE
       WRITE(thisVal,1050)errssn
      END IF
      CALL mkTableCellHeader(Mt1,thisHdr,'center',thisVal)
      CALL mkTableCell(Mt1,'@','&nbsp;')
      CALL writTag(Mt1,'</tr>')
      
      CALL writTag(Mt1,'</table>')
      IF(.not.Lcmpaq)CALL mkPOneLine(Mt1,'@','&nbsp;')

      IF(fstar.ne.bk)THEN
       CALL mkPOneLine(Mt1,'center',fstar//
     &           'Moving seasonality present at the one percent level.')
       IF(.not.Lcmpaq)CALL mkPOneLine(Mt1,'@','&nbsp;')
       RETURN
      ELSE IF(P2.ge.5D0)THEN
       CALL mkPOneLine(Mt1,'center',fstar//
     &   'No evidence of moving seasonality at the five percent level.')
       IF(.not.Lcmpaq)CALL mkPOneLine(Mt1,'@','&nbsp;')
       RETURN
      END IF
      CALL mkPOneLine(Mt1,'center',fstar//
     &          'Moving seasonality present at the five percent level.')
      IF(.not.Lcmpaq)CALL mkPOneLine(Mt1,'@','&nbsp;')
c     ------------------------------------------------------------------
 1010 FORMAT(a2,i4.4)
 1020 FORMAT(a2,i4.4,' ',a2,i4.4)
 1030 FORMAT(f19.4)
 1040 FORMAT(f19.5)
 1050 FORMAT(f19.3)
 1060 FORMAT(i9)
 1200 FORMAT(g19.12)
c     ------------------------------------------------------------------
      RETURN
      END
