C     Last change:  BCM  12 Jan 98    8:50 am
**==ssftst.f    processed by SPAG 4.03F  at 12:23 on 21 Jun 1994
      SUBROUTINE ssftst(Ncol,Lprt,Lsav)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      DOUBLE PRECISION NINE,THREE,TWO,SEVEN,ZERO
      PARAMETER (NINE=9D0,THREE=3D0,TWO=2D0,SEVEN=7D0,ZERO=0D0)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'ssap.prm'
      INCLUDE 'ssft.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      CHARACTER qf*(3),span*(6),label*(20),ftfmt*(46)
      INTEGER i,Ncol
      DOUBLE PRECISION ssm7,test1,test2
      LOGICAL Lprt,Lsav
      DIMENSION ssm7(MXCOL),qf(MXCOL),span(MXCOL),label(2)
c-----------------------------------------------------------------------
      DOUBLE PRECISION oldssm7(MXCOL),sstest1(MXCOL),sstest2(MXCOL)
c-----------------------------------------------------------------------
      DATA span/'Span 1','Span 2','Span 3','Span 4'/
      DATA label/'Stable seasonality','Moving seasonality'/
c-----------------------------------------------------------------------
      DO i=1,Ncol
       qf(i)='   '
       test1=ZERO
       test2=ZERO
       IF(Issqf(i).eq.0)qf(i)='yes'
       IF(Issqf(i).eq.1)qf(i)='???'
       IF(Issqf(i).eq.2)qf(i)=' no'
       test1=SEVEN/Ssfts(i)
       test2=(THREE*Ssmf(i))/Ssfts(i)
       oldssm7(i)=sqrt((test1+test2)/2)
       IF(oldssm7(i).gt.THREE)oldssm7(i)=THREE
       sstest1(i)=test1
       sstest2(i)=test2
       IF(test1.gt.NINE)test1=NINE
       IF(test2.gt.NINE)test2=NINE
       ssm7(i)=sqrt((test1+test2)/2)
      END DO
c-----------------------------------------------------------------------
      IF(Lprt)THEN
       CALL mkTableTag(Mt1,'w80','Summary of tests for stable and '//
     &                 'moving seasonality from table D8 for each span')
       CALL mkCaption(Mt1,'Summary of tests for stable and '//
     &                'moving seasonality from table D8 for each span')
       CALL writTag(Mt1,'<tr>')
       CALL mkTableCell(Mt1,'head','&nbsp;')
       DO i=1,Ncol
        CALL mkHeaderCellScope(Mt1,0,0,'col','@',span(i))
       END DO
       CALL writTag(Mt1,'</tr>')
c-----------------------------------------------------------------------
       CALL writTag(Mt1,'<tr>')
       CALL mkHeaderCellScope(Mt1,0,0,'row','@',label(1))
       DO i=1,Ncol
        WRITE(Mt1,1030)Ssfts(i)
       END DO
       CALL writTag(Mt1,'</tr>')
c-----------------------------------------------------------------------
       CALL writTag(Mt1,'<tr>')
       CALL mkHeaderCellScope(Mt1,0,0,'row','@',label(2))
       DO i=1,Ncol
        WRITE(Mt1,1030)Ssmf(i)
       END DO
       CALL writTag(Mt1,'</tr>')
c-----------------------------------------------------------------------
       CALL writTag(Mt1,'<tr>')
       CALL mkHeaderCellScope(Mt1,0,0,'row','@','M7')
       DO i=1,Ncol
        WRITE(Mt1,1030)Ssm7(i)
       END DO
       CALL writTag(Mt1,'</tr>')
c-----------------------------------------------------------------------
       CALL writTag(Mt1,'<tr>')
       CALL mkHeaderCellScope(Mt1,0,0,'row','@',
     &                        'Identifiable seasonality?')
       DO i=1,Ncol
        CALL mkTableCell(Mt1,'center',qf(i))
       END DO
       CALL writTag(Mt1,'</tr>')
c-----------------------------------------------------------------------
       CALL writTag(Mt1,'</table>')
       CALL mkPOneLine(Mt1,'@','&nbsp;')
c-----------------------------------------------------------------------
       CALL mkPClass(Mt1,'indent')
       WRITE(Mt1,1070)Cbr,Cbr
      END IF
c-----------------------------------------------------------------------
      IF(Lsav)THEN
       ftfmt=' '
       WRITE(ftfmt,1080)Ncol
       WRITE(Nform,ftfmt)'ssfstab:',(Ssfts(i),i=1,Ncol)
       WRITE(Nform,ftfmt)'ssfmov:',(Ssmf(i),i=1,Ncol)
*       WRITE(Nform,ftfmt)'sstest1:',(sstest1(i),i=1,Ncol)
*       WRITE(Nform,ftfmt)'sstest2:',(sstest2(i),i=1,Ncol)
       WRITE(Nform,ftfmt)'ssm7:',(ssm7(i),i=1,Ncol)
*       WRITE(Nform,ftfmt)'oldssm7:',(oldssm7(i),i=1,Ncol)
       ftfmt=' '
       WRITE(ftfmt,1090)Ncol
       WRITE(Nform,ftfmt)'ssident:',(qf(i),i=1,Ncol)
      END IF
c-----------------------------------------------------------------------
 1030 FORMAT(3x,'<td class="center">',f10.2,'</td>')
 1070  FORMAT('yes = Identifiable seasonality probably present',a,/,
     &        '??? = Identifiable seasonality probably not present',a,/,
     &        ' no = Identifiable seasonality not present</p>',/)
 1080 FORMAT('(a,',i1,'(3x,f8.2))')
 1090 FORMAT('(a,',i1,'(8x,a3))')
c-----------------------------------------------------------------------
      RETURN
      END
