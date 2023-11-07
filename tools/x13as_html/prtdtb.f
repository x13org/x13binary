C     Last change:  BCM  17 Apr 2003   11:12 pm
**==prtdtb.f    processed by SPAG 4.03F  at 08:58 on  5 Oct 1994
      SUBROUTINE prtdtb(Tdtbl)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     This subroutine prints out the trading day factors associated with
c     each type of month.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'mq3.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
c     Allow leap year output when adjust = leapyr (BCM August 2007)
c-----------------------------------------------------------------------
      DOUBLE PRECISION BIG
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.,BIG=10D16)
c-----------------------------------------------------------------------
      CHARACTER tdfmt*(7),daylbl*(15),datstr*(10)
      LOGICAL Pckxtd,Xrgmtd,Fulxtd,prlpyr
      INTEGER i,j,Tdtbl,Xtdzro,Xtddat,nchdat,ldec
      DOUBLE PRECISION Tdx11,Tdx11b,Tdmdl,Tdmdl1,daynum,Lpmdl,Lpmdl1
      DIMENSION daylbl(4,4),daynum(2,6),Tdmdl(PTD),
     &          Tdmdl1(PTD),Tdx11(PTD),Tdx11b(PTD),Xtddat(2),Lpmdl(2),
     &          Lpmdl1(2)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      INTEGER nblank
      EXTERNAL dpeq,nblank
c-----------------------------------------------------------------------
      COMMON /cmdltd / Tdmdl,Tdmdl1,Lpmdl,Lpmdl1
      COMMON /cx11td / Tdx11,Tdx11b
      COMMON /cxpktd / Xtdzro,Xtddat,Pckxtd,Xrgmtd,Fulxtd
c-----------------------------------------------------------------------
      DATA (daylbl(1,j),j=1,4)/'92-day quarters','91-day quarters',
     &                         'Leap year Q1   ','Non-Leap Q1    '/
      DATA (daylbl(2,j),j=1,4)/'31-day months  ','30-day months  ',
     &                         'Leap year Feb. ','Non-Leap Feb.  '/
      DATA (daylbl(3,j),j=1,4)/'    &nbsp;     ','    &nbsp;     ',
     &                         '100*(91/90.25) ','100*(90/90.25) '/
      DATA (daylbl(4,j),j=1,4)/'    &nbsp;     ','    &nbsp;     ',
     &                         '100*(29/28.25) ','100*(28/28.25) '/
c-----------------------------------------------------------------------
      DATA (daynum(1,j),j=1,6)/92D0,91D0,91D0,90D0,90.25D0,91.25D0/
      DATA (daynum(2,j),j=1,6)/31D0,30D0,29D0,28D0,28.25D0,30.4375D0/
c-----------------------------------------------------------------------
c     Print page header
c-----------------------------------------------------------------------
      CALL genSkip(1068)
      IF(Muladd.ne.1)THEN
       CALL writTagOneLine(Mt1,'h3','@',
     &                     'F 4. Multiplicative Trading Day Component'//
     &                     ' Factors:'//Cbr//
     &                     ' Day of Week and Leap Year Factors')
      ELSE
       CALL writTagOneLine(Mt1,'h3','@',
     &                     'F 4. Additive Day of the Week Trading Day'//
     &                     ' Component Factors')
      END IF
c-----------------------------------------------------------------------
      j=16
      IF(Ny.eq.4)j=1
      DO i=j,PTD
       IF(Tdtbl.eq.1.or.Tdtbl.eq.3)THEN
        IF(dpeq(Tdx11(i),DNOTST))Tdx11(i)=BIG
        IF(Xrgmtd.and.dpeq(Tdx11b(i),DNOTST))Tdx11b(i)=BIG
       END IF
       IF(Tdtbl.ge.2)THEN
        IF(dpeq(Tdmdl(i),DNOTST))Tdmdl(i)=BIG
        IF(Lrgmtd.and.dpeq(Tdmdl1(i),DNOTST))Tdmdl1(i)=BIG
       END IF
      END DO
      j=1
      IF(Ny.eq.12)j=2
c-----------------------------------------------------------------------
c     Produce format for type of trading day table
c-----------------------------------------------------------------------
      ldec=2
      IF(Muladd.eq.1)ldec=Kdec
      WRITE(tdfmt,1040)ldec
*c 1040 FORMAT('(1x,a15,7(1x,f8.',i1,'))')
 1040 FORMAT('(f16.',i1,')')
c-----------------------------------------------------------------------
c     Print table of trading day factors.
c-----------------------------------------------------------------------
      IF(Tdtbl.eq.1)THEN
       CALL prttd(Tdx11,Tdx11b,'Irregular Component Regression',30,
     &            Xtdzro,Xtddat,Xrgmtd,Fulxtd,tdfmt,Ny,Moqu)
      ELSE
       CALL prttd(Tdmdl,Tdmdl1,'regARIMA                      ',8,
     &            Tdzero,Tddate,Lrgmtd,Fulltd,tdfmt,Ny,Moqu)
      END IF
c-----------------------------------------------------------------------
c     IF both regARIMA and X-11 trading day factors are printed, print
c     out X-11 trading day here.
c-----------------------------------------------------------------------
      IF(Tdtbl.eq.3)
     & CALL prttd(Tdx11,Tdx11b,'Irregular Component Regression',30,
     &            Xtdzro,Xtddat,Xrgmtd,Fulxtd,tdfmt,Ny,Moqu)
      IF(Muladd.eq.1)RETURN
c-----------------------------------------------------------------------
c     Print message describing leap year effect.
c-----------------------------------------------------------------------
      prlpyr=F
      IF(Picktd)THEN
       prlpyr=T
      ELSE IF (Tdtbl.eq.2) THEN
       IF(iabs(Priadj).eq.4)THEN
        prlpyr=T
       ELSE IF (.not.dpeq(Lpmdl(2),DNOTST))THEN
        prlpyr=T
       END IF
      ELSE
       prlpyr=T
      END IF
      IF(prlpyr)THEN
       IF(dpeq(Lpmdl(2),DNOTST))THEN
        CALL mkTableTag(Mt1,'w50','@')
        CALL mkCaption(Mt1,'Nonseasonal component of length of '//
     &                 Moqu(1:nblank(Moqu))//
     &                 ' effect ("Leap Year" factors)')
        DO i=1,2
         CALL writTag(Mt1,'<tr>')
         WRITE(Mt1,1105)daylbl(j,i),100D0,daylbl((j+2),i)
         CALL writTag(Mt1,'</tr>')
        END DO
        DO i=3,4
         CALL writTag(Mt1,'<tr>')
         WRITE(Mt1,1105)daylbl(j,i),(daynum(j,i)/daynum(j,5))*100D0,
     &                 daylbl((j+2),i)
         CALL writTag(Mt1,'</tr>')
        END DO
        CALL writTag(Mt1,'</table>')
        CALL mkPOneLine(Mt1,'@','&nbsp;')
c-----------------------------------------------------------------------
       ELSE
        IF(dpeq(Lpmdl1(2),DNOTST))THEN
         CALL mkTableTag(Mt1,'w50','@')
         CALL mkCaption(Mt1,'Nonseasonal component of length '//
     &                  'of '//Moqu(1:nblank(Moqu))//
     &                  ' effect ("Leap Year" factors)')
        ELSE
         CALL wrtdat(Tddate,Ny,datstr,nchdat)
         IF(Fulltd.or.Tdzero.gt.0)THEN
          CALL mkTableTag(Mt1,'w50','@')
          CALL mkCaption(Mt1,'Nonseasonal component of length of '//
     &                   Moqu(1:nblank(Moqu))//
     &                   ' effect ("Leap Year" factors) before '//
     &                   datstr(1:nchdat))
         ELSE
          CALL mkTableTag(Mt1,'w50','@')
          CALL mkCaption(Mt1,'Nonseasonal component of length of '//
     &                   Moqu(1:nblank(Moqu))//
     &                   ' effect ("Leap Year" factors) starting '//
     &                   datstr(1:nchdat))
         END IF
        END IF
        DO i=1,2
         CALL writTag(Mt1,'<tr>')
         WRITE(Mt1,1105)daylbl(j,i),100D0,daylbl((j+2),i)
         CALL writTag(Mt1,'</tr>')
        END DO
        DO i=1,2
         CALL writTag(Mt1,'<tr>')
         WRITE(Mt1,1105)daylbl(j,i+2),Lpmdl(i),daylbl((j+2),i)
         CALL writTag(Mt1,'</tr>')
        END DO
        CALL writTag(Mt1,'</table>')
        CALL mkPOneLine(Mt1,'@','&nbsp;')
c-----------------------------------------------------------------------
        IF(.not.dpeq(Lpmdl1(2),DNOTST))THEN
         IF(Tdzero.eq.1)THEN
          CALL mkTableTag(Mt1,'w50','@')
          CALL mkCaption(Mt1,'Nonseasonal component of length of '//
     &                   Moqu(1:nblank(Moqu))//
     &                   ' effect ("Leap Year" factors) before '//
     &                   datstr(1:nchdat))
         ELSE
          CALL mkTableTag(Mt1,'w50','@')
          CALL mkCaption(Mt1,'Nonseasonal component of length of '//
     &                   Moqu(1:nblank(Moqu))//
     &                   ' effect ("Leap Year" factors) starting '//
     &                   datstr(1:nchdat))
         END IF
         DO i=1,2
          CALL writTag(Mt1,'<tr>')
          WRITE(Mt1,1105)daylbl(j,i),100D0,daylbl((j+2),i)
          CALL writTag(Mt1,'</tr>')
         END DO
         DO i=1,2
          CALL writTag(Mt1,'<tr>')
          WRITE(Mt1,1105)daylbl(j,i+2),Lpmdl1(i),daylbl((j+2),i)
          CALL writTag(Mt1,'</tr>')
         END DO
         CALL writTag(Mt1,'</table>')
         CALL mkPOneLine(Mt1,'@','&nbsp;')
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
 1105 FORMAT(2x,'<th>',a15,'</th><td class="center">',f7.2,'</td>',
     &       '<td class="center">',a15,'</td>')
      RETURN
      END
