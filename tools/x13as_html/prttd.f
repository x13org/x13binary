C     Last change:  BCM  28 Sep 1998   11:09 am
      SUBROUTINE prttd(Td,Td1,Ctype,Ntype,Tdzero,Tddate,Lrgmtd,Fulltd,
     &                 Tdfmt,Ny,Mq)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      DOUBLE PRECISION BIG
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.,BIG=10D16)
c-----------------------------------------------------------------------
      INCLUDE 'units.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION Td,Td1
      CHARACTER Ctype*(30),Tdfmt*(7),datstr*(10),daylbl*(15),Mq*(7),
     &          datlbl*(2),databb*(9),datcol*(3),thisId*(6),
     &          thisHdr*(20),thisVal*(16)
      LOGICAL Lrgmtd,Fulltd
      INTEGER i,j,k,Tdzero,Tddate,nchdat,Ny,Ntype
      DIMENSION daylbl(2,4),Tddate(2),Td(*),Td1(*),datlbl(7),databb(7),
     &          datcol(7)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      INTEGER nblank
      EXTERNAL dpeq,nblank
c-----------------------------------------------------------------------
      DATA (daylbl(1,j),j=1,4)/'92-day quarters','91-day quarters',
     &                         'Leap year Q1   ','Non-Leap Q1    '/
      DATA (daylbl(2,j),j=1,4)/'31-day months  ','30-day months  ',
     &                         'Leap year Feb. ','Non-Leap Feb.  '/
      DATA (datlbl(j),j=1,7)/'mo','tu','we','th','fr','sa','su'/
      DATA (datcol(j),j=1,7)/'Mon','Tue','Wed','Thu','Fri','Sat','Sun'/
      DATA (databb(j),j=1,7)/'Monday   ','Tuesday  ','Wednesday',
     &                       'Thursday ','Friday   ','Saturday ',
     &                       'Sunday   '/
      
c-----------------------------------------------------------------------
      j=1
      IF(Ny.eq.12)j=2
      CALL makDivId(Mt1,'f4','@')
      IF(Lrgmtd)THEN
       CALL wrtdat(Tddate,Ny,datstr,nchdat)
       IF(Fulltd.or.Tdzero.gt.0)THEN
        CALL mkTableTag(Mt1,'w80','Day of Week Component for '//
     &                 Ctype(1:Ntype)//' Trading Day Factors (before '//
     &                  datstr(1:nchdat)//')')
        CALL mkCaption(Mt1,'Day of Week Component for '//
     &                 Ctype(1:Ntype)//' Trading Day Factors (before '//
     &                 datstr(1:nchdat)//')')
       ELSE
        CALL mkTableTag(Mt1,'w80','Day of Week Component for '//
     &               Ctype(1:Ntype)//' Trading Day Factors (starting '//
     &                  datstr(1:nchdat)//')')
        CALL mkCaption(Mt1,'Day of Week Component for '//
     &               Ctype(1:Ntype)//' Trading Day Factors (starting '//
     &                 datstr(1:nchdat)//')')
       END IF
      ELSE
       CALL mkTableTag(Mt1,'w80','Day of Week Component for '//
     &                 Ctype(1:Ntype)//' Trading Day Factors')
       CALL mkCaption(Mt1,'Day of Week Component for '//Ctype(1:Ntype)//
     &                ' Trading Day Factors')
      END IF
c-----------------------------------------------------------------------
c     Set up headers in table
c-----------------------------------------------------------------------
      CALL writTag(Mt1,'<tr>')
      Ingr=Ingr+1
      WRITE(thisId,1010)'gr',Ingr
      CALL mkTableCellSpan(Mt1,'row',2,'head','&nbsp;')
      CALL mkHeaderCellId(Mt1,0,7,thisId,'@','@',
     &                    Mq(1:nblank(Mq))//'s starting on:')
      CALL writTag(Mt1,'</tr>')

      CALL writTag(Mt1,'<tr>')
      DO k=1,7
       Indy(k)=Indy(k)+1
       WRITE(thisId,1010)datlbl(k),Indy(k)
       CALL mkHeaderCellId(Mt1,0,0,thisId,'@',databb(k),datcol(k))
      END DO
      CALL writTag(Mt1,'</tr>')
      
c-----------------------------------------------------------------------
c     Print trading day factors for each type of month/quarter.  Print 
c     out results for 31/92 day months/quarters first.
c-----------------------------------------------------------------------
      CALL writTag(Mt1,'<tr>')

      Invl=Invl+1
      WRITE(thisId,1010)'vl',Invl
      CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@',daylbl(j,1))

      DO i=1,7
       k=i+7
       WRITE(thisHdr,1020)Invl,Ingr,datlbl(i),Indy(i)
       IF(dpeq(Td(k),BIG))THEN
        CALL mkTableCell(Mt1,'@','&nbsp;')
       ELSE
        WRITE(thisVal,Tdfmt)Td(k)
        CALL mkTableCellHeader(Mt1,thisHdr,'right',thisVal)
       END IF
      END DO

      CALL writTag(Mt1,'</tr>')
c-----------------------------------------------------------------------
c     Print out results for 30 day months (90 day quarters)
c-----------------------------------------------------------------------
      CALL writTag(Mt1,'<tr>')

      Invl=Invl+1
      WRITE(thisId,1010)'vl',Invl
      CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@',daylbl(j,2))

      DO i=1,7
       k=i
       WRITE(thisHdr,1020)Invl,Ingr,datlbl(i),Indy(i)
       IF(dpeq(Td(k),BIG))THEN
        CALL mkTableCell(Mt1,'@','&nbsp;')
       ELSE
        WRITE(thisVal,Tdfmt)Td(k)
        CALL mkTableCellHeader(Mt1,thisHdr,'right',thisVal)
       END IF
      END DO

      CALL writTag(Mt1,'</tr>')
c-----------------------------------------------------------------------
c     Print out results for Leap year Februaries (First Quarters)
c-----------------------------------------------------------------------
      CALL writTag(Mt1,'<tr>')

      Invl=Invl+1
      WRITE(thisId,1010)'vl',Invl
      CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@',daylbl(j,3))

      DO i=1,7
       k=i+21
       WRITE(thisHdr,1020)Invl,Ingr,datlbl(i),Indy(i)
       IF(dpeq(Td(k),BIG))THEN
        CALL mkTableCell(Mt1,'@','&nbsp;')
       ELSE
        WRITE(thisVal,Tdfmt)Td(k)
        CALL mkTableCellHeader(Mt1,thisHdr,'right',thisVal)
       END IF
      END DO

      CALL writTag(Mt1,'</tr>')
c-----------------------------------------------------------------------
c     Print out results for Leap year First Quarters
c-----------------------------------------------------------------------
      IF(Ny.eq.4)THEN
       CALL writTag(Mt1,'<tr>')

       Invl=Invl+1
       WRITE(thisId,1010)'vl',Invl
       CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@',daylbl(j,4))

       DO i=1,7
        k=i+14
        WRITE(thisHdr,1020)Invl,Ingr,datlbl(i),Indy(i)
        IF(dpeq(Td(k),BIG))THEN
         CALL mkTableCell(Mt1,'@','&nbsp;')
        ELSE
         WRITE(thisVal,Tdfmt)Td(k)
         CALL mkTableCellHeader(Mt1,thisHdr,'right',thisVal)
        END IF
       END DO

       CALL writTag(Mt1,'</tr>')
      END IF
c-----------------------------------------------------------------------
      CALL writTag(Mt1,'</table></div>')
      CALL mkPOneLine(Mt1,'@','&nbsp;')
c-----------------------------------------------------------------------
c     IF Change of Regime trading day variables were used,
c     print out trading day from change of regime here.
c-----------------------------------------------------------------------
      IF((Fulltd.or.Tdzero.eq.2).and.Lrgmtd)THEN
       CALL makDivId(Mt1,'f4a','@')
       IF(Tdzero.eq.1)THEN
        CALL mkTableTag(Mt1,'w80','Day of Week Component for '//
     &                 Ctype(1:Ntype)//' Trading Day Factors (before '//
     &                  datstr(1:nchdat)//')')
        CALL mkCaption(Mt1,'Day of Week Component for '//
     &                 Ctype(1:Ntype)//' Trading Day Factors (before '//
     &                 datstr(1:nchdat)//')')
       ELSE
        CALL mkTableTag(Mt1,'w80','Day of Week Component for '//
     &               Ctype(1:Ntype)//' Trading Day Factors (starting '//
     &                  datstr(1:nchdat)//')')
        CALL mkCaption(Mt1,'Day of Week Component for '//
     &               Ctype(1:Ntype)//' Trading Day Factors (starting '//
     &                 datstr(1:nchdat)//')')
       END IF
c-----------------------------------------------------------------------
c     Set up headers in table
c-----------------------------------------------------------------------
       CALL writTag(Mt1,'<tr>')
       Ingr=Ingr+1
       WRITE(thisId,1010)'gr',Ingr
       CALL mkTableCellSpan(Mt1,'row',2,'@','&nbsp;')
       CALL mkHeaderCellId(Mt1,0,7,thisId,'@','@',
     &                     Mq(1:nblank(Mq))//'s starting on:')
       CALL writTag(Mt1,'</tr>')

       CALL writTag(Mt1,'<tr>')
       DO k=1,7
        Indy(k)=Indy(k)+1
        WRITE(thisId,1010)datlbl(k),Indy(k)
        CALL mkHeaderCellId(Mt1,0,0,thisId,'@',databb(k),datcol(k))
       END DO
       CALL writTag(Mt1,'</tr>')
c-----------------------------------------------------------------------
       CALL writTag(Mt1,'<tr>')

       Invl=Invl+1
       WRITE(thisId,1010)'vl',Invl
       CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@',daylbl(j,1))

       DO i=1,7
        k=i+7
        WRITE(thisHdr,1020)Invl,Ingr,datlbl(i),Indy(i)
        IF(dpeq(Td1(k),BIG))THEN
         CALL mkTableCell(Mt1,'@','&nbsp;')
        ELSE
         WRITE(thisVal,Tdfmt)Td1(k)
         CALL mkTableCellHeader(Mt1,thisHdr,'right',thisVal)
        END IF
       END DO

       CALL writTag(Mt1,'</tr>')
c-----------------------------------------------------------------------
       CALL writTag(Mt1,'<tr>')

       Invl=Invl+1
       WRITE(thisId,1010)'vl',Invl
       CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@',daylbl(j,2))

       DO i=1,7
        k=i
        WRITE(thisHdr,1020)Invl,Ingr,datlbl(i),Indy(i)
        IF(dpeq(Td1(k),BIG))THEN
         CALL mkTableCell(Mt1,'@','&nbsp;')
        ELSE
         WRITE(thisVal,Tdfmt)Td1(k)
         CALL mkTableCellHeader(Mt1,thisHdr,'right',thisVal)
        END IF
       END DO

       CALL writTag(Mt1,'</tr>')
c-----------------------------------------------------------------------
       CALL writTag(Mt1,'<tr>')

       Invl=Invl+1
       WRITE(thisId,1010)'vl',Invl
       CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@',daylbl(j,3))

       DO i=1,7
        k=i+21
        WRITE(thisHdr,1020)Invl,Ingr,datlbl(i),Indy(i)
        IF(dpeq(Td1(k),BIG))THEN
         CALL mkTableCell(Mt1,'@','&nbsp;')
        ELSE
         WRITE(thisVal,Tdfmt)Td1(k)
         CALL mkTableCellHeader(Mt1,thisHdr,'right',thisVal)
        END IF
       END DO

       CALL writTag(Mt1,'</tr>')
c-----------------------------------------------------------------------
       IF(Ny.eq.4)THEN
        CALL writTag(Mt1,'<tr>')

        Invl=Invl+1
        WRITE(thisId,1010)'vl',Invl
        CALL mkHeaderCellId(Mt1,0,0,thisId,'@','@',daylbl(j,4))

        DO i=1,7
         k=i+14
         WRITE(thisHdr,1020)Invl,Ingr,datlbl(i),Indy(i)
         IF(dpeq(Td1(k),BIG))THEN
          CALL mkTableCell(Mt1,'@','&nbsp;')
         ELSE
          WRITE(thisVal,Tdfmt)Td1(k)
          CALL mkTableCellHeader(Mt1,thisHdr,'right',thisVal)
         END IF
        END DO

        CALL writTag(Mt1,'</tr>')
       END IF
c-----------------------------------------------------------------------
       CALL writTag(Mt1,'</table></div>')
       CALL mkPOneLine(Mt1,'@','&nbsp;')
      END IF
c-----------------------------------------------------------------------
 1010 FORMAT(a2,i4.4)
 1020 FORMAT('vl',i4.4,' gr',i4.4,' ',a2,i4.4)
c-----------------------------------------------------------------------
      RETURN
      END

