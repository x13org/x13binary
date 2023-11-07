C     Last change:  BCM  25 Nov 97    3:35 pm
**==out.f    processed by SPAG 4.03F  at 09:51 on  1 Mar 1994
      SUBROUTINE outchr(Title,Ntitle,Icode,Icod2)
      IMPLICIT NONE
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'tbltitle.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'chrt.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      LOGICAL T
      PARAMETER(T=.true.)
c     ------------------------------------------------------------------
      CHARACTER for*(18),for1*(22),Title*(PTTLEN)
      DOUBLE PRECISION almn,almx
      INTEGER i,Icod2,Icode,iline,j,j2,Ntitle,n
      DIMENSION Title(2),Ntitle(2)
c     ------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c     ------------------------------------------------------------------
C  WRITE OUT THE CHART
C***************
c     ------------------------------------------------------------------
c     Change 9/96 to handle more than one title  BCM
c     Change 4/97 to incorporate title dictionary, print only defined
c                    part of title (BCM)
c     ------------------------------------------------------------------
      IF(Ntitle(1).gt.0)THEN
       CALL makeSkipLink(Mt1,Idxtab,
     &                  'ASCII plot of Table '//Title(1)(1:Ntitle(1)),T)
       CALL writTagOneLine(Mt1,'h3','@',Title(1)(1:Ntitle(1)))
      END IF
      IF(Ntitle(2).gt.0)
     &   CALL writTagOneLine(Mt1,'h3','indent',Title(2)(1:Ntitle(2)))
      CALL writTag(Mt1,'<pre>')
c     ------------------------------------------------------------------
      iline=Icmax+1
      almn=Ymin
      almx=Ymax
      IF(Ymin.lt.0)almn=-almn
      IF(.not.dpeq(almn,0D0))almn=dlog10(almn)
      IF(Ymax.lt.0)almx=-almx
      IF(.not.dpeq(almx,0D0))almx=dlog10(almx)
      WRITE(for1,1020)iline
 1020 FORMAT('(1X,F10.2,',I3,'A1,F10.2)')
      IF(almn.lt.-6D0.or.almx.gt.6D0)THEN
       for1(5:5)='E'
       for1(9:9)='4'
       for1(17:17)='E'
       for1(21:21)='4'
      END IF
      IF(Icod2.eq.17)THEN
       for1(9:9)='4'
       for1(21:21)='4'
       IF(almn.lt.-4D0.or.almx.gt.4D0)THEN
        for1(5:5)='E'
        for1(17:17)='E'
       END IF
      END IF
      j2=1
      DO j=1,55
       IF(j.eq.1.or.j.eq.55)THEN
        WRITE(Mt1,1030)(Ip(i),i=1,10),(Ia(i,j),i=1,iline)
 1030   FORMAT(1X,120A1)
       ELSE IF(j.eq.Imid(j2))THEN
        WRITE(Mt1,for1)Ymid(j2),(Ia(i,j),i=1,iline),Ymid(j2)
        j2=j2+1
        IF(j2.gt.14)j2=1
       ELSE
        WRITE(Mt1,1040)(Ia(i,j),i=1,iline)
 1040   FORMAT(1X,10X,110A1)
       END IF
      END DO
      IF(Icode.eq.7)THEN
       WRITE(Mt1,1050)Iyear(1),Iyear(Nyr)
 1050  FORMAT(T20,I4,'  TO  ',I4)
      ELSE IF(Icode.eq.0)THEN
       IF(Icod2.eq.0)THEN
        WRITE(Mt1,1060)
     &        'S=SEASONAL FREQUENCIES, T=TRADING DAY FREQUENCIES'
       ELSE
        WRITE(Mt1,1060)'S=SEASONAL FREQUENCIES'
       END IF
 1060  FORMAT(12X,A)
      ELSE IF(Nseas.eq.12)THEN
       n=min0(9,Nyr)
       IF(Icmax.le.61)n=min0(5,Nyr)
       IF(Ifrst.le.6)THEN
        WRITE(Mt1,1070)(Iyear(i),i=1,n)
 1070   FORMAT(12X,I4,8(8X,I4))
       ELSE
        WRITE(Mt1,1080)(Iyear(i),i=2,n)
 1080   FORMAT(18X,I4,7(8X,I4))
       END IF
      ELSE
       n=min0(14,Inyr)
       IF(Icmax.le.61)n=min0(8,Inyr)
       for='(12X,I4,12(4X,I4))'
       IF(Ifrst.eq.1)THEN
        for(10:10)='3'
       ELSE IF(Ifrst.eq.2)THEN
        for(3:3)='5'
       ELSE IF(Ifrst.eq.3)THEN
        for(3:3)='4'
       ELSE IF(Ifrst.eq.4)THEN
        for(3:3)='3'
       END IF
       WRITE(Mt1,for)(Iyear(i),i=1,n)
      END IF
      CALL writTag(Mt1,'</pre>')
      RETURN
      END
