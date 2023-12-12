C     Last change:  BCM  19 Apr 2007    4:06 pm
**==wrttbl.f    processed by SPAG 4.03F  at 09:55 on  1 Mar 1994
      SUBROUTINE wrttbl(Tmp,Jyr,Tyrly,L,Kdec,Mt1,Tblfmt,Ipow,Lastcl)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C THIS SUBROUTINE WAS ORIGINALLY WRITTEN BY DAVE PALETZ OF SRD, 9/91
C REVISED: BRIAN MONSELL, 8/95
c-----------------------------------------------------------------------
C SOMETIMES ONE OR MORE MONTHS IN A YEAR HAVE NO DATA PROVIDED BY THE
C USER.  WHEN SO, THE PROGRAM DISPLAYS AN ERROR MESSAGE WHEN IT PRINTS
C THESE BLANK FIGURES IN THE TABLE AS REAL NUMBERS.  THIS WAS INTENDED
C SO THAT THE ASTERISKS WOULD REMIND THE USER THAT DATA WAS MISSING FOR
C THAT MONTH.  THE SUBROUTINE BELOW SUPRESSES THE ERROR MESSAGE WHILE
C CONTINUING TO PLACE THE ASTERISKS WHERE THEY ARE NEEDED.
C
C I:       Looping variable
C IPOW:    0 if figure should not be expressed as percentage; 1 if it
C          should be
C JYR:     Year of the data to print
C KDEC:    Number of places behind decimal to display
C L:       Number of observations for the year plus total (12+1=13 for
C          monthly figures; 4+1=5 for quarterly)
C LOOP:    Looping variable
C MT1:     The unit number of the output file
C TBLFMT:  The format to use in the table
C TMP:     The user supplied observations for one year
C TYRLY:   Title for the average or total output line
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION BIG,ZERO
      PARAMETER(BIG=10D16,ZERO=0D0)
c-----------------------------------------------------------------------
      DOUBLE PRECISION Tmp,postmp
      LOGICAL Lstar,Lastcl
      INTEGER i,Jyr,Kdec,L,loop,Mt1,Ipow,ipos,j,j2
      CHARACTER Tblfmt*(*),Tyrly*(5),chtmp*(30),blank*(6),thisOb*(30),
     &          dfmt*(10),xlin*(132),cy*(4)
      DIMENSION chtmp(PSP+1),Tmp(*),postmp(PSP+1)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      DOUBLE PRECISION ceilng
      INTEGER nblank
      EXTERNAL dpeq,ceilng,nblank
c-----------------------------------------------------------------------
      DATA blank/'&nbsp;'/
c-----------------------------------------------------------------------
c     write first entry
c-----------------------------------------------------------------------
      CALL writTag(Mt1,'<tr>')
      IF(Tyrly.eq.'XXXXX')THEN
       WRITE(Mt1,1001)Jyr
 1001  FORMAT('<th scope="row">',i4,'</th>')
      ELSE
       CALL mkHeaderCellScope(Mt1,0,0,'row','@',Tyrly)
      END IF
c-----------------------------------------------------------------------
C     Go through L elements one at a time checking to see if special
c     output is needed
c-----------------------------------------------------------------------
      Lstar=.false.
      DO i=1,L
c-----------------------------------------------------------------------
C     If the element is 1 quadrillion or more, this means the 
C     observation is assumed to be missing for this month/quarter.  
c     Set Lstar to true, and set up the label for missing values.
c     If the missing value is before the start or end of the series,
c     set the label to blanks instead of stars.
c-----------------------------------------------------------------------
       chtmp(i)=' '
       IF(dpeq(Tmp(i),DNOTST).or.Tmp(i).ge.BIG)THEN
        IF(.not.Lstar)Lstar=.true.
        chtmp(i)=blank
       ELSE
        IF(Tmp(i).lt.ZERO)Lstar=.true.
        postmp(i)=Tmp(i)
        IF(Ipow.eq.1)postmp(i)=postmp(i)*100d0
c-----------------------------------------------------------------------
c     For cases where Kdec = 0 and the decimal fraction is exactly .5,
c     make an adjustment to ensure the number will round properly
c     when printed (BCM April 2007)
c-----------------------------------------------------------------------
        IF(dpeq(postmp(i)-ceilng(postmp(i)-0.5D0),0.5D0).and.Kdec.eq.0)
     &     postmp(i)=postmp(i)+0.01D0
        WRITE(chtmp(i),Tblfmt)postmp(i)
       END IF
       thisOb=chtmp(i)
       IF(Lstar)THEN
        CALL mkTableCell(Mt1,'nowrap',thisOb(1:nblank(thisOb)))
       ELSE
        CALL mkTableCell(Mt1,'@',thisOb(1:nblank(thisOb)))
       END IF
      END DO
      IF(.not.(Tyrly.eq.'XXXXX'.or.Tyrly.eq.'  I  '.or.Tyrly.eq.'  S  '
     &     .or.Tyrly.eq.'RATIO'))CALL mkTableCell(Mt1,'@','&nbsp;')
      CALL writTag(Mt1,'</tr>')
c-----------------------------------------------------------------------
      RETURN
      END
