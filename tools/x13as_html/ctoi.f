C     Last change:  BCM  21 Nov 97   10:06 pm
**==ctoi.f    processed by SPAG 4.03F  at 09:47 on  1 Mar 1994
      INTEGER FUNCTION ctoi(Str,Ipos)
      IMPLICIT NONE
c     -----------------------------------------------------------------
      CHARACTER Str*(*)
      LOGICAL havint
      INTEGER digit,indx,Ipos,lstpt,nchr,insign
      EXTERNAL indx
c     -----------------------------------------------------------------
      insign=1
      nchr=len(Str)
      lstpt=Ipos
      havint=.false.
c     -----------------------------------------------------------------
      IF(Str(Ipos:Ipos).eq.'+'.or.Str(Ipos:Ipos).eq.'-')THEN
       IF(Str(Ipos:Ipos).eq.'-')insign=-1
       Ipos=Ipos+1
      END IF
c     -----------------------------------------------------------------
      ctoi=0
      DO Ipos=Ipos,nchr
       digit=indx('0123456789',Str(Ipos:Ipos))-1
       IF(digit.eq.-1)GO TO 10
       havint=.true.
       ctoi=10*ctoi+digit
      END DO
c     -----------------------------------------------------------------
   10 ctoi=insign*ctoi
      IF(.not.havint)Ipos=lstpt
c     -----------------------------------------------------------------
      RETURN
      END
