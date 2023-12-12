C     Last change:  BCM  21 Nov 97   10:04 pm
**==ctod.f    processed by SPAG 4.03F  at 09:47 on  1 Mar 1994
      DOUBLE PRECISION FUNCTION ctod(Str,Ipos)
      IMPLICIT NONE
c     ------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     -----------------------------------------------------------------
      CHARACTER Str*(*)
      LOGICAL havdbl
      INTEGER ctoi,digit,expint,exppos,Ipos,indx,lstpt,nchr
      DOUBLE PRECISION scl,dpsign
      EXTERNAL ctoi,indx
c     -----------------------------------------------------------------
      dpsign=1D0
      nchr=len(Str)
      lstpt=Ipos
      havdbl=F
c     -----------------------------------------------------------------
      IF(Str(Ipos:Ipos).eq.'+'.or.Str(Ipos:Ipos).eq.'-')THEN
       IF(Str(Ipos:Ipos).eq.'-')dpsign=-1D0
       Ipos=Ipos+1
      END IF
c     -----------------------------------------------------------------
      ctod=0D0
      DO Ipos=Ipos,nchr
       digit=indx('0123456789',Str(Ipos:Ipos))-1
       IF(digit.eq.-1)GO TO 10
       ctod=10D0*ctod+dble(digit)
       havdbl=T
      END DO
      Ipos=nchr+1
c     -----------------------------------------------------------------
   10 IF(Str(Ipos:Ipos).eq.'.'.and.Ipos.le.nchr)THEN
       Ipos=Ipos+1
c     -----------------------------------------------------------------
       scl=1D0
       DO Ipos=Ipos,nchr
        scl=scl*10D0
        digit=indx('0123456789',Str(Ipos:Ipos))-1
        IF(digit.eq.-1)GO TO 20
        ctod=ctod+dble(digit)/scl
        havdbl=T
       END DO
       Ipos=nchr+1
      END IF
c     -----------------------------------------------------------------
   20 ctod=dpsign*ctod
c     -----------------------------------------------------------------
      IF(havdbl.and.Ipos.lt.nchr)THEN
       IF(indx('eEdD^',Str(Ipos:Ipos)).gt.0)THEN
        exppos=Ipos
        Ipos=Ipos+1
        expint=ctoi(Str,Ipos)
        IF(Ipos.eq.exppos+1)THEN
         Ipos=exppos
        ELSE
         ctod=ctod*10D0**expint
        END IF
       END IF
      END IF
c     -----------------------------------------------------------------
      IF(.not.havdbl)THEN
       ctod=0D0
       Ipos=lstpt
      END IF
c     -----------------------------------------------------------------
      RETURN
      END
