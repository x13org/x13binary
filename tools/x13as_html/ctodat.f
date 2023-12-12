      SUBROUTINE ctodat(Str,Sp,Ipos,Idate,Locok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Puts the date in character format for outlier variables and
c printouts.  Seasonal period, Sp, is assumed to be known here.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
c     ------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      CHARACTER Str*(*)
      LOGICAL isdate,Locok
      INTEGER ctoi,Idate,Ipos,lstpt,Sp,strinx
      DIMENSION Idate(2)
      EXTERNAL ctoi,isdate,strinx
c     ------------------------------------------------------------------
      CHARACTER MODIC*36
      INTEGER indx,moptr
      DIMENSION moptr(0:12)
      PARAMETER(MODIC='JanFebMarAprMayJunJulAugSepOctNovDec')
      EXTERNAL indx
      DATA moptr/1,4,7,10,13,16,19,22,25,28,31,34,37/
c     ------------------------------------------------------------------
      Locok=T
      lstpt=Ipos
      Idate(YR)=ctoi(Str,Ipos)
      Idate(MO)=0
c     ------------------------------------------------------------------
      IF(Ipos.lt.len(Str))THEN
       IF(Str(Ipos:Ipos).eq.'.')THEN
        Ipos=Ipos+1
        IF(indx('0123456789',Str(Ipos:Ipos)).gt.0)THEN
         Idate(MO)=ctoi(Str,Ipos)
c     ------------------------------------------------------------------
        ELSE
         Idate(MO)=strinx(F,MODIC,moptr,1,12,Str(Ipos:Ipos+2))
c     ------------------------------------------------------------------
c     Change by BCM Nov 1995 to test if seasonal period is 12
c     ------------------------------------------------------------------
         IF(Idate(MO).gt.0)THEN
          IF(Sp.eq.12)THEN
           Ipos=Ipos+3
          ELSE
           Locok=F
           Ipos=lstpt
          END IF
         END IF
        END IF
       END IF
      END IF
c     ------------------------------------------------------------------
      IF(.not.isdate(Idate,Sp))THEN
       Locok=F
       Ipos=lstpt
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
