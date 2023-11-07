C     Last change:  BCM  25 Nov 97    2:57 pm
**==yrly.f    processed by SPAG 4.03F  at 09:56 on  1 Mar 1994
      SUBROUTINE yrly(Icode,Icod2,Jx,Noser)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'chrt.cmn'
c-----------------------------------------------------------------------
      INTEGER i,Icod2,Icode,j,Jx,jy1,jy2,l,Noser
      CHARACTER*1 itype
c-----------------------------------------------------------------------
C  PLOT CHART TYPES 1-6
c-----------------------------------------------------------------------
      DO i=1,N1
       Xyvec=Y1(i)
       CALL value
       l=mod(Ifrst+i-2,Nseas)+1
       itype=I1
c-----------------------------------------------------------------------
       IF(Nseas.eq.12)THEN
        itype=Ialpha(l)
       ELSE IF(Nseas.eq.4)THEN
        itype=Ialphq(l)
       END IF
c-----------------------------------------------------------------------
       IF(Icode.eq.0)THEN
        IF(Ixy.gt.Ibottm)THEN
         Ia(Jx,Ixy)=itype
        ELSE
         IF(Ia(Jx,Ixy).ne.'S'.and.Ia(Jx,Ixy).ne.'T')Ia(Jx,Ixy)=itype
        END IF
       ELSE
        Ia(Jx,Ixy)=itype
       END IF
c-----------------------------------------------------------------------
       IF((Icode-6).eq.0)THEN
        DO j=Ibottm-1,Ixy,-1
         Ia(Jx,j)=itype
        END DO
       ELSE IF(Icode.eq.0)THEN
        DO j=2,Ixy-1
         IF(Ia(Jx,j).eq.'S'.or.Ia(Jx,j).eq.'T')Ia(Jx,j)=' '
        END DO
        DO j=Ibottm-1,Ixy+1,-1
         IF(Ia(Jx,j).ne.'S'.and.Ia(Jx,j).ne.'T')Ia(Jx,j)=I1
        END DO
       END IF
c-----------------------------------------------------------------------
       IF(Noser.eq.2)THEN
        jy1=Ixy
        Xyvec=Y2(i)
        CALL value
        jy2=Ixy
        Ia(Jx,jy2)=I12
        IF(Icod2.eq.15)CALL dot(jy1,jy2,Jx)
       END IF
       Jx=Jx+1
      END DO
c-----------------------------------------------------------------------
      IF(Icod2.ge.15)Icode=Icod2
c-----------------------------------------------------------------------
      RETURN
      END
