C     Last change:  BCM   4 Sep 1998    1:43 pm
      SUBROUTINE vsfb(Sts,Stsi,Lfda,Llda,Nyr)
      IMPLICIT NONE
C-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'x11msc.cmn'
      INCLUDE 'x11opt.cmn'
C-----------------------------------------------------------------------
*      LOGICAL T
*      PARAMETER(T=.true.)
C-----------------------------------------------------------------------
      DOUBLE PRECISION savg,simon,Sts,Stsi,tmp1,w9,w15
      INTEGER i,j,jjj,k,kfda,Lfda,Llda,Nyr
*      LOGICAL allstb
      DIMENSION savg(PYRS),simon(PYRS+6),w9(40),w15(100),Sts(PLEN),
     &          Stsi(PLEN)
C-----------------------------------------------------------------------
      DOUBLE PRECISION totals
      EXTERNAL totals
C-----------------------------------------------------------------------
      DATA w9/
     &   0.246D0,0.221D0,0.197D0,0.173D0,0.112D0,0.051D0,
     &   0.208D0,0.192D0,0.176D0,0.160D0,0.144D0,0.092D0,0.028D0,
     &   0.173D0,0.163D0,0.154D0,0.143D0,0.133D0,0.123D0,0.079D0,
     &   0.032D0,
     &   0.141D0,0.137D0,0.132D0,0.128D0,0.123D0,0.117D0,0.113D0,
     &   0.075D0,0.034D0,
     &   0.084D0,0.120D0,0.118D0,0.117D0,0.116D0,0.114D0,0.113D0,
     &   0.111D0,0.073D0,0.034D0/
      DATA w15/
     1   .16000D0,.16000D0,.16000D0,.16000D0,.16000D0,.06667D0,.06667D0,
     &   .04444D0,.02222D0,
     2   .14667D0,.14667D0,.14667D0,.14667D0,.14667D0,.06667D0,.06667D0,
     &   .06667D0,.04444D0,.02220D0,
     3   .13333D0,.13333D0,.13333D0,.13333D0,.13333D0,.06667D0,.06667D0,
     &   .06667D0,.06667D0,.04444D0,.02223D0,
     4   .12000D0,.12000D0,.12000D0,.12000D0,.12000D0,.06667D0,.06667D0,
     &   .06667D0,.06667D0,.06667D0,.04444D0,.02221D0,
     5   .10667D0,.10667D0,.10667D0,.10667D0,.10667D0,.06667D0,.06667D0,
     &   .06667D0,.06667D0,.06667D0,.06667D0,.04444D0,.02219D0,
     6   .09333D0,.09333D0,.09333D0,.09333D0,.09333D0,.06667D0,.06667D0,
     &   .06667D0,.06667D0,.06667D0,.06667D0,.06667D0,.04444D0,.02222D0,
     7   .08000D0,.08000D0,.08000D0,.08000D0,.08000D0,.06667D0,.06667D0,
     &   .06667D0,.06667D0,.06667D0,.06667D0,.06667D0,.06667D0,.04444D0,
     &   .02220D0,
     8   .04889D0,.07111D0,.07111D0,.07111D0,.07111D0,.06667D0,.06667D0,
     &   .06667D0,.06667D0,.06667D0,.06667D0,.06667D0,.06667D0,.06667D0,
     &   .04444D0,.02220D0 /
C-----------------------------------------------------------------------
      kfda=Lfda+Nyr-1
      Mtype=Lterm+1
*      allstb=T
C-----------------------------------------------------------------------
C --- CHECK IF MOVING AVERAGE IS PRESELECTED.
C-----------------------------------------------------------------------
*      IF(Mtype.eq.7.or.Mtype.eq.1)THEN
*       Mtype=3
*       IF(Ksect.eq.1)Mtype=2
*      END IF
C-----------------------------------------------------------------------
C --- IF LESS THAN 5 COMPLETE YEARS SWITCH TO STABLE SEASONALITY.
C-----------------------------------------------------------------------
      IF(.not.Shrtsf.and.(Llda-Lfda-5*Nyr+1).lt.0)Mtype=6
      DO j=Lfda,kfda
       IF(((Llda-Lfda+1-5*Nyr).ge.0).or.Shrtsf)THEN
        jjj=mod(j,Nyr)
        IF(jjj.eq.0)jjj=Nyr
        Mtype=Lter(jjj)+1
       END IF
       IF(Mtype.eq.7.or.Mtype.eq.1)THEN
        Mtype=3
        IF(Ksect.eq.1)Mtype=2
       ELSE IF(Mtype.eq.8)THEN
        Mtype=7
       END IF
       k=0
       DO i=j,Llda,Nyr
        k=k+1
        simon(k)=Stsi(i)
       END DO
*       allstb=allstb.and.(Mtype.eq.6.or.(Mtype.eq.5.and.k.lt.20))
       IF(Shrtsf.and.k.eq.3.and.Mtype.eq.3)Mtype=6
       IF(Mtype.eq.2)THEN
C-----------------------------------------------------------------------
C --- COMPUTE A 3X3 MOVING AVERAGE.
C-----------------------------------------------------------------------
        CALL averag(simon,savg,1,k,3,3)
        savg(1)=(11D0*(simon(1)+simon(2))+5D0*simon(3))/27D0
        savg(k)=(11D0*(simon(k)+simon(k-1))+5D0*simon(k-2))/27D0
        IF(k.eq.3)THEN
         savg(2)=(simon(1)+simon(2)+simon(3))/3D0
        ELSE
         savg(2)=(0.7D0*(simon(1)+simon(3))+simon(2)+0.3D0*simon(4))/
     &            2.7D0
         savg(k-1)=(0.7D0*(simon(k)+simon(k-2))+simon(k-1)+0.3D0*
     &              simon(k-3))/2.7D0
        END IF
       ELSE IF(Mtype.eq.3)THEN
C-----------------------------------------------------------------------
C --- COMPUTE A 3X5 MOVING AVERAGE.
C-----------------------------------------------------------------------
        CALL averag(simon,savg,1,k,3,5)
        savg(1)=(17D0*(simon(1)+simon(2)+simon(3))+9D0*simon(4))/60D0
        savg(k)=(17D0*(simon(k)+simon(k-1)+simon(k-2))+9D0*simon(k-3))
     &          /60D0
        IF(k.eq.4)THEN
         savg(2)=(simon(1)+simon(2)+simon(3)+simon(4))/4D0
         savg(3)=(simon(1)+simon(2)+simon(3)+simon(4))/4D0
        ELSE
         savg(2)=(15D0*(simon(1)+simon(2)+simon(3))+11D0*simon(4)
     &           +4D0*simon(5))/60D0
         savg(k-1)=(15D0*(simon(k)+simon(k-1)+simon(k-2))+
     &              11D0*simon(k-3)+4D0*simon(k-4))/60D0
        END IF
        IF(k.eq.5)THEN
         savg(3)=(simon(1)+simon(2)+simon(3)+simon(4)+simon(5))/5D0
        ELSE IF(k.gt.5)THEN
         savg(3)=(9D0*simon(1)+13D0*(simon(2)+simon(3)+simon(4))
     &           +8D0*simon(5)+4D0*simon(6))/60D0
         savg(k-2)=(9D0*simon(k)+13D0*(simon(k-1)+simon(k-2)+simon(k-3))
     &             +8D0*simon(k-4)+4D0*simon(k-5))/60D0
        END IF
       ELSE IF(Mtype.eq.4)THEN
C-----------------------------------------------------------------------
C --- COMPUTE A 3X9 MOVING AVERAGE
C-----------------------------------------------------------------------
        CALL averag(simon,savg,1,k,3,9)
C-----------------------------------------------------------------------
C --- APPLY END WEIGHTS FOR THE 3X9
C-----------------------------------------------------------------------
        CALL endsf(simon,savg,k,w9,5)
       ELSE IF(Mtype.eq.5.and.k.ge.20)THEN
C-----------------------------------------------------------------------
C --- COMPUTE A 3X15 MOVING AVERAGE
C-----------------------------------------------------------------------
        CALL averag(simon,savg,1,k,3,15)
C-----------------------------------------------------------------------
C --- APPLY END WEIGHTS FOR THE 3X15
C-----------------------------------------------------------------------
        CALL endsf(simon,savg,k,w15,8)
       ELSE IF(Mtype.eq.6.or.Mtype.eq.5)THEN
C-----------------------------------------------------------------------
C --- STABLE SEASONAL. AVERAGE OF ALL SI RATIOS FOR THIS MONTH.
C-----------------------------------------------------------------------
        tmp1=totals(simon,1,k,1,1)
        DO i=1,k
         savg(i)=tmp1
        END DO
       ELSE IF(Mtype.eq.7)THEN
C-----------------------------------------------------------------------
C --- COMPUTE A 3-TERM MOVING AVERAGE.
C-----------------------------------------------------------------------
        CALL averag(simon,savg,1,k,1,3)
        savg(1) = 0.61D0*simon(1)+0.39D0*simon(2)
        savg(k) = 0.61D0*simon(k)+0.39D0*simon(k-1)
       END IF
       k=0
       DO i=j,Llda,Nyr
        k=k+1
        Sts(i)=savg(k)
       END DO
      END DO
C-----------------------------------------------------------------------
c     CHANGE BCM FEB 1996 - comment out return for linear seasonal 
c     adjustment
C-----------------------------------------------------------------------
c      IF(Linsa)RETURN
C-----------------------------------------------------------------------
C --- APPLY A 2 X NYR MOVING AVERAGE TO THE SEASONALS.
C-----------------------------------------------------------------------
      CALL vsfc(Sts,Lfda,Llda,Nyr,Lter)
C-----------------------------------------------------------------------
      RETURN
      END
