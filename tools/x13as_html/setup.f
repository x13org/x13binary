C     Last change:  BCM  12 Nov 1998   10:53 am
**==setup.f    processed by SPAG 4.03F  at 11:38 on  7 Nov 1994
      SUBROUTINE setup(Icode,Icod2)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'chrt.cmn'
      INCLUDE 'rho.cmn'
c-----------------------------------------------------------------------
      INTEGER i,Icod2,Icode,j,j2,k,k2
c-----------------------------------------------------------------------
      Fact1=52D0
      Ifact2=54
      Ibottm=55
C***************
C  SET UP CHART BOUNDARIES
C***************
      DO i=1,110
       DO j=1,55
        Ia(i,j)=' '
       END DO
      END DO
      IF(Nseas.eq.12.or.Icode.eq.7)THEN
       DO i=1,61
        Iyear(i)=int(Xdata3(i))
       END DO
      ELSE
       Inyr=0
       DO i=1,30,2
        j=i/2+1
        IF(Ifrst.eq.1)THEN
         Iyear(j)=int(Xdata3(i))
        ELSE
         Iyear(j)=int(Xdata3(i+1))
        END IF
        IF(i.le.Nyr)Inyr=Inyr+1
       END DO
      END IF
      Icmax=N1+1
      Ydiff=Ymax-Ymin
      DO j=1,53,4
       j2=((53-j)/4)+1
       Ymid(j2)=(Ydiff*(j-1))/52+Ymin
      END DO
      Ia(1,1)=I11
      Ia(1,Ibottm)=I10
      Ia(Icmax+1,1)=I8
      Ia(Icmax+1,Ibottm)=I9
      DO i=2,Ibottm-1
       Ia(1,i)=I4
       Ia(Icmax+1,i)=I4
      END DO
      IF(((Icode-7)*(Icod2-15)*Icode).ne.0)THEN
       k=Nseas-Ifrst+2
       DO i=1,Ibottm
        DO j=k,Icmax,Nseas
         Ia(j,i)=I7
        END DO
       END DO
      END IF
      IF(Icode.eq.0)THEN
       DO i=1,Ibottm
        DO j=1,Icmax
         j2=j-2
         IF(Nseas.eq.10)THEN
          IF(Lfqalt.and.j2.eq.36)Ia(j,i)='T'
          IF(j2.eq.42.or.j2.eq.52)Ia(j,i)='T'
         ELSE IF(Icod2.eq.0)THEN
          IF(Lfqalt.and.(j2.eq.35.or.j2.eq.41.or.j2.eq.46))Ia(j,i)='T'
          IF(j2.eq.5.or.j2.eq.11)Ia(j,i)='T'
         END IF
         IF(mod(j2,Nseas).eq.0.and.j2.gt.0)Ia(j,i)='S'
        END DO
       END DO
      END IF
      DO i=2,Icmax
       Ia(i,1)=I3
       Ia(i,Ibottm)=I3
       IF(((Icode-7)*(Icod2-15)*Icode).ne.0)THEN
        DO k2=3,12,3
         Ia(i,Imid(k2))=I7
        END DO
       END IF
      END DO
      RETURN
      END
