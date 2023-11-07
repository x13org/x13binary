C     Last change:  BCM  22 Jan 98   10:58 am
      SUBROUTINE tdxtrm(Sti,Faccal,Tday,Sigm,Kpart,Muladd,Fext,Irridx,
     &                  Irrend)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C --- THIS ROUTINE inserts AO regression variables into the X-11
C --- REGRESSION Matrix for extreme irregulars
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'xclude.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION ONE,ZERO
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.,ONE=1D0,ZERO=0D0)
c-----------------------------------------------------------------------
      DOUBLE PRECISION Sti,Sigm,tsd,Faccal,tdiff,tk,tmean,tcc,tirr,ex,
     &                 tkon,dvec
      INTEGER Tday,karray,i,m,kstd,Muladd,k,Kpart,Fext,Irridx,Irrend
      DIMENSION Sti(PLEN),Tday(PLEN),karray(PLEN),Faccal(PLEN),ex(PLEN),
     &          tmean(28),tcc(28),dvec(1)
c-----------------------------------------------------------------------
c     Set up logical vector of observations to exclude from regression
c-----------------------------------------------------------------------
      dvec(1)=ZERO
      CALL setlg(.false.,PLEN,Rgxcld)
      Nxcld=0
c-----------------------------------------------------------------------
      CALL cpyint(Tday,Posfob,1,karray)
      CALL setdp(DNOTST,PLEN,ex)
c-----------------------------------------------------------------------
      kstd=0
      DO WHILE (kstd.lt.2)
c-----------------------------------------------------------------------
c     If first time, calculate means for each type of month
c-----------------------------------------------------------------------
       tsd=0
       tk=0
       IF(Kpart.eq.2)THEN
c-----------------------------------------------------------------------
c     Initialize mean variables
c-----------------------------------------------------------------------
        tkon=ONE
        IF(Muladd.eq.1)tkon=ZERO
        DO i=1,28
         tcc(i)=ZERO
         IF(i.le.21)THEN
          tmean(i)=ZERO
         ELSE
          tmean(i)=tkon
         END IF
        END DO
c-----------------------------------------------------------------------
        DO i=Irridx,Irrend
         m=karray(i)
         IF(m.lt.15)THEN
          tmean(m)=tmean(m)+Sti(i)
          tcc(m)=tcc(m)+ONE
          tk=tk+ONE
         ELSE IF(m.le.21)THEN
          DO k=15,21
           tmean(k)=tmean(k)+Sti(i)
           tcc(k)=tcc(k)+ONE
          END DO
          tk=tk+ONE
         END IF
        END DO
c-----------------------------------------------------------------------
        DO i=1,21
         IF(tcc(i).gt.ZERO)tmean(i)=tmean(i)/tcc(i)
        END DO
        DO i=Irridx,Irrend
         m=karray(i)
         IF(m.le.21)THEN
          tdiff=Sti(i)-tmean(m)
          tsd=tsd+(tdiff*tdiff)
         END IF
        END DO
c-----------------------------------------------------------------------
       ELSE
c-----------------------------------------------------------------------
C --- COMPUTE SQ.DEV. OF Irregular from Calendar effects
c-----------------------------------------------------------------------
        DO i=Irridx,Irrend
         m=karray(i)
         IF(m.le.28)THEN
          tdiff=Sti(i)-Faccal(i)
          tsd=tsd+(tdiff*tdiff)
          tk=tk+ONE
         END IF
        END DO
       END IF
       tsd=sqrt(tsd/tk)*Sigm
       kstd=kstd+1
c-----------------------------------------------------------------------
C --- IDENTIFY EXTREME IRREGULARS BY ADDING 28 TO TYPE CODE
c-----------------------------------------------------------------------
       DO i=Irridx,Irrend
        m=karray(i)
        IF(m.le.28)THEN
         IF(Kpart.eq.2)THEN
          tirr=tmean(m)
         ELSE
          tirr=Faccal(i)
         END IF
         IF(abs(Sti(i)-tirr).gt.tsd)THEN
          karray(i)=karray(i)+28
          ex(i)=Sti(i)
          Rgxcld(i-Irridx+1)=T
          Nxcld=Nxcld+1
         END IF
        END IF
       END DO
c-----------------------------------------------------------------------
      END DO
c-----------------------------------------------------------------------
c     Print out table for extreme values
c-----------------------------------------------------------------------
      IF(Prttab(Fext))CALL table(ex,Irridx,Irrend,14,1,5,dvec,Fext)
      IF(Savtab(Fext))CALL punch(ex,Irridx,Irrend,Fext,F,F)
c-----------------------------------------------------------------------
      RETURN
      END
