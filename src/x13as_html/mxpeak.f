      SUBROUTINE mxpeak(Sxx,Tpeak,Domfqt,Ntfreq,Speak,Domfqs,Nsfreq,
     &                  Maxsxx,Nform,Spclab)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
c-----------------------------------------------------------------------
      CHARACTER labvec*(2),domfrq*(2),Spclab*(*)
      DOUBLE PRECISION Sxx,Maxsxx
      INTEGER Domfqt,Domfqs,i,i2,Nform,Pkidx,frq1,Tpeak,Ntfreq,Speak,
     &        Nsfreq
      DIMENSION Sxx(*),Tpeak(*),Speak(*),labvec(11)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
      DATA labvec/'t1','t2','t3','t4','t5','s1','s2','s3','s4','s5',
     &            's6'/
c-----------------------------------------------------------------------
      domfrq = 'no'
      i2 = 0
c-----------------------------------------------------------------------
      IF(Domfqt.eq.NOTSET.and.Domfqs.eq.NOTSET)THEN
       WRITE(Nform,1010)Spclab,domfrq
       RETURN
      ELSE IF(Domfqt.eq.NOTSET)THEN
       frq1=Domfqs
       i2=5
      ELSE IF(Domfqs.eq.NOTSET)THEN
       frq1=Domfqt
      ELSE IF(Sxx(Domfqt).gt.Sxx(Domfqs))THEN
       frq1=Domfqt
      ELSE
       frq1=Domfqs
       i2=5
      END IF
c-----------------------------------------------------------------------
      IF(dpeq(Maxsxx,Sxx(frq1)))THEN
       IF(i2.eq.0)THEN
        i=1
        DO WHILE (i.le.Ntfreq)
         IF(Tpeak(i).eq.frq1)THEN
          i2=i
          i=Ntfreq
         END IF
         i=i+1
        END DO
       ELSE
        i=1
        DO WHILE (i.le.Nsfreq)
         IF(Speak(i).eq.frq1)THEN
          i2=i+i2
          i=Nsfreq
         END IF
         i=i+1
        END DO
       END IF
       domfrq=labvec(i2)
      END IF
c-----------------------------------------------------------------------
      WRITE(Nform,1010)Spclab,domfrq
c-----------------------------------------------------------------------
 1010 FORMAT(a,'.dom: ',a)
      RETURN
      END
