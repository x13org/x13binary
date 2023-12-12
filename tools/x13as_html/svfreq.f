C     Last change:  BCM  4 Mar 2008    9:46 am
      SUBROUTINE svfreq(Ny,Svallf)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C     save frequency, spectral peak information into .udg file
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'spcidx.cmn'
      INCLUDE 'units.cmn'
c-----------------------------------------------------------------------
      LOGICAL Svallf
      INTEGER Ny,i
c-----------------------------------------------------------------------
      IF(Svallf)THEN
       write(Nform,1000)'nspecfreq',Nfreq
      ELSE
       write(Nform,1000)'nspecfreq',61
      END IF
c-----------------------------------------------------------------------
      write(Nform,1000)'ntdfreq',Ntfreq
      DO i = 1,Ntfreq
       write(Nform,1010)'t',i,'freq',Tfreq(i)
       IF (Svallf)THEN
        write(Nform,1020)'t',i,'index',Tpeak(i)-1
        write(Nform,1020)'t',i,'index.lower',Tlow(i)-1
        write(Nform,1020)'t',i,'index.upper',Tup(i)-1
       ELSE
        IF(Ny.eq.12)THEN
         IF(Ntfreq.eq.2)THEN
          IF(i.eq.1)THEN
           write(Nform,1020)'t',i,'index',42
          ELSE IF(i.eq.2)THEN
           write(Nform,1020)'t',i,'index',52
          END IF
         ELSE
          IF(i.eq.1)THEN
           write(Nform,1020)'t',i,'index',36
          ELSE IF(i.eq.2)THEN
           write(Nform,1020)'t',i,'index',42
          ELSE IF(i.eq.3)THEN
           write(Nform,1020)'t',i,'index',52
          END IF
         END IF
        ELSE
         IF(i.eq.1) THEN
          write(Nform,1020)'t',i,'index',5
         ELSE IF(i.eq.2) THEN
          write(Nform,1020)'t',i,'index',11
         ELSE IF(i.eq.3)THEN
          write(Nform,1020)'t',i,'index',35
         ELSE IF(i.eq.4)THEN 
          write(Nform,1020)'t',i,'index',41
         ELSE IF(i.eq.5)THEN 
          write(Nform,1020)'t',i,'index',46
         END IF
        END IF
       END IF
      END DO
c-----------------------------------------------------------------------
      write(Nform,1000)'nsfreq',Nsfreq
      DO i = 1,Nsfreq
       write(Nform,1010)'s',i,'freq',Sfreq(i)
       IF (Svallf)THEN
        write(Nform,1020)'s',i,'index',Speak(i)-1
        write(Nform,1020)'s',i,'index.lower',Slow(i)-1
        IF(i.lt.Nsfreq)write(Nform,1020)'s',i,'index.upper',Sup(i)-1
       ELSE
        IF(Ny.eq.12)THEN
         IF(i.eq.1) THEN
          write(Nform,1020)'s',i,'index',10
         ELSE IF(i.eq.2) THEN
          write(Nform,1020)'s',i,'index',20
         ELSE IF(i.eq.3)THEN
          write(Nform,1020)'s',i,'index',30
         ELSE IF(i.eq.4)THEN 
          write(Nform,1020)'s',i,'index',40
         ELSE IF(i.eq.5)THEN 
          write(Nform,1020)'s',i,'index',50
         ELSE IF(i.eq.6)THEN 
          write(Nform,1020)'s',i,'index',60
         END IF
        ELSE
         IF(i.eq.1) THEN
          write(Nform,1020)'s',i,'index',30
         ELSE IF(i.eq.2) THEN
          write(Nform,1020)'s',i,'index',60
         END IF
        END IF
       END IF
      END DO
c-----------------------------------------------------------------------
 1000 FORMAT(a,': ',i5)
 1010 FORMAT(a,i1,'.',a,': ',f12.8)
 1020 FORMAT(a,i1,'.',a,': ',i5)
c-----------------------------------------------------------------------
      RETURN
      END