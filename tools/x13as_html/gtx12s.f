C     Last change:  BCM  14 May 1998    7:56 am
      SUBROUTINE gtx12s(Plen,File,Nfil,Y,Start,Chnl,Nobs,Ncol,Freq,
     &                  Srsnam,Nser,Argok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Read the X12SAVE data file format
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'units.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      LOGICAL F,T
      INTEGER YR,MO
      PARAMETER(YR=1,MO=2,F=.false.,T=.true.)
c-----------------------------------------------------------------------
      CHARACTER File*(PFILCR),Srsnam*(64)
      DOUBLE PRECISION Y
      LOGICAL Argok
      INTEGER i,i2,itmp1,Plen,Start,Chnl,Nobs,Ncol,Freq,itmp,year,per,
     &        nyy,npr,Nfil,Nser
      DIMENSION Y(Plen),Start(2)
c-----------------------------------------------------------------------
c     Read header lines
c-----------------------------------------------------------------------
      READ(Chnl,1000)
 1000 FORMAT(/)
c-----------------------------------------------------------------------
      i=1
      DO WHILE (i.le.Plen)
c-----------------------------------------------------------------------
c     Read the date, observation from file
c-----------------------------------------------------------------------
       READ(Chnl,*,END=20,ERR=10)itmp1,(Y(i2),i2=i,i+Ncol-1)
c-----------------------------------------------------------------------
c     If this is the first observation, set the starting date.
c-----------------------------------------------------------------------
       year=itmp1/100
       per=mod(itmp1,100)
       IF(i.eq.1)THEN
        Start(YR)=itmp1/100
        Start(MO)=mod(itmp1,100)
        itmp=Start(YR)*Freq+Start(MO)
       ELSE
        itmp=itmp+1
        nyy=itmp/Freq
        npr=mod(itmp,Freq)
        IF(npr.eq.0)THEN
         nyy=nyy-1
         npr=Freq
        END IF
        IF(.not.((nyy.eq.year).and.(npr.eq.per)))THEN
         WRITE(STDERR,1001)nyy,npr,Srsnam(1:Nser),year,per
         WRITE(Mt2,1002)nyy,npr,Srsnam(1:Nser),year,per,Cbr
 1001    FORMAT(' ERROR: Expected to find observation ',i4,':',i2,
     &          ' of series ',a,/,
     &          '        not ',i4,':',i2,'.  Check input file and ',
     &          'format.',/)
 1002    FORMAT(' <p><strong>ERROR:</strong> Expected to find ',
     &          'observation ',i4,':',i2,' of series ',a,/,
     &          ' not ',i4,':',i2,'.',a,' Check input file and ',
     &          'format.</p>',/)
         Argok=F
         Nobs=0
         RETURN
        END IF
       END IF
c-----------------------------------------------------------------------
       i=i+Ncol
      END DO
c-----------------------------------------------------------------------
      IF(i.gt.Plen)THEN
       CALL eWritln('Problem reading '//File(1:Nfil)//'.',
     &              STDERR,Mt2,T,F)
       CALL writln('        Too many observations in file.',
     &             STDERR,Mt2,F,T)
       Argok=.false.
       Nobs=0
      END IF
c-----------------------------------------------------------------------
   10 CALL eWritln('Problem reading '//File(1:Nfil)//'.',STDERR,Mt2,T,F)
      CALL writln('        Check your input file and format.',
     &            STDERR,Mt2,F,T)
      Argok=.false.
      Nobs=0
c-----------------------------------------------------------------------
   20 RETURN
      END
