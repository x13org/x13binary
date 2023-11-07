C     Last change:  BCM  14 May 1998    7:54 am
      SUBROUTINE gtfrcm(Plen,File,Nfil,Y,Chnl,Nobs,Argok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Read the free formatted data with commas instead of periods for
c     decimal places
c     Created by : BCMonsell, April 2003
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'lex.i'
      INCLUDE 'units.cmn'
c-----------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
c-----------------------------------------------------------------------
      CHARACTER File*(PFILCR),Chrstr*(LINLEN)
      DOUBLE PRECISION Y
      LOGICAL Argok
      INTEGER i,i1,i2,ncomma,Plen,Chnl,Nobs,itmp,Nfil
      DIMENSION Y(Plen)
c-----------------------------------------------------------------------
      INTEGER nblank
      EXTERNAL nblank
c-----------------------------------------------------------------------
      i=1
      itmp=1
      DO WHILE (i.le.Plen)
c-----------------------------------------------------------------------
c     Read the data into a character vector
c-----------------------------------------------------------------------
       READ(Chnl,'(a)',END=20,ERR=10)chrstr
c-----------------------------------------------------------------------
c     convert commas in character string to periods.
c-----------------------------------------------------------------------
       CALL cvcmma(chrstr,ncomma)
       IF(ncomma.eq.0)THEN
        WRITE(STDERR,1010)File(1:Nfil),itmp
        WRITE(Mt2,1011)File(1:Nfil),itmp
 1010   FORMAT(/,' ERROR: Problem reading ',a,'.'/,
     &           '        No observations found in line ',i3,'.',/,
     &           '        Only use format="freecomma" when there are ',
     &           'commas in data file.',/)
 1011   FORMAT(/,' <p><strong>ERROR:</strong> Problem reading ',a,'.'/,
     &           ' No observations found in line ',i3,'.',/,
     &           ' Only use format="freecomma" when there are commas ',
     &           'in data file.</p>',/)
        Argok=F
        Nobs=0
        RETURN
       END IF
       i1=i+ncomma-1
       IF(i1.gt.Plen)THEN
        i=i1
        GO TO 30
       END IF
       read(chrstr,*)(Y(i2),i2=i,i1)
c-----------------------------------------------------------------------
       i=i+ncomma
       itmp=itmp+1
      END DO
c-----------------------------------------------------------------------
   30 IF(i.gt.Plen)THEN
       CALL eWritln('Problem reading '//File(1:Nfil)//'.',
     &              STDERR,Mt2,T,F)
       CALL writln('        Too many observations in file.',
     &             STDERR,Mt2,F,T)
       Argok=F
       Nobs=0
      END IF
c-----------------------------------------------------------------------
   10 CALL eWritln('Problem reading '//File(1:Nfil)//'.',STDERR,Mt2,T,F)
      CALL writln('        Check your input file and format.',
     &            STDERR,Mt2,F,T)
      Argok=F
      Nobs=0
c-----------------------------------------------------------------------
   20 RETURN
      END

