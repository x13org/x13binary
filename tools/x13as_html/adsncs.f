C     Last change:  BCM  26 Jan 98   12:57 pm
      SUBROUTINE adsncs(Begdat,Sp,Nrxy,Ncxy,Colttl,Colptr,Begcol,Endcol,
     &                  Xy,Begrgm)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Subroutine to add sine-cosine seasonal effect variables to
c an nrXy by ncXy Xy matrix in columns begcol to begcol+sp-2.
c The sine-cosine submatrix is nrXy by sp and the ith and i+1th
c variable are,
c     sin(2*isncos(i)*pi*t/sp)
c     cos(2*isncos(i)*pi*t/sp)
c where isncos(i) is between 1 and sp/2 and there are no more than
c sp/2 sine-cosine pairs.
c-----------------------------------------------------------------------
c Name  Type  Description
c-----------------------------------------------------------------------
c fac      d  Local factor without the harmonic, 2*pi/sp
c fcn      c  Local character string to determine weather the variable
c              is a sine or cosine function
c harmc    d  Local psp long vector of factors that are multiples of
c              the harmonic, 2*pi*harmonic/sp
c i        i  Local do loop index
c iharmc   i  Local index for the ith harmonic
c dmo       d  Local current month
c j        i  Local do loop index
c lsin     l  Local sp/2 vector of switches indicating that the
c              variable is a sine function otherwise it's a cosine
c              function
c ncol     i  Local number of sine-cosine variables, ncol<sp
c rdns     d  Local radians=2*pi*angle/360
c pi       i  Local PARAMETER for pie
c precol   i  Local number of columns before the sine-cosine variables
c two      i  Local PARAMETER for 2d0
c-----------------------------------------------------------------------
c     Variable typing and initialization
c-----------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'htmlout.cmn'
c     ------------------------------------------------------------------
      DOUBLE PRECISION PI,TWO,ZERO
      PARAMETER(PI=3.14159265358979d0,TWO=2D0,ZERO=0D0)
c     ------------------------------------------------------------------
      LOGICAL lsin,Begrgm
      CHARACTER Colttl*(*),str*(PCOLCR)
      INTEGER Begcol,Begdat,ctoi,Colptr,Endcol,i,ipos,j,nb,nchr,Nrxy,
     &        Ncxy,ncol,precol,predat,premo,Sp
      DOUBLE PRECISION fac,harmc,dmo,rdns,Xy
      DIMENSION Begdat(2),Colptr(0:Ncxy-1),harmc(PSP),lsin(PSP),
     &          predat(2),Xy(Ncxy,Nrxy),Begrgm(PLEN)
      EXTERNAL ctoi
c-----------------------------------------------------------------------
c     Set the begining and ending columns and the seasonal period
c-----------------------------------------------------------------------
      nb=Ncxy-1
      ncol=Endcol-Begcol+1
c-----------------------------------------------------------------------
c     Check that the begining and ending columns are between 1
c and ncXy.
c-----------------------------------------------------------------------
      IF(Begcol.lt.1.or.Endcol.gt.Ncxy-1.or.Endcol.lt.Begcol)THEN
       CALL eWritln('Column, 1<=begcol<=endcol<=    nb',
     &              STDERR,Mt2,T,F)
       CALL writTag(Mt2,Cbr)
       WRITE(STDERR,1010)Begcol,Endcol,Ncxy-1
       WRITE(Mt2,1010)Begcol,Endcol,Ncxy-1
       CALL writTag(Mt2,'</p>')
 1010  FORMAT(26x,3I8)
       CALL abend
       RETURN
      END IF
c-----------------------------------------------------------------------
c     Find the periodicities of the sine-cosine pairs need to be added
c and construct the factors, 2*pi*isncos(i)/sp.
c-----------------------------------------------------------------------
      fac=TWO*PI/Sp
      precol=Begcol-1
      DO j=1,ncol
       i=precol+j
       CALL getstr(Colttl,Colptr,nb,i,str,nchr)
       IF(Lfatal)RETURN
       lsin(j)=str(1:3).eq.'sin'
       ipos=9
       harmc(j)=fac*ctoi(str(1:nchr),ipos)
      END DO
c-----------------------------------------------------------------------
c     Add the SM variables row by row.  Imo is the current month.  If
c imo is the spth month though imo will be 0 then that row will be -1's.
c otherwise a 1 will be placed in the imo'th column of the seasonal
c effect submatrix or the begcol-1+imo column of the Xy matrix.
c Note that no matter where the series start each January or period 1
c is the sin(0) and cos(0).
c-----------------------------------------------------------------------
      CALL addate(Begdat,Sp,-2,predat)
      premo=predat(2)
c     ------------------------------------------------------------------
      DO i=1,Nrxy
       dmo=dble(mod(premo+i,Sp))
       DO j=1,ncol
        rdns=harmc(j)*dmo
        Xy(precol+j,i)=ZERO
        IF(Begrgm(i))THEN
         IF(lsin(j))THEN
          Xy(precol+j,i)=sin(rdns)
         ELSE
          Xy(precol+j,i)=cos(rdns)
         END IF
        END IF
       END DO
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
