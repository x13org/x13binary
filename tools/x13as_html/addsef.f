C     Last change:  BCM   4 Sep 1998    2:49 pm
      SUBROUTINE addsef(Begdat,Numrxy,Numcxy,Begcol,Endcol,Xy,Begrgm)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Subroutine to add seasonal effect variables to an nrxy by ncxy xy
c matrix in columns begcol to begcol+sp-2.  seasonal effect submatrix
c is nrxy by sp-1 and the ith seasonal effect has a 1 in month i, -1 in
c the spth month, and zero otherwise.
c-----------------------------------------------------------------------
c Name  Type  Description
c-----------------------------------------------------------------------
c i        i  Local do loop index
c imo      i  Local current month
c j        i  Local do loop index
c zero     i  Local PARAMETER for 0.0d0
c-----------------------------------------------------------------------
c     Variable typing and initialization
c-----------------------------------------------------------------------
      LOGICAL T
      PARAMETER(T=.true.)
c     ------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      LOGICAL F
      DOUBLE PRECISION ZERO,MONE,ONE
      PARAMETER(ZERO=0.0D0,MONE=-1.0D0,ONE=1.0D0,F=.false.)
c-----------------------------------------------------------------------
      CHARACTER str*(PCOLCR)
      LOGICAL Begrgm
      INTEGER Begdat,predat,i,j,imo,premo,Numrxy,Numcxy,Begcol,ipos,
     &        Endcol,ncol,precol,sp2,tcol,icol,iper,nchr
      DOUBLE PRECISION Xy
      DIMENSION Begdat(2),Begrgm(PLEN),predat(2),Xy(Numcxy,Numrxy),
     &          tcol(PSP)
c-----------------------------------------------------------------------
      INTEGER strinx,ctoi
      EXTERNAL strinx,ctoi
c-----------------------------------------------------------------------
      CHARACTER MONDIC*33
      INTEGER monptr,PMON
      PARAMETER(PMON=11)
      DIMENSION monptr(0:PMON)
      PARAMETER(MONDIC='janfebmaraprmayjunjulaugsepoctnov')
      DATA monptr/1,4,7,10,13,16,19,22,25,28,31,34/
c-----------------------------------------------------------------------
c     Set the begining and ending columns and the seasonal period
c-----------------------------------------------------------------------
      ncol=Endcol-Begcol+1
      sp2=ncol+1
c-----------------------------------------------------------------------
c     Check that the begining and ending columns are between 1
c and ncxy.
c-----------------------------------------------------------------------
      IF(Begcol.lt.1.or.Endcol.gt.Numcxy-1.or.Endcol.lt.Begcol)THEN
       CALL eWritln('Column, 1<=begcol<=endcol<=    nb',
     &              STDERR,Mt2,T,F)
       CALL writTag(Mt2,Cbr)
       WRITE(STDERR,1010)Begcol,Endcol,Numcxy-1
       WRITE(Mt2,1010)Begcol,Endcol,Numcxy-1
       CALL writTag(Mt2,'</p>')
 1010  FORMAT(26x,3I8)
       CALL abend()
       RETURN
      END IF
c-----------------------------------------------------------------------
      IF(sp2.lt.Sp)THEN
       ipos=1
       CALL setint(NOTSET,Sp-1,tcol)
       DO icol=Begcol,Endcol
        CALL getstr(Colttl,Colptr,Ncoltl,icol,str,nchr)
        IF(Lfatal)RETURN
        IF(Sp.eq.12)THEN
         iper=strinx(F,MONDIC,monptr,1,PMON,str(1:3))
        ELSE
         iper=ctoi(str(1:nchr),ipos)
        END IF
        tcol(iper)=icol-Begcol+1
       END DO
      END IF
c-----------------------------------------------------------------------
c     Add the SM variables row by row.  Imo is the current month.  If
c imo is the spth month though imo will be 0 then that row will be -1's.
c otherwise a 1 will be placed in the imo'th column of the seasonal
c effect submatrix or the begcol-1+imo column of the xy matrix.
c-----------------------------------------------------------------------
      CALL addate(Begdat,Sp,-1,predat)
      premo=predat(2)
      precol=Begcol-1
c-----------------------------------------------------------------------
      DO i=1,Numrxy
       imo=mod(premo+i,Sp)
c-----------------------------------------------------------------------
       DO j=Begcol,Endcol
        Xy(j,i)=ZERO
       END DO
       IF(Begrgm(i))THEN
        IF(imo.eq.0)THEN
         DO j=Begcol,Endcol
          Xy(j,i)=MONE
         END DO
c      ------------------------------------------------------------------
        ELSE
         IF(Sp.eq.sp2)THEN
          Xy(precol+imo,i)=ONE
         ELSE IF(tcol(imo).ne.NOTSET)THEN
          Xy(precol+tcol(imo),i)=ONE
         END IF
        END IF
       END IF
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
