C     Last change:  BCM  12 Mar 98   12:33 pm
      SUBROUTINE savmtx(Itbl,Begxy,Sp,Xy,Nrxy,Ncxy,Ttlstr,Ttlptr,Nttl)
      IMPLICIT NONE
c     ------------------------------------------------------------------
      INCLUDE 'cchars.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'savcmn.cmn'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      CHARACTER Ttlstr*(*),outstr*(6+22*PB),dash*(22)
      LOGICAL locok
      INTEGER begelt,Begxy,endelt,fh,ielt,idate,ipos,Itbl,Ncxy,Nrxy,
     &        Nttl,rdbdat,Sp,tpnt,Ttlptr
      DOUBLE PRECISION Xy
      DIMENSION Begxy(2),idate(2),Ttlptr(0:Nttl),Xy(*)
c-----------------------------------------------------------------------
      DATA dash /'----------------------'/
c-----------------------------------------------------------------------
c     Open file with the an extension which depends on the type of the
c series.
c-----------------------------------------------------------------------
      CALL opnfil(.true.,.false.,Itbl,fh,locok)
      IF(.not.locok)THEN
       CALL abend
       RETURN
      END IF
c-----------------------------------------------------------------------
c     Print the regression matrix or effects.
c Figure the columns of the matrix are nb+1.
c Print the column titles as the variable or column names then write
c the dash line.  The column names are taken from the ttlstr string
c vector.  Then write the dates before the nb columns.
c-----------------------------------------------------------------------
      WRITE(fh,1010)'Date',
     &              (TABCHR,Ttlstr(Ttlptr(ielt-1):Ttlptr(ielt)-1),
     &              ielt=1,Nttl)
 1010 FORMAT(1000a)
      WRITE(fh,1010)'----',(TABCHR,dash(1:Svsize),ielt=1,Nttl)
c     ------------------------------------------------------------------
      DO tpnt=1,Nrxy
       CALL addate(Begxy,Sp,tpnt-1,idate)
       IF(Sp.eq.1)THEN
        rdbdat=idate(YR)
       ELSE
        rdbdat=100*idate(YR)+idate(MO)
       END IF
c     ------------------------------------------------------------------
       begelt=Ncxy*(tpnt-1)+1
       endelt=Ncxy*tpnt-(Ncxy-Nttl)
       ipos=1
       CALL itoc(rdbdat,outstr,ipos)
       IF(Lfatal)RETURN
c     ------------------------------------------------------------------
       DO ielt=begelt,endelt
        outstr(ipos:ipos)=TABCHR
        ipos=ipos+1
        CALL dtoc(Xy(ielt),outstr,ipos)
        IF(Lfatal)RETURN
       END DO
c     ------------------------------------------------------------------
       WRITE(fh,1010)outstr(1:ipos-1)
      END DO
c-----------------------------------------------------------------------
c     Print the acf and pacf where for the acf the columns are the lag,
c acf, standard error, Ljung-Box statistic, (and p-value).
c-----------------------------------------------------------------------
      IF(locok)CALL fclose(fh)
      RETURN
      END
