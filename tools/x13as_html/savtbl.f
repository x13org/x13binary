C     Last change:  BCM  11 Jun 1998    4:05 pm
**==savtbl.f    processed by SPAG 4.03F  at 12:05 on 12 Jul 1994
      SUBROUTINE savtbl(Itbl,Begdat,I1,Na,Sp,Avec,Label,Nser,Lopgrf)
c-----------------------------------------------------------------------
c     Prints out a table in /rdb format, date then value.
c-----------------------------------------------------------------------
c     Itbl - index number of table
c     Begdat - beginning date of series in table -
c              integer array of length 2
c     i1 - pointer for position of first observation in table
c     Na - pointer for position of final observation in table
c     Avec - table data, double precison
c     label - series name, character scalar of length 64
c     Nser - length of the series name
c     Lopgrf - Logical variable denoting whether output will be stored
c              in graphics directory.
c-----------------------------------------------------------------------
      IMPLICIT NONE
c     ------------------------------------------------------------------
      INCLUDE 'cchars.i'
      INCLUDE 'error.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'filext.prm'
c     ------------------------------------------------------------------
      CHARACTER outstr*30,Label*(64),tmplbl*(70)
      LOGICAL locok,Lopgrf
      INTEGER Begdat,fh,idate,ipos,Itbl,Na,rdbdat,tpnt,I1,Sp,Nser,ns4
      DOUBLE PRECISION Avec
      DIMENSION Avec(*),Begdat(2),idate(2)
c-----------------------------------------------------------------------
      INTEGER MO,YR
      PARAMETER(MO=2,YR=1)
c     ------------------------------------------------------------------
      INCLUDE 'filext.var'
c-----------------------------------------------------------------------
c     Open file with the an extension which depends on the type of the
c series.
c-----------------------------------------------------------------------
      CALL opnfil(.true.,Lopgrf,Itbl,fh,locok)
      IF(.not.locok)THEN
       CALL abend
       RETURN
      END IF
c-----------------------------------------------------------------------
      ns4=Nser+4
      tmplbl(1:ns4)=Label(1:Nser)//'.'//tbxdic(Itbl)
c-----------------------------------------------------------------------
c     Print timeseries
c-----------------------------------------------------------------------
      WRITE(fh,1010)'date',TABCHR,tmplbl(1:ns4)
 1010 FORMAT(a:,a,a)
      WRITE(fh,1010)'------',TABCHR,'-----------------------'
c     ------------------------------------------------------------------
      DO tpnt=I1,Na
       CALL addate(Begdat,Sp,tpnt-1,idate)
       IF(Sp.eq.1)THEN
        rdbdat=idate(YR)
c     ------------------------------------------------------------------
       ELSE
        rdbdat=100*idate(YR)+idate(MO)
       END IF
c     ------------------------------------------------------------------
       ipos=1
       CALL itoc(rdbdat,outstr,ipos)
       IF(Lfatal)RETURN
       outstr(ipos:ipos)=TABCHR
       ipos=ipos+1
       CALL dtoc(Avec(tpnt),outstr,ipos)
       IF(Lfatal)RETURN
       WRITE(fh,1010)outstr(1:ipos-1)
      END DO
c     ------------------------------------------------------------------
      IF(locok)CALL fclose(fh)
      RETURN
c     ------------------------------------------------------------------
      END
