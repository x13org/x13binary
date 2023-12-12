      SUBROUTINE savd8b(Itbl,Begdat,I1,Na,Sp,Avec,Albl,Label,Nser,
     *                  Lopgrf)
c-----------------------------------------------------------------------
c     Prints out a table in /rdb format, date then value.
c-----------------------------------------------------------------------
      IMPLICIT NONE
c     ------------------------------------------------------------------
      INCLUDE 'cchars.i'
      INCLUDE 'error.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'filext.prm'
c     ------------------------------------------------------------------
      CHARACTER outstr*35,Label*(64),tmplbl*(70),Albl*(2)
      LOGICAL locok,Lopgrf
      INTEGER Begdat,fh,idate,ipos,Itbl,Na,rdbdat,tpnt,I1,Sp,Nser,ns4
      DOUBLE PRECISION Avec
      DIMENSION Avec(*),Albl(*),Begdat(2),idate(2)
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
      WRITE(fh,1010)'date',TABCHR,tmplbl(1:ns4),TABCHR,'Ext&Otl_Label',
     &              TABCHR,'LS_Label'
 1010 FORMAT(a:,a,a,a,a,a,a)
      WRITE(fh,1010)'------',TABCHR,'-----------------------',TABCHR,
     &              '-',TABCHR,
     &              '-'
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
       outstr(ipos:ipos)=TABCHR
       ipos=ipos+1
       outstr(ipos:ipos)=Albl(tpnt)(1:1)
       ipos=ipos+1
       outstr(ipos:ipos)=TABCHR
       ipos=ipos+1
       outstr(ipos:ipos)=Albl(tpnt)(2:2)
       WRITE(fh,1010)outstr(1:ipos)
      END DO
c     ------------------------------------------------------------------
      IF(locok)CALL fclose(fh)
      RETURN
c     ------------------------------------------------------------------
      END
