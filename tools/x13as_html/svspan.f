C     Last change:  BCM  15 Jan 98   12:22 pm
      SUBROUTINE svspan(X,Nopt,Dmax,Ltbl,Ncol,Lopgrf)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Saves the complete sliding spans table into a separate file,
c     with date, estimate, and maximum percentage difference
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'ssap.prm'
      INCLUDE 'error.cmn'
      INCLUDE 'ssap.cmn'
      INCLUDE 'cchars.i'
c-----------------------------------------------------------------------
      INTEGER MO,YR
      PARAMETER(MO=2,YR=1)
c-----------------------------------------------------------------------
      CHARACTER num*1
      CHARACTER*130 outstr
      LOGICAL locok,Lopgrf
      DOUBLE PRECISION Dmax,X
      INTEGER idate,fh,Ltbl,i,ipos,l,l0,rdbdat,Nopt,ssdate,Ncol
      DIMENSION idate(2),ssdate(2),X(MXLEN,MXCOL),Dmax(MXLEN,NEST),
     &          num(4)
c-----------------------------------------------------------------------
      DATA num/'1','2','3','4'/
c-----------------------------------------------------------------------
c     Open the file; if the file cannot be opened, stop
c-----------------------------------------------------------------------
      CALL opnfil(.true.,Lopgrf,Ltbl,fh,locok)
      IF(.not.locok)THEN
       CALL abend
       RETURN
      END IF
      ssdate(YR)=Iyr
      ssdate(MO)=Im
c-----------------------------------------------------------------------
c     Write file header
c-----------------------------------------------------------------------
      WRITE(fh,1010)'date',(TABCHR,'Span'//num(i),i=1,Ncol),TABCHR,
     &              'Max_%_DIFF'
      WRITE(fh,1010)'------',(TABCHR,'-----------------------',i=1,Ncol)
     &              ,TABCHR,'-----------------------'
c-----------------------------------------------------------------------
c   Write out the sliding spans information
c-----------------------------------------------------------------------
      DO l0=Im,Sslen+Im-1
       ipos=1
       CALL addate(ssdate,Nsea,l0-Im,idate)
       rdbdat=100*idate(YR)+idate(MO)
       CALL itoc(rdbdat,outstr,ipos)
       IF(Lfatal)RETURN
       outstr(ipos:ipos)=TABCHR
       ipos=ipos+1
       DO l=1,Ncol
        CALL dtoc(X(l0,l),outstr,ipos)
        IF(Lfatal)RETURN
        outstr(ipos:ipos)=TABCHR
        ipos=ipos+1
       END DO
c-----------------------------------------------------------------------
       CALL dtoc(Dmax(l0,Nopt),outstr,ipos)
       IF(Lfatal)RETURN
       WRITE(fh,1010)outstr(1:ipos-1)
      END DO
c-----------------------------------------------------------------------
c     Close the file.
c-----------------------------------------------------------------------
      IF(locok)CALL fclose(fh)
      RETURN
c-----------------------------------------------------------------------
 1010 FORMAT(a:,a,a,a,a,a,a:,a,a:,a,a)
      END
