C     Last change:  BCM  30 Sep 1998    9:09 am
**==tfmts.f    processed by SPAG 4.03F  at 09:54 on  1 Mar 1994
      SUBROUTINE tfmts(Ny,Outdec,Maxy,Miny,Muladd)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C --- THIS SUBROUTINE GENERATES THE FORMATS FOR SUBROUTINE TABLES.
c     as well as column headers.
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'tfmts.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
c     Include data dictionary of table formats
c-----------------------------------------------------------------------
c      INCLUDE 'tfmts.prm'
c     ------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c-----------------------------------------------------------------------
      DOUBLE PRECISION Miny,Maxy
      INTEGER Ny,Outdec,Muladd,obswid,fac,ipos,ipos2,ifmt,npos,itmp
      CHARACTER blnk*22,cmonth*3,cqtr*3,fbase*110,fobs*5,fsum*5,stmp*3
      DIMENSION cmonth(12),cqtr(4)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
      DATA cmonth/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep',
     &     'Oct','Nov','Dec'/
      DATA cqtr/'1st','2nd','3rd','4th'/
c-----------------------------------------------------------------------
c      INCLUDE 'tfmts.var'
c-----------------------------------------------------------------------
c --- initialize format variables to blanks.
c-----------------------------------------------------------------------
      CALL setchr(' ',7,Ifmt1)
      CALL setchr(' ',7,Ifmt2)
c-----------------------------------------------------------------------
c --- set formats for TABLE subroutine.  First, determine how wide the
c --- data should be
c-----------------------------------------------------------------------
      IF(Muladd.eq.1.and.abs(Miny).gt.Maxy)THEN
       obswid=int(log10(abs(Miny)))+Outdec+3
      ELSE
       IF(dpeq(Maxy,0D0))THEN
        obswid=Outdec+3
       ELSE
        obswid=int(log10(Maxy))+Outdec+3
       END IF
      END IF
      IF(Muladd.eq.0.or.Muladd.eq.2)THEN
       fac=Outdec+4
       IF(Outdec.eq.0)fac=fac+2
       IF(fac.gt.obswid)obswid=fac
      END IF
c-----------------------------------------------------------------------
      Tblwid=obswid+2
      IF(obswid.gt.20)THEN
       CALL wWritln('Data is very large for '//PRGNAM//
     &              ' print format.',STDERR,Mt2,T,F)
       CALL writln(
     &   '         Try dividing the series by power of 10, or use the',
     &             STDERR,Mt2,F,F)
       CALL writln('         divpower argument found in the series '//
     &             'and composite specs.',STDERR,Mt2,F,T)
*       Readok=F
*       RETURN
c       Tblcol=3
c       Tblwid=15
      END IF
c-----------------------------------------------------------------------
c     set up observation formats
c-----------------------------------------------------------------------
      IF(Tblwid.gt.9)then
       write(Ifmt1,1010)Tblwid,Outdec
       write(Ifmt2,1010)Tblwid+2,Outdec
 1010  format('(f',i2,'.',i1,')')
       Nfmt1=7
       Nfmt2=7
      ELSE
       write(Ifmt1,1020)Tblwid,Outdec
       Nfmt1=6
       IF(Tblwid.gt.7)THEN
        write(Ifmt2,1010)Tblwid+2,Outdec
        Nfmt2=7
       ELSE
        write(Ifmt2,1020)Tblwid+2,Outdec
        Nfmt2=6
       END IF
 1020  format('(f',i1,'.',i1,')')
      END IF
c-----------------------------------------------------------------------
c     Generate format for summary statistics at the end of the printout
c-----------------------------------------------------------------------
      CALL tfmts3(Outdec,Muladd,Tblwid,Ifmt3)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Generate vector for column headers.
c-----------------------------------------------------------------------
c     Set hdr array to be the name of the month or quarter
c-----------------------------------------------------------------------
      CALL setchr(' ',22,blnk)
      Colhdr(1)=blnk
      DO ipos=1,Ny
       ipos2=ipos+1
       Colhdr(ipos2)=blnk
       IF(Ny.eq.12)THEN
        Colhdr(ipos2)((Tblwid-3):(Tblwid-1))=cmonth(ipos)
       ELSE IF(Ny.eq.4)THEN
        Colhdr(ipos2)((Tblwid-3):(Tblwid-1))=cqtr(ipos)
       ELSE
        itmp=1
        CALL itoc(ipos,stmp,itmp)
        Colhdr(ipos2)((Tblwid-itmp):(Tblwid-1))=stmp(1:(itmp-1))
       END IF
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
