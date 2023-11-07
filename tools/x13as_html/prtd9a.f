C     Last change:  BCM  26 Apr 1998    2:48 pm
      SUBROUTINE prtd9a(Lprt)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Print out the D9A (Moving Seasonality Ratio) table 
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'tfmts.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'x11opt.cmn'
c-----------------------------------------------------------------------
      LOGICAL F
      PARAMETER(F=.false.)
c-----------------------------------------------------------------------
      CHARACTER fobs*(7),fsum*(7)
      DOUBLE PRECISION tmp
      LOGICAL Lprt
      INTEGER l,i,nline,n1,n2,n,ifmt,ipos,n3,npos
      DIMENSION tmp(PSP)
c-----------------------------------------------------------------------
c     Return if printing is turned off for this run.
c-----------------------------------------------------------------------
      IF(Lhiddn)RETURN
c-----------------------------------------------------------------------
c     If summary diagnostics are stored, store Ibar, Sbar and I/S
c-----------------------------------------------------------------------
      IF(Lsumm.gt.0)THEN
       DO i=1,Ny
        WRITE(Nform,1000)i,Rati(i),Rati(i+Ny),Rati(i+2*Ny)
       END DO
 1000  FORMAT('d9a.',i2.2,':',3(1x,E17.10))
       IF(.not.Lprt)RETURN
      END IF
c-----------------------------------------------------------------------
c     Print the complete table (column header, I, S, MSR) for each 
c     of the sets of months used in the main printout.
c-----------------------------------------------------------------------
      n1=1
      n2=Ny
c-----------------------------------------------------------------------
c     Create column headings
c-----------------------------------------------------------------------
      n1=1
      n2=Ny
      IF(Ny.eq.4)THEN
       l=5
      ELSE
       l=13
      END IF
      CALL prtcol(Ny,Mt1,5,'     ',' ',Colhdr)
c-----------------------------------------------------------------------
c     Generate the output format
c-----------------------------------------------------------------------
      if(Tblwid.gt.9)then
       write(fobs,1010)Tblwid
 1010  format('(f',i2,'.3)')
       ifmt=7
      else
       write(fobs,1020)Tblwid
 1020  format('(f',i1,'.3)')
       ifmt=6
      endif
      write(fsum,1010)Tblwid+2
c-----------------------------------------------------------------------
c     Print out variance of irregular component
c-----------------------------------------------------------------------
      n3=n2-n1+1
      DO i=n1,n2
       tmp(i-n1+1)=Rati(i)
      END DO
      CALL wrttbl(tmp,0,'  I  ',n3,3,Mt1,fobs(1:ifmt),0,F)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Print out variance of Seasonal component
c-----------------------------------------------------------------------
      DO i=n1,n2
       tmp(i-n1+1)=Rati(i+Ny)
      END DO
      CALL wrttbl(tmp,0,'  S  ',n3,3,Mt1,fobs(1:ifmt),0,F)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Print out Moving Seasonality Ratio
c-----------------------------------------------------------------------
      DO i=n1,n2
       tmp(i-n1+1)=Rati(i+2*Ny)
      END DO
      CALL wrttbl(tmp,0,'RATIO',n3,3,Mt1,fobs(1:ifmt),0,F)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
      CALL writTag(Mt1,'</table>')
      CALL mkPOneLine(Mt1,'@','&nbsp;')
      RETURN
      END
