C     Last change:  BCM  12 Mar 98   12:23 pm
      LOGICAL FUNCTION qquote(Astrng,Nchr)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     qquote.f, Release 1, Subroutine Version 1.4, Modified 03 Feb 1995.
c-----------------------------------------------------------------------
      INCLUDE 'lex.i'
      INCLUDE 'cchars.i'
c     -----------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      CHARACTER Astrng*(*),dmychr*1,chr*1,qchr*1,str*5
      INTEGER Nchr,nichr,pchr
c     ------------------------------------------------------------------
      CHARACTER getchr*1
      EXTERNAL getchr
c     -----------------------------------------------------------------
      pchr=len(Astrng)
      Nchr=0
c     -----------------------------------------------------------------
      chr=getchr(dmychr)
      IF(chr.ne.'"'.and.chr.ne.'''')THEN
       qquote=F
       CALL putbak(chr)
c     -----------------------------------------------------------------
      ELSE
       qquote=T
       Nchr=0
       qchr=chr
       DO WHILE (T)
c     -----------------------------------------------------------------
        chr=getchr(dmychr)
        IF(chr.eq.NEWLIN)THEN
         Pos(PCHAR)=Pos(PCHAR)-Nchr-2
         CALL inpter(PERROR,Pos,
     &               'Quote can''t wrap to next line--end-of-line '//
     &               'assumed to be end quote',T)
         Pos(PCHAR)=Pos(PCHAR)+Nchr+2
         Lexok=F
c     -----------------------------------------------------------------
        ELSE IF(Nchr.ge.pchr)THEN
         Pos(PCHAR)=Pos(PCHAR)-Nchr
         nichr=1
         CALL itoc(pchr+1,str,nichr)
         CALL inpter(PERROR,Pos,'QUOTE must be shorter than '//
     &               str(:(nichr-1))//' characters.',T)
         Pos(PCHAR)=Pos(PCHAR)+Nchr
         Lexok=F
c     -----------------------------------------------------------------
        ELSE IF(chr.ne.qchr)THEN
         Nchr=Nchr+1
         Astrng(Nchr:Nchr)=chr
         GO TO 10
        END IF
        GO TO 20
   10   CONTINUE
       END DO
      END IF
c     -----------------------------------------------------------------
   20 IF(qquote.and.Nchr.eq.0)THEN
       Pos(PCHAR)=Pos(PCHAR)-1
       CALL inpter(PERROR,Pos,
     &             'Quotes must contain at least one character.',T)
       Pos(PCHAR)=Pos(PCHAR)+1
       Lexok=F
      END IF
c     -----------------------------------------------------------------
      RETURN
      END
