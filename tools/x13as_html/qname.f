C     Last change:  BCM  14 Oct 97    9:11 am
      LOGICAL FUNCTION qname(Astrng,Nchr)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     qname.f, Release 1, Subroutine Version 1.4, Modified 03 Feb 1995.
c-----------------------------------------------------------------------
      LOGICAL T
      PARAMETER(T=.true.)
c     -----------------------------------------------------------------
      INCLUDE 'lex.i'
c-----------------------------------------------------------------------
      CHARACTER Astrng*(*),dmychr*1,chr*1,str*5
      INTEGER Nchr,nichr,pchr
c-----------------------------------------------------------------------
      CHARACTER getchr*1
      EXTERNAL getchr
c     -----------------------------------------------------------------
      qname=.false.
      pchr=len(Astrng)
      Nchr=0
c     -----------------------------------------------------------------
      chr=getchr(dmychr)
      IF((chr.ge.BIGA.and.chr.le.BIGZ).or.
     &   (chr.ge.LITTLA.and.chr.le.LITTLZ))THEN
       qname=.true.
       Nchr=1
       Astrng(Nchr:Nchr)=chr
       DO WHILE (.true.)
c     -----------------------------------------------------------------
        chr=getchr(dmychr)
        IF((chr.ge.BIGA.and.chr.le.BIGZ).or.
     &     (chr.ge.LITTLA.and.chr.le.LITTLZ).or.
     &     (chr.ge.CZERO.and.chr.le.CNINE).or.chr.eq.'-'.or.
     &     chr.eq.'_'.or.chr.eq.'@'.or.chr.eq.'$'.or.chr.eq.'%'.or.
     &     chr.eq.'.')THEN
         Nchr=Nchr+1
         IF(Nchr.le.pchr)THEN
          Astrng(Nchr:Nchr)=chr
          GO TO 10
         ELSE
          Pos(PCHAR)=Pos(PCHAR)-Nchr
          nichr=1
          CALL itoc(pchr+1,str,nichr)
          CALL inpter(PERROR,Pos,'NAME must be shorter than '//
     &                str(:(nichr-1))//' characters.',T)
          Pos(PCHAR)=Pos(PCHAR)+Nchr
          Lexok=.false.
         END IF
        END IF
        GO TO 20
   10   CONTINUE
       END DO
      END IF
c     -----------------------------------------------------------------
   20 CALL putbak(chr)
      DO WHILE (.true.)
       IF(qname)THEN
        IF(Astrng(Nchr:Nchr).eq.'.')THEN
         CALL putbak(Astrng(Nchr:Nchr))
         Nchr=Nchr-1
         GO TO 30
        END IF
       END IF
c     -----------------------------------------------------------------
       RETURN
   30  CONTINUE
      END DO
      END
