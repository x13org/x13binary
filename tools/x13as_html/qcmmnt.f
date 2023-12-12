C     Last change:  BCM  12 Mar 98   12:21 pm
      LOGICAL FUNCTION qcmmnt(Str,Nchr)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     qcmmnt.f, Release 1, Subroutine Version 1.4, Modified 03 Feb 1995.
c-----------------------------------------------------------------------
      LOGICAL T
      PARAMETER(T=.true.)
c     -----------------------------------------------------------------
      INCLUDE 'lex.i'
      INCLUDE 'cchars.i'
c     -----------------------------------------------------------------
      CHARACTER chr*1,dmychr*1,Str*(*),tmpstr*5
      INTEGER Nchr,nichr,pchr
c     -----------------------------------------------------------------
      CHARACTER getchr*1
      EXTERNAL getchr
c     -----------------------------------------------------------------
      IF(getchr(chr).eq.'#')THEN
       qcmmnt=.true.
       pchr=len(Str)
       Nchr=0
       DO WHILE (.true.)
c     -----------------------------------------------------------------
        chr=getchr(dmychr)
        IF(Nchr.ge.pchr)THEN
         Pos(PCHAR)=Pos(PCHAR)-Nchr
         nichr=1
         CALL itoc(pchr+1,tmpstr,nichr)
         CALL inpter(PERROR,Pos,'COMMENT must be shorter than '//
     &               tmpstr(:(nichr-1))//' characters.',T)
         Pos(PCHAR)=Pos(PCHAR)+Nchr
c     -----------------------------------------------------------------
        ELSE IF(chr.eq.CHREOF)THEN
         CALL putbak(chr)
c     -----------------------------------------------------------------
        ELSE IF(chr.ne.NEWLIN)THEN
         Nchr=Nchr+1
         Str(Nchr:Nchr)=chr
         GO TO 10
        END IF
        GO TO 20
   10   CONTINUE
       END DO
c     -----------------------------------------------------------------
      ELSE
       qcmmnt=.false.
       CALL putbak(chr)
      END IF
c     -----------------------------------------------------------------
   20 RETURN
      END
