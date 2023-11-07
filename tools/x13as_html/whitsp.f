C     Last change:  BCM  12 Mar 98   12:21 pm
**==whitsp.f    processed by SPAG 4.03F  at 09:55 on  1 Mar 1994
      INTEGER FUNCTION whitsp()
      IMPLICIT NONE
c     -----------------------------------------------------------------
      INCLUDE 'lex.i'
      INCLUDE 'cchars.i'
c     -----------------------------------------------------------------
      CHARACTER chr*1,dmychr*1
c     -----------------------------------------------------------------
      CHARACTER getchr*1
      EXTERNAL getchr
c     -----------------------------------------------------------------
      DO WHILE (.true.)
       chr=getchr(dmychr)
c     -----------------------------------------------------------------
       IF(chr.ne.' '.and.chr.ne.TABCHR.and.chr.ne.NEWLIN)THEN
c----------------------------------------------------------------------
c     Pass back any end-of-file
c----------------------------------------------------------------------
        IF(chr.eq.CHREOF)THEN
         whitsp=EOF
c----------------------------------------------------------------------
c     Put back the nonwhitespace character and return
c----------------------------------------------------------------------
        ELSE
         whitsp=NULL
         CALL putbak(chr)
        END IF
c     -----------------------------------------------------------------
        RETURN
       END IF
      END DO
      END
