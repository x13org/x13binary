      SUBROUTINE genfoot(Fh,Ifoot,Icode)
c-----------------------------------------------------------------------
c     genfoot.f, Release 1, Subroutine Version 1.1, Modified 19 Jun 2011
c-----------------------------------------------------------------------
      IMPLICIT NONE
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'ssap.prm'
      INCLUDE 'ssap.cmn'
      INCLUDE 'htmlout.prm'
c     ------------------------------------------------------------------
      INTEGER Fh,Ifoot,Icode
c     ------------------------------------------------------------------
      IF(Icode.eq.PSTAR1F.or.Icode.eq.PLGLNK)THEN
       WRITE(Fh,1010)Ifoot,'*'
      ELSE IF (Icode.eq.PSTAR2F)THEN
       WRITE(Fh,1010)Ifoot,'**'
      ELSE IF (Icode.eq.PAMP1F)THEN
       WRITE(Fh,1010)Ifoot,'&amp;'
      ELSE IF (Icode.eq.PAMP2F)THEN
       WRITE(Fh,1010)Ifoot,'&amp;&amp;'
      ELSE IF (Icode.eq.PATSG1F)THEN
       WRITE(Fh,1010)Ifoot,'@'
      ELSE IF (Icode.eq.PATSG2F)THEN
       WRITE(Fh,1010)Ifoot,'@@'
      ELSE IF (Icode.eq.PNOTINC)THEN
       WRITE(Fh,1010)Ifoot,'NT'
      ELSE IF (Icode.eq.PSIGNCH)THEN
       WRITE(Fh,1010)Ifoot,'SC'
      ELSE IF (Icode.eq.PINCON)THEN
       WRITE(Fh,1010)Ifoot,'IE'
      ELSE IF (Icode.eq.PTURNP)THEN
       WRITE(Fh,1010)Ifoot,'TP'
      ELSE IF (Icode.eq.PSSPCT1)THEN
       WRITE(Fh,1010)Ifoot,'1%'
      ELSE IF (Icode.eq.PSSPCT2)THEN
       WRITE(Fh,1010)Ifoot,'2%'
      ELSE IF (Icode.eq.PSSPCT3)THEN
       WRITE(Fh,1010)Ifoot,'3%'
      ELSE IF (Icode.eq.PSSPCT4)THEN
       WRITE(Fh,1010)Ifoot,'4%'
      ELSE IF (Icode.eq.PSSPLS1)THEN
       WRITE(Fh,1010)Ifoot,'1%'
      ELSE IF (Icode.eq.PSSPLS2)THEN
       WRITE(Fh,1010)Ifoot,'2%'
      ELSE IF (Icode.eq.PSSPLS3)THEN
       WRITE(Fh,1010)Ifoot,'3%'
      ELSE IF (Icode.eq.PSSPLS4)THEN
       WRITE(Fh,1010)Ifoot,'4%'
      ELSE IF (Icode.eq.PSSHSH1)THEN
       WRITE(Fh,1010)Ifoot,'1#'
      ELSE IF (Icode.eq.PSSHSH2)THEN
       WRITE(Fh,1010)Ifoot,'2#'
      ELSE IF (Icode.eq.PSSHSH3)THEN
       WRITE(Fh,1010)Ifoot,'3#'
      ELSE IF (Icode.eq.PSSHSH4)THEN
       WRITE(Fh,1010)Ifoot,'4#'
      ELSE IF (Icode.eq.PSSDLR1)THEN
       WRITE(Fh,1010)Ifoot,'1$'
      ELSE IF (Icode.eq.PSSDLR2)THEN
       WRITE(Fh,1010)Ifoot,'2$'
      ELSE IF (Icode.eq.PSSDLR3)THEN
       WRITE(Fh,1010)Ifoot,'3$'
      ELSE IF (Icode.eq.PSSDLR4)THEN
       WRITE(Fh,1010)Ifoot,'4$'
      ELSE IF (Icode.eq.PSSAT1)THEN
       WRITE(Fh,1010)Ifoot,'1@'
      ELSE IF (Icode.eq.PSSAT2)THEN
       WRITE(Fh,1010)Ifoot,'2@'
      ELSE IF (Icode.eq.PSSAT3)THEN
       WRITE(Fh,1010)Ifoot,'3@'
      ELSE IF (Icode.eq.PSSAT4)THEN
       WRITE(Fh,1010)Ifoot,'4@'
      ELSE IF (Icode.eq.PSIGNCH+PSSPCT1)THEN
       WRITE(Fh,1010)Ifoot,'SC, 1%'
      ELSE IF (Icode.eq.PSIGNCH+PSSPCT2)THEN
       WRITE(Fh,1010)Ifoot,'SC, 2%'
      ELSE IF (Icode.eq.PSIGNCH+PSSPCT3)THEN
       WRITE(Fh,1010)Ifoot,'SC, 3%'
      ELSE IF (Icode.eq.PSIGNCH+PSSPCT4)THEN
       WRITE(Fh,1010)Ifoot,'SC, 4%'
      ELSE IF (Icode.eq.PSIGNCH+PSSPLS1)THEN
       WRITE(Fh,1010)Ifoot,'SC, 1+'
      ELSE IF (Icode.eq.PSIGNCH+PSSPLS2)THEN
       WRITE(Fh,1010)Ifoot,'SC, 2+'
      ELSE IF (Icode.eq.PSIGNCH+PSSPLS3)THEN
       WRITE(Fh,1010)Ifoot,'SC, 3+'
      ELSE IF (Icode.eq.PSIGNCH+PSSPLS4)THEN
       WRITE(Fh,1010)Ifoot,'SC, 4+'
      ELSE IF (Icode.eq.PSIGNCH+PSSHSH1)THEN
       WRITE(Fh,1010)Ifoot,'SC, 1#'
      ELSE IF (Icode.eq.PSIGNCH+PSSHSH2)THEN
       WRITE(Fh,1010)Ifoot,'SC, 2#'
      ELSE IF (Icode.eq.PSIGNCH+PSSHSH3)THEN
       WRITE(Fh,1010)Ifoot,'SC, 3#'
      ELSE IF (Icode.eq.PSIGNCH+PSSHSH4)THEN
       WRITE(Fh,1010)Ifoot,'SC, 4#'
      ELSE IF (Icode.eq.PSIGNCH+PSSDLR1)THEN
       WRITE(Fh,1010)Ifoot,'SC, 1$'
      ELSE IF (Icode.eq.PSIGNCH+PSSDLR2)THEN
       WRITE(Fh,1010)Ifoot,'SC, 2$'
      ELSE IF (Icode.eq.PSIGNCH+PSSDLR3)THEN
       WRITE(Fh,1010)Ifoot,'SC, 3$'
      ELSE IF (Icode.eq.PSIGNCH+PSSDLR4)THEN
       WRITE(Fh,1010)Ifoot,'SC, 4$'
      ELSE IF (Icode.eq.PSIGNCH+PSSAT1)THEN
       WRITE(Fh,1010)Ifoot,'SC, 1@'
      ELSE IF (Icode.eq.PSIGNCH+PSSAT2)THEN
       WRITE(Fh,1010)Ifoot,'SC, 2@'
      ELSE IF (Icode.eq.PSIGNCH+PSSAT3)THEN
       WRITE(Fh,1010)Ifoot,'SC, 3@'
      ELSE IF (Icode.eq.PSIGNCH+PSSAT4)THEN
       WRITE(Fh,1010)Ifoot,'SC, 4@'
      ELSE IF (Icode.eq.PINCON+PSSPCT1)THEN
       WRITE(Fh,1010)Ifoot,'IE, 1%'
      ELSE IF (Icode.eq.PINCON+PSSPCT2)THEN
       WRITE(Fh,1010)Ifoot,'IE, 2%'
      ELSE IF (Icode.eq.PINCON+PSSPCT3)THEN
       WRITE(Fh,1010)Ifoot,'IE, 3%'
      ELSE IF (Icode.eq.PINCON+PSSPCT4)THEN
       WRITE(Fh,1010)Ifoot,'IE, 4%'
      ELSE IF (Icode.eq.PINCON+PSSPLS1)THEN
       WRITE(Fh,1010)Ifoot,'IE, 1+'
      ELSE IF (Icode.eq.PINCON+PSSPLS2)THEN
       WRITE(Fh,1010)Ifoot,'IE, 2+'
      ELSE IF (Icode.eq.PINCON+PSSPLS3)THEN
       WRITE(Fh,1010)Ifoot,'IE, 3+'
      ELSE IF (Icode.eq.PINCON+PSSPLS4)THEN
       WRITE(Fh,1010)Ifoot,'IE, 4+'
      ELSE IF (Icode.eq.PINCON+PSSHSH1)THEN
       WRITE(Fh,1010)Ifoot,'IE, 1#'
      ELSE IF (Icode.eq.PINCON+PSSHSH2)THEN
       WRITE(Fh,1010)Ifoot,'IE, 2#'
      ELSE IF (Icode.eq.PINCON+PSSHSH3)THEN
       WRITE(Fh,1010)Ifoot,'IE, 3#'
      ELSE IF (Icode.eq.PINCON+PSSHSH4)THEN
       WRITE(Fh,1010)Ifoot,'IE, 4#'
      ELSE IF (Icode.eq.PINCON+PSSDLR1)THEN
       WRITE(Fh,1010)Ifoot,'IE, 1$'
      ELSE IF (Icode.eq.PINCON+PSSDLR2)THEN
       WRITE(Fh,1010)Ifoot,'IE, 2$'
      ELSE IF (Icode.eq.PINCON+PSSDLR3)THEN
       WRITE(Fh,1010)Ifoot,'IE, 3$'
      ELSE IF (Icode.eq.PINCON+PSSDLR4)THEN
       WRITE(Fh,1010)Ifoot,'IE, 4$'
      ELSE IF (Icode.eq.PINCON+PSSAT1)THEN
       WRITE(Fh,1010)Ifoot,'IE, 1@'
      ELSE IF (Icode.eq.PINCON+PSSAT2)THEN
       WRITE(Fh,1010)Ifoot,'IE, 2@'
      ELSE IF (Icode.eq.PINCON+PSSAT3)THEN
       WRITE(Fh,1010)Ifoot,'IE, 3@'
      ELSE IF (Icode.eq.PINCON+PSSAT4)THEN
       WRITE(Fh,1010)Ifoot,'IE, 4@'
      ELSE IF (Icode.eq.PTURNP+PSSPCT1)THEN
       WRITE(Fh,1010)Ifoot,'TP, 1%'
      ELSE IF (Icode.eq.PTURNP+PSSPCT2)THEN
       WRITE(Fh,1010)Ifoot,'TP, 2%'
      ELSE IF (Icode.eq.PTURNP+PSSPCT3)THEN
       WRITE(Fh,1010)Ifoot,'TP, 3%'
      ELSE IF (Icode.eq.PTURNP+PSSPCT4)THEN
       WRITE(Fh,1010)Ifoot,'TP, 4%'
      ELSE IF (Icode.eq.PTURNP+PSSPLS1)THEN
       WRITE(Fh,1010)Ifoot,'TP, 1+'
      ELSE IF (Icode.eq.PTURNP+PSSPLS2)THEN
       WRITE(Fh,1010)Ifoot,'TP, 2+'
      ELSE IF (Icode.eq.PTURNP+PSSPLS3)THEN
       WRITE(Fh,1010)Ifoot,'TP, 3+'
      ELSE IF (Icode.eq.PTURNP+PSSPLS4)THEN
       WRITE(Fh,1010)Ifoot,'TP, 4+'
      ELSE IF (Icode.eq.PTURNP+PSSHSH1)THEN
       WRITE(Fh,1010)Ifoot,'TP, 1#'
      ELSE IF (Icode.eq.PTURNP+PSSHSH2)THEN
       WRITE(Fh,1010)Ifoot,'TP, 2#'
      ELSE IF (Icode.eq.PTURNP+PSSHSH3)THEN
       WRITE(Fh,1010)Ifoot,'TP, 3#'
      ELSE IF (Icode.eq.PTURNP+PSSHSH4)THEN
       WRITE(Fh,1010)Ifoot,'TP, 4#'
      ELSE IF (Icode.eq.PTURNP+PSSDLR1)THEN
       WRITE(Fh,1010)Ifoot,'TP, 1$'
      ELSE IF (Icode.eq.PTURNP+PSSDLR2)THEN
       WRITE(Fh,1010)Ifoot,'TP, 2$'
      ELSE IF (Icode.eq.PTURNP+PSSDLR3)THEN
       WRITE(Fh,1010)Ifoot,'TP, 3$'
      ELSE IF (Icode.eq.PTURNP+PSSDLR4)THEN
       WRITE(Fh,1010)Ifoot,'TP, 4$'
      ELSE IF (Icode.eq.PTURNP+PSSAT1)THEN
       WRITE(Fh,1010)Ifoot,'TP, 1@'
      ELSE IF (Icode.eq.PTURNP+PSSAT2)THEN
       WRITE(Fh,1010)Ifoot,'TP, 2@'
      ELSE IF (Icode.eq.PTURNP+PSSAT3)THEN
       WRITE(Fh,1010)Ifoot,'TP, 3@'
      ELSE IF (Icode.eq.PTURNP+PSSAT4)THEN
       WRITE(Fh,1010)Ifoot,'TP, 4@'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP)THEN
       WRITE(Fh,1010)Ifoot,'SC, TP'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPCT1)THEN
       WRITE(Fh,1010)Ifoot,'SC, TP, 1%'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPCT2)THEN
       WRITE(Fh,1010)Ifoot,'SC, TP, 2%'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPCT3)THEN
       WRITE(Fh,1010)Ifoot,'SC, TP, 3%'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPCT4)THEN
       WRITE(Fh,1010)Ifoot,'SC, TP, 4%'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPLS1)THEN
       WRITE(Fh,1010)Ifoot,'SC, TP, 1+'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPLS2)THEN
       WRITE(Fh,1010)Ifoot,'SC, TP, 2+'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPLS3)THEN
       WRITE(Fh,1010)Ifoot,'SC, TP, 3+'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPLS4)THEN
       WRITE(Fh,1010)Ifoot,'SC, TP, 4+'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSHSH1)THEN
       WRITE(Fh,1010)Ifoot,'SC, TP, 1#'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSHSH2)THEN
       WRITE(Fh,1010)Ifoot,'SC, TP, 2#'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSHSH3)THEN
       WRITE(Fh,1010)Ifoot,'SC, TP, 3#'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSHSH4)THEN
       WRITE(Fh,1010)Ifoot,'SC, TP, 4#'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSDLR1)THEN
       WRITE(Fh,1010)Ifoot,'SC, TP, 1$'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSDLR2)THEN
       WRITE(Fh,1010)Ifoot,'SC, TP, 2$'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSDLR3)THEN
       WRITE(Fh,1010)Ifoot,'SC, TP, 3$'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSDLR4)THEN
       WRITE(Fh,1010)Ifoot,'SC, TP, 4$'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSAT1)THEN
       WRITE(Fh,1010)Ifoot,'SC, TP, 1@'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSAT2)THEN
       WRITE(Fh,1010)Ifoot,'SC, TP, 2@'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSAT3)THEN
       WRITE(Fh,1010)Ifoot,'SC, TP, 3@'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSAT4)THEN
       WRITE(Fh,1010)Ifoot,'SC, TP, 4@'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP)THEN
       WRITE(Fh,1010)Ifoot,'IE, TP'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPCT1)THEN
       WRITE(Fh,1010)Ifoot,'IE, TP, 1%'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPCT2)THEN
       WRITE(Fh,1010)Ifoot,'IE, TP, 2%'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPCT3)THEN
       WRITE(Fh,1010)Ifoot,'IE, TP, 3%'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPCT4)THEN
       WRITE(Fh,1010)Ifoot,'IE, TP, 4%'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPLS1)THEN
       WRITE(Fh,1010)Ifoot,'IE, TP, 1+'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPLS2)THEN
       WRITE(Fh,1010)Ifoot,'IE, TP, 2+'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPLS3)THEN
       WRITE(Fh,1010)Ifoot,'IE, TP, 3+'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPLS4)THEN
       WRITE(Fh,1010)Ifoot,'IE, TP, 4+'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSHSH1)THEN
       WRITE(Fh,1010)Ifoot,'IE, TP, 1#'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSHSH2)THEN
       WRITE(Fh,1010)Ifoot,'IE, TP, 2#'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSHSH3)THEN
       WRITE(Fh,1010)Ifoot,'IE, TP, 3#'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSHSH4)THEN
       WRITE(Fh,1010)Ifoot,'IE, TP, 4#'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSDLR1)THEN
       WRITE(Fh,1010)Ifoot,'IE, TP, 1$'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSDLR2)THEN
       WRITE(Fh,1010)Ifoot,'IE, TP, 2$'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSDLR3)THEN
       WRITE(Fh,1010)Ifoot,'IE, TP, 3$'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSDLR4)THEN
       WRITE(Fh,1010)Ifoot,'IE, TP, 4$'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSAT1)THEN
       WRITE(Fh,1010)Ifoot,'IE, TP, 1@'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSAT2)THEN
       WRITE(Fh,1010)Ifoot,'IE, TP, 2@'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSAT3)THEN
       WRITE(Fh,1010)Ifoot,'IE, TP, 3@'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSAT4)THEN
       WRITE(Fh,1010)Ifoot,'IE, TP, 4@'
      ELSE IF (Icode.ge.PMINTR1.and.Icode.le.PMINTR6)THEN
       WRITE(Fh,1010)Ifoot,'-'
      ELSE IF (Icode.eq.PLSHD8) THEN
       WRITE(Fh,1010)Ifoot,'+'
      ELSE IF (Icode.eq.PSTRD8) THEN
       WRITE(Fh,1010)Ifoot,'*'
      ELSE IF (Icode.eq.PHSHD8) THEN
       WRITE(Fh,1010)Ifoot,'#'
      ELSE IF (Icode.eq.PATSD8) THEN
       WRITE(Fh,1010)Ifoot,'@'
      ELSE IF (Icode.eq.PAMPD8) THEN
       WRITE(Fh,1010)Ifoot,'&amp;'
      ELSE IF (Icode.eq.PSTRD8+PLSHD8) THEN
       WRITE(Fh,1010)Ifoot,'*+'
      ELSE IF (Icode.eq.PHSHD8+PLSHD8) THEN
       WRITE(Fh,1010)Ifoot,'#+'
      ELSE IF (Icode.eq.PATSD8+PLSHD8) THEN
       WRITE(Fh,1010)Ifoot,'@+'
      ELSE IF (Icode.eq.PAMPD8+PLSHD8) THEN
       WRITE(Fh,1010)Ifoot,'&amp;+'
      ELSE IF (Icode.eq.PSTR1TP) THEN
       WRITE(Fh,1010)Ifoot,'*'
      ELSE IF (Icode.eq.PSTR2TP) THEN
       WRITE(Fh,1010)Ifoot,'**'
      END IF
c     ------------------------------------------------------------------
      CALL writTag(Fh,'<dd>')
      IF(Icode.eq.PSTAR1F)THEN
       WRITE(Fh,1020)
       WRITE(Fh,1030)Ifoot,'Regression Table'
      ELSE IF(Icode.eq.PSTAR2F)THEN
       WRITE(Fh,1040)
       WRITE(Fh,1030)Ifoot,'Regression Table'
      ELSE IF(Icode.eq.PAMP1F)THEN
       WRITE(Fh,1050)
       WRITE(Fh,1030)Ifoot,'Regression Table'
      ELSE IF(Icode.eq.PAMP2F)THEN
       WRITE(Fh,1060)
       WRITE(Fh,1030)Ifoot,'Regression Table'
      ELSE IF(Icode.eq.PATSG1F)THEN
       WRITE(Fh,1070)
       WRITE(Fh,1030)Ifoot,'Regression Table'
      ELSE IF(Icode.eq.PATSG2F)THEN
       WRITE(Fh,1080)
       WRITE(Fh,1030)Ifoot,'Regression Table'
      ELSE IF (Icode.eq.PNOTINC)THEN
       WRITE(Fh,1090)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PINCON)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PTURNP)THEN
       WRITE(Fh,1120)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSSPCT1)THEN
       WRITE(Fh,1130)Cut(1,1),Cut(1,2)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSSPCT2)THEN
       WRITE(Fh,1130)Cut(1,2),Cut(1,3)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSSPCT3)THEN
       WRITE(Fh,1130)Cut(1,3),Cut(1,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSSPCT4)THEN
       WRITE(Fh,1140)Cut(1,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSSPLS1)THEN
       WRITE(Fh,1130)Cut(2,1),Cut(2,2)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSSPLS2)THEN
       WRITE(Fh,1130)Cut(2,2),Cut(2,3)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSSPLS3)THEN
       WRITE(Fh,1130)Cut(2,3),Cut(2,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSSPLS4)THEN
       WRITE(Fh,1140)Cut(2,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSSHSH1)THEN
       WRITE(Fh,1130)Cut(3,1),Cut(3,2)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSSHSH2)THEN
       WRITE(Fh,1130)Cut(3,2),Cut(3,3)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSSHSH3)THEN
       WRITE(Fh,1130)Cut(3,3),Cut(3,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSSHSH4)THEN
       WRITE(Fh,1140)Cut(3,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSSDLR1)THEN
       WRITE(Fh,1130)Cut(4,1),Cut(4,2)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSSDLR2)THEN
       WRITE(Fh,1130)Cut(4,2),Cut(4,3)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSSDLR3)THEN
       WRITE(Fh,1130)Cut(4,3),Cut(4,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSSDLR4)THEN
       WRITE(Fh,1140)Cut(4,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSSAT1)THEN
       WRITE(Fh,1130)Cut(5,1),Cut(5,2)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSSAT2)THEN
       WRITE(Fh,1130)Cut(5,2),Cut(5,3)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSSAT3)THEN
       WRITE(Fh,1130)Cut(5,3),Cut(5,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSSAT4)THEN
       WRITE(Fh,1140)Cut(5,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PSSPCT1)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1130)Cut(1,1),Cut(1,2)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PSSPCT2)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1130)Cut(1,2),Cut(1,3)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PSSPCT3)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1130)Cut(1,3),Cut(1,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PSSPCT4)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1140)Cut(1,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PSSPLS1)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1130)Cut(2,1),Cut(2,2)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PSSPLS2)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1130)Cut(2,2),Cut(2,3)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PSSPLS3)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1130)Cut(2,3),Cut(2,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PSSPLS4)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1140)Cut(2,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PSSHSH1)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1130)Cut(3,1),Cut(3,2)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PSSHSH2)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1130)Cut(3,2),Cut(3,3)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PSSHSH3)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1130)Cut(3,3),Cut(3,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PSSHSH4)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1140)Cut(3,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PSSDLR1)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1130)Cut(4,1),Cut(4,2)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PSSDLR2)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1130)Cut(4,2),Cut(4,3)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PSSDLR3)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1130)Cut(4,3),Cut(4,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PSSDLR4)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1140)Cut(4,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PSSAT1)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1130)Cut(5,1),Cut(5,2)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PSSAT2)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1130)Cut(5,2),Cut(5,3)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PSSAT3)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1130)Cut(5,3),Cut(5,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PSSAT4)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1140)Cut(5,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PINCON+PSSPCT1)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1130)Cut(1,1),Cut(1,2)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PINCON+PSSPCT2)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1130)Cut(1,2),Cut(1,3)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PINCON+PSSPCT3)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1130)Cut(1,3),Cut(1,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PINCON+PSSPCT4)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1140)Cut(1,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PINCON+PSSPLS1)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1130)Cut(2,1),Cut(2,2)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PINCON+PSSPLS2)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1130)Cut(2,2),Cut(2,3)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PINCON+PSSPLS3)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1130)Cut(2,3),Cut(2,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PINCON+PSSPLS4)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1140)Cut(2,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PINCON+PSSHSH1)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1130)Cut(3,1),Cut(3,2)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PINCON+PSSHSH2)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1130)Cut(3,2),Cut(3,3)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PINCON+PSSHSH3)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1130)Cut(3,3),Cut(3,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PINCON+PSSHSH4)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1140)Cut(3,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PINCON+PSSDLR1)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1130)Cut(4,1),Cut(4,2)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PINCON+PSSDLR2)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1130)Cut(4,2),Cut(4,3)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PINCON+PSSDLR3)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1130)Cut(4,3),Cut(4,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PINCON+PSSDLR4)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1140)Cut(4,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PINCON+PSSAT1)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1130)Cut(5,1),Cut(5,2)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PINCON+PSSAT2)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1130)Cut(5,2),Cut(5,3)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PINCON+PSSAT3)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1130)Cut(5,3),Cut(5,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PINCON+PSSAT4)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1140)Cut(5,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PTURNP+PSSPCT1)THEN
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(1,1),Cut(1,2)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PTURNP+PSSPCT2)THEN
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(1,2),Cut(1,3)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PTURNP+PSSPCT3)THEN
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(1,3),Cut(1,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PTURNP+PSSPCT4)THEN
       WRITE(Fh,1120)
       WRITE(Fh,1140)Cut(1,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PTURNP+PSSPLS1)THEN
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(2,1),Cut(2,2)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PTURNP+PSSPLS2)THEN
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(2,2),Cut(2,3)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PTURNP+PSSPLS3)THEN
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(2,3),Cut(2,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PTURNP+PSSPLS4)THEN
       WRITE(Fh,1120)
       WRITE(Fh,1140)Cut(2,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PTURNP+PSSHSH1)THEN
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(3,1),Cut(3,2)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PTURNP+PSSHSH2)THEN
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(3,2),Cut(3,3)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PTURNP+PSSHSH3)THEN
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(3,3),Cut(3,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PTURNP+PSSHSH4)THEN
       WRITE(Fh,1120)
       WRITE(Fh,1140)Cut(3,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PTURNP+PSSDLR1)THEN
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(4,1),Cut(4,2)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PTURNP+PSSDLR2)THEN
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(4,2),Cut(4,3)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PTURNP+PSSDLR3)THEN
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(4,3),Cut(4,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PTURNP+PSSDLR4)THEN
       WRITE(Fh,1120)
       WRITE(Fh,1140)Cut(4,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PTURNP+PSSAT1)THEN
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(5,1),Cut(5,2)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PTURNP+PSSAT2)THEN
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(5,2),Cut(5,3)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PTURNP+PSSAT3)THEN
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(5,3),Cut(5,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PTURNP+PSSAT4)THEN
       WRITE(Fh,1120)
       WRITE(Fh,1140)Cut(5,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1120)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPCT1)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(1,1),Cut(1,2)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPCT2)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(1,2),Cut(1,3)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPCT3)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(1,3),Cut(1,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPCT4)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1120)
       WRITE(Fh,1140)Cut(1,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPLS1)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(2,1),Cut(2,2)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPLS2)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(2,2),Cut(2,3)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPLS3)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(2,3),Cut(2,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPLS4)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1120)
       WRITE(Fh,1140)Cut(2,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSHSH1)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(3,1),Cut(3,2)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSHSH2)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(3,2),Cut(3,3)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSHSH3)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(3,3),Cut(3,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSHSH4)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1120)
       WRITE(Fh,1140)Cut(3,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSDLR1)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(4,1),Cut(4,2)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSDLR2)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(4,2),Cut(4,3)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSDLR3)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(4,3),Cut(4,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSDLR4)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1120)
       WRITE(Fh,1140)Cut(4,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSAT1)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(5,1),Cut(5,2)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSAT2)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(5,2),Cut(5,3)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSAT3)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(5,3),Cut(5,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSAT4)THEN
       WRITE(Fh,1100)
       WRITE(Fh,1120)
       WRITE(Fh,1140)Cut(5,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1120)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPCT1)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(1,1),Cut(1,2)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPCT2)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(1,2),Cut(1,3)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPCT3)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(1,3),Cut(1,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPCT4)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1120)
       WRITE(Fh,1140)Cut(1,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPLS1)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(2,1),Cut(2,2)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPLS2)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(2,2),Cut(2,3)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPLS3)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(2,3),Cut(2,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSPLS4)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1120)
       WRITE(Fh,1140)Cut(2,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSHSH1)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(3,1),Cut(3,2)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSHSH2)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(3,2),Cut(3,3)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSHSH3)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(3,3),Cut(3,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSHSH4)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1120)
       WRITE(Fh,1140)Cut(3,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSDLR1)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(4,1),Cut(4,2)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSDLR2)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(4,2),Cut(4,3)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSDLR3)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(4,3),Cut(4,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSDLR4)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1120)
       WRITE(Fh,1140)Cut(4,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSAT1)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(5,1),Cut(5,2)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSAT2)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(5,2),Cut(5,3)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSAT3)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1120)
       WRITE(Fh,1130)Cut(5,3),Cut(5,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
      ELSE IF (Icode.eq.PSIGNCH+PTURNP+PSSAT4)THEN
       WRITE(Fh,1110)
       WRITE(Fh,1120)
       WRITE(Fh,1140)Cut(5,4)
       WRITE(Fh,1030)Ifoot,'Sliding Spans Table'
c     ------------------------------------------------------------------
      ELSE IF (Icode.ge.PMINTR1.and.Icode.le.PMINTR6)THEN
       WRITE(Fh,1150)
       IF(Icode.eq.PMINTR1)WRITE(Fh,1030)Ifoot,'Table B 7'
       IF(Icode.eq.PMINTR2)WRITE(Fh,1030)Ifoot,'Table B 12'
       IF(Icode.eq.PMINTR3)WRITE(Fh,1030)Ifoot,'Table C 7'
       IF(Icode.eq.PMINTR4)WRITE(Fh,1030)Ifoot,'Table C 12'
       IF(Icode.eq.PMINTR5)WRITE(Fh,1030)Ifoot,'Table D 7'
       IF(Icode.eq.PMINTR6)WRITE(Fh,1030)Ifoot,'Table D 12'
c     ------------------------------------------------------------------
      ELSE IF (Icode.eq.PLSHD8) THEN
       WRITE(Fh,1160)
       WRITE(Fh,1030)Ifoot,'Table D 8.B'
      ELSE IF (Icode.eq.PSTRD8) THEN
       WRITE(Fh,1170)
       WRITE(Fh,1030)Ifoot,'Table D 8.B'
      ELSE IF (Icode.eq.PHSHD8) THEN
       WRITE(Fh,1180)
       WRITE(Fh,1030)Ifoot,'Table D 8.B'
      ELSE IF (Icode.eq.PATSD8) THEN
       WRITE(Fh,1190)
       WRITE(Fh,1030)Ifoot,'Table D 8.B'
      ELSE IF (Icode.eq.PAMPD8) THEN
       WRITE(Fh,1200)
       WRITE(Fh,1030)Ifoot,'Table D 8.B'
      ELSE IF (Icode.eq.PSTRD8+PLSHD8) THEN
       WRITE(Fh,1170)
       WRITE(Fh,1160)
       WRITE(Fh,1030)Ifoot,'Table D 8.B'
      ELSE IF (Icode.eq.PHSHD8+PLSHD8) THEN
       WRITE(Fh,1180)
       WRITE(Fh,1160)
       WRITE(Fh,1030)Ifoot,'Table D 8.B'
      ELSE IF (Icode.eq.PATSD8+PLSHD8) THEN
       WRITE(Fh,1190)
       WRITE(Fh,1160)
       WRITE(Fh,1030)Ifoot,'Table D 8.B'
      ELSE IF (Icode.eq.PAMPD8+PLSHD8) THEN
       WRITE(Fh,1200)
       WRITE(Fh,1160)
       WRITE(Fh,1030)Ifoot,'Table D 8.B'
      ELSE IF (Icode.eq.PLGLNK) THEN
       WRITE(Fh,1210)
       WRITE(Fh,1030)Ifoot,'Link to External Files'
      ELSE IF (Icode.eq.PSTR1TP) THEN
       WRITE(Fh,1220)
       WRITE(Fh,1030)Ifoot,'Tukey Spectral Peaks Table'
      ELSE IF (Icode.eq.PSTR2TP) THEN
       WRITE(Fh,1230)
       WRITE(Fh,1030)Ifoot,'Tukey Spectral Peaks Table'
      END IF
      CALL writTag(Fh,'</dd>')
c     ------------------------------------------------------------------
 1010 FORMAT('<dt id="footnote',i4.4,'">Definition of ',a,'</dt>')
 1020 FORMAT('<p>For full trading-day and stable seasonal effects, ',
     &       'the derived',/,
     &       'parameter estimate is obtained indirectly as minus ',
     &       'the sum',/,
     &       'of the directly estimated parameters that define the ',
     &       'effect.</p>')
 1030 FORMAT('<p><a href="#foot',i4.4,'">Back to ',a,'</a></p>')
 1040 FORMAT('<p>For the one coefficient trading-day effect, the ',
     &       'derived',/,
     &       ' parameter estimate is obtained indirectly as minus ',
     &       '-2.5 times',/,
     &       ' the directly estimated parameter that defines ',
     &       'the effect.</p>')
 1050 FORMAT('<p>The I values estimate the regression coefficients',
     &     /,'   for the span of data before the change date.</p>')
 1060 FORMAT('<p>The I values estimate how much the early regression',
     &     /,'   coefficients differ from those estimated for the span',
     &       ' of data',/,' starting at the change date.</p>')
 1070 FORMAT('<p>The II values estimate the regression coefficients',
     &     /,' for the span of data starting at the change date.</p>')
 1080 FORMAT('<p>The II values estimate how much the early regression',
     &     /,'   coefficients differ from those estimated for the span',
     &       ' of data',/,' before the change date.</p>')
 1090 FORMAT('<p>Observation not included in sliding spans ',
     &       'comparisons.</p>')
 1100 FORMAT('<p>A sign change can be found for this observation.</p>')
 1110 FORMAT('<p>The estimates of this effect are inconsistent',
     &         ' for this observation;',
     &       /,' one span indicates that the effect causes an ',
     &         'increase in the ',
     &       /,' observed value, another that it causes a decrease.',
     &         '</p>')
 1120 FORMAT('<p>Span values for this observation have a turning ',
     &       'point.</p>')
 1130 FORMAT('<p>The maximum percentage difference is greater than ',
     &       'or equal to ',f4.1,'%',/,' but less than ',f4.1,'%.</p>')
 1140 FORMAT('<p>The maximum percentage difference is greater than ',
     &        'or equal to ',f4.1,'%.</p>')
 1150 FORMAT('<p>Trend cycle estimate that had a negative value',
     &       ' replaced.</p>')
 1160 FORMAT('<p>Values around a level shift most likely to be ',
     &       'influenced by it.</p>')
 1170 FORMAT('<p>Extreme value as determined by X-11 extreme value ',
     &       'procedure.</p>')
 1180 FORMAT('<p>regARIMA outlier (either AO, LS, TC, or Ramp).</p>')
 1190 FORMAT('<p>Extreme value and at least one type of regARIMA ',
     &       'outlier.</p>')
 1200 FORMAT('<p>More than one type of regARIMA outlier.</p>')
 1210 FORMAT('<p>Link to Log File specified is only valid if complete ',
     &       'paths were specified',/,
     &       'for the output and meta files.</p>')
 1220 FORMAT('<p>Tukey spectral peak probability greater than 0.99</p>')
 1230 FORMAT('<p>Tukey spectral peak probability greater than 0.90</p>')
cc     ------------------------------------------------------------------
      RETURN
      END
