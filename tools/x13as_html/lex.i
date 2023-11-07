c-----------------------------------------------------------------------
c     lex.i, Release 1, Parm File Version 1.3, Modified 03 Feb 1995.
c-----------------------------------------------------------------------
c BIGA      c p  Uppercase A
c BIGZ      c p  Uppercase Z
c CNINE   c p  The character 9
c CZERO   c p  The character 0
c dmychr    c l  Dummy character variable
c dmydbl    d l  Dummy double precision number
c dmyint    i l  Dummy integer
c EOF       i c  End-of-file
c Input     i c  Input channel number This should remain constant
c                 unless the channel is initialized
c Line      c c  Linlen long string of the current input line
c Lineln    i c  Length of the current line, <=LINLEN+1 including
c                 NEWLIN
c Lineno    i c  Line number of the current input line
c LITTLA   c p  Lower case a
c LITTLZ   c p  Lower case z
c LINLEN    i c  Parameter for the maximum length of the current line
c NEWLIN   c p  ASCII character for a new line
c Pos       i c  Position of the pointer on the current line
c NAME      c p  Code for token type variable name
c QUOTE     c p  Code for token type quoted string
c INTGR     c p  Code for integer token type
c DBL       c p  Code for double precision token type
c TAB       c p  ASCII code for horizontal tab
c----------------------------------------------------------------------
      INTEGER PERROR,PWARN,PERRNP,PWRNNP
      PARAMETER(PERROR=1,PWARN=2,PERRNP=3,PWRNNP=4)
c----------------------------------------------------------------------
      CHARACTER*1 BIGA,BIGZ,CNINE,CZERO,LITTLA,LITTLZ
      CHARACTER DIGITS*10,LCASE*26,UCASE*26
      INTEGER LINLEN,PBUFSZ,PCHAR,PLINE
      PARAMETER (BIGA='A',BIGZ='Z',CNINE='9',CZERO='0',
     & LITTLA='a',LITTLZ='z')
      PARAMETER(LINLEN=133,PBUFSZ=3,PCHAR=2,PLINE=1)
C     ------------------------------------------------------------------
      PARAMETER(DIGITS='0123456789',
     & LCASE='abcdefghijklmnopqrstuvwxyz',
     & UCASE='ABCDEFGHIJKLMNOPQRSTUVWXYZ')
c     -----------------------------------------------------------------
      INTEGER BADTOK,COMMA,COMMNT,NAME,QUOTE,INTGR,DBL,LBRACE,RBRACE,
     &        LPAREN,RPAREN,LBRAKT,RBRAKT,NULL,PERIOD,PLUS,MINUS,
     &        EQUALS,EOF,SLASH,STAR,COLON,BSLASH
      PARAMETER(COMMA=12,COMMNT=35,NAME=31,QUOTE=34,INTGR=48,DBL=101,
     &          LBRACE=123,RBRACE=125,LPAREN=40,RPAREN=41,LBRAKT=91,
     &          RBRAKT=93,NULL=0,PERIOD=46,PLUS=43,MINUS=45,EQUALS=61,
     &          EOF=26,BADTOK=21,SLASH=47,STAR=42,COLON=58,BSLASH=92)
C
C COMMON variables
C
c      INCLUDE 'cchars.i'
      LOGICAL Lexok
      CHARACTER Linex*(LINLEN+1),Nxttok*(LINLEN)
      INTEGER Errpos(2),Inputx,Lineln,Lineno,Lstpos(2),Nxtkln,Nxtktp,
     &        Pos(2)
      COMMON /clex  / Pos,Lineln,Lineno,Inputx,Errpos,
     &                Lstpos,Nxtkln,Nxtktp,Lexok,Linex,Nxttok
