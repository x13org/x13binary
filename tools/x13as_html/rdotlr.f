C     Last change:  BCM  25 Nov 97    8:48 am
      SUBROUTINE rdotlr(Otlttl,Begspn,Sp,Otlind,Begotl,Endotl,Locok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Reads an outlier specifier 'AOyr.mo', 'LSyr.mo', 'TCyr.mo',
c  'SOyr.mo', 'TLSyr.mo-yr.mo', or 'RPyr.mo-yr.mo' and returns the type 
c  of outlier, time point(s).
c-----------------------------------------------------------------------
c     CHANGES FOR TC Outlier by Brian Monsell June 1997
c     CHANGES FOR SO Outlier by Brian Monsell July 2003
c     CHANGES FOR TLS Outlier by Brian Monsell April 2009
c-----------------------------------------------------------------------
c Name   Type Description
c-----------------------------------------------------------------------
c begspn  i  Input 2 long vector (yr,mo) for the begining data of the
c             data or regression variables, used as a reference to
c             calculate t0.
c begotl  i  Output begining point for a ramp outlier and t0 for an AO
c             or level shift.
c endotl  i  Output end point for a ramp outlier and undefined for an
c             AO or LS
c otldat  i  Local 4 long vector for the begining (yr,mo) and possibly
c             the ending (yr,mo) of the outlier.
c otlttl  c  Input outlier specifier to be read
c otltyp  c  Output outlier type, either AO, LS, TC, or RP, for 
c             additive, level shift, temporary change or ramp outlier 
c             respectively.
c sp      i  Length of the seasonal period
c-----------------------------------------------------------------------
c     Data typing and variable initialization
c-----------------------------------------------------------------------
      LOGICAL T
      PARAMETER(T=.true.)
c     -----------------------------------------------------------------
      INCLUDE 'lex.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
c     ------------------------------------------------------------------
      INTEGER PEXERR
      PARAMETER(PEXERR=3)
c     ------------------------------------------------------------------
      CHARACTER Otlttl*(*),errstr*(LINLEN)
      LOGICAL Locok
      INTEGER begdat,Begotl,Begspn,enddat,Endotl,ipos,Sp,strinx,itmp,
     &        nott,nerr
      DIMENSION begdat(2),Begspn(2),enddat(2),itmp(2)
      EXTERNAL strinx
c-----------------------------------------------------------------------
c     This argument dictionary was created with the command
c     ../../dictionary/strary otl < ../../dictionary/outlier.type.dic
c-----------------------------------------------------------------------
      CHARACTER OTLDIC*18
      INTEGER Otlind,otlptr,POTL
      PARAMETER(POTL=9)
      DIMENSION otlptr(0:POTL)
      PARAMETER(OTLDIC='aolstcrpmvtlsoqiqd')
      DATA otlptr/1,3,5,7,9,11,13,15,17,19/
c-----------------------------------------------------------------------
c     Read the type and date(s)
c-----------------------------------------------------------------------
      Endotl=0
      nott=len(Otlttl)
      errstr=' '
      Locok=.true.
      Otlind=strinx(.false.,OTLDIC,otlptr,1,POTL,Otlttl(1:2))
c     ------------------------------------------------------------------
      IF(Otlind.eq.0)THEN
       nerr=nott+45
       errstr(1:nerr)='Outlier type, "'//Otlttl(1:nott)//
     &               '" is not an AO, LS, RP, SO, TL, TC, MV, QI or QD.'
       CALL inpter(PEXERR,itmp,errstr(1:nerr),T)
       Locok=.false.
       RETURN
      END IF
c     ------------------------------------------------------------------
      ipos=3
      CALL ctodat(Otlttl,Sp,ipos,begdat,Locok)
      IF(.not.Locok)THEN
       nerr=nott+42
       errstr(1:nerr)='Outlier "'//Otlttl(1:nott)//
     &                '" does not occur on a valid date.'
       CALL inpter(PEXERR,itmp,errstr(1:nerr),T)
       RETURN
      END IF
      CALL dfdate(begdat,Begspn,Sp,Begotl)
      Begotl=Begotl+1
c-----------------------------------------------------------------------
c     Ramp outlier end date
c-----------------------------------------------------------------------
      IF(Locok.and.(Otlind.eq.RP.or.Otlind.eq.TLS.or.Otlind.eq.QI.or.
     &              Otlind.eq.QD))THEN
       IF(Otlttl(ipos:ipos).ne.'-')THEN
        IF(Otlind.eq.RP)THEN
         nerr=nott+30
         errstr(1:nerr)='"'//Otlttl(1:nott)//
     &                  '" is an invalid ramp outlier.'
        ELSE IF(Otlind.eq.QI.or.Otlind.eq.QD)THEN
         nerr=nott+40
         errstr(1:nerr)='"'//Otlttl(1:nott)//
     &                  '" is an invalid quadratic ramp outlier.'
        ELSE
         nerr=nott+47
         errstr(1:nerr)='"'//Otlttl(1:nott)//
     &                  '" is an invalid temporary level shift outlier.'
        END IF
        CALL inpter(PEXERR,itmp,errstr(1:nerr),T)
        Locok=.false.
c     ------------------------------------------------------------------
       ELSE
        ipos=ipos+1
        CALL ctodat(Otlttl,Sp,ipos,enddat,Locok)
        IF(.not.Locok)THEN
         IF(Otlind.eq.RP)THEN
          nerr=nott+47
          errstr(1:nerr)='Ramp outlier "'//Otlttl(1:nott)//
     &                   '" does not have a valid end date.'
         ELSE IF(Otlind.eq.QI.or.Otlind.eq.QD)THEN
          nerr=nott+57
          errstr(1:nerr)='Quadratic Ramp outlier "'//Otlttl(1:nott)//
     &                   '" does not have a valid end date.'
         ELSE
          nerr=nott+45
          errstr(1:nerr)='TL outlier "'//Otlttl(1:nott)//
     &                   '" does not have a valid end date.'
         END IF
         CALL inpter(PEXERR,itmp,errstr(1:nerr),T)
        ELSE             
         CALL dfdate(enddat,Begspn,Sp,Endotl)
         Endotl=Endotl+1
        END IF
       END IF
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
