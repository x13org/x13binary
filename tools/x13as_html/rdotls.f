C     Last change: allow AOSdate-0.0,LSSdate-0.0 format
C     previous change:  BCM  25 Nov 97    8:48 am
      SUBROUTINE rdotls(Otlttl,Begspn,Endmdl,Sp,Otlind,Begotl,Endotl,
     &                  Locok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Reads an outlier sequence specifier 'LSSyr.mo-yr.mo' or
c     'AOSyr.mo-yr.mo' and returns the type of 
c     outlier, time point(s).
c-----------------------------------------------------------------------
c     CHANGES FOR AOS,LSS Outlier by Brian Monsell April 2012
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
      INCLUDE 'lex.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
c     ------------------------------------------------------------------
      INTEGER PEXERR
      LOGICAL F,T
      PARAMETER (F=.false.,T=.true.,PEXERR=3)
c     ------------------------------------------------------------------
      CHARACTER Otlttl*(*),errstr*(LINLEN)
      LOGICAL Locok
      INTEGER begdat,Endmdl,Begotl,Begspn,enddat,Endotl,ipos,Sp,strinx,
     &        itmp,nott,nerr
      DIMENSION begdat(2),Endmdl(2),Begspn(2),enddat(2),itmp(2)
      EXTERNAL strinx
c-----------------------------------------------------------------------
c     This argument dictionary was created with the command
c     ../../dictionary/strary otl < ../../dictionary/outlier.type.dic
c-----------------------------------------------------------------------
      CHARACTER OTLDIC*6
      INTEGER Otlind,otlptr,POTL
      PARAMETER(POTL=2)
      DIMENSION otlptr(0:POTL)
      PARAMETER(OTLDIC='aoslss')
      DATA otlptr/1,4,7/
c-----------------------------------------------------------------------
c     Read the type and date(s)
c-----------------------------------------------------------------------
      Endotl=0
      nott=len(Otlttl)
      errstr=' '
      Locok=.true.
      Otlind=strinx(F,OTLDIC,otlptr,1,POTL,Otlttl(1:3))
c     ------------------------------------------------------------------
      IF(Otlind.eq.0)THEN
       nerr=nott+47
       errstr(1:nerr)='Outlier sequence type, "'//Otlttl(1:nott)//
     &                '" is not an AOS or LSS.'
       CALL inpter(PEXERR,itmp,errstr(1:nerr),T)
       Locok=F
       RETURN
      END IF
c     ------------------------------------------------------------------
      ipos=4
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
c     Sequence outlier end date
c-----------------------------------------------------------------------
      IF(Locok)THEN
       IF(Otlttl(ipos:ipos).ne.'-')THEN
        IF(Otlind.eq.AO)THEN
         nerr=nott+41
         errstr(1:nerr)='"'//Otlttl(1:nott)//
     &                  '" is an invalid AO sequence variable.'
        ELSE
         nerr=nott+50
         errstr(1:nerr)='"'//Otlttl(1:nott)//
     &                  '" is an invalid level shift sequence variable.'
        END IF
        CALL inpter(PEXERR,itmp,errstr(1:nerr),T)
        Locok=F
c     ------------------------------------------------------------------
       ELSE
        ipos=ipos+1
        CALL ctodat(Otlttl,Sp,ipos,enddat,Locok)
c     ------------------------------------------------------------------
c     If date = 0.0, set enddat to end of model span,
c     reset Locok to TRUE
c     ------------------------------------------------------------------
        if (enddat(YR).eq.0.and.enddat(MO).eq.0) then
            enddat(YR) = Endmdl(YR)
            enddat(MO) = Endmdl(MO)
            Locok = T
        end if
        IF(.not.Locok)THEN
         nerr=nott+55
         IF(Otlind.eq.AO)THEN
          errstr(1:nerr)='AO sequence variable "'//Otlttl(1:nott)//
     &                   '" does not have a valid end date.'
         ELSE
          errstr(1:nerr)='LS sequence variable "'//Otlttl(1:nott)//
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
