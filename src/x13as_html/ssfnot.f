      SUBROUTINE ssfnot(Fh,I,Nopt,Nop2,Fnotky,Nssky)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c    create footnotes for full printout of sliding spans -
c    this will create an integer vector of footnote numbers (Fnotvc)
c    an integer vector of unique footnote codes which can be used to
c    generate the key for the table (Fnotky) with the number of unique
c    footnotes for the table (Nssky).
c    Written by BCM - December 2006
c-----------------------------------------------------------------------
      INTEGER PSSTP,PSSSC,PSSNT
      PARAMETER(PSSTP=5,PSSSC=6,PSSNT=7)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'ssap.prm'
      INCLUDE 'ssap.cmn'
      INCLUDE 'sspvec.cmn'
      INCLUDE 'htmlout.prm'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      INTEGER Fnotky,Nopt,Nop2,Nssky,Fh,I
      CHARACTER Fntstr*10
      DIMENSION Fnotky(PSSNT)
c-----------------------------------------------------------------------
c     For each observation, check to see if any of the indicator
c     variables has determined whether the observation was flagged as
c     an extreme (Per), changed direction (Csign), or indicated a
c     turning point (Cturn)
c-----------------------------------------------------------------------
*      DO i=Im,Sslen+Im-1
       CALL setchr(' ',10,Fntstr)
       IF(Per(i,Nopt).eq.-1)THEN
        IF(Fnotky(PSSNT).eq.NOTSET)THEN
         Fnotky(PSSNT)=1
         Nssky=Nssky+1
        END IF
        Infoot=Infoot+1
        Vfoot(Infoot)=PNOTINC
        WRITE(Fh,1060)'NT',Infoot,'NT',Infoot
       ELSE IF(Per(i,Nopt).gt.0)THEN
        IF(Fnotky(Per(i,Nopt)).eq.NOTSET)THEN
         Fnotky(Per(i,Nopt))=1
         Nssky=Nssky+1
        END IF
        IF((Cturn(i,Nopt).eq.1).and.(Csign(i,Nopt).eq.1).and.
     &      Per(i,Nopt).gt.0)THEN
         Infoot=Infoot+1
         IF(Nop2.gt.0)THEN
          WRITE(Fntstr,1010)'SC',Per(i,Nopt),Ch(Nopt)
          Vfoot(Infoot)=PTURNP+PSIGNCH+(Per(i,Nopt)+(Nopt-1)*4)+10
         ELSE
          WRITE(Fntstr,1010)'IE',Per(i,Nopt),Ch(Nopt)
          Vfoot(Infoot)=PTURNP+PINCON+(Per(i,Nopt)+(Nopt-1)*4)+10
         END IF
 1010    FORMAT(a,', TP, ',i1,a1)
         IF(Fnotky(PSSSC).eq.NOTSET)THEN
          Fnotky(PSSSC)=1
          Nssky=Nssky+1
         END IF
         IF(Fnotky(PSSTP).eq.NOTSET)THEN
          Fnotky(PSSTP)=1
          Nssky=Nssky+1
         END IF
         WRITE(Fh,1060)Fntstr,Infoot,Fntstr,Infoot
         
        ELSE IF((Csign(i,Nopt).eq.1).and.Per(i,Nopt).gt.0)THEN
         Infoot=Infoot+1
         IF(Nop2.gt.0)THEN
          WRITE(Fntstr,1020)'SC',Per(i,Nopt),Ch(Nopt)
          Vfoot(Infoot)=PSIGNCH+(Per(i,Nopt)+(Nopt-1)*4)+10
         ELSE
          WRITE(Fntstr,1020)'IE',Per(i,Nopt),Ch(Nopt)
          Vfoot(Infoot)=PINCON+(Per(i,Nopt)+(Nopt-1)*4)+10
         END IF
 1020    FORMAT('  ',a2,', ',i1,a1,'  ')
         IF(Fnotky(PSSSC).eq.NOTSET)THEN
          Fnotky(PSSSC)=1
          Nssky=Nssky+1
         END IF
         WRITE(Fh,1060)Fntstr,Infoot,Fntstr,Infoot
        ELSE IF((Cturn(i,Nopt).eq.1).and.Per(i,Nopt).gt.0)THEN
         WRITE(Fntstr,1030)Per(i,Nopt),Ch(Nopt)
 1030    FORMAT('  TP, ',i1,a1,'  ')
         IF(Fnotky(PSSTP).eq.NOTSET)THEN
          Fnotky(PSSTP)=1
          Nssky=Nssky+1
         END IF
         Infoot=Infoot+1
         Vfoot(Infoot)=PTURNP+(Per(i,Nopt)+(Nopt-1)*4)+10
         WRITE(Fh,1060)Fntstr,Infoot,Fntstr,Infoot
        ELSE IF(Per(i,Nopt).gt.0)THEN
         WRITE(Fntstr,1040)Per(i,Nopt),Ch(Nopt)
 1040    FORMAT('    ',i1,a1,'    ')
         Infoot=Infoot+1
         Vfoot(Infoot)=(Per(i,Nopt)+(Nopt-1)*4)+10
         WRITE(Fh,1060)Fntstr,Infoot,Fntstr,Infoot
        END IF
       ELSE IF((Cturn(i,Nopt).eq.1).and.(Csign(i,Nopt).eq.1)) THEN
        Infoot=Infoot+1
        IF(Nop2.gt.0)THEN
         Fntstr='  SC, TP  '
         Vfoot(Infoot)=PTURNP+PSIGNCH
        ELSE
         Fntstr='  IE, TP  '
         Vfoot(Infoot)=PTURNP+PINCON
        END IF
        IF(Fnotky(PSSSC).eq.NOTSET)THEN
         Fnotky(PSSSC)=1
         Nssky=Nssky+1
        END IF
        IF(Fnotky(PSSTP).eq.NOTSET)THEN
         Fnotky(PSSTP)=1
         Nssky=Nssky+1
        END IF
        WRITE(Fh,1060)Fntstr,Infoot,Fntstr,Infoot
       ELSE IF(Cturn(i,Nopt).eq.1)THEN
        IF(Fnotky(PSSTP).eq.NOTSET)THEN
         Fnotky(PSSTP)=1
         Nssky=Nssky+1
        END IF
        Infoot=Infoot+1
        Vfoot(Infoot)=PTURNP
        WRITE(Fh,1060)'TP',Infoot,'TP',Infoot
       ELSE IF(Csign(i,Nopt).eq.1)THEN
        Infoot=Infoot+1
        IF(Nop2.gt.0)THEN
         Fntstr='    SC    '
         Vfoot(Infoot)=PSIGNCH
        ELSE
         Fntstr='    IE    '
         Vfoot(Infoot)=PINCON
        END IF
        IF(Fnotky(PSSSC).eq.NOTSET)THEN
         Fnotky(PSSSC)=1
         Nssky=Nssky+1
        END IF
        WRITE(Fh,1060)Fntstr,Infoot,Fntstr,Infoot
       ELSE
        CALL mkTableCell(Fh,'@','&nbsp;')
       END IF
*      END DO
c-----------------------------------------------------------------------
 1060 FORMAT('<td class="center">',a,/,'<a href="#footnote',i4.4,
     &       '" class="longdesc">','Link to definition of ',a,'</a>',/
     &       '<a name="foot',i4.4,'"></a></td>')
c-----------------------------------------------------------------------
      RETURN
      END
      