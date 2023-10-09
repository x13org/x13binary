      subroutine ABORTA(str)
      implicit none
      character str*(*)
      stop 'function ABORTA'
      end

      Subroutine AddTramPols(Pols,PolsDim,Npols,PolToAdd,DimPolToAdd)
c     Add in the array Pols(MaxPol,MaxPolDim), the new polynomial [1,PolToAdd],
c     Add in the array PolsDim(MaxPol), the new dimension DimPolToAdd+1 of [1,PolToAdd]
c     Increment Npols.
      IMPLICIT NONE
      include 'polynom.i'
      integer Npols,PolsDim(MaxPol),DimPolToAdd
      real*8 Pols(MaxPol,MaxPolDim),PolToAdd(*)
      integer i
      if (DimPolToAdd .eq. 0) return
      if (Npols .ge. MaxPol) then
        call ABORTA('AddPols reach the MaxPol polynomials')
      end if
      Npols=Npols+1
      Pols(Npols,1)=1
      Do i=1,DimPolToAdd
        Pols(Npols,i+1)=PolToAdd(i)
      enddo
      PolsDim(Npols)=DimPolToAdd+1
      end
c
c
cc
c
      Subroutine AddBJPols(Pols,PolsDim,Npols,PolToAdd,DimPolToAdd)
c     The inpul polynomial PolToAdd is in Box-Jenkins notation (-PolToAdd(:) to pass to Tramo notation)
c     Add in the array Pols(MaxPol,MaxPolDim), the new polynomial [1,PolToAdd],
c     Add in the array PolsDim(MaxPol), the new dimension DimPolToAdd+1 of [1,PolToAdd]
c     Increment Npols.
      IMPLICIT NONE
      include 'polynom.i'
      integer Npols,PolsDim(MaxPol),DimPolToAdd
      real*8 Pols(MaxPol,MaxPolDim),PolToAdd(*)
      integer i
      if (DimPolToAdd .eq. 0) return
      if (Npols .ge. MaxPol) then
        call ABORTA('AddPols reach the MaxPol polynomials')
      end if
      Npols=Npols+1
      Pols(Npols,1)=1
      Do i=1,DimPolToAdd
        Pols(Npols,i+1)=-PolToAdd(i)
      enddo
      PolsDim(Npols)=DimPolToAdd+1
      end
c
c
cc
      Subroutine AddPols(Pols,PolsDim,Npols,PolToAdd,DimPolToAdd)
c     Add in the array Pols(MaxPol,MaxPolDim), the new polynomial PolToAdd,
c     Add in the array PolsDim(MaxPol), the new dimension DimPolToAdd of PolToAdd
c     Increment Npols.
      IMPLICIT NONE
      include 'polynom.i'
      integer Npols,PolsDim(MaxPol),DimPolToAdd
      real*8 Pols(MaxPol,MaxPolDim),PolToAdd(*)
      integer i
      if (DimPolToAdd .eq. 0) return
      if (Npols .ge. MaxPol) then
        call ABORTA('AddPols reach the MaxPol polynomials')
      end if
      Npols=Npols+1
      Pols(Npols,1)=1
      Do i=1,DimPolToAdd
        Pols(Npols,i)=PolToAdd(i)
      enddo
      PolsDim(Npols)=DimPolToAdd
      end
c
c
cc
      Subroutine AppendStrCut(Str1,MaxStr1Length,
     $                 Str2,MaxStr2Length)
c     Dada la dimension de STR2 (MaxStr2Length),mete en
c   Str2 los primeros MasStr2Length caracteres de Str2+Str1
c        IN/OUT  Str2
c        IN    Str1,MaxStr1Length,MaxStr2Length
      IMPLICIT NONE
      integer MaxStr1Length,MaxStr2Length
      character*(*) Str1,Str2
      EXTERNAL ISTRLEN
      integer ISTRLEN
      integer lStr1,lStr2
      lStr1=ISTRLEN(Str1)
      lStr2=ISTRLEN(Str2)
      if (lStr1.eq.0) return
      if ((lStr1+lStr2).ge.MAxStr1Length) then
         if (lSTr2.lt.MaxStr2Length-1) then
           if (lStr2.eq.0)then 
             write(Str2,'(A)')
     $            Str1(1:MaxStr2Length-1)
           else
             Str2(1:(lStr2+MaxStr2Length-lStr2-1))=Str2(1:lStr2)//
     $         Str1(1:MaxStr2Length-lStr2-1)  
           end if
         end if
      else
         if (lStr2.eq.0)then
           write(Str2,'(A)')
     $            Str1(1:MaxStr2Length-1)
         else
          Str2(1:(lStr2+lStr1))=Str2(1:lStr2)//Str1(1:lStr1)  
         end if 
      end if
      end
c
      Subroutine AppendStrCutRight(Str1,Str2)
c     Dada la dimension de STR2 (MaxStr2Length),mete en
c   Str2 los primeros MasStr2Length caracteres de Str1+Str2
      IMPLICIT NONE
      include 'stream.i'
      include 'polynom.i'
      character Str1*(MaxStrLength),Str2*(MaxPolDim),
     &          tstr*(MaxStrLength)
      EXTERNAL ISTRLEN
      integer ISTRLEN
      integer lStr1,lStr2
      lStr1=ISTRLEN(Str1)
      lStr2=ISTRLEN(Str2)
      if ((lStr1+lStr2).ge.MaxStrLength) then
         if (lSTr1.lt.MaxStrLength) then
           write(tstr,'(A,A)')Str1(1:lStr1),
     $            Str2(1:MaxStrLength-lStr1-1)
           Str2=tstr
         else
           write(Str2,'(A)')Str1(1:MaxStrLength-1)
         end if
      else
         write(tstr,'(A,A)')Str1(1:lStr1),Str2(1:lStr2)   
         Str2=tstr
      end if
      end

      subroutine AppendStrRight(Str,word,outtxt,line)
c     Str+word+OutTxt+Line=>OutTxt+Line (adding returns if HTML<>0 )
c     Where Line is the last line of the text
c
      IMPLICIT NONE
      include 'stream.i'
      include 'polynom.i'
      character Str*(MaxStrLength),word*(MaxLineLength),
     $          outTxt*(maxStrLength),line*(MaxLineLength),
     $          tTxt*(maxStrLength)
      external ISTRLEN,ABORTA
      integer ISTRLEN
      integer lStr,lword,lline,louttxt
c
      lStr=ISTRLEN(Str)
      lword=ISTRLEN(word)
      lline=ISTRLEN(line)
      loutTxt=ISTRLEN(OutTxt)
      if ((lword+lLine+4+lOutTxt) .ge. MaxStrLength) then
        call ABORTA('AppendStr: reach MaxStrLength')
      end if
      write(tTxt,1010) Str(1:lstr),Word(1:lWord),
     $      OutTxt(1:lOutTxt),Line(1:lLine)
      OutTxt(1:lOutTxt+lstr+Lword+lLine)=
     $     tTxt(1:lOutTxt+lstr+Lword+lLine)
! c      write(*,*)' OutTxt = ',OutTxt(1:lOutTxt+lstr+Lword+lLine)
 1010 FORMAT(A,A,A,A)
      Line=' '
      end
      subroutine AppendStr(Str,word,outtxt,line)
c     OutTxt+Line+Str+word=>OutTxt+Line (adding returns if HTML<>0 )
c     Where Line is the last line of the text
c
      IMPLICIT NONE
      include 'stream.i'
      include 'polynom.i'
      character Str*(*),word*(*),
     $          outTxt*(maxStrLength),line*(MaxLineLength)
      external ISTRLEN,ABORTA
      integer ISTRLEN
      integer lStr,lword,lline,louttxt
c
      lStr=ISTRLEN(Str)
      lword=ISTRLEN(word)
      lline=ISTRLEN(line)
      loutTxt=ISTRLEN(OutTxt)
      if ((lStr+lword+lLine+4+lOutTxt) .ge. MaxStrLength) then
        call ABORTA('AppendStr: reach MaxStrLength')
      end if
      OutTxt(1:(lOutTxt+lstr+Lword+lLine))=OutTxt(1:lOutTxt)//
     $     Line(1:lLine)//Str(1:lStr)//Word(1:lWord)
      Line=' '
      end
c
c
c
      Subroutine AppendLine(OutTxt,Line)
c     OutTxt=OutTxt+Line
c     Line=' '
      IMPLICIT NONE
      include 'stream.i'
      include 'polynom.i'
      character Line*(MaxLineLength),OutTxt*(MaxStrLength)
c     external functions
      external ISTRLEN
      integer ISTRLEN
c     Local Variables
      integer lLine,lOutTxt
c
      lLine=ISTRLEN(Line)
      lOutTxt=ISTRLEN(OutTxt)
      if ((lLine+lOutTxt+2).gt.MaxStrLength) then
        call ABORTA('AppendLine MaxStrLength reached')
      end if
      OutTxt(1:(lOutTxt+lLine)) = OutTxt(1:lOutTxt)//Line(1:lLine)
      Line=' '
      end
c
c
c
      subroutine StrPolyn(Bchar,Pol,PolDim,tolInteger,StrPol,Line)
c     Write the Pol(1:PolDim) in StrPol+Line; StrPol or Line can be empty strings
      Implicit None
      include 'stream.i'
      include 'polynom.i'
      real*8 Pol(*),tolInteger
      integer PolDim
      character Bchar*(MaxBcharLength),StrPol*(maxStrLength),
     $         Line*(MaxLineLength)
c     external functions
      intrinsic ABS
      external ISTRLEN
      integer ISTRLEN
c     LOCAL variables
      character ExpChar*(MaxLineLength),signChar,
     $         tmpWord*(MaxLineLength),tmpStr*(MaxStrLength)
      integer i,NumberNonZero,IntValue,lExpChar
c
      StrPol=' '
      Line=' '
      NumberNonZero=0
      Do i=1,PolDim
       if (ABS(pol(i)) .gt. tolInteger) then
        NumberNonZero=NumberNonZero+1
       end if
      enddo
      if (NumberNonZero.gt.1) then
        Line='('
      end if
      Do i=1,PolDim
       tmpWord=' '
       tmpStr=' '
       if (ABS(pol(i)) .gt. tolInteger) then
        if (i.eq.1) then
          expChar=' '
        else if (i.eq.2) then
          expChar(1:MaxBcharLength)=Bchar
        else if ((i.gt.2).and. (i.le.10)) then
          write(expchar,1010)Bchar(1:ISTRLEN(Bchar)),i-1
        else
          write(expchar,1020)Bchar(1:ISTRLEN(Bchar)),i-1
        end if
 1010   format(A,'<sup>',I1,'</sup>')
 1020   format(A,'<sup>',I2,'</sup>')
        lExpChar=ISTRLEN(ExpChar)
        if (pol(i).gt.0) then
          signchar='+'
        else
          signChar='-'
        end if
        if ((abs(pol(i)-ANINT(Pol(i))) .lt. TolInteger) .and.
     $    abs(Pol(i)).lt. 99.5d0) then
          IntValue=abs(ANINT(Pol(i)))
          if (Intvalue.ge. 10) then
            write(tmpWord,'(A,I2,A)')
     $       SignChar,Intvalue,Expchar(1:lExpchar)
          else if (Intvalue .gt. 2) then
            write(tmpWord,'(A,I1,A)')
     $       SignChar,Intvalue,Expchar(1:lExpchar)
          else if (i.ne.1) then
            write(tmpWord,'(A,A)') SignChar,ExpChar(1:lExpChar)
          else if (Pol(i).lt.0)then
            write(tmpword,'(A,"1")') SignChar
          else if (PolDim.gt.1) then
            write(tmpword,'("1")')
          end if
        else 
          if (Pol(i).ge.0.0d0) then
            write(tmpWord,'("+",G11.4)') Pol(i)
          else
            write(tmpWord,'(G11.4)') Pol(i)
          end if
          call AppendStr(' ',ExpChar,tmpStr,tmpWord)
        end if
        call AppendStr(tmpStr,tmpWord,strPol,line)
       end if
      enddo
      if (NumberNonZero.gt.1) then
        call AppendStr(' ',')',StrPol,line)
      end if
      end
c
c
c     return in strModel the model of "PHI(B)modelName=TH(B)at at~niid(0,V)"
      subroutine showModel(PHI,nPHI,TH,nTH,V,modelName,strModel)
      implicit none
      include 'polynom.i'
      include 'stream.i'
c      INPUT PARAMETERS
      real*8 PHI(*),TH(*),V
      integer nPHI,nTH
      character modelName*(MaxBcharLength)
c      OUTPUT PARAMETERS
      character StrModel*(MaxStrLength)
c      LOCAL PARAMETERS
      character StrPol*(MaxStrLength),Line*(MaxLineLength),
     $       tmpLine*(MaxLineLength)
c
      call StrPolyn('B    ',PHI,nPHI,1.0D-6,StrPol,Line)
      strModel=' '
      tmpLine='['
      call AppendStr(StrPol,Line,StrModel,tmpLine)
      call AppendStr(' ',']',strModel,tmpLine)
      call AppendStr(' ',modelName,strModel,tmpLine)
      call AppendStr(' ','=',strModel,tmpLine)
      call strPolyn('B    ',TH,nTH,1.0D-6,strPol,Line)
      call AppendStr(strPol,Line,strModel,tmpLine)
      write(Line,'("at at~niid(0,",G11.4,")")')V
      call AppendStr(' ',Line,StrModel,tmpLine)
      call AppendLine(strModel,tmpLine)
      end
c
c
c
      subroutine getDeltaMQStr(Bchar,MQ,bd,DeltaMqStr)
      IMPLICIT NONE
      include 'stream.i'
      include 'polynom.i'
      character Bchar*(MaxBcharLength),DeltaMqStr*(MaxLineLength)
      Integer MQ,bd
c     external functions
      external ISTRLEN,StrPolyn
      integer ISTRLEN
c     local variables
      character tmpStr*(MaxStrLength)
      real*8 DeltaMq(MaxPolDim)
      integer i
c
      DeltaMQStr=' '
      if (bd.gt.0) then
        DeltaMQ(1)=1.0d0
        do i=2,MQ
         DeltaMQ(i)=0.0d0
        end do
        DeltaMQ(MQ+1)=-1.0d0
        call StrPolyn(Bchar,DeltaMQ,1+mq,1.0D-6,tmpStr,DeltaMQStr)
        call AppendStrCutRight(tmpStr,DeltaMQStr)
        if (bd.ge.2) then
          write(DeltaMQStr,1010) DeltaMQStr(1:ISTRLEN(DeltaMQStr)),bd
          tmpStr='</sup>'
          call AppendStrCut(tmpStr,MaxLineLength,DeltaMQStr,
     $                      MaxLineLength)
        end if
      end if
 1010 format(A,'<sup>',I2)
      end
c
c
c
      Subroutine getStrPols(Bchar,Pols,PolsDim,nPols,d,MQ,bd,NS,
     $                      OutPol,Line)
c     Given the polynomials Pols(1:nPols,:) of order PolsDim(1:nPols)
c     and d regular differences, bd seasonal differences, ns annual aggregations
c     with frequency mq, we write the polynomial in OutPol+Line
c     where Bchar='B' is how we represent the equations variables(we can use 'B' or 'Xt'...)
c      Note: OutPol also depends on HMTL=0 or HTML=1
        IMPLICIT NONE
      include 'stream.i'
      include 'polynom.i'
      real*8 diffInt
      parameter(diffInt=1.0D-6)
      real*8 Pols(MaxPol,MaxPolDim)
      integer nPols,d,bd,ns,MQ,PolsDim(*)
      character Bchar*(MaxBcharLength),OutPol*(MaxStrLength),
     $         Line*(MaxLineLength)
      external ISTRLEN,StrPolyn,AppendStr,getDeltaMQStr
      integer ISTRLEN
      integer i,j,value
      real*8 AuxPol(MaxPolDim)
      character StrPol*(MaxStrLength),LinePol*(MaxLineLength)
c
      line=' '
      OutPol=' '
      Do i=1,nPols
        do j=1,MaxPolDim
          AuxPol(j)=Pols(i,j)
        enddo
        value=PolsDim(i)
        call StrPolyn(Bchar,AuxPol,value,diffInt,StrPol,LinePol)
        call AppendStr(strPol,LinePol,OutPol,Line)
      enddo
      call getDeltaMQstr(Bchar,1,d,LinePol)
      if (d.gt.1) then 
        call AppendStr(' ','[',OutPol,Line)
        call AppendStr(' ',LinePol,OutPol,Line)
        call AppendStr(' ',']',OutPol,Line)
      else
        call AppendStr(' ',LinePol,OutPol,Line)
      end if
      call getDeltaMQstr(Bchar,MQ,bd,LinePol)
      call AppendStr(' ',LinePol,OutPol,Line)
      if (ns.eq.1) then
       do i=1,mq
         AuxPol(i)=1.0D0
       enddo
       call StrPolyn(Bchar,AuxPol,MQ,diffInt,StrPol,LinePol)
       call AppendStr(StrPol,LinePol,OutPol,Line)
      end if
      end
c
c
c
      subroutine StrHpModel(Bchar,PolsAR,PolsDimAR,nPolsAR,d,mq,bd,
     $          PolsMA,PolsDimMa,nPolsMA,Kc,Km,ModelStrCt,ModelStrMt)
      IMPLICIT NONE
      include 'stream.i'
      include 'polynom.i'
      character Bchar*(MaxBcharLength)
      real*8 PolsAR(MaxPol,MaxPolDim),PolsMA(MaxPol,MaxPolDim),Kc,Km
      integer PolsDimAR(MaxPol),nPolsAR,PolsDimMA(MaxPol),nPolsMA,
     $       d,mq,bd
      character ModelStrCt*(MaxStrLength),ModelStrMt*(MaxStrLength)
c     Local variables
      integer ns,dc,bdc,MAdc,i,j
      character MAstr*(MaxStrLength),MALineStr*(MaxLineLength),
     $          ARstr*(MaxStrLength),ARLineStr*(MaxLineLength),
     $          tmpLine*(MaxLineLength),tmpStr*(MaxStrLength),
     $          tmpLine2*(MaxLineLength)
c -----------------------------------------------------------------------
c  Initialize
c -----------------------------------------------------------------------
      DO i=1,maxPolDim
       DO j=1,maxPol
        PolsAR(j,i)=0D0
        PolsMA(j,i)=0D0
        IF(j.eq.1)THEN
         PolsDimAR(j)=0
         PolsDimMA(j)=0
        END IF
       END DO
      END DO
c -----------------------------------------------------------------------
      Madc=2
      if (d.ge.MAdc) then
        dc=d-MAdc
        MAdc=0
        ns=0
        bdc=bd
      else
        dc=0
        MADC=MADC-d-bd
        bdc=0
        ns=bd
      end if
      call getStrPols(Bchar,PolsAR,PolsDimAR,nPolsAR,d,mq,bd,0,
     $              ARstr,ARlineStr)
      call getStrPols(Bchar,PolsMA,PolsDimMA,nPolsMA,0,mq,0,0,
     $              MAstr,MAlineStr)
      tmpStr=' '
      tmpLine='['
      call AppendStr(ARstr,ARLineStr,tmpStr,TmpLine)
      call AppendStr(' ',']m(t)=',tmpstr,tmpLine)
      call AppendStr(MAstr,MAlineStr,tmpStr,tmpLine)
      write(tmpLine2,'("  niid~(0,",G11.4)') Km
      call AppendStr(' ',tmpLine2,tmpStr,tmpLine)
      call AppendStr(' ','Va)',tmpStr,tmpLine)
      call AppendLine(tmpstr,tmpLine)
      ModelStrMt=tmpStr
      call getStrPols(Bchar,PolsAR,PolsDimAR,nPolsAR,dc,mq,bdc,ns,
     $               ARstr,ARlineStr)
      if (MADC.gt.0) then
        call getStrPols(Bchar,PolsMA,PolsDimMA,nPolsMA,MAdc,mq,0,0,
     $                 MAstr,MAlineStr)
      end if
      tmpStr=' '
      tmpLine='['
      call AppendStr(ARstr,ARlineStr,tmpStr,tmpLine)
      call AppendStr(' ',']C(t)=',tmpStr,tmpLine)
      call AppendStr(MAstr,MAlineStr,tmpStr,tmpLine)
      write(tmpLine2,'("  niid~(0,",G11.4)') Kc
      call AppendStr(' ',tmpLine2,tmpStr,tmpLine)
      call AppendStr(' ','Va)',tmpStr,tmpLine)
      call AppendLine(tmpstr,tmpLine)
      ModelStrCt=tmpStr
      end
c
c
c
      Subroutine PresentaHPsa(THhp,chis,nCHIS,cycs,nCYCS,cycns,nCYCNS,
     $              THadj,nTHADJ,Dp,varwna,Kc,Km,ModelStrCt,ModelStrMt)
c     Dp=d+BD
      IMPLICIT NONE
      include 'stream.i'
      include 'polynom.i'
      real*8 chis(5),cycs(5),cycns(5),THadj(5),varwna,Kc,Km,THhp(3)
      integer nCHIS,nCYCS,nCYCNS,nTHadj,Dp
      character ModelStrCt*(MaxStrLength),ModelStrMt*(maxStrLength)
c     Local variables
      integer PolsDimMA(MaxPol),PolsDimAR(MaxPol),nPolsAR,nPolsMA
      real*8 PolsAR(MaxPol,MaxPolDim),PolsMA(MaxPol,MaxPolDim)
      character Bchar*(MaxBcharLength)
      Bchar='B'
      nPolsAR=0
      call AddPols(PolsAR,PolsDimAR,nPolsAR,THhp,3)
      call AddPols(PolsAR,PolsDimAR,nPolsAR,Chis,nChis)
      call AddPols(PolsAR,PolsDimAR,NpolsAR,Cycs,nCycs)
      call AddPols(PolsAR,PolsDimAR,NpolsAR,Cycns,nCycns)
      nPolsMA=0
      call AddPols(PolsMA,PolsDimMA,nPolsMA,THadj,nTHadj)
      call StrHpModel(Bchar,PolsAR,PolsDimAR,nPolsAR,Dp,1,0,
     $           PolsMA,PolsDimMA,nPolsMA,Kc*varwna,Km*varwna,
     $           ModelStrCt,ModelStrMt)
      end
c     
c
      Subroutine PresentaHPXt(THhp,PHI,p,d,TH,q,mq,
     $                        BPHI,bp,bd,BTH,bq,Kc,Km,MoStrCt,MoStrMt)
c     THhp(B)*PHI(B)*(1-B^mq)^bd (1-B)^(d-2)Ct=TH(B)BTH(B)act  actñiid(0,Kc)
c     THhp(B)*PHI(B)*(1-B^mq)^bd (1-B)^d Mt=TH(B)BTH(B)amt amtñiid(0,Km)
c     Los polinomios se entran en formato Tramo 
      include 'polynom.i'
      include 'stream.i'
      Real*8 THhp(3),PHI(3),TH(3),BPHI(*),BTH(*),Kc,Km
      integer p,d,q,mq,bp,bd,bq
      character MoStrCt*(MaxStrLength),MoStrMt*(MaxStrLength)
      real*8 PolsAR(MaxPol,MaxPolDim),PolsMA(MaxPol,MaxPolDim),
     $      PolBPHI(MaxPolDim),PolBTH(MaxPolDim)
      character Bchar*(MaxBcharLength)
      integer PolsDimAR(MaxPol),PolsDimMA(MaxPol),NpolsAR,NpolsMA
      integer i
      real*8 tramoPHI(3),tramoTH(3)
      Bchar='B'
      NpolsAR=0
      call AddPols(PolsAR,PolsDimAR,NpolsAR,THhp,3)
      do i=1,3
        tramoPHI(i)=-PHI(i)
      enddo
      call AddTramPols(PolsAR,PolsDimAR,NpolsAR,tramoPHI,p)
      do i=1,MaxPolDim
        PolBPHI(i)=0.0d0
      enddo
      PolBPHI(1)=1.0d0
      if (bp.eq.1) then
        PolBPHI(mq+1)=-BPHI(1)
      end if
      call AddPols(PolsAR,PolsDimAR,NpolsAR,PolBPHI,1+mq*bp)
      NpolsMA=0
      do i=1,3
        tramoTH(i)=-TH(i)
      enddo
      call AddTramPols(PolsMA,PolsDimMA,NpolsMA,tramoTH,q)
      do i=1,maxPolDim
        PolBTH(i)=0.0d0
      enddo
      PolBTH(1)=1.0d0
      if (bq.eq.1) then
        PolBTH(1+mq)=-BTH(1)
      end if
      call AddPols(PolsMA,PolsDimMA,NpolsMA,PolBTH,1+mq*bq)
      call strHPmodel(Bchar,PolsAR,PolsDimAR,NpolsAR,d,mq,bd,PolsMA,
     $               PolsDimMA,NpolsMA,Kc,Km,MoStrCt,MoStrMt)
      end
c
c
c
      subroutine PresentaHP(THhp,HPcycle,Km,HPlam,varw,
     $        MoStrCt,MoStrMt)
      IMPLICIT NONE
      INCLUDE 'srslen.prm'
      include 'dimensions.i'
      integer n1,n12,n10
      parameter (n10 = 10, n12 = 12, n1 = 1)
c     include 'calc.i'
c     include 'calfor.i'
      include 'stream.i'
      include 'models.i'
      include 'hspect.i'
      include 'polynom.i'
      character MoStrCt*(MaxStrLength),MoStrMt*(MaxStrLength)
      real*8 THhp(3),Km,HPlam,varw
      integer HPcycle
C     CalC.i but THSTAR=>THSTAT2 to use with models.i
c     increase THSTAR2 array length to avoid the variables after THSTAR2
c     to be ignored - Jan. 2021
      integer mTYPE,P,D,Q,BP,BD,BQ,PBP,PQ,NW,INIT,BPQ,IMEAN,IPR
      real*8 DETPRI
      real*8 W(mpkp),PHI(3*N1),TH(3*N1),BPHI(3*N1),BTH(3*N1),
     $       PHIST(2*N12+3*N1),THSTAR2(40)
      common /calc/ W,PHI,TH,BPHI,BTH,PHIST,THSTAR2,DETPRI,mTYPE,
     $              P,D,Q,BP,BD,BQ,PBP,PQ,NW,INIT,BPQ,IMEAN,IPR
c     CALFOR.i but QSTAR=>QSTAR2
      integer PSTAR,QSTAR2,MQ
      common /calfor/ PSTAR,QSTAR2,MQ
c     Local variables
      real*8 Kc
      Kc=Km*HPlam
      if (HPcycle.eq.1) then
        call PresentaHPSA(THhp,CHIS,NCHIS,CYCs,0,CYCNS,0,
     $       THETP,nTHETP,d+bd,varw,Kc,Km,MoStrCt,MoStrMt)
      else if (HPcycle.eq.2) then
        call PresentaHPSA(THhp,CHIS,NCHIS,CYCs,nCYCS,CYCNS,nCYCNS,
     $       THadj,nTHadj,d+bd,varw,Kc,Km,MoStrCt,MoStrMt)
      else if (HPcycle.eq.3) then
        call PresentaHPxt(THhp,PHI,p,d,TH,q,mq,BPHI,bp,bd,
     $            BTH,bq,Kc,Km,MoStrCt,MoStrMt)
      end if
      end
c
c
c
      subroutine strFicModel(HPth,moHPstr)
      IMPLICIT NONE
      include 'stream.i'
      real*8 HPth(3)
      character moHPstr*(MaxLineLength)
      character line*(MaxLineLength),MAline*(MaxLineLength),
     $          tmpStr*(MaxStrLength),MAStr*(MaxStrLength)
      tmpStr=' '
      call getDeltaMQstr('B    ',1,2,Line)
      call AppendStr(' ','z(t)=',tmpStr,Line)
      call StrPolyn('B    ',HPth,3,.1D-15,MAStr,MAline)
      call AppendStr(MAStr,MAline,tmpStr,line)
      call AppendStr(' ','b(t)',tmpStr,line)
      call AppendLine(tmpStr,Line)
      moHPstr=tmpStr(1:MaxLineLength)
      end

