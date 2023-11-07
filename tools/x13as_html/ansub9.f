C     Last change:      REG  30 Aug 2005
C     Previous change:  BCM   4 Oct 2002    1:11 pm
C
      subroutine USRENTRY(rbuff,brbuff,erbuff,ilim1,ilim2,ifunc)
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
C.. In/Out Status: Read, Not Written ..
      integer brbuff,ilim1
C.. In/Out Status: Read, Not Written ..
      integer erbuff,ilim2
C.. In/Out Status: Not Read, Not Written ..
      real*8 rbuff(ilim1:ilim2)
C.. In/Out Status: Not Read, Not Written ..
      integer ifunc
C   LINES OF CODE ADDED FOR X-13A-S : 8
      LOGICAL dpeq,allzro
      INTEGER i
      EXTERNAL dpeq
      INCLUDE 'srslen.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'seatcm.cmn'
      INCLUDE 'seatdg.cmn'
      INCLUDE 'seatlg.cmn'
      INCLUDE 'seatmd.cmn'
      INCLUDE 'stcfcm.cmn'
C   END OF CODE BLOCK
C
C ... Executable Statements ...
C
C   LINES OF CODE ADDED FOR X-13A-S : 333
      IF (IFUNC.eq.1309) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Seatsa(i+Pos1ob-1) = RBUFF(i)
       allzro = (dpeq(RBUFF(i),DNOTST).or.dpeq(RBUFF(i),0D0)).and.allzro
       END DO
       Havesa=.not.allzro
      ELSE IF (IFUNC.eq.1310) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Seattr(i+Pos1ob-1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Havetr = .not.allzro
      ELSE IF (IFUNC.eq.1201) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Seatsf(i+Pos1ob-1) = RBUFF(i)
       allzro = (dpeq(RBUFF(i),DNOTST).or.dpeq(RBUFF(i),0D0)).and.allzro
       END DO
       Havesf = .not.allzro
c-----------------------------------------------------------------------
      ELSE IF (IFUNC.eq.1409) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Setfsa(i-BRBUFF+1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Havfsa=.not.allzro
       Nsfsa=ERBUFF-BRBUFF+1
      ELSE IF (IFUNC.eq.1410) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Setftr(i-BRBUFF+1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Havftr = .not.allzro
       Nsftr=ERBUFF-BRBUFF+1
      ELSE IF (IFUNC.eq.1411) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Setfsf(i-BRBUFF+1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Havfsf = .not.allzro
       Nsfsf=ERBUFF-BRBUFF+1
      ELSE IF (IFUNC.eq.1412) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Setfir(i-BRBUFF+1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Havfir = .not.allzro
       Nsfir=ERBUFF-BRBUFF+1
      ELSE IF (IFUNC.eq.1413) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
         Setfcy(i-BRBUFF+1) = RBUFF(i)
         allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Havfcy = .not.allzro
       Nsfcy=ERBUFF-BRBUFF+1
c-----------------------------------------------------------------------
      ELSE IF (IFUNC.eq.1010) THEN
       Iprsm=ERBUFF-BRBUFF
      ELSE IF (IFUNC.eq.1011) THEN
       Iqrsm=ERBUFF-BRBUFF
      ELSE IF (IFUNC.eq.1012) THEN
       Ipssm=ERBUFF-BRBUFF
      ELSE IF (IFUNC.eq.1013) THEN
       Iqssm=ERBUFF-BRBUFF
      ELSE IF (IFUNC.eq.1016) THEN
       Idrsm=INT(RBUFF(BRBUFF))
      ELSE IF (IFUNC.eq.1017) THEN
       Idssm=INT(RBUFF(BRBUFF))
      END IF
c-----------------------------------------------------------------------
      IF(Issap.eq.2.or.Irev.eq.4)RETURN
c-----------------------------------------------------------------------
      IF (IFUNC.eq.1311) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Seataf(i+Pos1ob-1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Haveaf = .not.allzro
      ELSE IF (IFUNC.eq.1312) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Seatir(i+Pos1ob-1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Haveir = .not.allzro
      ELSE IF (IFUNC.eq.1313) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Seatcy(i+Pos1ob-1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Havecy = .not.allzro
      ELSE IF (IFUNC.eq.1203) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Stocsa(i+Pos1ob-1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hvstsa = .not.allzro
      ELSE IF (IFUNC.eq.1204) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Stocir(i+Pos1ob-1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hvstir = .not.allzro
c-----------------------------------------------------------------------
      ELSE IF (IFUNC.eq.3201.or.IFUNC.eq.1135) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Stcftr(i-BRBUFF+1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hvstft = .not.allzro
      ELSE IF (IFUNC.eq.3202.or.IFUNC.eq.1141) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Stcfsf(i-BRBUFF+1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hvstfs = .not.allzro
      ELSE IF (IFUNC.eq.3203.or.IFUNC.eq.1133) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Stcfor(i-BRBUFF+1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hvstfo = .not.allzro
       Nfcfor=ERBUFF-BRBUFF+1
      ELSE IF (IFUNC.eq.3204.or.IFUNC.eq.1138) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Stcfsa(i-BRBUFF+1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hvstfa = .not.allzro
      ELSE IF (IFUNC.eq.3205.or.IFUNC.eq.1170) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Stcfcy(i-BRBUFF+1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hvstfc = .not.allzro
c-----------------------------------------------------------------------
      ELSE IF (IFUNC.eq.1139) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Ssefsa(i-BRBUFF+1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hsefsa=.not.allzro
      ELSE IF (IFUNC.eq.1136) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Sseftr(i-BRBUFF+1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hseftr = .not.allzro
      ELSE IF (IFUNC.eq.1142) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Ssefsf(i-BRBUFF+1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hsefsf = .not.allzro
      ELSE IF (IFUNC.eq.1134) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Ssefor(i-BRBUFF+1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hsefor = .not.allzro
      ELSE IF (IFUNC.eq.1171) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Ssefcy(i-BRBUFF+1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hsefcy = .not.allzro
      ELSE IF (IFUNC.eq.1140) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Ssrfsa(i-BRBUFF+1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hsrfsa=.not.allzro
      ELSE IF (IFUNC.eq.1137) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Ssrftr(i-BRBUFF+1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hsrftr = .not.allzro
      ELSE IF (IFUNC.eq.1143) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Ssrfsf(i-BRBUFF+1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hsrfsf = .not.allzro
      ELSE IF (IFUNC.eq.1172) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Ssrfcy(i-BRBUFF+1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hsrfcy = .not.allzro
      ELSE IF (IFUNC.eq.2021) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Spitrc(i+Pos1ob-1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hpitrc = .not.allzro
      ELSE IF (IFUNC.eq.2022) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Spis(i+Pos1ob-1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hpis = .not.allzro 
      ELSE IF (IFUNC.eq.2023) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Spitra(i+Pos1ob-1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hpitra = .not.allzro
      ELSE IF (IFUNC.eq.2024) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Spisa(i+Pos1ob-1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hpisa = .not.allzro
c-----------------------------------------------------------------------
      ELSE IF (IFUNC.eq.1042) THEN
       Kurt=RBUFF(BRBUFF)
      ELSE IF (IFUNC.eq.1043) THEN
       Kurtse=RBUFF(BRBUFF)
      ELSE IF (IFUNC.eq.1044) THEN
       Testnm=RBUFF(BRBUFF)
      ELSE IF (IFUNC.eq.1045) THEN
       Skew=RBUFF(BRBUFF)
      ELSE IF (IFUNC.eq.1046) THEN
       Skewse=RBUFF(BRBUFF)
      ELSE IF (IFUNC.eq.1047) THEN
       Sdres=RBUFF(BRBUFF)
      ELSE IF (IFUNC.eq.1048) THEN
       Dwstat=RBUFF(BRBUFF)
      ELSE IF (IFUNC.eq.1049) THEN
       SeasNP=RBUFF(BRBUFF)
      ELSE IF (IFUNC.eq.2051) THEN
       Ssghst=INT(RBUFF(BRBUFF))
      ELSE IF (IFUNC.eq.2052) THEN
       Ssgcnc=INT(RBUFF(BRBUFF))
      ELSE IF (IFUNC.eq.2053) THEN
       Ssgfct=INT(RBUFF(BRBUFF))
c-----------------------------------------------------------------------
      ELSE IF (IFUNC.eq.1103) THEN
       Ceetrn=RBUFF(BRBUFF)
      ELSE IF (IFUNC.eq.1106) THEN
       Ceesad=RBUFF(BRBUFF)
c-----------------------------------------------------------------------
      ELSE IF (IFUNC.eq.1517) THEN
       DO i=BRBUFF,ERBUFF
        Prsetr(i-BRBUFF+1)=RBUFF(i)
       END DO
      ELSE IF (IFUNC.eq.1518) THEN
       DO i=BRBUFF,ERBUFF
        Prsesa(i-BRBUFF+1)=RBUFF(i)
       END DO
c-----------------------------------------------------------------------
      ELSE IF (IFUNC.eq.1950) THEN
       Aadasa=RBUFF(BRBUFF)
      ELSE IF (IFUNC.eq.1951) THEN
       Aadatr=RBUFF(BRBUFF)
c-----------------------------------------------------------------------
      ELSE IF (IFUNC.eq.1900) THEN
       Tsetrn=RBUFF(BRBUFF)
      ELSE IF (IFUNC.eq.1901) THEN
       Tsesea=RBUFF(BRBUFF)
      ELSE IF (IFUNC.eq.1902) THEN
       Tsetcm=RBUFF(BRBUFF)
      ELSE IF (IFUNC.eq.1903) THEN
       Tsesad=RBUFF(BRBUFF)
c-----------------------------------------------------------------------
      ELSE IF (IFUNC.eq.1910) THEN
       Vartrn(1)=RBUFF(BRBUFF)
      ELSE IF (IFUNC.eq.1911) THEN
       Vartrn(2)=RBUFF(BRBUFF)
      ELSE IF (IFUNC.eq.1912) THEN
       Vartrn(3)=RBUFF(BRBUFF)
      ELSE IF (IFUNC.eq.1913) THEN
       Varsad(1)=RBUFF(BRBUFF)
      ELSE IF (IFUNC.eq.1914) THEN
       Varsad(2)=RBUFF(BRBUFF)
      ELSE IF (IFUNC.eq.1915) THEN
       Varsad(3)=RBUFF(BRBUFF)
      ELSE IF (IFUNC.eq.1916) THEN
       Varirr(1)=RBUFF(BRBUFF)
      ELSE IF (IFUNC.eq.1108) THEN
       Varirr(2)=RBUFF(BRBUFF)
      ELSE IF (IFUNC.eq.1109) THEN
       Varirr(3)=RBUFF(BRBUFF)
      ELSE IF (IFUNC.eq.1917) THEN
       Varsea(1)=RBUFF(BRBUFF)
      ELSE IF (IFUNC.eq.1918) THEN
       Varsea(2)=RBUFF(BRBUFF)
      ELSE IF (IFUNC.eq.1919) THEN
       Varsea(3)=RBUFF(BRBUFF)
c-----------------------------------------------------------------------
      ELSE IF (IFUNC.eq.2001) THEN
        DO i = BRBUFF, ERBUFF
          Tcnum(i-BRBUFF+1) = RBUFF(i)
        END DO
        Ntcnum = ERBUFF - BRBUFF + 1
      ELSE IF (IFUNC.eq.2002) THEN
        DO i = BRBUFF, ERBUFF
          Tcden(i-BRBUFF+1) = RBUFF(i)
        END DO
        Ntcden = ERBUFF - BRBUFF + 1
      ELSE IF (IFUNC.eq.2003) THEN
        Tcvar=RBUFF(BRBUFF)
      ELSE IF (IFUNC.eq.2004) THEN
        DO i = BRBUFF, ERBUFF
          Snum(i-BRBUFF+1) = RBUFF(i)
        END DO
        Nsnum = ERBUFF - BRBUFF + 1
      ELSE IF (IFUNC.eq.2005) THEN
        DO i = BRBUFF, ERBUFF
          Sden(i-BRBUFF+1) = RBUFF(i)
        END DO
        Nsden = ERBUFF - BRBUFF + 1
      ELSE IF (IFUNC.eq.2006) THEN
        Svar=RBUFF(BRBUFF)
      ELSE IF (IFUNC.eq.2007) THEN
        DO i = BRBUFF, ERBUFF
          Trnum(i-BRBUFF+1) = RBUFF(i)
        END DO
        Ntrnum = ERBUFF - BRBUFF + 1
      ELSE IF (IFUNC.eq.2008) THEN
        DO i = BRBUFF, ERBUFF
          Trden(i-BRBUFF+1) = RBUFF(i)
        END DO
        Ntrden = ERBUFF - BRBUFF + 1
      ELSE IF (IFUNC.eq.2009) THEN
        Trvar=RBUFF(BRBUFF)
      ELSE IF (IFUNC.eq.2010) THEN
        Irrvar=RBUFF(BRBUFF)
      ELSE IF (IFUNC.eq.2011) THEN
       DO i = BRBUFF, ERBUFF
        Sanum(i-BRBUFF+1) = RBUFF(i)
       END DO
       Nsanum = ERBUFF - BRBUFF + 1
      ELSE IF (IFUNC.eq.2012) THEN
       DO i = BRBUFF, ERBUFF
        Saden(i-BRBUFF+1) = RBUFF(i)
       END DO
       Nsaden = ERBUFF - BRBUFF + 1
      ELSE IF (IFUNC.eq.2013) THEN
       Savar=RBUFF(BRBUFF)
      ELSE IF (IFUNC.eq.2014) THEN
       DO i = BRBUFF, ERBUFF
        Tcwkf(i-BRBUFF+1) = RBUFF(i)
       END DO
       Ntcwkf = ERBUFF - BRBUFF + 1
      ELSE IF (IFUNC.eq.2015) THEN
       DO i = BRBUFF, ERBUFF
        Sawkf(i-BRBUFF+1) = RBUFF(i)
       END DO
       Nsawkf = ERBUFF - BRBUFF + 1
      ELSE IF (IFUNC.eq.2016) THEN
       DO i = BRBUFF, ERBUFF
        Swkf(i-BRBUFF+1) = RBUFF(i)
       END DO
       Nswkf = ERBUFF - BRBUFF + 1
      ELSE IF (IFUNC.eq.2017) THEN
       DO i = BRBUFF, ERBUFF
        Trwkf(i-BRBUFF+1) = RBUFF(i)
       END DO
       Ntrwkf = ERBUFF - BRBUFF + 1
      ELSE IF (IFUNC.eq.2018) THEN
       DO i = BRBUFF, ERBUFF
        Irwkf(i-BRBUFF+1) = RBUFF(i)
       END DO
       Nirwkf = ERBUFF - BRBUFF + 1
      ELSE IF (IFUNC.eq.2049) THEN
       DO i = BRBUFF, ERBUFF
        Srsdex(i-BRBUFF+1) = RBUFF(i)
       END DO
       Nrsdex = ERBUFF - BRBUFF + 1
      ELSE IF (IFUNC.eq.1110) THEN
       DO i = BRBUFF, ERBUFF
        Sep(i) = RBUFF(i)
       END DO
      ELSE IF (IFUNC.eq.1111) THEN
       DO i = BRBUFF, ERBUFF
        Seq(i) = RBUFF(i)
       END DO
      ELSE IF (IFUNC.eq.1112) THEN
       DO i = BRBUFF, ERBUFF
        Sebp(i) = RBUFF(i)
       END DO
      ELSE IF (IFUNC.eq.1113) THEN
       DO i = BRBUFF, ERBUFF
        Sebq(i) = RBUFF(i)
       END DO
c-----------------------------------------------------------------------
      ELSE IF (IFUNC.eq.2200) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Sttrse(i+Pos1ob-1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hvtrse=.not.allzro
       Lsttse = ERBUFF + Pos1ob - 1
      ELSE IF (IFUNC.eq.2201) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Stsfse(i+Pos1ob-1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hvsfse = .not.allzro
       Lstsse = ERBUFF + Pos1ob - 1
      ELSE IF (IFUNC.eq.2202) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Stcyse(i+Pos1ob-1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hvcyse = .not.allzro
       Lstyse = ERBUFF + Pos1ob - 1
      ELSE IF (IFUNC.eq.2203) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Stsase(i+Pos1ob-1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hvsase = .not.allzro
       Lstase = ERBUFF + Pos1ob - 1
      ELSE IF (IFUNC.eq.1256) THEN
       allzro = Hvtrse
       DO i = BRBUFF, ERBUFF
        Sttrse(i+Posfob) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hvtrse=.not.allzro
       Lsttse = ERBUFF + Posfob
      ELSE IF (IFUNC.eq.1258) THEN
       allzro = Hvsfse
       DO i = BRBUFF, ERBUFF
        Stsfse(i+Posfob) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hvsfse = .not.allzro
       Lstsse = ERBUFF + Posfob
      ELSE IF (IFUNC.eq.1259) THEN
       allzro = Hvcyse
       DO i = BRBUFF, ERBUFF
        Stcyse(i+Posfob) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hvcyse = .not.allzro
       Lstyse = ERBUFF + Posfob
      ELSE IF (IFUNC.eq.1257) THEN
       allzro = Hvsase
       DO i = BRBUFF, ERBUFF
        Stsase(i+Posfob) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hvsase = .not.allzro
       Lstase = ERBUFF + Posfob
c-----------------------------------------------------------------------
      ELSE IF (IFUNC.eq.2501) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Setcyc(i+Pos1ob-1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hvscyc = .not.allzro
      ELSE IF (IFUNC.eq.2502) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Setltt(i+Pos1ob-1) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hvsltt = .not.allzro
c-----------------------------------------------------------------------
      ELSE IF (IFUNC.eq.3001) THEN
       allzro = .true.
       DO i = BRBUFF, ERBUFF
        Odiff(i) = RBUFF(i)
        allzro = dpeq(RBUFF(i),0D0).and.allzro
       END DO
       Hvodff = .not.allzro
       Nodiff = ERBUFF - BRBUFF
      END IF
C   END OF CODE BLOCK
      end
C
C
C*GETSERIENAMES
C+
C
C       SUBROUTINE GETSERIENAMES (TITLE,NZ,NYER,NPERS,IFAIL)
C
C THIS SUBROUTINE PROVIDE THE SERIES TITLE, ITS
C NUMBER OF OBSERVATION, ITS STARTING YEAR, ITS FIRST PERIOD
C AND ITS THE FREQUENCY FOR THE TABLE OUTPUT TO SEATS
C
C   TITLE : CHARACTER*80 the series title
C   NZ    : INTEGER the number of observations
C   NYER  : INTEGER the first year of the series
C   NPERS : INTEGER the first period of the series
C   IFAIL : INTEGER the return values are :
C                           0 if the read is OK
C                           1 some error has occurred
C--
C   LINES OF CODE COMMENTED FOR X-13A-S : 1
C      subroutine GETSERIENAMES(title,nz,nyer,npers,ifail)
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 1
      subroutine GETSERIENAMES(stitle,nz,nyer,npers,nfreq,ifail)
C   END OF CODE BLOCK
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
C.. In/Out Status: Not Read, Overwritten ..
C   LINES OF CODE COMMENTED FOR X-13A-S : 1
C      character*80 title
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 1
      character*80 stitle
C   END OF CODE BLOCK
      integer nz
      integer nyer
      integer npers
      integer nfreq
      integer ifail
C   LINES OF CODE ADDED FOR X-13A-S : 3
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'title.cmn'
C   END OF CODE BLOCK
C
C ... Executable Statements ...
C
      ifail = 0
C   LINES OF CODE COMMENTED FOR X-13A-S : 2
C      read (5,'(A)',END = 5000) title
C      read (5,*,END = 5000) nz, nyer, npers
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 4
      stitle=Title
      nz=Nspobs
      nyer=Begspn(YR)
      npers=Begspn(MO)
      nfreq=Sp
C   END OF CODE BLOCK
      return
C   LINES OF CODE COMMENTED FOR X-13A-S : 1
C 5000 ifail = 1
C   END OF CODE BLOCK
      end
C
C
C*GETSERIES
C+
C       SUBROUTINE GETSERIES(OZ,NZ,IFAIL)
C
C THIS SUBROUTINE PROVIDE THE SERIES TO SEATS
C
C OZ    : REAL*8 ARRAY the series to be processed
C NZ    : INTEGER the number of observation
C IFAIL : INTEGER return value are :
C                        0 if everything is OK
C                        1 if some error has occurred
C
C--
      subroutine GETSERIES(oz,nz,ifail)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      include 'dimensions.i'
C
C.. Formal Arguments ..
C.. In/Out Status: Not Read, Maybe Written ..
      real*8 oz(mpkp)
C.. In/Out Status: Read, Not Written ..
      integer nz
C.. In/Out Status: Not Read, Overwritten ..
      integer ifail
C
C.. Local Scalars ..
      integer i 
C   LINES OF CODE ADDED FOR X-13A-S : 3
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'orisrs.cmn'
      INCLUDE 'units.cmn'
      integer i2
C   END OF CODE BLOCK
C
C ... Executable Statements ...
C
      ifail = 0
C   LINES OF CODE COMMENTED FOR X-13A-S : 1
C      read (5,*,ERR = 5000,END = 5000) (oz(i), i = 1,nz)
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 4
      DO I=1,NZ
       i2=i+Pos1ob-1
*       OZ(I)=Orig(i2)
       OZ(I)=Stcsi(i2)
      END DO
C   END OF CODE BLOCK
*      write(Mtprof,*) '  oz(1) = ',oz(1)
      return
C   LINES OF CODE COMMENTED FOR X-13A-S : 1
C 5000 ifail = 1
C   END OF CODE BLOCK
      end
C
C
C
C* NMLSTS
C+
C
C
C THIS SUBROUTINE PROVIDE TO MANIPULATE THE NAMELIST INPUT.
C
C    The list of parameter named L_... correspond exactly to the
C    SEATS input parameter.
C    ICODE : INTEGER is used to specify the action to be taken
C            on the namelist :
C                             0 set the namelist to the default values
C                             1 read the namelist
C                             2 save the namelist
C                             3 remove the saved namelist
C                             4 get the saved namelist
C                             5 write NAMELIST in REPORT.BUG file
C                               opened with devnum 77
C    IFAIL : INTEGER the return value :
C                             0 everything OK
C                             1 some error has occurred
C--
C Modified by REG on 30 Aug 2005 to add l_nfixed to NMLSTS parameter list

      subroutine NMLSTS(l_Nochmodel,l_type,l_init,l_lam,l_imean,
     $            l_p,l_d,l_q,l_bp,l_bd,l_bq,l_sqg,l_mq,l_m,l_iqm,
     $            l_maxit,l_fh,l_noserie,l_pg,l_modelsumm,l_out,
     $            l_seas,l_noadmiss,l_OutNA,l_stochTD,
     $            l_iter,l_qmax,l_har,l_bias,l_tramo,
     $            l_model,l_noutr,l_nouir,l_nous,l_npatd,l_npareg,
     $            l_interp,l_rsa,l_fortr,l_neast,l_epsiv,
     $            l_epsphi,l_ta,l_xl,l_rmod,l_blqt,
     $            l_tmu,l_phi,l_th,l_bphi,l_bth,l_thlim,l_bthlim,
     $            l_crmean,l_hplan,l_hpcycle,
     $            l_rogtable,l_centrregs,
     $            l_statseas,l_units,l_kunits,
     $            l_acfe,l_posbphi,l_printphtrf,l_tabtables,
     $            l_psieinic,l_psiefin,
     $            l_firstobs,l_lastobs,l_HPper,l_maxSpect,
     $            l_brol,l_blamda,l_bserie,l_bmid,l_bcMark,
     $            l_Odate,l_Olen,l_DetSeas,l_nds,nz,l_nfixed,icode,
     $            ifail)
C
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      integer n1
      LOGICAL F,T
      parameter (n1 = 1,F=.false.,T=.true.)
C
C.. Formal Arguments ..
      integer l_type,l_init,l_lam,l_imean,l_p,l_d,l_q,l_bp,l_bd,l_bq,
     $        l_sqg,l_mq,l_m,l_iqm,l_maxit,l_fh,l_noserie,
     $        l_pg,l_out,l_seas,l_noadmiss,l_outNA,l_stochTD,l_iter,
     $        l_qmax,l_har,l_bias,l_tramo,l_model,l_noutr,l_nouir,
     $        l_npatd,l_npareg,l_interp,l_rsa,l_fortr,l_neast
      integer l_nous,l_Nochmodel,nz
      integer l_hpcycle,l_rogtable,l_statseas,
     $        l_units,l_kunits,l_crmean,l_acfe,l_posbphi,
     $        l_printphtrf,icode,l_centrregs,l_psieinic,l_psiefin,
     $        ifail
      real*8 l_epsiv,l_epsphi,l_ta,l_xl,l_rmod,l_blqt,
     $       l_tmu,l_phi(3*n1),l_th(3*n1),l_bphi(3*n1),l_bth(3*n1),
     $       l_thlim,l_bthlim,l_hplan,l_HPper,l_maxSpect
      real*8 l_brol,l_blamda
      real*8 l_DetSeas(12*n1)
      integer l_bserie,l_bmid,l_bcMark
      character l_tabtables*100
      character l_firstobs*7,l_lastobs*7,l_Odate*7
      integer l_Olen,l_nds,l_modelsumm
C.. Added by REG on 30 Aug 2005 to create input/output variable l_nfixed
      integer l_nfixed
C
C.. Local Scalars ..
      integer bd,bias,bp,bq,d,fh,fortr,har,hpcycle,i,imean,
     $        init,interp,iqm,iter,l,lam,m,maxit,model,mq,
     $        neast,noadmiss,outNA,stochTD,modelsumm,
     $        noserie,nouir,noutr,npareg,npatd,out,
     $        p,pg,q,qmax,rogtable,rsa,statseas,units,kunits
      integer nous,Nochmodel,centrregs,psieinic,psiefin
      integer Olen,nds
      integer seas,sqg,tramo,type,crmean,acfe,posbphi,printphtrf
      real*8 blqt,epsiv,epsphi,hplan,rmod,
     $       ta,thlim,bthlim,tmu,xl,HPper,maxSpect
      real*8 brol,blamda
      real*8 DetSeas(12*n1)
      integer bserie,bmid,bcMark
      character tabtables*100
      character firstobs*7,lastobs*7,Odate*7 
C.. Added by REG on 30 Aug 2005 to create local variable nfixed
      integer nfixed
C
C.. Local Arrays ..
      real*8 bphi(3*n1),bth(3*n1),phi(3*n1),th(3*n1)
C
C.. External Calls ..
      external CLOSEDEVICE, OPENDEVSCRATCH, SETDEFAULT
C
C.. Namelist Declarations ..
C.. Modified by REG on 30 Aug 2005 to add nfixed to input namelist
      namelist /input/ type, init, lam, imean, p, d, q, bp, bd, bq, sqg,
     $                 mq, phi, th, bphi, bth, l, m, iqm, 
     $                 maxit, epsiv, epsphi, fh, noserie, pg,modelsumm,
     $                 ta, xl, out, seas, noadmiss, outNA, stochTD,
     $                 crmean, iter, bias, tramo, model, noutr, nouir,
     $                 neast, npatd, npareg, interp, rsa, qmax, rmod,
     $                 thlim, bthlim, har, fortr, blqt, tmu,
     $                 hplan, hpcycle, rogtable, 
     $                 statseas, units, kunits, acfe,
     $                 posbphi, nous, nochmodel, printphtrf, centrregs,
     $                 tabtables, psieinic, psiefin, 
     $                 firstobs,lastobs,HPper,maxSpect,brol,
     $                 blamda,bserie,bmid,bcMark,Odate,Olen,
     $                 DetSeas,Nds,nfixed
C   LINES OF CODE ADDED FOR X-13A-S : 40
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'x11fac.cmn'
      INCLUDE 'rev.prm'
      INCLUDE 'rev.cmn'
      INCLUDE 'sspinp.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'seattb.i'
      INCLUDE 'seatop.cmn'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      INTEGER strinx
      LOGICAL dpeq,istrue
      DOUBLE PRECISION totals
      EXTERNAL dpeq,totals,istrue,strinx
c     ------------------------------------------------------------------
      DOUBLE PRECISION Y,Userx,Critvl,Ciprob,Lam2,tmp,Cvalfa,Cvrduc,
     &                 Chi2cv,Tlimit,Pvaic
      DOUBLE PRECISION QsRsd,QsRsd2
      INTEGER Fcntyp,Nobs,Nrusrx,Bgusrx,Mxiter,Mxnlit,Mxcklg,Begtst,
     &        Endtst,Fctdrp,Begsrs,Frstsy,Begmdl,Endmdl,Nomnfy,iqst,
     &        Lsrun,imu
      DIMENSION Y(PLEN),Begsrs(2),Userx(PUSERX),Bgusrx(2),Critvl(POTLR),
     &          Begtst(2),Endtst(2),Begmdl(2),Endmdl(2)
c     ------------------------------------------------------------------
      COMMON /armalm/ Lam2,Fcntyp
c add  DOUBLE PRECISION QsRsd,QsRsd2, same as arima.cmn - Jan. 2021
      COMMON /armadp/ Y,Userx,Critvl,Ciprob,Cvalfa,Cvrduc,Chi2cv,Tlimit,
     &                Pvaic,QsRsd,QsRsd2
      COMMON /armain/ Nobs,Nrusrx,Bgusrx,Mxiter,Mxnlit,Mxcklg,Begtst,
     &                Endtst,Fctdrp,Begsrs,Frstsy,Begmdl,Endmdl,Nomnfy,
     &                Lsrun
c     ------------------------------------------------------------------
      INTEGER PR
      PARAMETER (PR=PLEN/4)
      INCLUDE 'autoq.cmn'
C   END OF CODE BLOCK
C
C ... Executable Statements ...
C
C
C
      ifail = 0
      if (icode .eq. 0) then
C.. Modified by REG on 30 Aug 2005 to add l_nfixed to SETDEFAULT
c   parameter list
       call SETDEFAULT(l_Nochmodel,l_type,l_init,l_lam,l_imean,l_p,
     $       l_d,l_q,l_bp,l_bd,l_bq,l_sqg,l_mq,l_m,l_iqm,
     $       l_maxit,l_fh,l_noserie,l_pg,l_modelsumm,
     $       l_out,l_seas,l_noadmiss,l_outNa,l_stochTD,
     $       l_iter,l_qmax,l_har,
     $       l_bias,l_tramo,l_model,l_noutr,l_nouir,l_nous,
     $       l_npatd,l_npareg,l_interp,l_rsa,l_fortr,
     $       l_neast,l_epsiv,l_epsphi,l_ta,l_xl,l_rmod,
     $       l_blqt,l_tmu,l_phi,l_th,l_bphi,l_bth,
     $       l_thlim,l_bthlim,l_crmean,l_hplan,l_hpcycle,
     $       l_rogtable,l_centrregs,
     $       l_statseas,l_units,l_kunits,
     $       l_acfe,l_posbphi,l_printphtrf,
     $       l_tabtables,l_psieinic,
     $       l_psiefin,l_firstobs,l_lastobs,
     $       l_hpPer,l_maxSpect,l_brol,l_blamda,l_bserie,
     $       l_bmid,l_bcMark,l_Odate,l_Olen,l_DetSeas,l_nds,nz,l_nfixed)
       return
      end if
      if (icode .eq. 1) then
       do i=1,12*n1
        DetSeas(i)=l_DetSeas(i)
       enddo
       modelsumm=l_modelsumm
       nds = l_nds
       Olen = l_Olen
       Odate = l_Odate
       firstobs = l_firstobs
       lastobs = l_lastobs
       psieinic = l_psieinic
       psiefin  = l_psiefin
       maxSpect   = l_maxSpect
       brol=l_brol
       blamda=l_blamda
       bserie=l_bserie
       bmid=l_bmid
       bcmark=l_bcMark
       tabtables = l_tabtables
       nochmodel = l_Nochmodel
       acfe = l_acfe
       posbphi=l_posbphi
       printphtrf = l_printphtrf
       units = l_units
       kunits = l_kunits
       statseas = l_statseas
       seas = l_seas
       blqt = l_blqt
       tmu = l_tmu
       fortr = l_fortr
       har = l_har
       rsa = l_rsa
       rmod = l_rmod
       thlim = l_thlim
       bthlim = l_bthlim
       qmax = l_qmax
       model = l_model
       noutr = l_noutr
       nouir = l_nouir
       nous = l_nous
       neast = l_neast
       npatd = l_npatd
       npareg = l_npareg
       bias = l_bias
       sqg = l_sqg
       type = l_type
       init = l_init
       interp = l_interp
       noserie = l_noserie
       lam = l_lam
       imean = l_imean
       p = l_p
       d = l_d
       fh = l_fh
       q = l_q
       bp = l_bp
       bd = l_bd
       bq = l_bq
       mq = l_mq
       out = l_out
       do i = 1,3*n1
        phi(i) = l_phi(i)
        bphi(i) = l_bphi(i)
        th(i) = l_th(i)
        epsphi = l_epsphi
        bth(i) = l_bth(i)
       end do
       xl = l_xl
       m = l_m
       iqm = l_iqm
       ta = l_ta
       maxit = l_maxit
       epsiv = l_epsiv
       pg = l_pg
       noadmiss = l_noadmiss
       OutNA = l_outNA
       stochTD = l_stochTD
       tramo = l_tramo
       crmean = l_crmean
       iter = l_iter
       hplan = l_hplan
       hpPer = l_HPper
       hpcycle = l_hpcycle
       rogtable = l_rogtable
       centrregs = l_centrregs
C.. Added by REG on 30 Aug 2005 to set nfixed based on l_nfixed
       nfixed = l_nfixed
C   LINES OF CODE COMMENTED FOR X-13A-S : 83
C       read (5,input,END = 5000,ERR = 5000)
C       do i=1,12*n1
C        l_DetSeas(i)=DetSeas(i)
C       enddo
C       l_modelsumm=modelsumm
C       l_nds = nds
C       l_Odate = Odate
C       l_Olen = Olen
C       l_firstobs = firstobs
C       l_lastobs = lastobs
C       l_html = html
C       l_psieinic = psieinic
C       l_psiefin = psiefin
*       l_maxSpect = maxSpect
*       l_brol=brol
*       l_blamda=blamda
*       l_bserie=bserie
*       l_bmid=bmid
*       l_bcMark=bcMark
C       l_tabtables = tabtables
C       l_Nochmodel = nochmodel
C       l_printphtrf = printphtrf
C       l_acfe = acfe
C       l_posbphi=posbphi
C       l_units = units
C       l_kunits = kunits
C       l_statseas = statseas
C       l_seas = seas
C       l_blqt = blqt
C       l_tmu = tmu
C       l_fortr = fortr
C       l_har = har
C       l_rsa = rsa
C       l_rmod = rmod
*       l_thlim = thlim
*       l_bthlim = bthlim
C       l_qmax = qmax
C       l_model = model
C       l_noutr = noutr
C       l_nouir = nouir
C       l_neast = neast
C       l_npatd = npatd
C       l_npareg = npareg
C       l_bias = bias
C       l_sqg = sqg
C       l_type = type
C       l_init = init
C       l_interp = interp
C       l_noserie = noserie
C       l_lam = lam
C       l_imean = imean
C       l_p = p
C       l_d = d
C       l_fh = fh
C       l_q = q
C       l_bp = bp
C       l_bd = bd
C       l_bq = bq
C       l_mq = mq
C       do i = 1,3*n1
C        l_phi(i) = phi(i)
C        l_bphi(i) = bphi(i)
C        l_th(i) = th(i)
C        l_epsphi = epsphi
C        l_bth(i) = bth(i)
C       end do
C       l_xl = xl
C       l_m = m
C       l_iqm = iqm
C       l_ta = ta
C       l_maxit = maxit
C       l_epsiv = epsiv
C       l_pg = pg
C       l_noadmiss = noadmiss
C       OutNA =l_outNA
C       stochTD = l_stochTD
C       l_tramo = tramo
C       l_crmean = crmean
C       l_iter = iter
C       l_out = out
C       l_hplan = hplan
C       l_HPper = HPper
C       l_hpcycle = hpcycle
C       l_rogtable = rogtable
C       l_centrregs = centrregs
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 61
c     ------------------------------------------------------------------
c     set up values from X-12 modeling procedures
c     ------------------------------------------------------------------
          IF (dpeq(Lam2,0D0)) THEN
             L_LAM=0
          ELSE
             L_LAM=1
          END IF
C.. Modified by REG on 30 Aug 2005 to add l_nfixed to nmlmdl parameter list
          CALL nmlmdl(3*N1,L_p,L_bp,L_d,L_bd,L_q,L_bq,L_Th,L_Bth,L_Phi,
     &                L_BPhi,Xl,L_Nfixed)
          IF(Lfatal)RETURN
          L_PG = 1
          L_INIT = 2
          IF((Issap.eq.2.and.Sstran).or.(Irev.eq.4.and.Rvtran))THEN
            L_OUT = 2
            Lsgud=F
c            L_OUT = 3
          ELSE
           IF(Out2.ne.NOTSET)L_Out=Out2
           IF(istrue(Prttab,LSETRN,NTBL-11))L_OUT=2
c           IF(istrue(Prttab,LSETRN,NTBL-11))L_OUT=3
           Lsgud=T
          END IF
          L_TRAMO = 1
          IF(Nusrrg.gt.0)L_NPAREG=1
          IF(Nflwtd.gt.0.or.Nln.gt.0.or.Nlp.gt.0)L_NPATD=1
          IF(NAO.gt.0.or.NTC.gt.0)L_NOUIR=1
          IF(NLS.gt.0.or.NRamp.gt.0)L_NOUTR=1
          IF(Nhol.gt.0)L_NEAST=1
          IF(NSO.gt.0)L_NOUS=1
          iqst = MIN(Mxcklg,2*Sp)
          L_BLQT = Qs(iqst)
          tmp = totals(Y,Frstsy,Frstsy+Nspobs-1,1,1)
          L_TMU = tmp
          L_MQ = Sp
          IF(Lnoadm)THEN
           L_NOADMISS=1
          ELSE
           L_NOADMISS=0
          END IF
          l_outNA = 0
          l_stochTD = -1
          IF(Kmean.eq.NOTSET)THEN
           L_IMEAN=0
           imu=strinx(F,Grpttl,Grpptr,1,Ngrptl,'Constant')
           IF(imu.gt.0)L_IMEAN=1
          ELSE IF (Kmean.eq.1) THEN
           L_IMEAN=1
          ELSE
           L_IMEAN=0
          END IF
          IF(Lhp)THEN
           IF(dpeq(Hplan2,DNOTST))THEN
            L_hpcycle=-1
           ELSE
            L_hpcycle=1
            IF(Hptrgt.ne.NOTSET)L_hpcycle=Hptrgt
           END IF
          ELSE
           L_hpcycle=0
          END IF
          IF(Lstsea)THEN
           L_statseas=1
          ELSE
           L_statseas=0
          END IF
          IF(Lmdsum)THEN
           L_modelsumm=1
          ELSE
           L_modelsumm=0
          END IF
          IF(Qmax2.ne.NOTSET)L_QMAX=Qmax2
          IF(Maxit2.ne.NOTSET)L_MAXIT=Maxit2
          IF(Iphtrf.ne.NOTSET)L_printphtrf=Iphtrf
          IF(Tabtbl(1:1).ne.CNOTST)L_tabtables=Tabtbl
          IF(.not.dpeq(Epsph2,DNOTST))L_Epsphi=Epsph2
          IF(.not.dpeq(Xl2,DNOTST))L_Xl=Xl2
          IF(.not.dpeq(Rmod2,DNOTST))L_rmod=Rmod2
          IF(.not.dpeq(Epsiv2,DNOTST))L_epsiv=Epsiv2
          IF(.not.dpeq(Hplan2,DNOTST))THEN
           L_hplan=Hplan2
           IF(L_hpcycle.eq.0)THEN
            L_hpcycle=1
            IF(Hptrgt.ne.NOTSET)L_hpcycle=Hptrgt
           END IF
          END IF
          L_fh=Nfcst
*          IF(Nfcst.gt.2*Sp)L_fh=2*Sp
          IF(Bias2.eq.NOTSET)THEN
           IF(L_Bias.eq.-2)L_Bias=1
          ELSE
           L_Bias=Bias2
          END IF
C   END OF CODE BLOCK
c  add code for benchmarking options once I know what they are
*        l_maxSpect = 
*        l_brol =
*        l_blamda =
*        l_bserie =
*        l_bmid = 
*        l_bcMark =
*
       return
      else if (icode .eq. 2) then
       call OPENDEVSCRATCH(32)
       do i=1,12*n1
        DetSeas(i)=l_DetSeas(i)
       enddo
       modelsumm=l_modelsumm
       firstobs = l_firstobs
       lastobs = l_lastobs
       psieinic = l_psieinic
       psiefin = l_psiefin
       maxSpect = l_maxSpect
       brol=l_brol
       blamda=l_blamda
       bserie=l_bserie
       bmid=l_bmid
       bcMark=l_bcMark
       tabtables = l_tabtables
       nochmodel = l_Nochmodel
       printphtrf = l_printphtrf
       acfe = l_acfe
       posbphi=l_posbphi
       units = l_units
       kunits = l_kunits
       statseas = l_statseas
       seas = l_seas
       blqt = l_blqt
       tmu = l_tmu
       fortr = l_fortr
       har = l_har
       rsa = l_rsa
       rmod = l_rmod
       thlim = l_thlim
       bthlim = l_bthlim
       qmax = l_qmax
       model = l_model
       noutr = l_noutr
       nouir = l_nouir
       nous = l_nous
       neast = l_neast
       npatd = l_npatd
       npareg = l_npareg
       bias = l_bias
       sqg = l_sqg
       type = l_type
       init = l_init
       interp = l_interp
       noserie = l_noserie
       lam = l_lam
       imean = l_imean
       p = l_p
       d = l_d
       fh = l_fh
       q = l_q
       bp = l_bp
       bd = l_bd
       bq = l_bq
       mq = l_mq
       out = l_out
       do i = 1,3*n1
        phi(i) = l_phi(i)
        bphi(i) = l_bphi(i)
        th(i) = l_th(i)
        epsphi = l_epsphi
        bth(i) = l_bth(i)
       end do
       xl = l_xl
       m = l_m
       iqm = l_iqm
       ta = l_ta
       maxit = l_maxit
       epsiv = l_epsiv
       pg = l_pg
       noadmiss = l_noadmiss
       tramo = l_tramo
       crmean = l_crmean
       iter = l_iter
       hplan = l_hplan
       HPper = l_HPper
       hpcycle = l_hpcycle
       rogtable = l_rogtable
       centrregs = l_centrregs
C.. Added by REG on 30 Aug 2005 to set nfixed based on l_nfixed
       nfixed = l_nfixed
       write (32,input)
       return
      else
       if (icode .eq. 3) then
        call CLOSEDEVICE(32)
        return
       end if
       if (icode .eq. 4) then
        rewind (32)
        read (32,input)
        do i=1,12*n1
         l_DetSeas(i)=DetSeas(i)
        enddo
        l_modelsumm = modelsumm
        l_nds = nds
        l_Odate = Odate
        l_Olen = Olen
        l_firstobs = firstobs
        l_lastobs = lastobs
        l_psieinic = psieinic
        l_psiefin = psiefin
        l_maxSpect = maxSpect
        l_brol=brol
        l_blamda=blamda
        l_bserie=bserie
        l_bmid=bmid
        l_bcMark=bcMark
        l_tabtables = tabtables
        l_Nochmodel = nochmodel
        l_printphtrf = printphtrf
        l_acfe = acfe
        l_posbphi=posbphi
        l_units = units
        l_kunits = kunits
        l_statseas = statseas
        l_seas = seas
        l_blqt = blqt
        l_tmu = tmu
        l_fortr = fortr
        l_har = har
        l_rsa = rsa
        l_rmod = rmod
        l_bthlim = bthlim
        l_qmax = qmax
        l_model = model
        l_noutr = noutr
        l_nouir = nouir
        l_nous = nous
        l_neast = neast
        l_npatd = npatd
        l_npareg = npareg
        l_bias = bias
        l_sqg = sqg
        l_type = type
        l_init = init
        l_interp = interp
        l_noserie = noserie
        l_lam = lam
        l_imean = imean
        l_p = p
        l_d = d
        l_fh = fh
        l_q = q
        l_bp = bp
        l_bd = bd
        l_bq = bq
        l_mq = mq
        do i = 1,3*n1
         l_phi(i) = phi(i)
         l_bphi(i) = bphi(i)
         l_th(i) = th(i)
         l_epsphi = epsphi
         l_bth(i) = bth(i)
        end do
        l_m = m
        l_iqm = iqm
        l_ta = ta
        l_maxit = maxit
        l_epsiv = epsiv
        l_pg = pg
        l_noadmiss = noadmiss
        l_outNA= OutNA
        l_stochTD = stochTD
        l_tramo = tramo
        l_crmean = crmean
        l_iter = iter
        l_out = out
        l_hplan = hplan
        l_HPper = HPper
        l_hpcycle = hpcycle
        l_rogtable = rogtable
        l_centrregs = centrregs
C.. Added by REG on 30 Aug 2005 to set l_nfixed based on nfixed
        l_nfixed = nfixed
        return
       end if
      end if
 5000 ifail = 1
      end
C
C* TAKEDETTRAMO
C+
C
C       SUBROUTINE TAKEDETTRAMO (TRAM,PAOUTR,PAOUIR,PAEAST,PATD,
C                             NEFF,PAREG,NPAREG,NLEN,LAM,IFAIL)
C
C THIS FUNCTION PROVIDE TO TAKE THE DETERMINISTIC COMPONENT FORM TRAMO
C
C  TRAM   : REAL*8 array with the original series from tramo
C  PAOUTR : REAL*8 array with the level shift outlier component from TRAMO
C  PAOUIR : REAL*8 array with transitory change outlier component from TRAMO
C  PAEAST : REAL*8 array withe the easter effect component from TRAMO
C  PATD   : REAL*8 array with the trading day effect component from TRAMO
C  NEFF   : INTEGER ARRAY which test if the regression component variable
C           for the sthocastic component of SEATS exist or not
C  PAREG  : REAL*8 matrix with the regression component effects to assigne
C           to the SEATS components
C  NPAREG : INTEGER check if exists the regression variable component
C  NLEN   : INTEGER the length of the series + its forecast
C  LAM    : INTEGER the transformation used by SEATS
C  IFAIL  : INTEGER return the value :
C                             0 if everything is OK
C                             1 if some error is occurred
C--
      subroutine TAKEDETTRAMO(tram,paoutr,paouir,paous,paeast,patd,
C   LINES OF CODE COMMENTED FOR X-13A-S : 1      
C     $                        pareg,tse,npareg,nlen,nf,lam,ifail)
C   END OF CODE BLOCK      
C   LINES OF CODE ADDED FOR X-13A-S : 1
     $                        neff,pareg,tse,npareg,nlen,nf,lam2,ifail)
C   END OF CODE BLOCK 
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
*      INCLUDE 'model.prm'
      include 'dimensions.i'
      real*8 ZERO,ONE,ONEHND
      parameter (ZERO=0.0d0,ONE=1.0d0)
C
C.. Formal Arguments ..
C   LINES OF CODE COMMENTED FOR X-13A-S : 1
C      integer neff(0:6),npareg,nlen,nf,lam,ifail
C   END OF CODE BLOCK 
C   LINES OF CODE ADDED FOR X-13A-S : 1
      integer neff(0:7),npareg,nlen,nf,lam2,ifail
C   END OF CODE BLOCK       
      real*8 tram(mpkp),paoutr(mpkp),paouir(mpkp),paous(mpkp),
     $       paeast(mpkp),patd(mpkp),pareg(mpkp,0:7),tse(kp),
     $       facint
C
C.. Local Scalars ..
      integer i,j
C   LINES OF CODE ADDED FOR X-13A-S : 7
      integer i2
*      INCLUDE 'arima.cmn'
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'x11fac.cmn'
      INCLUDE 'seatad.cmn'
*      INCLUDE 'inpt.cmn'
C   END OF CODE BLOCK
C
C ... Executable Statements ...
C
      ifail = 0
C   LINES OF CODE COMMENTED FOR X-13A-S : 8
C      read (5,*,END = 5000)
C     $     (
C     $     tram(i), paoutr(i), paouir(i), paeast(i), patd(i), i = 1,nlen
C     $     )
C      read (5,*,END = 5000)
C     $     (
C     $     tram(nlen+i), paoutr(nlen+i), paouir(nlen+i), paeast(nlen+i),
C     $     patd(nlen+i), tse(i), i = 1,nf)
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 22
      DO i = 1,NLEN+Nf
       TRAM(i)=Orixs(i)
       i2=i+Pos1ob-1
       PATD(i)=FACTD(i2)
       PAEAST(i)=FACHOL(i2)
       PAOUTR(i)=FACLS(i2)
       IF(LAM2.eq.0)THEN
        PAOUIR(i)=FACAO(i2)*FACTC(i2)
       ELSE
        PAOUIR(i)=FACAO(i2)+FACTC(i2)
       END IF
       PAOUS(i)=FACSO(i2)
      END DO
      do i = 1,nf
       tse(i)=Fctses(i)
      end do
      neff(0) = 0
      neff(1) = 0
      neff(2) = 0
      neff(3) = 0
      neff(4) = 0
      neff(5) = 0
      neff(6) = 0
      neff(7) = 0
C   END OF CODE BLOCK
      facint=ZERO
      if (lam2 .eq. 0) facint=ONE
      if (npareg .eq. 1) then
C   LINES OF CODE COMMENTED FOR X-13A-S : 6
C       read (5,*,END = 5000) (neff(i), i = 0,6)
C       read (5,*,END = 5000)
C     $      (
C     $      pareg(i,0), pareg(i,1), pareg(i,2), pareg(i,3), pareg(i,4),
C     $      pareg(i,5), pareg(i,6), i = 1,nlen+nf)
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 19
       IF(Nusrrg.gt.0)THEN
        IF(Adjusr.eq.1)THEN
         IF(Finusr)THEN
          Neff(0)=1
         ELSE
          Neff(4)=1
         END IF
        END IF
c     source added to generate seasonal regression factor when SEATS 
c     is specified for seasonal adjustment (BCM 04-10-05)
        IF(Adjsea.eq.1)NEFF(2)=1
        IF(Adjcyc.eq.1)NEFF(5)=1
        DO i=1,NLEN+nf
         i2=i+Pos1ob-1
         IF(Adjusr.eq.1)THEN
          IF(Finusr)THEN
           PAREG(i,0)=Facusr(i2)
           PAREG(i,4)=facint
          ELSE
           PAREG(i,4)=Facusr(i2)
           PAREG(i,0)=facint
          END IF
         ELSE
          pareg(i,0)=facint
          PAREG(i,4)=facint
         END IF
         PAREG(i,1)=facint
c     source added to generate seasonal regression factor when SEATS 
c     is specified for seasonal adjustment (BCM 04-10-05)
         IF(Adjsea.eq.1)THEN
          PAREG(i,2)=Facsea(i2)
         ELSE
          PAREG(i,2)=facint
         END IF
         PAREG(i,3)=facint
         IF(Adjcyc.eq.1)THEN
          PAREG(i,5)=Faccyc(i2)
         ELSE
          PAREG(i,5)=facint
         END IF
         PAREG(i,6)=facint
         PAREG(i,7)=facint
        END DO
       END IF
C   END OF CODE BLOCK 
       return
      else
C   LINES OF CODE COMMENTED FOR X-13A-S : 1
C       if (lam .eq. 0) then
C   END OF CODE BLOCK 
C   LINES OF CODE ADDED FOR X-13A-S : 1
       if (lam2 .eq. 0) then
C   END OF CODE BLOCK 
        do i = 1,mpkp
         do j = 0,7
          pareg(i,j) = ONE
         end do
        end do
       else
        do i = 1,mpkp
         do j = 0,7
          pareg(i,j) = ZERO
         end do
        end do
       end if
       return
      end if
C   LINES OF CODE COMMENTED FOR X-13A-S : 1
C 5000 ifail = 1
C   END OF CODE BLOCK 
      end
C.. Modified by REG on 30 Aug 2005 to add l_nfixed to SETDEFAULT
c   parameter list
      subroutine SETDEFAULT(l_Nochmodel,l_type,l_init,l_lam,l_imean,
     $       l_p,l_d,l_q,l_bp,l_bd,l_bq,l_sqg,l_mq,l_m,l_iqm,
     $       l_maxit,l_fh,l_noserie,l_pg,l_modelsumm,
     $       l_out,l_seas,l_noadmiss,l_outNa,l_stochTD,
     $       l_iter,l_qmax,l_har,
     $       l_bias,l_tramo,l_model,l_noutr,l_nouir,l_nous,
     $       l_npatd,l_npareg,l_interp,l_rsa,l_fortr,
     $       l_neast,l_epsiv,l_epsphi,l_ta,l_xl,l_rmod,
     $       l_blqt,l_tmu,l_phi,l_th,l_bphi,l_bth,
     $       l_thlim,l_bthlim,l_crmean,l_hplan,l_hpcycle,
     $       l_rogtable,l_centrregs,
     $       l_statseas,l_units,l_kunits,l_acfe,l_posbphi,
     $       l_printphtrf,l_tabtables,l_psieinic,
     $       l_psiefin,l_firstobs,l_lastobs,
     $       l_HPper,l_maxSpect,l_brol,l_blamda,l_bserie,l_bmid,
     $       l_bcMark,l_Odate,l_Olen,l_DetSeas,l_nds,nz,l_nfixed)
C
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      integer n1
      parameter (n1 = 1)
      real*8 ZERO,ONE,TWO,ONEHND,MONE
      parameter (ZERO=0.0d0,ONE=1.0d0,TWO=2.0d0,ONEHND=100.0d0,
     &           MONE=-1.0d0)
C
C.. Formal Arguments ..
      integer l_type,l_init,l_lam,l_imean,l_p,l_d,l_q,l_bp,l_bd,l_bq,
     $        l_sqg,l_mq,l_m,l_iqm,l_maxit,l_fh,l_noserie,
     $        l_pg,l_modelsumm,l_out,l_seas,l_noadmiss,l_outNA,
     $        l_stochTD,l_iter,l_qmax,l_har,l_bias,l_tramo,l_model,
     $        l_noutr,l_nouir,l_npatd,l_npareg,l_interp,l_rsa
      integer l_fortr,l_neast,l_nous,l_Nochmodel
      integer l_hpcycle,l_rogtable,l_statseas,
     $        l_units, l_kunits,l_crmean,
     $        l_acfe,l_posbphi,l_printphtrf,
     $        l_centrregs,l_psieinic,l_psiefin,nz
      real*8 l_epsiv,l_epsphi,l_ta,l_xl,l_rmod,l_blqt,
     $       l_tmu,l_phi(3*n1),l_th(3*n1),l_bphi(3*n1),l_bth(3*n1),
     $       l_thlim,l_bthlim,l_hplan,l_HPper,l_maxSpect
      real*8 l_brol,l_blamda
      real*8 l_DetSeas(12*n1)
      integer l_bserie,l_bmid,l_bcMark
      character l_tabtables*100
      character l_firstobs*7,l_lastobs*7,l_Odate*7
      integer l_Olen,l_nds
C.. Added by REG on 30 Aug 2005 to create input/output variable l_nfixed
      integer l_nfixed
C
C.. Local Scalars ..
      integer i
C
C ... Executable Statements ...
C
      do i=1,12*n1
       l_DetSeas(i)=ZERO
      enddo
      l_modelsumm = -1
      l_nds = 0
      l_Odate='00-0000'
      l_Olen=0
      l_tabtables = 'all'
      l_psieinic = -24
      l_psiefin = 24
      l_maxSpect = ONEHND
      l_brol=ONE
      l_blamda=ZERO
      l_bserie=0
      l_bmid=0
      l_bcMark=0
      l_firstobs = '00-0000'
      l_lastobs = '00-0000'
      l_Nochmodel = 0
      l_printphtrf = 0
      l_acfe  = 0
      l_posbphi = 0
      l_units = 0
      l_kunits = 0
      l_statseas = 0
      l_seas = 1
      l_blqt = ZERO
      l_tmu = ZERO
      l_fortr = 1
      l_har = 0
      l_rsa = 0
      l_rmod = .50d0
      l_thlim = ZERO
      l_bthlim = ZERO
      l_qmax = 50
      l_model = 0
      l_noutr = 0
      l_nouir = 0
      l_nous = 0
      l_neast = 0
      l_npatd = 0
      l_npareg = 0
      l_bias = -2
      l_sqg = 1
      l_type = 0
      l_init = 0
      l_interp = 0
      l_noserie = 0
      l_lam = 1
      l_imean = 1
      l_p = 0
      l_d = 1
      l_fh = 8
      l_q = 1
      l_bp = 0
      l_bd = 1
      l_bq = 1
      l_mq = 12
      l_out = 0
      do i = 1,3
       l_phi(i) = ZERO
       l_bphi(i) = ZERO
       l_th(i) = ZERO
       l_epsphi = TWO
       l_bth(i) = ZERO
      end do
      l_xl = 0.99d0
      l_m = 36
      l_iqm = 999
      l_ta = ONEHND
      l_maxit = 20
      l_epsiv = 0.001d0
C   LINES OF CODE COMMENTED FOR X-13A-S : 1
C      l_pg = 0
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 1
      l_pg = 1
C   END OF CODE BLOCK 
      l_noadmiss = 1
      l_outNA = 0
      l_stochTD = -1
      l_tramo = 999
      l_crmean = 0
      l_hpcycle = -1
C   LINES OF CODE COMMENTED FOR X-13A-S : 1
C      l_rogtable = 0
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 1
      l_rogtable = 1
C   END OF CODE BLOCK
      l_hplan = MONE
      l_hpPer = MONE
      l_centrregs = 1
c      if ((l_iter.ne.1) .and. (l_iter.ne.3)) then
       l_iter = 0
c      end if
C.. Added by REG on 30 Aug 2005 to initialize l_nfixed
      l_nfixed = 0
      end
C
C*OPENINFILE
C+
C       SUBROUTINE OPENINFILE(FNAME,IFAIL)
C
C THIS SUBROUTINE PROVIDE TO OPEN THE INPUT FILE
C
C    FNAME : CHARACTER*180 the input file name
C    IFAIL : INTEGER the return value are :
C
C                               0 ok
C                               1 an error condition during the open
C                               2 check if file exist failed
C--
      subroutine OPENINFILE(fname,ifail)
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
C.. In/Out Status: Maybe Read, Not Written ..
      character*180 fname
C.. In/Out Status: Not Read, Overwritten ..
      integer ifail
C
C.. External Calls ..
      external OPENDEVICEASIS
C
C ... Executable Statements ...
C
      call OPENDEVICEASIS(fname,5,1,ifail)
      end
C*CLOSEINFILE
C
C       SUBROUTINE CLOSEINFILE()
C
C THIS FUNCTION PROVIDE TO CLOSE THE INPUT DEVICE
C--
      subroutine CLOSEINFILE
C
C.. Implicits ..
      implicit none
C
C.. External Calls ..
      external CLOSEDEVICE
C
C ... Executable Statements ...
C
      call CLOSEDEVICE(5)
      end
C
C
C

cc
c
cc
cc
c
cc
      integer function SerCount()
C 
C.. Implicits .. 
      implicit none
C 
C 
C.. Local Scalars .. 
      include 'stdio.i'
      SerCount = Imeta
      RETURN
      end

c
cc
      character*(*) function GetToken(Line,index)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      integer index
      character*(*) Line
C
C.. Local Scalars ..
      integer i,numtok,intok,stTok,enTok
C
C.. External Functions ..
      integer ISTRLEN
      external ISTRLEN
      numtok = 0
      intok = 0
      stTok = 0
      enTok = 0
      do i=1, ISTRLEN(Line)
       if ((intok .eq. 0) .and. (Line(i:i) .ne. ' ')) then
        numtok = numtok + 1
        intok = 1
        stTok = i
       end if
       if (Line(i:i) .eq. ' ') then
        intok = 0
        enTok = i-1
       end if
       if ((numtok .eq. index) .and. (intok .eq. 0) ) then
        GetToken = Line(stTok:enTok)
        return
       end if
      end do
      if ((numtok .eq. index) .and. (intok .eq. 1) ) then
       GetToken = Line(stTok:ISTRLEN(Line))
       return
      end if
      GetToken = ''
      return
      end
CC
C
CC
      logical function isDouble (Txt)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      character*(*) Txt
C
C.. Local Scalars ..
      real*8 Dnum
      integer iflag
      read (txt,'(f18.0)',iostat=iflag) Dnum
      if (iflag > 0) then
       isDouble = .false.
      else
        isDouble = .true.
      end if
      return
      end
CC
C
CC
      logical function isString (Txt)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      character*(*) Txt
C
C.. Local Scalars ..
      integer ntoken,i
      character token*180
C 
C.. External Functions .. 
      integer GETNUMTOKEN
      logical ISDOUBLE
      character GETTOKEN*180
      external GETNUMTOKEN, GETTOKEN, ISDOUBLE
      ntoken = GETNUMTOKEN(txt)
      do i = 1,ntoken
       token = GETTOKEN(Txt,i)
       if (.not. ISDOUBLE(token)) then
        isString = .true.
        return
       end if
      end do
      isString = .false.
      return
      end

      integer function GetNumToken(Line)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      character*(*) Line
C
C.. Local Scalars ..
      integer i,numtok,intok
C
C.. External Functions ..
      integer ISTRLEN
      external ISTRLEN
      numtok = 0
      intok = 0
      do i=1, ISTRLEN(Line)
       if ((intok .eq. 0) .and. (Line(i:i) .ne. ' ')) then
        numtok = numtok + 1
        intok = 1
       end if
       if (Line(i:i) .eq. ' ') then
        intok = 0
       end if
      end do
      GetNumToken = numtok
      return
      end
cc
c
cc
      integer function RIndex (Txt,subTxt)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      character*(*) Txt,subTxt
C
C.. Local Scalars ..
      integer nTxt,idx,idx0
C
C.. External Functions ..
      integer ISTRLEN
      external ISTRLEN
      nTxt=ISTRLEN(Txt)
      idx = Index(Txt,subTxt)
      idx0=idx
      RIndex=0
      do while ((idx .ne.0 ) .and. (idx0+1 .lt. nTxt))
       idx = Index(Txt(idx0+1:nTxt),subTxt)
       idx0=idx+idx0
      end do
      RIndex=idx0
      return
      end
