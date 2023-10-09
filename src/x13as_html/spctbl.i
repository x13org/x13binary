c-----------------------------------------------------------------------
c     Table pointer variables used for prttbl and savtbl are of the form
c L<spec><type> where the spec codes are 
c-----------------------------------------------------------------------
c spectrum      SPC, SP
c-----------------------------------------------------------------------
c and the types are
c-----------------------------------------------------------------------
c spectrum of original series       S0
c spectrum of residuals             RS
c spectrum, diff seas. adj. srs     S1
c spectrum, modified irregular      S2
c spectrum of sa series             S1S
c spectrum of mod irregular         S2S
c spectrum of extended residuals    ERS
c spectrum of ind sa series         S1I
c spectrum of ind mod irregular     S2I
c spectrum of composite series      S0C
c-----------------------------------------------------------------------

      INTEGER LSPCS0,LSPCRS,LSPCS1,LSPCS2,LSPS1S,LSPS2S,LSPERS,LSPS1I,
     &        LSPS2I,LSPS0C,LSPTS0,LSPTRS,LSPTS1,LSPTS2,LSPT1S,LSPT2S,
     &        LSPTER,LSPT1I,LSPT2I,LSPT0C,LSPCQS,LSPQSI,LSPCTP,LSPCQC,
     &        LSPNPA,LSPNPI
      PARAMETER(
     &          LSPCS0=93,LSPCRS=94,LSPCS1=95,LSPCS2=96,LSPS1S=97,
     &          LSPS2S=98,LSPERS=99,LSPS1I=100,LSPS2I=101,LSPS0C=102,
     &          LSPTS0=103,LSPTRS=104,LSPTS1=105,LSPTS2=106,LSPT1S=107,
     &          LSPT2S=108,LSPTER=109,LSPT1I=110,LSPT2I=111,LSPT0C=112,
     &          LSPCQS=113,LSPQSI=114,LSPCTP=115,LSPCQC=116,LSPNPA=117,
     &          LSPNPI=118)
