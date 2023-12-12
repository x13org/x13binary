c-----------------------------------------------------------------------
c     Table pointer variables used for prttbl and savtbl are of the form
c L<spec><type> where the spec codes are 
c-----------------------------------------------------------------------
c composite      CMP, CP
c-----------------------------------------------------------------------
c and the types are
c-----------------------------------------------------------------------
c aggregate series                  A1
c aggheader                         AH
c aggtest                           AT
c indirect d8                       D8
c indirect d9                       D9
c indirect seasonal                 SF
c indirect final seasonal diff.    FSD
c indirect seasonally adjusted      SA
c indirect trend                    TRN
c indirect irregular                IRR
c indirect e1                       E1
c indirect e2                       E2
c indirect e3                       E3
c indirect change original srs      E5
c indirect change sa                E6
c indirect change adjusted sa       E6A
c indirect change rounded sa        E6R
c indirect mcd moving avg           F1
c indirect x11 diagnostics          F2
c indirect q stats                  F3
c indirect yearly totals            E4
c indirect ftest d8                 D8F
c indirect moving seasonality ratio D9A
c indirect residual seas f-test     RSF
c indirect adjusted fin seas adj    SAA
c indirect rounded fin seas adj     RND
c original series                   A1P
c series vrs. ind sa series         E0
c ratios of ind series              R1
c ratios of ind sa series           R2
c ind final seasonal factors        SP
c ind final seasonally adj.         AP
c ind final trend component         TP    
c ind final irregular component     IP
c-----------------------------------------------------------------------
      INTEGER LCMPA1,LCMPA3,LCMPB1,LCPB1P,LCPA18,LCPA19,LCMPAH,LCMPAT,
     &        LCMPD8,LCMPD9,LCMPSF,LCPIPS,LCPFSD,LCMPSA,LCPTRN,LCPIRR,
     &        LCPIPI,LCMPE1,LCMPE2,LCMPE3,LCMPE5,LCPE5P,LCMPE6,LCPE6P,
     &        LCPE6A,LCP6AP,LCPE6R,LCP6RP,LCMPE7,LCPE7P,LCMPE8,LCPE8P,
     &        LCPE11,LCPE18,LCPEEB,LCMPF1,LCMPF2,LCMPF3,LCMPE4,LCPD8F,
     &        LCPD9A,LCPRSF,LCPSAA,LCPRND,LCPA1P,LCMPE0,LCMPR1,LCMPR2,
     &        LCMPSP,LCMPAP,LCMPTP,LCMPIP,LCPILS,LCPIAO,LCPFCF,LCPCAF,
     &        LCPIPA,LCPCRI,LCPRRI,LCPFFC
      PARAMETER(
     &          LCMPA1=289,LCMPA3=290,LCMPB1=291,LCPB1P=292,LCPA18=293,
     &          LCPA19=294,LCMPAH=295,LCMPAT=296,LCMPD8=297,LCMPD9=298,
     &          LCMPSF=299,LCPIPS=300,LCPFSD=301,LCMPSA=302,LCPTRN=303,
     &          LCPIRR=304,LCPIPI=305,LCMPE1=306,LCMPE2=307,LCMPE3=308,
     &          LCMPE5=309,LCPE5P=310,LCMPE6=311,LCPE6P=312,LCPE6A=313,
     &          LCP6AP=314,LCPE6R=315,LCP6RP=316,LCMPE7=317,LCPE7P=318,
     &          LCMPE8=319,LCPE8P=320,LCPE11=321,LCPE18=322,LCPEEB=323,
     &          LCMPF1=324,LCMPF2=325,LCMPF3=326,LCMPE4=327,LCPD8F=328,
     &          LCPD9A=329,LCPRSF=330,LCPSAA=331,LCPRND=332,LCPA1P=333,
     &          LCMPE0=334,LCMPR1=335,LCMPR2=336,LCMPSP=337,LCMPAP=338,
     &          LCMPTP=339,LCMPIP=340,LCPILS=341,LCPIAO=342,LCPFCF=343,
     &          LCPCAF=344,LCPIPA=345,LCPCRI=346,LCPRRI=347,LCPFFC=348)
