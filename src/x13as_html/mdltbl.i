c-----------------------------------------------------------------------
c     Table pointer variables used for prttbl and savtbl are of the form
c L<spec><type> where the spec codes are 
c-----------------------------------------------------------------------
c series         SER
c span           SPN
c transform      TRN
c regression     REG
c identification IDN or ID
c arima          ARM
c automdl        AU
c estimate       EST
c outlier        OTL
c check          CHK or CK
c forecast       FOR
c-----------------------------------------------------------------------
c and the types are
c-----------------------------------------------------------------------
c header                          HD
c span                            SP
c plot of original series        A1P
c spec file                       IN
c spectrum of original series     S0
c files to be stored/saved        SV
c prior adjustment                PA
c prior adjusted series, untrans  A3
c transformed series              DT
c trend of prior adjusted series A10
c regression matrix               DT
c regression aic test for td     ATD
c acf                            ACF
c acf plot                       ACP
c pacf                           PCF
c pacf plot                      PCP
c automatic model header         HDR
c automatic model test           MDL
c automatic model choice         MCH
c options                         OP
c iterations                      IT
c iteration errors                IE
c model                           MD
c reg correlation matrix          CM
c estimates                       ES
c arma corr matrix                AM
c statistics                      ST
c formulae                        FM
c arma roots                      RT
c regression effects              RE
c model residuals                 RS
c outlier header                  HD
c outlier iterations              IT
c outlier t-tests                 TS
c temporary ls tests              TL
c acf                            ACF
c acf plot                       ACP
c pacf                           PCF
c pacf plot                      PCP
c histogram                      HST
c transformed scale               TS # or tests
c variances                       VR
c original scale                  OS
c-----------------------------------------------------------------------
      INTEGER LSRSHD,LSRSSP,LSRA1P,LSRSIN,LSRSSV,LSRSMV,LSRA18,LSRA19,
     &        LSRSB1,LSRB1P,LTRSCN,LTRACP,LTRNPA,LTRA2P,LTRA2T,LTRNA3,
     &        LTRA3P,LTRA4D,LTRA4P,LTRNDT,LTRAIC,LREGDT,LRGATS,LRGOTL,
     &        LREGAO,LREGLC,LREGTC,LREGSO,LREGTD,LRGHOL,LRGUSR,LRGA10,
     &        LRGA13,LRGCTS,LRGTDW,LIDACF,LIDACP,LIDPCF,LIDPCP,LIDRGC,
     &        LAUHDR,LAUURT,LAUMCH,LAUURM,LAUMDL,LAUB5M,LAUOTH,LAUOTI,
     &        LAUOTT,LAUOFT,LAUDFT,LAUFLB,LAUFNT,LAXHDR,LAXHDB,LAXMDL,
     &        LAXMCH,LESTOP,LESTIT,LESTIE,LESTMD,LESTCM,LESTES,LESTAM,
     &        LESTST,LESTFM,LESTRT,LESTRE,LESTRS,LESRRS,LESAFC,LOTLHD,
     &        LOTLIT,LOTLTS,LOTLTL,LOTLFT,LCKACF,LCKPCF,LCKAC2,LCKHST,
     &        LCKNRM,LCKDW,LCKFRT,LCKIPC,LFORTS,LFORVR,LFOROS,LFORTB,
     &        LFORBC
      PARAMETER(
     &          LSRSHD=1,LSRSSP=2,LSRA1P=3,LSRSIN=4,LSRSSV=5,LSRSMV=6,
     &          LSRA18=7,LSRA19=8,LSRSB1=9,LSRB1P=10,LTRSCN=11,
     &          LTRACP=12,LTRNPA=13,LTRA2P=14,LTRA2T=15,LTRNA3=16,
     &          LTRA3P=17,LTRA4D=18,LTRA4P=19,LTRNDT=20,LTRAIC=21,
     &          LREGDT=22,LRGATS=23,LRGOTL=24,LREGAO=25,LREGLC=26,
     &          LREGTC=27,LREGSO=28,LREGTD=29,LRGHOL=30,LRGUSR=31,
     &          LRGA10=32,LRGA13=33,LRGCTS=34,LRGTDW=35,LIDACF=36,
     &          LIDACP=37,LIDPCF=38,LIDPCP=39,LIDRGC=40,LAUHDR=41,
     &          LAUURT=42,LAUMCH=43,LAUURM=44,LAUMDL=45,LAUB5M=46,
     &          LAUOTH=47,LAUOTI=48,LAUOTT=49,LAUOFT=50,LAUDFT=51,
     &          LAUFLB=52,LAUFNT=53,LAXHDR=54,LAXHDB=55,LAXMDL=56,
     &          LAXMCH=57,LESTOP=58,LESTIT=59,LESTIE=60,LESTMD=61,
     &          LESTCM=62,LESTES=63,LESTAM=64,LESTST=65,LESTFM=66,
     &          LESTRT=67,LESTRE=68,LESTRS=69,LESRRS=70,LESAFC=71,
     &          LOTLHD=72,LOTLIT=73,LOTLTS=74,LOTLTL=75,LOTLFT=76,
     &          LCKACF=77,LCKPCF=79,LCKAC2=81,LCKHST=83,LCKNRM=84,
     &          LCKDW=85,LCKFRT=86,LCKIPC=87,LFORTS=88,LFORVR=89,
     &          LFOROS=90,LFORTB=91,LFORBC=92)
