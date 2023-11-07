c-----------------------------------------------------------------------
c	PLEN is the integer PARAMETER  for the maximum length of a
c series.
c-----------------------------------------------------------------------
c Name  Type Description
c-----------------------------------------------------------------------
c PFCST   i  PARAMETER for the maximum number of forecasts
c POBS    i  PARAMETER for the maximum length of the series 
c PLEN    i  PARAMETER for the maximum length of the series + back and
c             forecasts
c PYRS    i  PARAMETER for the maximum number of years in the series +
c             back and forecasts
c PYR1    i  PARAMETER for the maximum number of years in the series 
c PTD     i  PARAMETER for the number of types of trading day factors
c             (based on lenght of month, starting period)
c     PSP - maximum length of seasonal period  (formerly in model.cmn)
c-----------------------------------------------------------------------
      INTEGER POBS,PLEN,PFCST,PYR1,PYRS,PSRSCR,PTD,PSP
      PARAMETER(PSP=12,PFCST=10*PSP,PYR1=65,POBS=PYR1*PSP,PYRS=PYR1+10,
     &          PLEN=POBS+(2*PFCST),PSRSCR=79,PTD=28)
