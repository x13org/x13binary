c-----------------------------------------------------------------------
c     Table pointer variables used for prttbl and savtbl are of the form
c L<spec><type> where the spec codes are 
c-----------------------------------------------------------------------
c x11regress     XRG or XR
c-----------------------------------------------------------------------
c and the types are
c-----------------------------------------------------------------------
c extreme values for X-11 regression                IRX
c X-11 regression model                             XRG
c trading day factors                               TDF
c holiday factors                                   HLF
c combined calendar factors                         CLF
c automatic outlier header for X-11 regression      OHD
c automatic outlier iterations for X-11 regression  OIT
c automatic outlier tests for X-11 regression       OTT
c final X-11 regression matrix                      XMX
c-----------------------------------------------------------------------
      INTEGER LXRGA4,LXRIRX,LXRXRG,LXRTDF,LXRTDC,LXRHLF,LXRCLF,LXRCLC,
     &        LXROHD,LXROIT,LXROTT,LXROFT,LXRXMX,LXRXCM,LXAICT
      PARAMETER(
     &          LXRGA4=218,LXRIRX=219,LXRXRG=221,LXRTDF=223,LXRTDC=225,
     &          LXRHLF=227,LXRCLF=229,LXRCLC=231,LXROHD=233,LXROIT=234,
     &          LXROTT=235,LXROFT=236,LXRXMX=237,LXRXCM=238,LXAICT=239)
