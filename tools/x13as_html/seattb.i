c-----------------------------------------------------------------------
c     Table pointer variables used for prttbl and savtbl are of the form
c L<spec><type> where the spec codes are 
c-----------------------------------------------------------------------
c Seats      SET, ST
c-----------------------------------------------------------------------
c and the types are
c-----------------------------------------------------------------------
c spectrum of sa series         S1
c spectrum of irregular         S2
c-----------------------------------------------------------------------
      INTEGER LSETRN,LSETAC,LSESEA,LSEPSS,LSEIRR,LSEPSI,LSESA,LSESAC,
     &        LSETRA,LSEPSC,LSECAF,LSEPSA,LSEFCD,LSES18,LSESEB,LSEWKF,
     &        LSEMDC,LSEPIN,LSESGS,LSESGC,LSETGS,LSETGC,LSESDC,LSETDC,
     &        LSESFS,LSESFC,LSETFS,LSETFC,LSEDOR,LSEDSA,LSEDTR,LSESSM,
     &        LSECYC,LSELTT,LSESSF,LSESSA,LSESTR,LSESCY,LSESE2,LSESE3,
     &        LSESTL
      PARAMETER(
     &          LSETRN=349,LSETAC=350,LSESEA=351,LSEPSS=352,LSEIRR=353,
     &          LSEPSI=354,LSESA=355,LSESAC=356,LSETRA=357,LSEPSC=358,
     &          LSECAF=359,LSEPSA=360,LSEFCD=361,LSES18=366,LSESEB=367,
     &          LSEWKF=368,LSEMDC=369,LSEPIN=370,LSESGS=374,LSESGC=375,
     &          LSETGS=376,LSETGC=377,LSESDC=378,LSETDC=379,LSESFS=380,
     &          LSESFC=381,LSETFS=382,LSETFC=383,LSEDOR=384,LSEDSA=385,
     &          LSEDTR=386,LSESSM=387,LSECYC=388,LSELTT=389,LSESSF=390,
     &          LSESSA=391,LSESTR=392,LSESCY=393,LSESE2=394,LSESE3=395,
     &          LSESTL=396)
