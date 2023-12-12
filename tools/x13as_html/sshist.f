C     Last change:  BCM   5 Jan 1999   11:51 am
      SUBROUTINE sshist(Dmax,Nopt,Iagr,Ext,Iext,Eststr,Nstr,Lrange,Lp,
     &                  Ls,Lpspan,Ssdiff)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'ssap.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'error.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'ssap.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'mq3.cmn'
c-----------------------------------------------------------------------
      LOGICAL Lp,Ls,Lrange,Lpspan,first,Ssdiff
      CHARACTER c*(7),Ext*(2),Eststr*(45)
      DOUBLE PRECISION Dmax,xo
      INTEGER i,it,l2,n,n1,n2,Nopt,Iagr,Nstr,Iext,nmq
      DIMENSION Dmax(MXLEN,NEST),xo(MXLEN)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      INTEGER nblank
      EXTERNAL dpeq,nblank
c-----------------------------------------------------------------------
      l2=Sslen2-1
      it=0
      n2=Ic
      first=T
      DO i=Ic,l2
       IF(dpeq(Dmax(i,Nopt),DNOTST))THEN
        it=it+1
        IF(first)n2=n2+1
       ELSE IF(mod(i,Nsea).eq.2.and.Nopt.eq.2.and.
     &         dpeq(Dmax(i,Nopt),0D0))THEN
        it=it+1
        IF(first)n2=n2+1
       ELSE
        n=i-it-Ic+1
        xo(n)=Dmax(i,Nopt)
        IF(first)first=F
       END IF
      END DO
c-----------------------------------------------------------------------
      n1=1
      IF(Iagr.eq.6)n1=2
c-----------------------------------------------------------------------
      IF(Muladd.eq.1.and.Ssdiff)THEN
       CALL histx(xo,n,Muladd,Nsea,Iyr,n2,Iext,Ssdiff,Lp,Ls,
     &            'Maximum Absolute Differences across spans',41)
      ELSE
       CALL histx(xo,n,Muladd,Nsea,Iyr,n2,Iext,F,Lp,Ls,
     &            'Maximum Percent Differences across spans ',40)
      END IF
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Compute and print out "tail" histogram.
c-----------------------------------------------------------------------
      IF((.not.Ssdiff).and.Lp.and.Lrange)THEN
       nmq=nblank(Moqu)
c     ------------------------------------------------------------------
       CALL mkTableTag(Mt1,'w70','@')
       CALL mkCaption(Mt1,'  Breakdown of the maximum percentage '//
     &                'differences of the '//Eststr(1:Nstr)//
     &                '  for flagged '//Moqu(1:nmq)//'s.')
c     ------------------------------------------------------------------
       c=' '
       IF(Lpspan)c(4:6)=Ch(Nopt)//'  '
       DO i=1,3
        CALL writTag(Mt1,'<tr>')
        IF(Lpspan)WRITE(c(3:3),1010)i
        WRITE(Mt1,1020)c,Cut(Nopt,i),Cut(Nopt,i+1),Kount(Nopt,i)
        CALL writTag(Mt1,'</tr>')
       END DO
c     ------------------------------------------------------------------
       IF(Lpspan)c(3:3)='4'
       CALL writTag(Mt1,'<tr>')
       WRITE(Mt1,1030)c,Cut(Nopt,4),Kount(Nopt,4)
       CALL writTag(Mt1,'</tr>')
       CALL writTag(Mt1,'</table>')
       CALL mkPOneLine(Mt1,'@','&nbsp;')
      END IF
      IF((.not.Ssdiff).and.Ls)THEN
       DO i=1,3
        WRITE(Nform,1040)Ext(1:n1),i,Cut(Nopt,i),Cut(Nopt,i+1),
     &                   Kount(Nopt,i)
       END DO
       WRITE(Nform,1050)Ext(1:n1),Cut(Nopt,4),Kount(Nopt,4)
      END IF
c-----------------------------------------------------------------------
 1010 FORMAT(i1)
 1020 FORMAT(' <td class="head">',a,'</td><td> Greater than or ',
     &         'equal to ',f4.1,'% but less than ',f4.1,'%</td><td>',
     &         1x,i3,'</td>')
 1030 FORMAT(' <td class="head">',a,'</td><td> Greater than or equal ',
     &        'to ',f4.1,'%</td><td>',1x,i3,'</td>')
 1040 FORMAT('s3.',a,'.thist',i1,':',2x,f4.1,2x,f4.1,2x,i3)
 1050 FORMAT('s3.',a,'.thist4:',2x,f4.1,8x,i3)
c-----------------------------------------------------------------------
      RETURN
      END
