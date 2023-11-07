*      subroutine FrameHead(nio,idx,contents)
*cc
*c Create an html file with two frame and index and the output
*cc
*C
*C.. Implicits ..
*      implicit none
*C
*C.. Formal Arguments ..
*       integer nio
*       character*(*) idx,contents
*       integer ISTRLEN
*       external ISTRLEN
*       call writeDoctype(nio,3)
*       write(nio,'(''<html lang="en-US">'')')
*       write (nio,'(''<head>'')')
*       write(nio,'(''<META HTTP-EQUIV="Content-Type" '',
*     $            ''CONTENT="text/html; charset=iso-8859-1">'')')
*       write(nio,'(''<meta name="lang" content="en-US" >'')')
*       write(nio,'(''<title>FrameSet File</title>'')') 
*       write (nio,'(''</HEAD>'')')
*       write (nio,'(''<FRAMESET COLS="15%,85%">'')')
*       write (nio,'(''<FRAME  NAME="index" title="index frame" '',
*     &              ''SRC="'',A,''">'')') 
*     &             idx(1:istrlen(idx))
*c       write (nio,'(''MARGINWIDTH=1 MARGINHEIGHT=1 SCROLLING=auto>'')')
*       write (nio,'(''<FRAME NAME="output" title="output" '',
*     &              ''SRC="'',a,''">'')')
*     &             contents(1:istrlen(contents))
*c       write (nio,'(''MARGINWIDTH=10 MARGINHEIGHT=5 SCROLLING=auto>'')')
*       write (nio,'(''</FRAMESET></HTML>'')')
*      return
*      end
cc
c
cc
*      subroutine IdxHead(nio,contents)
*cc
*c Create an html file with two frame and index and the output
*cc
*C
*C.. Implicits ..
*      implicit none
*C
*C.. Formal Arguments ..
*       integer nio
*       character*(*) contents
*       integer ISTRLEN
*       external ISTRLEN
*       include 'html.i'
*       call WriteDoctype (nio,2)
*       write(nio,'(''<html lang="en-US">'')')
*       write (nio,'(''<head>'')')
*       write(nio,'(''<META HTTP-EQUIV="Content-Type" '',
*     $            ''CONTENT="text/html; charset=iso-8859-1">'')')
*       write(nio,'(''<meta name="lang" content="en-US" >'')')
*       write (nio,'(''<title> SEATS OUTPUT INDEX</title>'')')
*       call WriteCSS (nio,9)
*       write (nio,'(''</head><body>'')') 
*       write (nio,'(''<p class="titulo">Seats output</p>'')') 
*       write (nio,'(''<ul>'')')
*             write (nio,'(''<li><a HREF="'',a,
*     &             ''#home" target="output">Home</a>'')')
*     &             contents(1:istrlen(contents))
*       nul = 1
*       ToutFileName=contents
*      return
*      end
cc
c
cc
*      subroutine AddIdx(nidxio,noutio,contents,href,tab,tabOrder)
*cc
*c Create an html file with two frame and index and the output
*cc
*C
*C.. Implicits ..
*      implicit none
*C
*C.. Parameters ..
*      integer nidevice
*      parameter (nidevice = 71)
*C
*C.. Formal Arguments ..
*       integer nidxio,noutio,tab,tabOrder
*       character*(*) contents,href,locont*280
*c locals
*       character sf*60
*c
*       intrinsic ABS
*       integer ISTRLEN
*       external ISTRLEN
*       include 'html.i'
*       locont = contents
*       call STRCAP(locont)
*       if ((nul .eq. 2) .and. (tab .eq. 1)) then
*        write (nidxio,'("</ul>")')
*        nul = nul - 1
*       end if
*       if (abs(taborder).lt.10) then 
*        sf='(''<p><a NAME="'',a,''" tabindex="'',i1,''"></a></p>'')'
*       else if (abs(tabOrder).lt.100) then
*        sf='(''<p><a NAME="'',a,''" tabindex="'',i2,''"></a></p>'')'
*       else if (abs(tabOrder).lt.1000)then
*        sf='(''<p><a NAME="'',a,''" tabindex="'',i3,''"></a></p>'')'
*       else
*        sf='(''<p><a NAME="'',a,''" tabindex="'',i4,''"></a></p>'')'
*       end if
*       write(noutio,sf) href(1:istrlen(href)),tabOrder
*       write (nidxio,'("<li>")')
*       write(nidxio,'(''<A HREF="'',a,''#'',a,''" target="output">'',a,
*     &       ''</A>'')') Toutfilename(1:istrlen(Toutfilename)),
*     &        href(1:istrlen(href)) ,locont(1:istrlen(locont))
*       if (tab .eq. 1) then
*        write (nidxio,'("<ul>")')
*        nul = nul + 1
*       else if (tab .eq. -1) then
*        write (nidxio,'("</ul></li>")')
*        nul = nul - 1
*       else 
*        write (nidxio,'("</li>")')        
*       end if
*       if (nidevice .ne. nidxio) then
*        nul = 1
*       end if
*      return
*      end
*cc
*c
*cc
*cc
*c
*cc
*      subroutine AddIdx2(nidxio,noutio,contentsIdx,contentsMain,
*     &                   tab,level)
*cc
*c Create an html file with two frame and index and the output
*cc
*C
*C.. Implicits ..
*      implicit none
*C
*C.. Parameters ..
*      integer nidevice
*      parameter (nidevice = 71)
*C
*C.. Formal Arguments ..
*      integer nidxio,noutio,tab,level
*      character*(*) locont*280,locont2*280
*      character*(*) contentsMain,contentsIdx
*c
*      intrinsic ABS
*      integer ISTRLEN
*      external ISTRLEN
*      include 'html.i'
*      include 'indhtml.i'
*c
*c
*       if (noutio.ne.16) return
*       locont = contentsIdx
*       locont2=contentsMain
*       iId=iId+1
*       iTab=iTab+1
*       call STRCAP(locont)
*       if ((nul .eq. 2) .and. (tab .eq. 1)) then
*        write (nidxio,'("</ul>")')
*        nul = nul - 1
*       end if
*       if ((level.gt.1) .and. (level.lt.6)) then        
*        if (contentsmain.ne." ") then        
*         if ((level.eq.2).and.(iAkey.lt.9)) then
*          iAkey=iaKey+1
*          write(noutio,'(''<h'',i1,''><a name="'',i4,
*     &      ''" accesskey="'',i1,''" tabindex='',i4,''>'',
*     $       a,''</A></h'',i1,''>'')')
*     &     level,iId,iAkey,iTab,locont2(1:istrlen(locont2)),level
*         else 
*          write(noutio,'(''<h'',i1,''><a name="'',i4,''" tabindex='',
*     &    i4,''>'',a,''</a></h'',i1,''>'')') level,iId,iTab,
*     &    locont2(1:istrlen(locont2)),level       
*         end if
*        else
*         write(noutio,'(''<h'',i1,''><a name="'',i4,''" tabindex='',
*     &   i4,''>'',a,''</A></h'',i1,''>'')') level,iId,iTab,
*     &   locont(1:istrlen(locont)),level         
*        end if
*       else
*c                  tabla
*        if (level.eq.8) then
*         write(noutio,'(''<caption><a name="'',i4,''" tabindex='',i4,
*     &  ''>'',A,''</a></caption>'')') iId,iTab,
*     &  locont2(1:istrlen(locont2))    
*        else if (level.eq.9) then
*         write(noutio,'(''<a name="'',i4,''" tabindex='',i4,
*     &  ''>'',A,''</a>'')') iId,iTab,locont2(1:istrlen(locont2))
*        else 
*         write(noutio,'(''<p><A NAME="'',i4,''" tabindex='',i4,
*     &  ''></A></p>'')') iId,iTab
*        end if 
*       end if
*       write (nidxio,'("<li>")')
*       write(nidxio,'(''<A HREF="'',a,''#'',i4,''" target="output">'',
*     &       a,''</A>'')') Toutfilename(1:istrlen(Toutfilename)),
*     &                  iId ,locont(1:istrlen(locont))           
*       if (tab .eq. 1) then
*        write (nidxio,'("<ul>")')
*        nul = nul + 1
*       else if (tab .eq. -1) then
*        write (nidxio,'("</ul></li>")')
*        nul = nul - 1
*       else 
*        write (nidxio,'("</li>")')        
*       end if
*       if (nidevice .ne. nidxio) then
*        nul = 1
*       end if
*      return
*      end
c
c
cc
*      subroutine EndTab(nio)
*cc
*c Create an html file with two frame and index and the output
*cc
*C
*C.. Implicits ..
*      implicit none
*C
*C.. Formal Arguments ..
*       integer nio
*       integer ISTRLEN
*       external ISTRLEN
*       write (nio,'(''</UL>'')')
*      return
*      end
*cc
*c
*cc 
*      subroutine Tail(nio)
*C
*C.. Implicits ..
*       implicit none
*C
*C.. Formal Arguments ..
*       integer nio
*       write (nio,'(''</BODY>'')')
*       write (nio,'(''</HTML>'')')
*      return
*        end
cc
c
cc
*      subroutine OutHead(nio,longtitle)
*C
*C.. Implicits ..
*       implicit none
*C
*C.. Formal Arguments ..
*       integer nio
*       character*(*) longtitle
*       write (nio,'(''<HTML>'')')
*       write (nio,'(''<HEAD><TITLE>SEATS OUTPUT</TITLE>'')')
*       write (nio,'(''</HEAD>'')')
*       write (nio,'(''<BODY bgcolor="white">'')')
*       write (nio,'(''<A NAME="home"> </A>'')')
*       write (nio,'(''<H1 ALIGN="CENTER"> Seats Output for series : '',
*     &             a,''</H1>'')') Longtitle
*      return
*      end
cc
c
cc
C
C htmltable 3 reemplazara htmltable cuando finalicen las pruebas
*      subroutine HTMLTABLE(TabTitle,data)
*C
*C.. Implicits ..
*      implicit none
*C
*C.. Formal Arguments ..
*C.. In/Out Status: Maybe Read, Not Written ..
*      real*8 data(*)
*C.. In/Out Status: Read, Overwritten ..
*      character*(*) TabTitle
*C
*C.. Local Arrays ..
*      character fdecp1(7)*8,fn1(0:12)*8,fn2(12)*8,mth(12)*34,srt(11)*4,
*     $          srt0(4)*4,srt1(6)*4,wrt0(9)*37,wrt2(8)*46,wrt99(7)*30
*      integer*4 decp
*C.. Intrinsic Functions ..
*      intrinsic ABS, INT, LOG10
*      include 'sform.i'
*      include 'stream.i'
*C
*C.. Data Declarations ..
*      data mth/
*     $     '<abbr title="January">JAN</abbr>  ',
*     $     '<abbr title="February">FEB</abbr> ',
*     $     '<abbr title="March">MAR</abbr>    ',
*     $     '<abbr title="April">APR</abbr>    ',
*     $     'MAY                               ',
*     $     '<abbr title="June">JUN</abbr>     ',
*     $     '<abbr title="July">JUL</abbr>     ',
*     $     '<abbr title="August">AUG</abbr>   ',
*     $     '<abbr title="September">SEP</abbr>',
*     $     '<abbr title="October">OCT</abbr>  ',
*     $     '<abbr title="November">NOV</abbr> ',
*     $     '<abbr title="December">DEC</abbr> '/
*      data srt/
*     $     '1ST','2ND','3RD','4TH','5TH','6TH','7TH','8TH','9TH','10TH',
*     $     '11TH'/
*      data srt0/'1ST','2ND','1ST','2ND'/
*      data srt1/'1ST','2ND','3RD','1ST','2ND','3RD'/
*      data wrt2/'(''<tr><th scope="row">'',I4,''</th>'',',
*     &          'N2',
*     &          '(''<td></td>''),',
*     &          'N1',
*     &          '(''<td>'',F10',
*     &          '.DECP',
*     &          ',''</td>'')',
*     &          ',''</tr>'')'/
*      data wrt0/
*     &     '(''<tr><th scope="row">'',I4,',
*     &     '''-'',I4,''</Th>'',',
*     &     'N2',
*     &     '(''<td></td>''),',
*     &     'N1',
*     &     '(''<td>'',F10',
*     &     '.DECP',
*     &     ',''</td>'')',
*     &     '''</tr>'')'/
*      data wrt99/'(''<tr>'',',
*     &           '''<th scope="col">YEAR</TH>''',
*     &           'N2',
*     &           '(''<th scope="col">''',
*     &             'A4',
*     &           '''</th>'')',
*     &           '''</tr>'')'/
*      data fdecp1/'.0','.1','.2','.3','.4','.5','.6'/
*      data fn1/'0','1','2','3','4','5','6','7','8','9','10','11','12'/
*      data fn2/
*     $     '2','12','22','32','42','52','62','72','82','092','102','112'
*     $     /
*C.. Local Scalars ..
*      integer i,i1,i2,ifact,j,jfact,kfreq,ndecp,nnper,ny,nyr
*      integer*4 yr
*      real*8 sum,zz
*C
*C ... Executable Statements ...
*C
*      decp = 3
*      write (Nio,'(''<table class="htmtab1">'')')
*      write (Nio,'(''<caption class="htmtab1">'',a)') TabTitle
*      kfreq = Nfreq
*      if (kfreq .lt. 4) then
*       if (Nfreq .eq. 3) then
*        kfreq = 6
*       else
*        kfreq = 4
*       end if
*      end if
*      nnper = Nper
*      if (Nper .gt. Nfreq) then
*       Nper = Nfreq
*      end if
*      ndecp = decp
*      if (decp .ge. 6) then
*       decp = 6
*      end if
*      ifact = 0
*      zz = LOG10(ABS(data(1))+.0000000001d0)
*      sum = ABS(zz)
*      do i = 2,Nz
*       if (zz .gt. 0.0d0) then
*        sum = 0.0d0
*        goto 5000
*       else
*        zz = LOG10(ABS(data(i))+.0000000001d0)
*        if ((ABS(zz).lt.sum) .and. (zz.lt.0.0d0)) then
*         sum = ABS(zz)
*        end if
*       end if
*      end do
* 5000 if (zz .gt. 0.0d0) then
*       sum = 0.0d0
*      end if
*      if (sum .gt. 1.0d0) then
*       ifact = INT(sum)
*       if (ifact .gt. 6) then
*        ifact = 6
*       end if
*       if (ifact .gt. 0) then
*        write (Nio,'(''<br>X  10.0D'',I2)') -ifact
*       end if
*      end if
*      jfact = 0
*      zz = LOG10(ABS(data(1))+.0000000001d0)
*      sum = zz
*      do i = 2,Nz
*       zz = LOG10(ABS(data(i))+.0000000001d0)
*       if ((zz.gt.sum) .and. (zz.gt.0.0d0)) then
*        sum = zz
*       end if
*      end do
*      if (sum .gt. 4.0d0) then
*       jfact = INT(sum) - 2
*       if (jfact .gt. 0) then
*        write (Nio,'(''<br>X  10.0D'',I2)') jfact
*       end if
*      end if
*      write (Nio,'(''</caption>'')')
*      write(nio,'(''<thead>'')')
*      yr = Nyer
*      if (Nfreq .eq. 12) then
* 7000  format ('<tr><th scope="col">YEAR</th>',2x,
*     &         12('<th scope="col">',a,'</th>'),'</tr>')
*       write (Nio,7000) (mth(I), I = 1,12)
*      else if (Nfreq .eq. 3) then
* 7001  format ('<tr><th scope="col">YEAR</th>',2x,
*     &         6('<th scope="col">',a4,'</th>'),'</tr>')
*       write (Nio,7001) (srt1(i), i = 1,6)
*      else if (Nfreq .eq. 2) then
* 7002  format ('<tr><th scope="col">YEAR</th>',2x,
*     &         4('<th scope="col">',a4,'</th>'),'</tr>')
*       write (Nio,7002) (srt0(i), i = 1,4)
*      else if (Nfreq .eq. 1) then
*       write (Nio,7002) (srt(i), i = 1,4)
*      else
*       wrt99(3) = fn1(Nfreq)
*       write (Nio,wrt99) (srt(i), i = 1,Nfreq)
*      end if
*      write(nio,'(''</thead>'')')
*      write(nio,'(''<tbody>'')')
*      nyr = (Nz-(Nfreq-Nper+1)) / Nfreq
*      ny = (Nz-(Nfreq-Nper+1)) - nyr*Nfreq
*      if (ny .ne. 0) then
*       nyr = nyr + 1
*      end if
*      nyr = nyr + 1
*      wrt2(6) = fdecp1(decp+1)
*      do i = 1,nyr
*       i1 = (i-1)*kfreq - (Nper-2)
*       i2 = i*kfreq - (Nper-1)
*c       if (i2 .ge. Nz) then
*c        i2 = Nz
*c       end if
*       if (Nfreq .ge. 4) then
*        wrt2(2) = fn2(1)
*        wrt2(4) = fn1(kfreq)
*       else
*        wrt0(3) = fn2(1)
*        wrt0(5) = fn1(kfreq)
*        wrt0(7) = fdecp1(decp+1)
*       end if
*       if (i .eq. 1) then
*        if (Nfreq .ge. 4) then
*         wrt2(4) = fn1(kfreq-Nper+1)
*         wrt2(2) = fn1(Nper-1)
*        else
*         wrt0(3) = fn1(Nper-1)
*         wrt0(5) = fn1(kfreq-Nper+1)
*        end if
*        i1 = 1
*       else
*        wrt0(4) = ' '
*        wrt0(3) = ' '
*        wrt2(2) = ' '
*        wrt2(3) = ' '
*        if (i2 .gt. nz) then
*         wrt2(4) =fn1(nz-i1+1)
*         write(wrt2(8) ,'(",",i2,A)') 
*     &         kfreq-nz+i1-1, '("<td></td>"),"</tr>")'')'
*         i2=nz 
*        end if
*       end if
*       if (Nper .eq. 1) then
*        wrt0(4) = ' '
*        wrt0(3) = ' '
*        wrt2(2) = ' '
*        wrt2(3) = ' '
*       end if
*       if (Nfreq .lt. 4) then
*        if (ifact .gt. 0) then
*         write (Nio,wrt0)
*     $         yr, (yr+kfreq/Nfreq-1),
*     $         (data(j)*(10.0d0**ifact), j = i1,i2)
*        else
*         write (Nio,wrt0)
*     $         yr, (yr+kfreq/Nfreq-1),
*     $         (data(j)*(10.0d0**(-jfact)), j = i1,i2)
*        end if
*       else if (ifact .gt. 0) then
*        write (Nio,wrt2) yr, (data(j)*(10.0d0**ifact), j = i1,i2)
*       else
*        write (Nio,wrt2) yr, (data(j)*(10.0d0**(-jfact)), j = i1,i2)
*       end if
*       if (Nfreq .lt. 4) then
*        yr = yr + kfreq/Nfreq
*       else
*        yr = yr + 1
*       end if
*       if (i2 .ge. Nz) goto 5001
*      end do
* 5001 decp = ndecp
*      Nper = nnper
*      write (nio,'(''</tbody>'')')
*      write (Nio,'(''</TABLE>'')')
*      wrt2(3) = '(''<TD></TD>''),'
*      wrt0(4) = '(''<TD></TD>''),'
*      wrt2(4)='N1'
*      wrt2(8) =',''</TR>'')'
*      end
*c
*      subroutine HTMLTABLE3(TabTitle,data,AddIdx,tab,IdxTitle)
*C
*C.. Implicits ..
*      implicit none
*C
*C.. Formal Arguments ..
*C.. In/Out Status: Maybe Read, Not Written ..
*      real*8 data(*)
*C.. In/Out Status: Read, Overwritten ..
*      character*(*) TabTitle,idxtitle
*      logical addidx
*      integer tab
*C
*C.. Local Arrays ..
*      character fdecp1(7)*8,fn1(0:12)*8,fn2(12)*8,mth(12)*34,srt(11)*4,
*     $          srt0(4)*4,srt1(6)*4,wrt0(9)*37,wrt2(8)*46,wrt99(7)*30
*      integer*4 decp
*C.. Intrinsic Functions ..
*      intrinsic ABS, INT, LOG10
*      include 'sform.i'
*      include 'stream.i'
*      include 'indhtml.i'
*C
*C.. Data Declarations ..
*      data mth/
*     $     '<abbr title="January">JAN</abbr>  ',
*     $     '<abbr title="February">FEB</abbr> ',
*     $     '<abbr title="March">MAR</abbr>    ',
*     $     '<abbr title="April">APR</abbr>    ',
*     $     'MAY                               ',
*     $     '<abbr title="June">JUN</abbr>     ',
*     $     '<abbr title="July">JUL</abbr>     ',
*     $     '<abbr title="August">AUG</abbr>   ',
*     $     '<abbr title="September">SEP</abbr>',
*     $     '<abbr title="October">OCT</abbr>  ',
*     $     '<abbr title="November">NOV</abbr> ',
*     $     '<abbr title="December">DEC</abbr> '/
*      data srt/
*     $     '1ST','2ND','3RD','4TH','5TH','6TH','7TH','8TH','9TH','10TH',
*     $     '11TH'/
*      data srt0/'1ST','2ND','1ST','2ND'/
*      data srt1/'1ST','2ND','3RD','1ST','2ND','3RD'/
*      data wrt2/'(''<tr><th scope="row">'',I4,''</th>'',',
*     &          'N2',
*     &          '(''<td></td>''),',
*     &          'N1',
*     &          '(''<td>'',F10',
*     &          '.DECP',
*     &          ',''</td>'')',
*     &          ',''</tr>'')'/
*      data wrt0/
*     &     '(''<tr><th scope="row">'',I4,',
*     &     '''-'',I4,''</Th>'',',
*     &     'N2',
*     &     '(''<td></td>''),',
*     &     'N1',
*     &     '(''<td>'',F10',
*     &     '.DECP',
*     &     ',''</td>'')',
*     &     '''</tr>'')'/
*      data wrt99/'(''<tr>'',',
*     &           '''<th scope="col">YEAR</TH>''',
*     &           'N2',
*     &           '(''<th scope="col">''',
*     &             'A4',
*     &           '''</th>'')',
*     &           '''</tr>'')'/
*      data fdecp1/'.0','.1','.2','.3','.4','.5','.6'/
*      data fn1/'0','1','2','3','4','5','6','7','8','9','10','11','12'/
*      data fn2/
*     $     '2','12','22','32','42','52','62','72','82','092','102','112'
*     $     /
*C.. Local Scalars ..
*      integer i,i1,i2,ifact,j,jfact,kfreq,ndecp,nnper,ny,nyr
*      integer*4 yr
*      real*8 sum,zz
*C
*C ... Executable Statements ...
*C
*      decp = 3
*      write(Nio,'(''<table class="htmtab1"><caption>'')')   
*      if (Addidx) then
*       if ((idxTitle.eq.' ').or.(idxTitle.eq.'')) then
*        call AddIdx2(71,nio,TabTitle,TabTitle,tab,9)
*       else
*        call AddIdx2(71,nio,IdxTitle,TabTitle,tab,9)
*       end if
*      else
*       iTab=iTab+1
*       write(Nio,'(''<a tabindex='',i4,''>'',a,
*     &   ''</a>'')') iTab,TabTitle
*      end if
*      kfreq = Nfreq
*      if (kfreq .lt. 4) then
*       if (Nfreq .eq. 3) then
*        kfreq = 6
*       else
*        kfreq = 4
*       end if
*      end if
*      nnper = Nper
*      if (Nper .gt. Nfreq) then
*       Nper = Nfreq
*      end if
*      ndecp = decp
*      if (decp .ge. 6) then
*       decp = 6
*      end if
*      ifact = 0
*      zz = LOG10(ABS(data(1))+.0000000001d0)
*      sum = ABS(zz)
*      do i = 2,Nz
*       if (zz .gt. 0.0d0) then
*        sum = 0.0d0
*        goto 5000
*       else
*        zz = LOG10(ABS(data(i))+.0000000001d0)
*        if ((ABS(zz).lt.sum) .and. (zz.lt.0.0d0)) then
*         sum = ABS(zz)
*        end if
*       end if
*      end do
* 5000 if (zz .gt. 0.0d0) then
*       sum = 0.0d0
*      end if
*      if (sum .gt. 1.0d0) then
*       ifact = INT(sum)
*       if (ifact .gt. 6) then
*        ifact = 6
*       end if
*       if (ifact .gt. 0) then
*        write (Nio,'(''<br>X  10.0D'',I2)') -ifact
*       end if
*      end if
*      jfact = 0
*      zz = LOG10(ABS(data(1))+.0000000001d0)
*      sum = zz
*      do i = 2,Nz
*       zz = LOG10(ABS(data(i))+.0000000001d0)
*       if ((zz.gt.sum) .and. (zz.gt.0.0d0)) then
*        sum = zz
*       end if
*      end do
*      if (sum .gt. 4.0d0) then
*       jfact = INT(sum) - 2
*       if (jfact .gt. 0) then
*        write (Nio,'(''<br>X  10.0D'',I2)') jfact
*       end if
*      end if
*      write (Nio,'(''</caption>'')')
*      write(nio,'(''<thead>'')')
*      yr = Nyer
*      if (Nfreq .eq. 12) then
* 7000  format ('<tr><th scope="col">YEAR</th>',2x,
*     &         12('<th scope="col">',a,'</th>'),'</tr>')
*       write (Nio,7000) (mth(I), I = 1,12)
*      else if (Nfreq .eq. 3) then
* 7001  format ('<tr><th scope="col">YEAR</th>',2x,
*     &         6('<th scope="col">',a4,'</th>'),'</tr>')
*       write (Nio,7001) (srt1(i), i = 1,6)
*      else if (Nfreq .eq. 2) then
* 7002  format ('<tr><th scope="col">YEAR</th>',2x,
*     &         4('<th scope="col">',a4,'</th>'),'</tr>')
*       write (Nio,7002) (srt0(i), i = 1,4)
*      else if (Nfreq .eq. 1) then
*       write (Nio,7002) (srt(i), i = 1,4)
*      else
*       wrt99(3) = fn1(Nfreq)
*       write (Nio,wrt99) (srt(i), i = 1,Nfreq)
*      end if
*      write(nio,'(''</thead>'')')
*      write(nio,'(''<tbody>'')')
*      nyr = (Nz-(Nfreq-Nper+1)) / Nfreq
*      ny = (Nz-(Nfreq-Nper+1)) - nyr*Nfreq
*      if (ny .ne. 0) then
*       nyr = nyr + 1
*      end if
*      nyr = nyr + 1
*      wrt2(6) = fdecp1(decp+1)
*      do i = 1,nyr
*       i1 = (i-1)*kfreq - (Nper-2)
*       i2 = i*kfreq - (Nper-1)
*c c      if (i2 .ge. Nz) then
*c        i2 = Nz
*c       end if
*       if (Nfreq .ge. 4) then
*        wrt2(2) = fn2(1)
*        wrt2(4) = fn1(kfreq)
*       else
*        wrt0(3) = fn2(1)
*        wrt0(5) = fn1(kfreq)
*        wrt0(7) = fdecp1(decp+1)
*       end if
*       if (i .eq. 1) then
*        if (Nfreq .ge. 4) then
*         wrt2(4) = fn1(kfreq-Nper+1)
*         wrt2(2) = fn1(Nper-1)
*        else
*         wrt0(3) = fn1(Nper-1)
*         wrt0(5) = fn1(kfreq-Nper+1)
*        end if
*        i1 = 1
*       else
*        wrt0(4) = ' '
*        wrt0(3) = ' '
*        wrt2(2) = ' '
*        wrt2(3) = ' '
*        if (i2 .gt. nz) then
*         wrt2(4) =fn1(nz-i1+1)
*         write(wrt2(8) ,'(",",i2,A)') 
*     &         kfreq-nz+i1-1, '("<td></td>"),"</tr>")'')'
*            i2 = Nz
*        end if
*       end if
*       if (Nper .eq. 1) then
*        wrt0(4) = ' '
*        wrt0(3) = ' '
*        wrt2(2) = ' '
*        wrt2(3) = ' '
*       end if
*       if (Nfreq .lt. 4) then
*        if (ifact .gt. 0) then
*         write (Nio,wrt0)
*     $         yr, (yr+kfreq/Nfreq-1),
*     $         (data(j)*(10.0d0**ifact), j = i1,i2)
*        else
*         write (Nio,wrt0)
*     $         yr, (yr+kfreq/Nfreq-1),
*     $         (data(j)*(10.0d0**(-jfact)), j = i1,i2)
*        end if
*       else if (ifact .gt. 0) then
*        write (Nio,wrt2) yr, (data(j)*(10.0d0**ifact), j = i1,i2)
*       else
*        write (Nio,wrt2) yr, (data(j)*(10.0d0**(-jfact)), j = i1,i2)
*       end if
*       if (Nfreq .lt. 4) then
*        yr = yr + kfreq/Nfreq
*       else
*        yr = yr + 1
*       end if
*       if (i2 .ge. Nz) goto 5001
*      end do
* 5001 decp = ndecp
*      Nper = nnper
*      write (nio,'(''</tbody>'')')
*      write (Nio,'(''</TABLE>'')')
*      wrt2(3) = '(''<TD></TD>''),'
*      wrt0(4) = '(''<TD></TD>''),'
*      wrt2(4)='N1'
*      wrt2(8) =',''</TR>'')'
*      end
*cc
*c
*cc
*      subroutine HTMLTABLE1(TabTitle,data,nfor,addIdx,tab,Idxtitle)
*C
*C.. Implicits ..
*      implicit none
*C
*C.. Formal Arguments ..
*C.. In/Out Status: Maybe Read, Not Written ..
*      character*(*) TabTitle
*      character*(*) IdxTitle
*      real*8 data(*)
*C.. In/Out Status: Read, Overwritten ..
*C.. In/Out Status: Read, Not Written ..
*      integer nfor,tab
*      logical addidx
*C
*C.. Local Scalars ..
*      integer i,i1,i2,ifact,j,jfact,kfreq,ndecp,nfreq1,nnper,
*     $        nper1,nx,ny,nyr
*      integer*4 yr
*      real*8 sum,zz
*C
*C.. Local Arrays ..
*      character fdecp1(7)*8,fn1(0:12)*8,fn2(12)*8,fnfreq(3)*8,
*     $          srt(11)*4,srt0(4)*4,srt1(6)*4,wrt0(9)*55,wrt2(8)*55,
*     $          wrt99(7)*30,mth(12)*34
*      integer*4 decp
*C
*C.. Intrinsic Functions ..
*      intrinsic ABS, INT, LOG10
*      include 'sform.i'
*      include 'stream.i'
*      include 'indhtml.i'
*C
*C.. Data Declarations ..
*      data mth/
*     $     '<abbr title="January">JAN</abbr>  ',
*     $     '<abbr title="February">FEB</abbr> ',
*     $     '<abbr title="March">MAR</abbr>    ',
*     $     '<abbr title="April">APR</abbr>    ',
*     $     'MAY',
*     $     '<abbr title="June">JUN</abbr>     ',
*     $     '<abbr title="July">JUL</abbr>     ',
*     $     '<abbr title="August">AUG</abbr>   ',
*     $     '<abbr title="September">SEP</abbr>',
*     $     '<abbr title="October">OCT</abbr>  ',
*     $     '<abbr title="November">NOV</abbr> ',
*     $     '<abbr title="December">DEC</abbr> '/
*      data srt/
*     $     '1ST','2ND','3RD','4TH','5TH','6TH','7TH','8TH','9TH','10TH',
*     $     '11TH'/
*      data srt0/'1ST','2ND','1ST','2ND'/
*      data srt1/'1ST','2ND','3RD','1ST','2ND','3RD'/
*      data fdecp1/'.0','.1','.2','.3','.4','.5','.6'/
*      data fn1/'0','1','2','3','4','5','6','7','8','9','10','11','12'/
*      data fn2/
*     $     '2','12','22','32','42','52','62','72','82','092','102','112'
*     $     /
*      data fnfreq/'4','12','6'/
*C
*C ... Executable Statements ...
*C
*c Rober: Los data dan problemas en nuestro compilador
*      wrt2(1)='(''<tr><td scope="row" class="date">'',I4,''</td>'','
*      wrt2(2)='N2'
*      wrt2(3)='(''<td></td>''),'
*      wrt2(4)='N1'
*      wrt2(5)='(''<td>'',F10'
*      wrt2(6)='.DECP'
*      wrt2(7)=',''</td>'')'
*      wrt2(8)=',''</tr>'')'
*      wrt0(1)='(''<tr><td class="date" scope="row">'',I4,'
*      wrt0(2)='''-'',I4,''</td>'','
*      wrt0(3)='N2'
*      wrt0(4)='(''<td></td>''),'
*      wrt0(5)='N1'
*      wrt0(6)='(''<td>'',F10'
*      wrt0(7)='.DECP'
*      wrt0(8)=',''</td>'')'
*      wrt0(9)='''</tr>'')'
*      wrt99(1)='(''<tr>'','
*      wrt99(2)='''<th scope="col">YEAR</th>'''
*      wrt99(3)='N2'
*      wrt99(4)='(''<th scope="col">'''
*      wrt99(5)='A4'
*      wrt99(6)='''</th>'')'
*      wrt99(7)='''</tr>'')'
*c************
*      decp = 3
*      kfreq = Nfreq
*      if (kfreq .lt. 4) then
*       if (Nfreq .eq. 3) then
*        kfreq = 6
*       else
*        kfreq = 4
*       end if
*      end if
*      nnper = Nper
*      if (Nper .gt. Nfreq) then
*       Nper = Nfreq
*      end if
*      ndecp = decp
*      if (decp .ge. 6) then
*       decp = 6
*      end if
*      write (Nio,'(''<table class="htmtab1"><caption>'')')
*      if (Addidx) then
*       if ((idxTitle.eq.' ').or.(idxTitle.eq.'')) then
*        call AddIdx2(71,nio,TabTitle,TabTitle,tab,9)
*       else
*        call AddIdx2(71,nio,IdxTitle,TabTitle,tab,9)
*       end if
*      else
*       iTab=iTab+1
*       write(Nio,'(''<a tabindex='',i4,''>'',a,
*     &   ''</a>'')') iTab,TabTitle
*      end if
*c      write (Nio,'(''<caption class="htmtab1">'',a)') TabTitle
*      zz = LOG10(ABS(data(1))+.0000000001d0)
*      sum = ABS(zz)
*      do i = 2,Nz+nfor
*       if (zz .gt. 0.0d0) then
*        sum = 0.0d0
*        goto 5000
*       else
*        zz = LOG10(ABS(data(i))+.0000000001d0)
*        if ((ABS(zz).lt.sum) .and. (zz.lt.0.0d0)) then
*         sum = ABS(zz)
*        end if
*       end if
*      end do
* 5000 if (zz .gt. 0.0d0) then
*       sum = 0.0d0
*      end if
*      ifact = 0
*      if (sum .gt. 1.0d0) then
*       ifact = INT(sum)
*       if (ifact .gt. 6) then
*        ifact = 6
*       end if
*       if (ifact .gt. 0) then
*        write (Nio,'(''<br> X  10.0D'',I2)') -ifact
*       end if
*      end if
*      jfact = 0
*      zz = LOG10(ABS(data(1))+.0000000001d0)
*      sum = zz
*      do i = 2,Nz+nfor
*       zz = LOG10(ABS(data(i))+.0000000001d0)
*       if ((zz.gt.sum) .and. (zz.gt.0.0d0)) then
*        sum = zz
*       end if
*      end do
*      if (sum .gt. 4.0d0) then
*       jfact = INT(sum) - 2
*       if (jfact .gt. 0) then
*        write (Nio,'(''<br>X  10.0D'',I2)') jfact
*       end if
*      end if
*      write (Nio,'(''</caption>'')')
*      write (nio,'(''<thead>'')')
*      yr = Nyer
*      if (Nfreq .eq. 12) then
* 7000  format ('<tr><th scope="col">YEAR</th>',
*     &         2x,12('<th scope="col">',a34,'</th>'),'</tr>')
*       write (Nio,7000) (Mth(I), I = 1,12)
*      else if (Nfreq .eq. 3) then
* 7001  format ('<tr><th scope="col">YEAR</th>',2x,
*     &         6('<th scope="col">',a4,'</th>'),'</tr>')
*       write (Nio,7001) (srt1(i), i = 1,6)
*      else if (Nfreq .eq. 2) then
* 7002  format ('<tr><th scope="col">YEAR</th>',2x,
*     &         4('<th scope="col">',a4,'</th>'),'</tr>')
*       write (Nio,7002) (srt0(i), i = 1,4)
*      else if (Nfreq .eq. 1) then
*       write (Nio,7002) (srt(i), i = 1,4)
*      else
*       wrt99(3) = fn1(Nfreq)
*       write (Nio,wrt99) (srt(i), i = 1,Nfreq)
*      end if
*      write (nio,'(''</thead>'')')
*      write (nio,'(''<tbody>'')')
*      nyr = (Nz-(Nfreq-Nper+1)) / Nfreq
*      ny = (Nz-(Nfreq-Nper+1)) - nyr*Nfreq
*      if (ny .ne. 0) then
*       nyr = nyr + 1
*      end if
*      nyr = nyr + 1
*      wrt2(6) = fdecp1(decp+1)
*      do i = 1,nyr
*       i1 = (i-1)*kfreq - (Nper-2)
*       i2 = i*kfreq - (Nper-1)
*       if (i2 .ge. Nz) then
*        i2 = Nz
*       end if
*       if (Nfreq .ge. 4) then
*        wrt2(2) = fn1(0)
*        wrt2(4) = fn1(kfreq)
*       else
*        wrt0(3) = fn1(0)
*        wrt0(5) = fn1(kfreq)
*        wrt0(7) = fdecp1(decp+1)
*       end if
*       if (i .eq. 1) then
*        if (Nfreq .ge. 4) then
*         wrt2(4) = fn1(kfreq-Nper+1)
*         wrt2(2) = fn1(Nper-1)
*        else
*         wrt0(3) = fn1(Nper-1)
*         wrt0(5) = fn1(kfreq-Nper+1)
*        end if
*        i1 = 1
*       else
*        wrt0(4) = ' '
*        wrt0(3) = ' '
*        wrt2(3) = ' '
*        wrt2(2) = ' '
*        if (i2 .ge. nz) then
*         wrt2(4) =fn1(nz-i1+1)
*         write(wrt2(8) ,'(",",i2,A)') 
*     &         kfreq-nz+i1-1, '("<td></td>"),"</tr>")'')'
*        end if
*       end if
*       if (Nper .eq. 1) then
*        wrt0(4) = ' '
*        wrt0(3) = ' '
*        wrt2(3) = ' '
*        wrt2(2) = ' '
*       end if
*       if (Nfreq .lt. 4) then
*        if (ifact .gt. 0) then
*         write (Nio,wrt0)
*     $         yr, (yr+kfreq/Nfreq-1),
*     $         (data(j)*(10.0d0**ifact), j = i1,i2)
*        else
*         write (Nio,wrt0)
*     $         yr, (yr+kfreq/Nfreq-1),
*     $         (data(j)*(10.0d0**(-jfact)), j = i1,i2)
*        end if
*       else if (ifact .gt. 0) then
*        write (Nio,wrt2) yr, (data(j)*(10.0d0**ifact), j = i1,i2)
*       else
*       write (Nio,wrt2) yr, (data(j)*(10.0d0**(-jfact)), j = i1,i2)
*       end if
*       if (Nfreq .lt. 4) then
*        yr = yr + kfreq/Nfreq
*       else
*        yr = yr + 1
*       end if
*       if (i2 .ge. Nz) goto 5001
*      end do
* 5001 continue
*      write (nio,'(''</tbody>'')')
*      wrt2(3) = '(''<TD></TD>''),'
*      wrt0(4) = '(''<TD></TD>''),'
*c      write (Nio,'(''</TABLE>'')')
*      decp = ndecp
*C
*C OUTPUT THE FORECAST
*C
*      nfreq1 = Nfreq
*      nper1 = Nper
*      nx = Nz / nfreq1
*      nx = Nz - nx*nfreq1
*      if (nx .gt. 0) then
*       nper1 = nper1 + nx
*       if (nper1 .gt. nfreq1) then
*        nper1 = nper1 - nfreq1
*       else
*        yr = yr - 1
*       end if
*      end if
*c      write (Nio,'(''<table>'')')
*c      write (Nio,'(''<caption><strong>'',a,
*c     &             ''</strong></caption>'')')'FORECAST'
*      write (nio,'(''<tbody class="fore">'')')
*      nyr = (nfor-(nfreq1-nper1+1)) / nfreq1
*      ny = (nfor-(nfreq1-nper1+1)) - nyr*nfreq1
*      if (ny .ne. 0) then
*       nyr = nyr + 1
*      end if
*      nyr = nyr + 1
*      do i = 1,nyr
*       i1 = (i-1)*kfreq - (Nper1-2)
*       i2 = i*kfreq - (Nper1-1)
*       if (i2 .ge. nfor) then
*        i2 = nfor
*       end if
*       wrt2(2) = fn2(1)
*       wrt2(4) = fnfreq(1)
*       if (Nfreq .ge. 4) then
*        wrt2(2) = fn2(1)
*        wrt2(4) = fn1(kfreq)
*       else
*        wrt0(3) = fn2(1)
*        wrt0(5) = fn1(kfreq)
*        wrt0(7) = fdecp1(decp+1)
*       end if
*       if (i .eq. 1) then
*        if (Nfreq .ge. 4) then
*         wrt2(4) = fn1(kfreq-Nper1+1)
*         wrt2(2) = fn1(Nper1-1)
*         wrt2(8)= '''</tr>'')'       
*        else
*         wrt0(3) = fn1(Nper1-1)
*         wrt0(5) = fn1(kfreq-Nper1+1)
*        end if
*        i1 = 1
*       else
*        wrt0(4) = ' '
*        wrt0(3) = ' '
*        wrt2(3) = ' '
*        wrt2(2) = ' '
*        if (i2 .ge. nfor) then
*         wrt2(4) =fn1(nfor-i1+1)
*         write(wrt2(8) ,'(",",i2,A)') 
*     &         kfreq-nfor+i1-1, '("<td></td>"),"</tr>")'')'
*        end if
*       end if
*       if (Nper1 .eq. 1) then
*        wrt0(4) = ' '
*        wrt0(3) = ' '
*        wrt2(3) = ' '
*        wrt2(2) = ' '
*       end if
*       if (Nfreq .lt. 4) then
*        if (ifact .gt. 0) then
*         write (Nio,wrt0)
*     $         yr, (yr+kfreq/Nfreq-1),
*     $         (data(Nz+j)*(10.0d0**ifact), j = i1,i2)
*        else
*         write (Nio,wrt0)
*     $         yr, (yr+kfreq/Nfreq-1),
*     $         (data(Nz+j)*(10.0d0**(-jfact)), j = i1,i2)
*        end if
*       else if (ifact .gt. 0) then
*        write (Nio,wrt2) yr, (data(Nz+j)*(10.0d0**ifact), j = i1,i2)
*       else
*        write (Nio,wrt2) yr, (data(Nz+j)*(10.0d0**(-jfact)), j = i1,i2)
*       end if
*       if (Nfreq .lt. 4) then
*        yr = yr + kfreq/Nfreq
*       else
*        yr = yr + 1
*       end if
*       if (i2 .ge. nfor) goto 5002
*      end do
* 5002 decp = ndecp
*      wrt2(3) = '(''<TD></TD>''),'
*      wrt0(4) = '(''<TD></TD>''),'
*      write (Nio,'(''</tbody></TABLE>'')')
*      Nfreq = nfreq1
*      Nper = nnper
*      end
cc
c
cc     
*      subroutine HTMLFORTBL(fo,freg,ftr,fsa,fs,fcyc,fir,tse,siepf,
*     $              siepfl,sieaf,sieafl,neff,mq,nouir,noutr,npatd,
*     $         neast,nchi,npsi,ncyc,ncycth,lamd,nper,nyer,nz,lfor,
*     $         isCloseToTD,varwnc)
*C
*C.. Implicits ..
*      implicit none
*C
*C.. Parameters ..
*      integer kl,kp
*      parameter (kp = 65, kl = 80)
*C
*C.. Formal Arguments ..
*      integer neff(0:7),mq,nouir,noutr,npatd,neast,nchi,npsi,ncyc,
*     $        ncycth,lamd,nper,nyer,nz, lfor
*      real*8 fo(-kp:kp),freg(-kp:kp),ftr(-kp:kp),fsa(-kp:kp),fs(-kp:kp),
*     $       fcyc(-kp:kp),fir(-kp:kp),tse(kl),siepf(kl),siepfl(kl),
*     $       sieaf(kl),sieafl(kl),varwnc
*      logical isCloseToTD
*C
*C.. Local Scalars ..
*      integer i,j,jnlastper,jnlastyear,ncols,nf,nlastper,nlastyear,nse
*C
*C.. Local Arrays ..
*      character fn(0:12)*12,fstline(7)*85,mth(12)*34,scnline(7)*16,
*     $          srt(11)*4,thrline(7)*16,wrt(8)*60,wrt1(5)*70,
*     $          wrt2(4)*12,wrt3(5)*70,wrt4(4)*40
*      real*8 formatrix(kp,14),tmp(kp)
*C
*C.. External Calls ..
*      external USRENTRY
*C
*C.. Intrinsic Functions ..
*      intrinsic MAX, MOD
*      include 'stream.i'
*C
*C.. Data Declarations ..
*      data wrt/
*     $     '(''<tr><th scope="col">DATE</th>',
*     $     '<th scope="col">FORECAST</th>',
*     $     '<th scope="col">',
*     $     '<acronym title="standard error">SE</acronym></th>'',',
*     $     'N',
*     $     '(''<th scope="col">FORECAST</th><th scope="col">',
*     $     '<acronym title="standard error of the revision">',
*     $     'SER</acronym></th>''),''</tr>'')'/
*      
*      data wrt1/'(''<tr><td class="date" scope="row">'',A34,',
*     $             '''-'',I4,''</td>'',',
*     $             'N',
*     $             '(''<td>'',F16.4,',
*     $             '''</td><td>'',F16.4,''</td>''),''</tr>'')'/
*      data wrt2/'(''<tr>'',','N','(A70',')''</tr>'')'/
*      data wrt3/'("<tr><th>",A34,',
*     $             '"-",I4,"</th>",',
*     $             'N',
*     $             '(','"<td>",F16.4,"</td>"),"</tr>")'/
*      data wrt4/'(''<tr><td></td>'',','N','(A70',')''</tr>'')'/
*      data fn/'0','1','2','3','4','5','6','7','8','9','10','11','12'/
*      data mth/
*     $     '<abbr title="January">JAN</abbr>  ',
*     $     '<abbr title="February">FEB</abbr> ',
*     $     '<abbr title="March">MAR</abbr>    ',
*     $     '<abbr title="April">APR</abbr>    ',
*     $     'MAY',
*     $     '<abbr title="June">JUN</abbr>     ',
*     $     '<abbr title="July">JUL</abbr>     ',
*     $     '<abbr title="August">AUG</abbr>   ',
*     $     '<abbr title="September">SEP</abbr>',
*     $     '<abbr title="October">OCT</abbr>  ',
*     $     '<abbr title="November">NOV</abbr> ',
*     $     '<abbr title="December">DEC</abbr> '/
*      data srt/
*     $     '1ST','2ND','3RD','4TH','5TH','6TH','7TH','8TH','9TH','10TH',
*     $     '11TH'/
*C
*C ... Executable Statements ...
*C
*      ncols = 1
*      nse = 1
*      nf = MAX(lfor,MAX(8,2*mq))
*      do i = 1,nf
*       formatrix(i,ncols) = fo(i)
*       formatrix(i,ncols+1) = tse(i)
*      end do
*      ncols = ncols + 1
*      fstline(ncols-nse)='<td></td><th colspan="2" scope="colgroup">'//
*     $                     'ORIGINAL (UNCORRECTED)</th>'
*      scnline(ncols-nse) = ''
*      thrline(ncols-nse) = ''
*      if ((nchi.gt.1) .or. (noutr.eq.1) .or. (neff(1).eq.1) .or. 
*     $     (neff(7).eq.1)) then
*       nse = nse + 1
*       ncols = ncols + 1
*       do i = 1,nf
*        formatrix(i,ncols) = ftr(i)
*        tmp(i) = ftr(i)
*       end do
*       ncols = ncols + 1
*       if (lamd .eq. 0) then
*        do i = 1,nf
*         formatrix(i,ncols) = siepfl(i)
*        end do
*       else
*        do i = 1,nf
*         formatrix(i,ncols) = siepf(i)
*        end do
*       end if
*       call USRENTRY(tmp,1,nf,1410)
*       fstline(ncols-nse) = '<th colspan="2" scope="colgroup">'//
*     $                      'TREND-CYCLE</th>'
*       scnline(ncols-nse) = ''
*       thrline(ncols-nse) = ''
*      end if
*      if ((npsi.gt.1) .or. (neast.eq.1) .or. (neff(2).eq.1) .or.
*     $    (npatd.eq.1)) then
*       nse = nse + 1
*       ncols = ncols + 1
*       do i = 1,nf
*        formatrix(i,ncols) = fsa(i)
*        tmp(i) = fsa(i)
*       end do
*       ncols = ncols + 1
*       if (lamd .eq. 0) then
*        do i = 1,nf
*         formatrix(i,ncols) = sieafl(i)
*        end do
*       else
*        do i = 1,nf
*         formatrix(i,ncols) = sieaf(i)
*        end do
*       end if
*       call USRENTRY(tmp,1,nf,1409)
*       fstline(ncols-nse) = '<th colspan="2" scope="colgroup">'//
*     $                      'SA SERIES</th>'
*       scnline(ncols-nse) = ''
*       thrline(ncols-nse) = ''
*      else if (neff(0) .eq. 1) then
*       nse = nse + 1
*       ncols = ncols + 1
*       do i = 1,nf
*        formatrix(i,ncols) = fsa(i)
*        tmp(i) = fsa(i)
*       end do
*       ncols = ncols + 1
*       if (lamd .eq. 0) then
*        do i = 1,nf
*         formatrix(i,ncols) = sieafl(i)
*        end do
*       else
*        do i = 1,nf
*         formatrix(i,ncols) = sieaf(i)
*        end do
*       end if
*       call USRENTRY(tmp,1,nf,1409)
*       fstline(ncols-nse) = '<th colspan="2" scope="colgroup">'//
*     $                      'SA SERIES</th>'
*       scnline(ncols-nse) = ''
*       thrline(ncols-nse) = ''
*      end if
*      if (neff(0) .eq. 1) then
*       ncols = ncols + 1
*       do i = 1,nf
*        formatrix(i,ncols) = freg(i)
*       end do
*       fstline(ncols-nse) = '<th scope="col">'//
*     $                      'SEPARATE REGRESSION EFFECT</th>'
*       scnline(ncols-nse) = ''
*       thrline(ncols-nse) = ''
*      end if
*      if ((npsi.gt.1) .or. (neast.eq.1) .or. (neff(2).eq.1) .or.
*     $    (npatd.eq.1)) then
*       ncols = ncols + 1
*       do i = 1,nf
*        formatrix(i,ncols) = fs(i)
*        tmp(i) = fs(i)
*       end do
*       call USRENTRY(tmp,1,nf,1411)
*       if (lamd .eq. 0) then
*        fstline(ncols-nse) = '<th scope="col">'//
*     $                       'SEASONAL FACTORS</th>'
*        scnline(ncols-nse) = ''
*       else
*        fstline(ncols-nse) = '<th scope="col">'//
*     $                       'SEASONAL COMPONENT</th>'
*        scnline(ncols-nse) = ''
*       end if
*       thrline(ncols-nse) = ' '
*      end if
*      if ((neff(3).eq.1) .or. (nouir.eq.1) .or. 
*     $    (varwnc.gt.1.0D-10 .and.(ncycth.eq.1 .or.ncyc.gt.1))
*     $     .or. (neff(5).eq.1)) then
*       ncols = ncols + 1
*       if (lamd .eq. 1) then
*        do i = 1,nf
*         formatrix(i,ncols) = fir(i) + fcyc(i)
*         tmp(i) = fir(i)
*        end do
*       else
*        do i = 1,nf
*         formatrix(i,ncols) = (fir(i)*fcyc(i)) / 100.0d0
*         tmp(i) = fir(i)
*        end do
*       end if
*       call USRENTRY(tmp,1,nf,1412)
*       if (IsCloseToTD) then
*        if (lamd .eq. 0) then
*          fstline(ncols-nse) =
*     $              '<th scope="col">TDfinal.-IRREG. FACTORS</th>'
*        else
*          fstline(ncols-nse) ='<th scope="col">TDfinal.-IRREG.</th>'
*        end if
*       else
*        if (lamd .eq. 0) then
*          fstline(ncols-nse) =
*     $              '<th scope="col">TRANS.-IRREG. FACTORS</th>'
*        else
*          fstline(ncols-nse) ='<th scope="col">TRANS.-IRREG.</th>'
*        end if
*       end if
*       thrline(ncols-nse) = ' '
*      end if
*      nlastper = nper
*      nlastyear = nyer
*      do i = 2,nz
*       if (MOD(nlastper,mq) .eq. 0) then
*        nlastyear = nlastyear + 1
*        nlastper = 0
*       end if
*       nlastper = nlastper + 1
*      end do
*      nlastper = nlastper + 1
*      if (nlastper .gt. mq) then
*       nlastper = 1
*       nlastyear = nlastyear + 1
*      end if
*      jnlastper = nlastper
*      jnlastyear = nlastyear
*      write (nio,'(''<table>'')')
*      wrt2(2) = fn(nse)
*      write (Nio,wrt2) (fstline(i), i = 1,nse)
*      wrt(5) = fn(nse-1)
*      wrt1(3) = fn(nse)
*      write (Nio,wrt)
*      if (mq .eq. 12) then
*       wrt1(1)='(''<tr><td class="date" scope="row">'',A34,'
*       do i = 1,nf
*        write (Nio,wrt1)
*     $        mth(nlastper), nlastyear, (formatrix(i,j), j = 1,nse*2)
*        if (nlastper .eq. mq) then
*         nlastper = 1
*         nlastyear = nlastyear + 1
*        else
*         nlastper = nlastper + 1
*        end if
*       end do
*      else
*       wrt1(1)='(''<tr><td class="date" scope="row">'',A3,'
*       do i = 1,nf
*        write (Nio,wrt1)
*     $        srt(nlastper), nlastyear, (formatrix(i,j), j = 1,nse*2)
*        if (nlastper .eq. mq) then
*         nlastper = 1
*         nlastyear = nlastyear + 1
*        else
*         nlastper = nlastper + 1
*        end if
*       end do
*      end if
*      write (Nio,'("</table>")')
*      if (nse*2 .lt. ncols) then
*       nlastper = jnlastper
*       nlastyear = jnlastyear
*       wrt4(2) = fn(ncols-2*nse)
*       write (nio,'(''<table>'')')
*       write (Nio,wrt4) (fstline(i), i = nse+1,ncols-nse)
*C       wrt(3) = fn(ncols-2*nse)
*       wrt3(3) = fn(ncols-2*nse)
*       if (mq .eq. 12) then
*        do i = 1,nf
*         write (Nio,wrt3)
*     $         mth(nlastper), nlastyear,
*     $         (formatrix(i,j), j = nse*2+1,ncols)
*         if (nlastper .eq. mq) then
*          nlastper = 1
*          nlastyear = nlastyear + 1
*         else
*          nlastper = nlastper + 1
*         end if
*        end do
*       else
*        do i = 1,nf
*         write (Nio,wrt3)
*     $         srt(nlastper), nlastyear,
*     $         (formatrix(i,j), j = nse*2+1,ncols)
*         if (nlastper .eq. mq) then
*          nlastper = 1
*          nlastyear = nlastyear + 1
*         else
*          nlastper = nlastper + 1
*         end if
*        end do
*       end if
*       write (Nio,'("</table>")')
*      end if
*      write (Nio,'(''<p><em>SE  : standard error of the observation '',
*     $  ''series forecast.'',''<br>SER : standard error of the '',
*     $  ''revision.</em></p>'',
*     $  ''<p class="ub">Note 1 :</p>'')')
*      write (nio,'(''<p class="note">Since the component is '',
*     $  ''never observed, the forecast error is of little '',
*     $  ''applied interest. What is of interest '',
*     $  ''is the se of the revision the forecast '',
*     $  ''of the component will undergo (until it becomes '',
*     $  ''the final or historical estimator).</p>'')')
*      write (Nio,'(''<p class="ub">Note 2:</p> '')')
*      write (nio,'(''<p class=note>SER(Seasonal)'',
*     $             '' = SER (SA Series)</p>'')')
*      end
cc
c
cc
*      subroutine serr(nio)
*C.. Implicits ..
*      implicit none
*      integer nio
*      write (nio,'(''<p><Strong>ERROR : </Strong>'')')
*      return
*      end
cc
c
cc    
*      subroutine eerr(nio)
*C.. Implicits ..
*      implicit none
*      integer nio
*      write (nio,'(''</p>'')')
*      return
*      end
cc
c
cc    
c      subroutine swarn(nio)
C.. Implicits ..
c      implicit none
c     integer nio
C     call serror(nio)
c     write (nio,'(''<HR><CENTER><I><P>'')')
c     write (nio,'(''<font color=red><Strong>Warning'',
c     &             '' : </Strong></font>'')')
c     return
c     end
cc
c
cc    
c      subroutine ewarn(nio)
C.. Implicits ..
c      implicit none
c     integer nio
c     write (nio,'(''<P><HR></CENTER></I><P>'')')
c     call eerr(nio)
c     return
c     end

cc
c
cc    
*      subroutine swarn(nio)
*c.. Implicits ..
*      implicit none
*      integer nio
*      write (nio,'(''<p class="warn">'')')
*      write (nio,'(''<Strong>Warning'',
*     &             '' : </Strong><br>'')')
*      return
*      end
*cc
*c
*cc    
*      subroutine ewarn(nio)
*c.. Implicits ..
*      implicit none
*      integer nio
*      write (nio,'(''</p>'')')
*      return
*      end
cc
c
cc    
c      subroutine SNote(nio)
C.. Implicits ..
c      implicit none
c     integer nio
c     write (nio,'(''<font color=red><p><Strong>Attention'',
c    &             '' : </Strong></font>'')')
c     return
c     end
cc
c
cc
*      subroutine SNote(nio)
*C.. Implicits ..
*      implicit none
*      integer nio
*      write (nio,'(''<p class="snote"><Strong>Attention'',
*     &             '' : </Strong><br>'')')
*      return
*      end
cc
c
cc    
*      subroutine Enote(nio)
*C.. Implicits ..
*      implicit none
*      integer nio
*      write (nio,'(''</p>'')')
*      return
*      end
cc
c
cc
      subroutine STRCAP(String)
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
      character*(*) String
C
C.. Local Scalars ..
      integer I,Iasc,J
C
C.. External Functions ..
      integer ISTRLEN
      external ISTRLEN
C
C.. Intrinsic Functions ..
      intrinsic CHAR, ICHAR
C
C ... Executable Statements ...
C
      J = ISTRLEN(String)
      Iasc = ICHAR(String(1:1))
      if ((Iasc.gt.96) .and. (Iasc.lt.123)) then
       String(1:1) = CHAR(Iasc-32)
      end if
      do I = 2,J
       Iasc = ICHAR(String(I:I))
       if ((Iasc.gt.64) .and. (Iasc.lt.91)) then
        String(I:I) = CHAR(Iasc+32)
       end if
      end do
      end
cc
c
cc
c
*      Subroutine Introduc(nio)
*c INPUT variables
*      integer nio
*      include 'build.i'
* 4003 format(/,20x,'New version of PROGRAM SEATS',//,
*     &       8x,'"SIGNAL EXTRACTION IN ARIMA TIME SERIES"',//,
*     &       8x,'© Victor Gomez and Agustin Maravall, 1996',//,
*     &       8x,'VERSION: (',A,')',//,//,
*     &       8x,'Developed by Gianluca Caporello and Agustin Maravall',
*     &       ' at the Bank of Spain.',//,
*     &       8x,'With programming help from',//,
*     &       8x,'Domingo Perez Canete (2000 - )',
*     &       '    Roberto Lopez Pavon (2005 - )',//,
*     &       8x,'Christophe Planas (1992 - 1994)   ',
*     &       'Gabriele Fiorentini (1990 - 1991).',//,
*     &       4x,'(Parts of the program are based as an experimental',
*     &       ' program developed by J.P. Burman',//,
*     &       16x,' at the Bank of England, 1982 version.)',//)
*      write(nio,4003) compdate
*      end
cc
c
cc
      Subroutine IntrodH(nio)
c INPUT variables
      integer nio
      include 'htmlout.cmn'
      include 'build.i'
c
      CALL writTagOneLine(Nio,'h1','center','SEATS+')
      CALL mkPOneLine(Nio,'center','(based on program Seats, '//
     $   '<span lang="es">V&iacute;ctor G&oacute;mez</span> and '//
     $   '<span lang="es">Agust&iacute;n Maravall</span> &copy;,1996)')    
      CALL mkPOneLine(Nio,'center',
     &        'Developed by <span lang="it">Gianluca Caporello</span>'//
     $        ' and <span lang="es">Agust&iacute;n Maravall</span>'//
     $        ' at the Bank of Spain.')
      CALL mkPOneLine(Nio,'center','<em>Developed at the Bank of '//
     $        'Spain by <span lang="it">Gianluca Caporello</span> '//
     $        'and <span lang="es">Agust&iacute;n Maravall</span>'//
     $        '</em>'//Cbr//'with programming support from'//Cbr//
     $        '<em><span lang="es"> Domingo P&eacute;rez '// 
     $        'Ca&ntilde;ete</span> and <span lang="es">'//
     $        'Roberto L&oacute;pez Pav&oacute;n.</span></em>')
      CALL mkPOneLine(Nio,'center','Help from Christophe Planas '//
     $        '(1992 - 1994) and <span lang="it">Gabriele Fiorentini '//
     $        '(1990 - 1991)</span> is also acknowledged.')
      CALL mkPOneLine(Nio,'center',
     $                '<em>VERSION</em>: 1.0 ('//compdate//')')
      return
      end
cc
c
cc
      subroutine OpenFilePsie(ireturn)
c.. Implicits ..
      implicit none
c
      logical F
      parameter(F = .false.)
c
c.. Formal Argument In/Out
      integer ireturn
      include 'dirs.i'
      include 'stream.i'
      include 'stdio.i'
c
      integer ISTRLEN
      external ISTRLEN
c
      character fname*180
c
      fname = Cursrs(1:Nfilcr)//'_psie.html'
*       fname = outdir(1:ISTRLEN(outdir)) // '\' // 'psie.htm'
      call OPENDEVICE(fname,37,0,ireturn)
      if (ireturn .eq. 0) then
       CALL mkHead(37,fname,fname,F,1,0,F)
      end if
      end 
cc
c
cc
*      subroutine OpenFileTables(ireturn,iter,niter,title,numser)
*c.. Implicits ..
*      implicit none
*c
*c.. Formal Argument In/Out
*      integer ireturn,niter,iter,numser
*      character nombreser*180,title*80
*      include 'dirs.i'
*      include 'stream.i'
*c
*      integer ISTRLEN
*      external ISTRLEN
*c
*      character fname*180
*c
*      ireturn=0
*      if (HTML .eq. 1) then
*       if (numser.gt.1) then
*        open(86,FILE =OUTDIR(1:ISTRLEN(OUTDIR))//'\tabSidx.txt'
*     &         ,IOSTAT = ireturn)
*       end if   
*       fname = outdir(1:ISTRLEN(outdir)) // '\' // 'ts_1.htm'
*       write(fname,'(i6)') niter
*       call lefttrim(fname)
*       fname=outdir(1:ISTRLEN(outdir))//'\tables\ts_'//
*     & fname(1:ISTRLEN(fname))//'.htm'
*       open (36,FILE = fname(1:ISTRLEN(fname)),IOSTAT = ireturn)
*       if (numser.gt.1) then
*        if (iter.ne.1) then            
*         write(86,'(A,";",A)') title(1:ISTRLEN(title)), fname 
*        else 
*         write(nombreSer,'(i6)') niter
*         call lefttrim(nombreSer)      
*         write(86,'("Model ",A,";",A)') 
*     &         nombreSer(1:ISTRLEN(nombreSer)), fname 
*        end if
*       end if
*       if (ireturn .eq. 0) then
*        call WriteDoctype(36,1)
*        write (36,'("<HTML><HEAD>")')
*        write (36,'(''<META HTTP-EQUIV="Content-Type" '',
*     &             ''CONTENT="text/html;charset=iso-8859-1">'')')
*        write (36,*) '<meta name="lang" content="en-US" >'
*        write (36,'("<TITLE>Table-s.htm",
*     $        " File with the series produced by SEATS extended over",
*     $        " the forecasting Period</TITLE>")')
*        call writeCSS(36,8)
*        write(36,'(''</HEAD><BODY>'')') 
*       end if
*      else
*       if (niter.eq.1) then
*        fname = outdir(1:ISTRLEN(outdir)) // '\' // 'table-s.out'
*        call OPENDEVICE(fname,36,0,ireturn)
*       end if
*      end if
*      end
cc
c
cc
      subroutine OpenSummary(io,fname,ireturn)
c.. Implicits ..
      implicit none
c
C
      LOGICAL F
      PARAMETER (F=.false.)
c.. Formal Argument In/Out
      character fname*180
      integer ireturn,io
c-----
       call OPENDEVICE(fname,io,0,ireturn)
       if (ireturn .eq. 0) then
*        call WriteDoctype(io,1)
        CALL mkHead(io,fname,'SEATS Output Summary',F,1,7,F)
*        call writeCSS(io,7)
*        write(io,'(''</HEAD><BODY>'')') 
       end if
      end
c---
cc
c
cc
      subroutine OpenCompMatrix(ireturn,mq)
C.. Implicits ..
      implicit none
C
      LOGICAL F
      PARAMETER (F=.false.)
C.. Formal Arguments ..
C.. In/Out Status: Not Read, Overwritten ..
      integer ireturn,mq
      include 'dirs.i'
      include 'stream.i'
      include 'stdio.i'
c
      character filename*180
c
      integer ISTRLEN
      external ISTRLEN
c     
      integer j
      character AuxString*350 
c
      filename = Cursrs(1:Nfilcr)//'_trnmod.html'
*       filename=Outdir(1:ISTRLEN(Outdir)) // '\trendmod.htm'
      call OPENDEVICE (filename,61,0,ireturn)
      CALL mkHead(61,filename,'Trend-Cycle Component Matrix',F,1,1,F)
      CALL makDivId(61,'trendcycle.matrix','@')
      CALL mkTableTag(61,'w90','Trend-Cycle Component Matrix')
      CALL writTag(61,'<tr>')
      write (61,6100)
 6100 format('<th scope="col">&nbsp;n&nbsp;</th>',
     $        '<th scope="col">Title</th>',
     $        '<th scope="col">&nbsp;D&nbsp;</th>',/,
     $  '<th scope="col">PHIP(1)</th><th scope="col">PHIP(2)</th>',
     $  '<th scope="col">PHIP(3)</th><th scope="col">PHIP(4)</th>',/,
     $  '<th scope="col">THP(1)</th><th scope="col">THP(2)</th>',
     $  '<th scope="col">THP(3)</th>',/,
     $  '<th scope="col">THP(4)</th><th scope="col">THP(5)</th>',
     $  '<th scope="col">THP(6)</th><th scope="col">THP(7)</th>',/,
     $  '<th scope="col">Stand Innov Var</th>')
      CALL writTag(61,'</tr>')
c
      filename = Cursrs(1:Nfilcr)//'_samod.html'
*       filename=Outdir(1:ISTRLEN(Outdir)) // '\samod.htm'
      call OPENDEVICE (filename,63,0,ireturn)
      CALL mkHead(63,filename,'Seasonally Adjusted Component Matrix',F,
     $            1,1,F)
      CALL makDivId(63,'seasadj.matrix','@')
      CALL mkTableTag(63,'w90','Seasonally Adjusted Component Matrix')
      CALL writTag(63,'<tr>')
      write (63,6300)
 6300 format('<th scope="col">&nbsp;n&nbsp;</th>',
     $       '<th scope="col">Title</th>',
     $       '<th scope="col">&nbsp;D&nbsp;</th>')
      do j=1,16
        write (63,6310) 'PHIN', j
      end do
      do j=1,17
        write (63,6310) 'THN', j
      end do
 6310 format('<th scope="col">',a,'(',i2.2,')</th>')
      write (63,6320)
 6320 format('<th scope="col">Standard Innovation Variance</th></tr>')
c
      filename = Cursrs(1:Nfilcr)//'_seamod.html'
*       filename=Outdir(1:ISTRLEN(Outdir)) // '\seasmod.htm'
      call OPENDEVICE (filename,62,0,ireturn)
      CALL mkHead(62,filename,'Seasonal Component Matrix',F,1,1,F)
      CALL makDivId(62,'seasonal.matrix','@')
      CALL mkTableTag(62,'w90','Seasonal Component Matrix')
      CALL writTag(62,'<tr>')
      write (62,6200)
 6200 format('<th scope="col">&nbsp;n&nbsp;</th>',
     $       '<th scope="col">Title</th><th>&nbsp;D&nbsp;</th>')
      do j=1,mq+2
        write (62,6310) 'PHIS', j
      end do 
      do j=1,2*mq+1
        write (62,6310) 'THS', j
      end do
      write (62,6320)
       
      filename = Cursrs(1:Nfilcr)//'_tramod.html'
*       filename=Outdir(1:ISTRLEN(Outdir)) // '\transmod.htm'
      call OPENDEVICE (filename,64,0,ireturn)
      CALL mkHead(64,filename,
     &            'Transitory and Irregular Component Models',F,1,1,F)
      CALL makDivId(64,'transirr.matrix','@')
      CALL mkTableTag(64,'w90',
     &                'Transitory and Irregular Component Matrix')
      CALL writTag(64,'<tr>')
      write (64, 6400)
 6400 format('<th scope="col">&nbsp;n&nbsp;</th>',
     $       '<th scope="col">Title</th>')
      do j=1,mq+3
         write (64,6310) 'PHIC', j
      end do
      do j=1,mq+3
         write (64,6310) 'THC', j
      end do
      write(64,6410) 'TRANSITORY'
      write(64,6410) 'Irregular'
 6410 format('<th scope="col">',a,' Innovation Variance</th>')
      CALL writTag(64,'</tr>')
      return
      end
cc
c
cc
      subroutine CloseCompMatrix()
C.. Implicits ..
      implicit none
      integer idevice
      do idevice=61,64
       CALL writTag(idevice,'</table></div>')
       CALL writTag(idevice,'</body>')
       CALL writTag(idevice,'</html>')
       close(idevice)
      end do
      return
      end
cc
c
cc
      subroutine CloseOldMatrix()
C.. Implicits ..
      implicit none
      include 'seatserr.i'
      integer idevice
      do idevice=65,67
       CALL writTag(idevice,'</table>')
       CALL writTag(idevice,'</body>')
       CALL writTag(idevice,'</html>')
       close(idevice)
      end do
      CALL writTag(74,'</table>')
      CALL writTag(74,'</body>')
      CALL writTag(74,'</html>')
      close(74)
       if (countError.gt.0) then
        CALL writTag(76,'</table>')
        CALL writTag(76,'</body>')
        CALL writTag(76,'</html>')
       end if
      close(76)
      return
      end
cc
c
cc
c
c Icode:
c   1: html 4.01 strict    
c   2: html 4.01 transicional
c   3: html 4.01 frames
c   4: HTML 2.0
c   5: html 3.2     
      subroutine WriteDoctype(u,icode)
      implicit none
      integer u,icode
       if (icode .eq. 1) then   
        write (u,1010) 'W3C','HTML 4.01'    
        write (u,1020) 'html4/strict'
       else if (icode .eq. 2) then
        write (u,1010) 'W3C','HTML 4.01 Transitional'    
        write (u,1020) 'html4/loose'
       else if (icode .eq. 3) then
        write (u,1010) 'W3C','HTML 4.01 frameset'    
        write (u,1020) 'html4/frameset'
       else if (icode .eq. 4) then       
        write (u,1010) 'IETF','HTML 2.0'    
       else if (icode .eq. 5) then       
        write (u,1010) 'W3C','HTML 3.2 Final'
       else if (icode .eq. 0) then
        write (u,1010) 'W3C','XHTML 1.0 Transitional'    
        write (u,1020) 'xhtml1/DTD/xhtml1-transitional'
       end if
c-----------------------------------------------------------------------
 1010 FORMAT('<!DOCTYPE html PUBLIC "-//',a,'//DTD ',a,'//EN"')
 1020 FORMAT('   "http://www.w3.org/TR/',a,'.dtd">')
c-----------------------------------------------------------------------
      return
      end
cc
c
cc
*      subroutine HeadHtmlMeta(io,doctype,titulo,icode)
*      implicit none
*      integer io,doctype,icode
*      character*80 titulo
*      include 'build.i'
*      call writeDoctype(io,doctype)
*      write(io,'(''<html lang="en-US">'')')
*      write (io,'(''<head>'')')
*      write(io,'(''<META HTTP-EQUIV="Content-Type" '',
*     $            ''CONTENT="text/html;charset=iso-8859-1">'')')
*      write(io,'(''<meta name="lang" content="en-US" >'')')
*      write(io,'(''<title>'',A,''</title>'')') titulo 
*      write(io,'(''<!--*** Seats '',A,'' ***-->'')')
*     $            CompDate
*      call writeCSS(io,0)
*      write(io,'(''</head><body>'')') 
*c     write (io,'(''<p><a NAME="home"></A></p>'')')
*      end
cc
c
cc
*      subroutine endValidHtml(io,icode)
*      integer io,icode 
*      if (icode .eq. 1) then
*       write(io,'(''<div="endFile"><p>'')')      
*       write(io,'(''<a href="http://validator.w3.org/'',
*     $  ''#validate-by-upload"><img '',
*     $  '' src="http://www.w3.org/Icons/valid-html401-blue"'',
*     $  ''alt="Valid HTML 4.01 Strict" height="31" width="88"></a>'')')
*       write(io,'(''<div="endFile"><p>'')')
*      end if
*      end
cc
c
cc
      subroutine writeCSS(io,icode,Lxhtml)
C.. Implicits ..
      implicit none
      logical Lxhtml
      integer io,icode
c
       CALL writTag(io,'<style type="text/css">')
       IF(Lxhtml)CALL writTag(io,'/*<![CDATA[*/')
       if (icode .eq. 1) then 
c  informacion centrada en la hoja
        CALL writTag(io,'body{text-align:center}')
c  tabla por defecto
        CALL writTag(io,'table {font-size: 80%;text-align:justify; '//
     $              'border:1px solid black;border-collapse:collapse;'//
     $               'margin-top:0.25em;margin-left:1%;'//
     $               'margin-right:1%;width:98%}')
        CALL writTag(io, 'th,td{text-align:center;white-space: '//
     $               'nowrap;border:1px inset black; padding:2px;'//
     $               'margin:0em;}')  
        CALL writTag(io,'th {background-color: #BDBDBD;}')    
c  clases
        CALL writTag(io, '.right{text-align:right;white-space: '//
     $         'nowrap;border:1px inset black; padding:2px;'//
     $         'margin:0px}')   
        CALL writTag(io, '.aleft{text-align:left;white-space: '//
     $         'nowrap;border:1px inset black; padding:2px;'//
     $         'margin:0px}')   
        CALL writTag(io, '  td.head { background-color: #BDBDBD; '//
     $         'text-align : center;  font-weight: bold; }')
        CALL writTag(io,'table td:hover,th:hover {background:#FFCC99;'//
     $   ' cursor: crosshair; font-weight:bold; font-size:1.5em;}')
        CALL writTag(io, ' .w90 {  margin-left : 5%;  margin-right : '//
     $         '5%;  width : 90%  }')
        CALL writTag(io, ' .w60 {  margin-left : 20%;  margin-right :'//
     $         ' 20%;  width : 60%  }')
        CALL writTag(io, ' .w30 {  margin-left : 35%;  margin-right :'//
     $         ' 35%;  width : 30%  }')
       else if (icode .eq. 0) then
c icode para seats.out
        CALL writTag(io,'body{ font-family: "Arial", "Helvetica", '//
     $             'sans-serif; font-size:0.9em;}')
        CALL writTag(io,'.sHead,h1{text-align:center;}')
        CALL writTag(io,'h1,h2 {font-size:x-large;}')
        CALL writTag(io,'h3{font-size:large;}')
        CALL writTag(io,'h2{margin-top:7em}')
        CALL writTag(io,'h3{margin-top:4em;}')
        CALL writTag(io,'div {margin-left:2%}')
        CALL writTag(io,'li {margin-top:1em}')
        CALL writTag(io,'ul {margin:0em}')
        CALL writTag(io,'.pol td{text-align:left;}')
c  
        CALL writTag(io,'table {font-size: 84%;text-align:justify; '//
     $       'border:0.1em solid black;border-collapse:collapse;'//
     $       'margin-top:2em}')
        CALL writTag(io, 'th,td{text-align:center;white-space: '//
     $         'nowrap;border:0.1em solid gray; padding:0.3em;'//
     $         'margin:0em;}') 
        CALL writTag(io, 'caption{text-align:left;white-space: '//
     $         'nowrap;font-size: larger;padding-top:2em;}') 
        CALL writTag(io, '.note {font-style:italic;}')
        CALL writTag(io, '.ub,caption {text-decoration:underline}')
        CALL writTag(io, '.ub,.date,caption {font-weight:bold;} ')
        CALL writTag(io, 'em{font-weight:bold;font-style:normal;}')
c  
        CALL writTag(io, '.aright{text-align:right;white-space: '//
     $         'nowrap;border:0px ; padding:0.5em; margin:0px;}')   
        CALL writTag(io, '.aleft{text-align:left;white-space: '//
     $         'nowrap;border:0px; padding:0.5em;margin:0px;}') 
        CALL writTag(io, '.acf table{text-align:justify;'//
     $               'border:0.1em solid black;'//
     $               'border-collapse:collapse;margin:0em}')
        CALL writTag(io, '.acf td, .acf th{border:0.1em inset black;'//
     $               'margin:0em}')
        CALL writTag(io,'th{background-color: #E6E6E6;}')
        CALL writTag(io,'table td:hover,th:hover {background:#FFCC99;'//
     $   ' cursor: crosshair; font-weight:bold; font-size:1.5em;}')
        CALL writTag(io, '  td.head { background-color: #E6E6E6; '//
     $         'text-align : center;  font-weight: bold; }')
        CALL writTag(io, ' .w90 {  margin-left : 5%;  margin-right : '//
     $         '5%;  width : 90%  }')
        CALL writTag(io, '  .center { text-align : center; }')
c   summarys
       else if (icode.eq.7) then
        CALL writTag(io,'body{ font-family: "Arial", "Helvetica", '//
     &           'sans-serif; font-size:0.9em; margin-left:5%;}')
        CALL writTag(io,'h1{ font-size: 160%;}')
        CALL writTag(io,'h2{ font-size: 140%;margin-top:3em;}')
        CALL writTag(io,'table {font-size: 84%;text-align:justify; '//
     &            'border:0.1em solid black;border-collapse:'//
     &            'collapse;margin-top:1em}')
        CALL writTag(io,'th,td{text-align:center;white-space: nowrap;'//
     &        'border:0.1em solid gray; padding:0.3em;margin:0em;}')
        CALL writTag(io,'caption{text-align:left;white-space: nowrap;'//
     &              ' font-size: larger;padding:2em 0em 1em; '//
     &              'font-weight:bold;font-size: 120%;}')
        CALL writTag(io,'th{background-color: #E6E6E6;}')
        CALL writTag(io, '  td.head { background-color: #E6E6E6; '//
     $         'text-align : center;  font-weight: bold; }')
        CALL writTag(io,'span{margin-left:2em;}')
        CALL writTag(io,'em{font-weight:bold;}')
        CALL writTag(io,'  table.x11 {    margin-left : 7.5%;    '//
     $                  'margin-right : 7.5%;    width : 85%;  }')
        CALL writTag(io,' .w70 {  margin-left : 15%;  margin-right : '//
     $                  '15%;  width : 70%  }')
        CALL writTag(io,' .w60 {  margin-left : 20%;  margin-right : '//
     $                  '20%;  width : 60%  }')
        CALL writTag(io,' .w50 {  margin-left : 25%;  margin-right : '//
     $                  '25%;  width : 50%  }')
        CALL writTag(io, '  .center { text-align : center; }')
       else if (icode .eq. 8) then
c icode para table-s
        CALL writTag(io,'body{ font-family: "Arial", "Helvetica", '//
     $             'sans-serif; font-size:0.9em;}')
        CALL writTag(io,'h1,h2 {font-size:medium;}')
c  
        CALL writTag(io,'table {font-size: 84%;text-align:justify; '//
     $   'border:0.1em solid black;border-collapse:collapse;'//
     $   'margin-top:0em;margin-left:1%;'//
     $   'margin-right:1%;width:98%}')
        CALL writTag(io, 'th,td{text-align:center;white-space: '//
     $   'nowrap;border:0.1em solid gray; padding:0.3em;}') 
        CALL writTag(io,'th {background-color: #BDBDBD;}')
        CALL writTag(io,'.f{background-color:#F2F2F2;}')
        CALL writTag(io,'.h{background-color:#D8D8D8;}')        
       else if (icode .eq. 9) then
        CALL writTag(io,'body{background-color:#ffffff; '//
     $               'color:#000000; margin-left:0.4em}')
        CALL writTag(io,'.titulo{text-align:left; font-size: 2em; '//
     $               'color:#B22222;}')
        CALL writTag(io,'ul{margin-left:1em;}')
        CALL writTag(io,'li {margin-top:0.5em; font-size: 0.9em}')
        CALL writTag(io,'li li {margin-top:0.3em; font-size: 0.9em}')
       end if
      IF(Lxhtml)CALL writTag(io,'/*]]>*/')
      CALL writTag(io,'</style>')
      return
      end
c     
c
cc
      subroutine OpenClasicMatrix(ireturn)
C.. Implicits ..
      implicit none
C
      logical F
      parameter(F=.false.)
C.. Formal Arguments ..
C.. In/Out Status: Not Read, Overwritten ..
      integer ireturn
      include 'dirs.i'
      include 'stream.i'
      include 'stdio.i'
      include 'htmlout.cmn'
      character filename*180
c
      integer ISTRLEN
      external ISTRLEN
c     
       filename = Cursrs(1:Nfilcr)//'_sgen.html'
*      filename=Outdir(1:ISTRLEN(Outdir)) // '\sgeneral.htm'
      call OPENDEVICE (filename,65,0,ireturn)
      CALL mkHead(65,filename,'Seats General Matrix',F,1,1,F)
      CALL makDivId(65,'seats.general.matrix','@')
      CALL mkTableTag(65,'w90','Seats General Matrix')
      CALL writTag(65,'<tr>')
      CALL mkHeaderCellScope(65,2,0,'col','@','n')
      CALL mkHeaderCellScope(65,2,0,'col','@','Title')
      CALL mkHeaderCellScope(65,2,0,'col','@','Preadjustment')
      CALL mkHeaderCellScope(65,2,0,'col','@','Model'//Cbr//'Changed')
      CALL mkHeaderCellScope(65,2,0,'col','@','Approximation'//Cbr//
     &                       'to NA')
      CALL mkHeaderCellScope(65,0,7,'colgroup','@','Model')
      CALL mkHeaderCellScope(65,2,0,'col','@','SD(a)')
      CALL mkHeaderCellScope(65,2,0,'col','@','SEAS_NP(a)')
      CALL mkHeaderCellScope(65,2,0,'col','@','Spectral'//Cbr//
     &                       'Factors')
      CALL mkHeaderCellScope(65,2,0,'col','@','Check'//Cbr//
     &           'on <abbr title="autocorrelation function">ACF</abbr>')
      CALL mkHeaderCellScope(65,2,0,'col','@','Check'//Cbr//
     &         'on <abbr title="cross correlation function">CCF</abbr>')
      CALL mkHeaderCellScope(65,0,5,'colgroup','@','Deteriminant'//Cbr//
     &                       'Component Modified')
      CALL writTag(65,'</tr>')
      CALL writTag(65,'<tr>')
      CALL mkHeaderCellScope(65,0,0,'col','@','m&nbsp;&nbsp;')
      CALL mkHeaderCellScope(65,0,0,'col','@','p&nbsp;')
      CALL mkHeaderCellScope(65,0,0,'col','@','d&nbsp;')
      CALL mkHeaderCellScope(65,0,0,'col','@','q&nbsp;')
      CALL mkHeaderCellScope(65,0,0,'col','@','bp')
      CALL mkHeaderCellScope(65,0,0,'col','@','bd')
      CALL mkHeaderCellScope(65,0,0,'col','@','bq')
      CALL mkHeaderCellScope(65,0,0,'col','@',
     &                       '&nbsp;&nbsp;TC&nbsp;&nbsp;')
      CALL mkHeaderCellScope(65,0,0,'col','@',
     &                       '&nbsp;&nbsp;S&nbsp;&nbsp;&nbsp;')
      CALL mkHeaderCellScope(65,0,0,'col','@',
     &                       '&nbsp;&nbsp;U&nbsp;&nbsp;&nbsp;')
      CALL mkHeaderCellScope(65,0,0,'col','@','TRANSITORY')
      CALL mkHeaderCellScope(65,0,0,'col','@',
     &                       '&nbsp;&nbsp;SA&nbsp;&nbsp;')
      CALL writTag(65,'</tr>')
c
      filename = Cursrs(1:Nfilcr)//'_outP.html'
c      filename=Outdir(1:ISTRLEN(Outdir)) // '\OutPara.htm'
      call OPENDEVICE (filename,74,0,ireturn)
      CALL mkHead(74,filename,'ARMA Parameters chosen by Seats',F,1,1,F)
      CALL makDivId(74,'arma.seats.matrix','@')
      CALL mkTableTag(74,'w90',
     &                'ARMA Parameters chosen by Seats')
      CALL writTag(74,'<tr>')
      CALL mkHeaderCellScope(74,0,0,'col','@','n')
      CALL mkHeaderCellScope(74,0,0,'col','@','TITLE')
      CALL mkHeaderCellScope(74,0,0,'col','@','#NAiter')
      CALL mkHeaderCellScope(74,0,0,'col','@','Q')
      CALL mkHeaderCellScope(74,0,0,'col','@','PHI1')
      CALL mkHeaderCellScope(74,0,0,'col','@','PHI2')
      CALL mkHeaderCellScope(74,0,0,'col','@','PHI3')
      CALL mkHeaderCellScope(74,0,0,'col','@','BPHI')
      CALL mkHeaderCellScope(74,0,0,'col','@','m')
      CALL mkHeaderCellScope(74,0,0,'col','@','p')
      CALL mkHeaderCellScope(74,0,0,'col','@','d')
      CALL mkHeaderCellScope(74,0,0,'col','@','q')
      CALL mkHeaderCellScope(74,0,0,'col','@','bp')
      CALL mkHeaderCellScope(74,0,0,'col','@','bd')
      CALL mkHeaderCellScope(74,0,0,'col','@','bq')
      CALL mkHeaderCellScope(74,0,0,'col','@','TH1')
      CALL mkHeaderCellScope(74,0,0,'col','@','TH2')
      CALL mkHeaderCellScope(74,0,0,'col','@','TH3')
      CALL mkHeaderCellScope(74,0,0,'col','@','BTH')
      CALL mkHeaderCellScope(74,0,0,'col','@','mean')
      CALL writTag(74,'</tr>')
c
      filename = Cursrs(1:Nfilcr)//'_sparI.html'
*      filename=Outdir(1:ISTRLEN(Outdir)) // '\sparami.htm'
      call OPENDEVICE (filename,66,0,ireturn)
      CALL mkHead(66,filename,'Seats sparami Matrix',F,1,1,F)
      CALL makDivId(66,'seats.sparami.matrix','@')
      CALL mkTableTag(66,'w90','Seats sparami Matrix')
      CALL writTag(66,'<tr>')
      CALL mkHeaderCellScope(66,3,0,'@','@','n')
      CALL mkHeaderCellScope(66,2,6,'@',
     &                       'standard deviation of the innovation',
     &                       'SD(innov)')
      CALL mkHeaderCellScope(66,2,2,'@',
     &                    'Standard Error of the Estimate (Concurrent)',
     &                       'SE Est.'//Cbr//'(Conc.)')
      CALL mkHeaderCellScope(66,2,2,'@',
     &                    'Standard Error of the Revision (Concurrent)',
     &                       'SE Rev.'//Cbr//'(Conc.)')
      CALL mkHeaderCellScope(66,0,5,'@','@',
     &                       '<abbr title="Standard Error">SE</abbr>:'//
     &                       ' Rates of Growth')
      CALL writTag(66,'</tr>')
      CALL writTag(66,'<tr>')
      CALL mkHeaderCellScope(66,0,2,'@','@',
     &                       'SE T11'//Cbr//'(One Period)')
      CALL mkHeaderCellScope(66,0,3,'@','@',
     &                       'SE T1Mq'//Cbr//'(Annual Centered)')
      CALL writTag(66,'</tr>')
      CALL writTag(66,'<tr>')
      write(66,6600)
 6600 format('<th>TC</th><th>S</th><th>TRANS</th>',
     $       '<th>stocTD</th><th>U</th>',/,
     $       '<th>SA</th><th>TC</th><th>SA</th><th>TC</th>',
     $       '<th>SA</th>',/,'<th>TC</th><th>SA</th><th>X</th>',
     $       '<th>TC</th><th>SA</th>')
c     
      CALL writTag(66,'</tr>')
c
      filename = Cursrs(1:Nfilcr)//'_sparII.html'
c      filename=Outdir(1:ISTRLEN(Outdir)) // '\sparamii.htm'
      call OPENDEVICE (filename,67,0,ireturn)
      CALL mkHead(67,filename,'Seats sparamii Matrix',F,1,1,F)
      CALL makDivId(67,'seats.sparamii.matrix','@')
      CALL mkTableTag(67,'w90','Seats sparamii Matrix')
      CALL writTag(67,'<tr>')
      CALL mkHeaderCellScope(67,3,0,'@','@','n')
      CALL mkHeaderCellScope(67,3,0,'@','@','Title')
      CALL mkHeaderCellScope(67,0,4,'@','@',
     &                       'Convergence'//Cbr//'(in %)')
      CALL mkHeaderCellScope(67,2,3,'@','Significant Stochastic '//
     &                       'Seasonality 95th percentile',
     &                       'Signif.Stoch.'//Cbr//'Season. (95%)')
      CALL mkHeaderCellScope(67,2,2,'@','@','DAA')
      CALL writTag(67,'</tr>')

      CALL writTag(67,'<tr>')
      CALL mkHeaderCellScope(67,0,2,'@','@','1Y')
      CALL mkHeaderCellScope(67,0,2,'@','@','5Y')
      CALL writTag(67,'</tr>')
      CALL writTag(67,'<tr>')
      write(67,6700)
 6700 format('<th>TC</th><th>SA</th><th>TC</th><th>SA</th>',/
     $       '<th>Historical</th><th>Preliminary</th><th>Forecast</th>',
     $       /,'<th>TC</th><th>SA</th></tr>')
      CALL writTag(67,'</tr>')
      return
      end
C
C
C
      subroutine HPparOUT(HPper,HPlan,HPpar)
      implicit none
      include 'stream.i'
      integer HPpar
      real*8 HPlan,HPper
c
        if (HPPAR .eq. 0) then
          write(Nio,1100) HPper,' (Default value).'
        else
          write(Nio,1100) HPper,'.'
          write(Nio,'(''<p><strong>Period</strong>'',
     $       '' associated with a 50% gain of filter:'',
     $       F10.1,''</p>'')')HPper
        end if
        write(Nio,1200) HPlan
c
 1100 format('<p><strong>Period</strong> associated with a 50% ',
     $       'gain of filter:',F10.1,a,'</p>')
 1200 format('<p>Implied value for <strong>HP LAMBDA</strong>=',F15.4,
     $       ' </p>')
c
      return
      end
c
c
c
      subroutine OutHPcycle(HPcycle)
      IMPLICIT NONE
      include 'stream.i'
      integer HPCYCLE
      if (HPcycle.eq.1) then
        CALL mkPOneLine(Nio,'@','DECOMPOSITION OF THE TREND-CYCLE'//
     $        ' COMPONENT INTO : LONG-TERM TREND + CYCLE')
      else if (HPcycle.eq.2) then
        CALL mkPOneLine(Nio,'@','CYCLE EXTRACTED FROM SEASONALLY '//
     $        'ADJUSTED SERIES')
      else
        CALL mkPOneLine(Nio,'@','CYCLE EXTRACTED FROM ORIGINAL SERIES')
      end if
      return
      end
c
c
c
      subroutine OutHeadHP(MoStrCt,MoStrMt,HPth,Km,
     $         HPper,HPlam,HPpar,HPcycle,VfcBc,
     $         VfcM,VfBc,WithoutVf,MQ,DBD,Vcomp)
      IMPLICIT NONE
      include 'stream.i'
      include 'spectra.i'
      include 'sig.i'
      include 'htmlout.cmn'
      character MoStrCt*(MaxStrLength),MoStrMt*(MaxStrLength),
     $        LongTermCad*22
      real*8 HPth(3),Km,HPper,HPlam,Vcomp
      integer HPpar,HPcycle,MQ,DBD
      real*8  VfcBc,VfcM,VfBc
      integer WithoutVf
c External
      external ISTRLEN
      integer ISTRLEN
      intrinsic SQRT
c LOCAL PARAMETERS
      character StrFicMo*120
      integer lmoStrMt
      real*8 Stdc,Stdm
c
      Stdc=SQRT(Km*HPlam*Vcomp)*SQF
      Stdm=SQRT(Km*Vcomp)*SQF
      If (HPcycle.eq.1) then
        LongTermCad='LONG TERM TREND'
      else if (HPcycle.eq.2) then
        LongTermCad='SA series without BC'
      else
        LongTermCad='Series without BC'
      end if
      call strFicModel(HPth,StrFicMo)
      CALL genSkip(1060)
      CALL writTagOneLine(Nio,'h3','@','PART 6 :'//
     $    ' ESTIMATION OF THE CYCLE - MODIFIED HODRICK-PRESCOTT FILTER')
      call OutHPcycle(HPcycle)
      call HPparOUT(HPper,HPlam,HPpar)
      CALL mkPOneLine(Nio,'@','"FICTICIOUS" MODEL FOR WK '//
     $                'IMPLEMENTATION OF FILTER')
      CALL mkPOneLine(Nio,'@',StrFicMo(1:ISTRLEN(StrFicMo)))
      CALL mkPOneLine(Nio,'bold','ARIMA Models')
      CALL mkPOneLine(Nio,'@','Stochastic '//
     $                LongTermCad(1:istrlen(LongTermCad))//' m(t)'//
     $                Cbr//MoStrMt(1:ISTRLEN(MoStrMt)))
      CALL mkPOneLine(Nio,'@','Stochastic Cycle c(t)')
      CALL mkPOneLine(Nio,'@',MoStrCt(1:ISTRLEN(MoStrCt)))
      CALL mkPOneLine(Nio,'bold','Standard deviation of innovations')
      write(Nio,1000)'Long Term Trend:  ',Stdm
      write(Nio,1000)'Business Cycle:   ',Stdc
 1000 FORMAT('<p>',a,G15.4,'</p>')
      CALL mkPOneLine(Nio,'bold','FINAL ERRORS')
      if (withoutVf.eq.1) then
         CALL mkPOneLine(Nio,'@','The business Cycle Component got '//
     $     'unit roots in the AR part, so the variance of final '//
     $     'error of Business Cycle and '//
     $     LongTermCad(1:istrlen(LongTermCad))//' is infinite.')
      else if (withoutVf.gt.1) then
         CALL mkPOneLine(Nio,'@','The AR part of Business Cycle '// 
     $     'component got roots too close to unity to proper '//
     $     'calculate the final error variance')
      else if (withoutVf.eq.0) then
         write(Nio,1010)VfcBc
 1010    format('<p>Var(final error of Business Cycle Component) = ',
     $          G15.4,'</p>')
         write(Nio,1020)LongTermCad(1:istrlen(LongTermCad)),VfcM
 1020    format('<p>Var(final error of ',A,' Component)= ',
     $          G15.4,'</p>')
c           write(Nio,'(
c     $                "<p>Var(final error of Business Cycle)= ",
c     $             G15.4," in units of Va</p>")')VfBc
      end if
      CALL genSkip(1078)
      call AreaStat(spectBC,Lspect,MQ,'SPECTRUM OF CYCLE',DBD)
      end
cc
c
cc      
      subroutine WrTabHtmPol(wData,longDat,nio,icode)
      IMPLICIT NONE
C-----------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
C-----------------------------------------------------------------------
c      Parameters
      real*8 wData(*)
      integer longDat,icode,nio,nfirstCol,nlastCol
c    local
      integer i,j,lsSum,lsH,ntab,n0,n1,lCoeff
      character sSummary*80,sH*10,idstr*80,onestr*80
      logical lTop,lEnd,lOneLine
c
      include 'htmlout.cmn'
      include 'transcad.i'
c
      select case (icode)
        case(1)
          sSummary='Total moving average polynomial'
          lsSum=31
          sH='THETA'
          lsH=5
          lTop=T
          idstr='total.moving.average.polynomial'
          n0=31
          lEnd=T
          lCoeff=1
          lOneLine=F
        case(2) 
          sSummary='Seasonal polynomial coefficients'
          lsSum=32
          sH='PHIS'
          lsH=4
          lTop=T
          idstr='seasonal.polynomial.coefficients'
          n0=32
          lEnd=F
          lCoeff=1
          lOneLine=T
          onestr='Stationary seasonal polynomial'
          n1=30
        case(3) 
          sH='DELS'
          lsH=4
          lTop=F
          lEnd=F
          lCoeff=2
          lOneLine=T
          onestr='Nonstationary seasonal polynomial'
          n1=33
        case(4) 
          sH='PHIST'
          lsH=5
          lTop=F
          lEnd=T
          lCoeff=2
          lOneLine=T
          onestr='Autoregressive seasonal polynomial'
          n1=34
        case(5)
          sSummary='Seasonally adjusted polynomial coefficients'
          lsSum=43
          sH='PHIN'
          lsH=4
          lTop=T
          idstr='seasonally.adjusted.polynomial.coefficients'
          n0=43
          lEnd=F
          lCoeff=1
          lOneLine=T
          onestr='Stationary seasonally adjusted polynomial'
          n1=41
        case(6) 
          sH='DELN'
          lsH=4
          lTop=F
          lEnd=F
          lCoeff=2
          lOneLine=T
          onestr='Nonstationary seasonally adjusted polynomial'
          n1=44
        case(7) 
          sH='PHINT'
          lsH=5 
          lTop=F
          lEnd=T
          lCoeff=2
          lOneLine=T
          onestr='Nonstationary seasonally adjusted polynomial'
          n1=44
        case(8) 
          sSummary='Total Denominator (Coefficients of the total '//
     $             'autoregressive polynomial)'
          lsSum=71
          sH='PHIT'
          lsH=4    
          lTop=T
          idstr='total.denominator.AR.polynomial'
          n0=31
          lEnd=T
          lCoeff=1
          lOneLine=F
        case(10)
          sSummary='Trend-cycle polynomial coefficients'
          lsSum=35
          sH='PHIP'
          lsH=4
          lTop=T
          idstr='trend.cycle.polynomial.coefficients'
          n0=31
          lEnd=F
          lCoeff=1
          lOneLine=T
          onestr='Stationary trend-cycle polynomial'
          n1=33
        case(11)
          sH='DELP'
          lsH=4
          lTop=F
          lEnd=F
          lCoeff=2
          lOneLine=T
          onestr='Nonstationary trend-cycle polynomial'
          n1=36
        case(12)
          sH='PHIPT'
          lsH=5
          lTop=F
          lEnd=T
          lCoeff=2
          lOneLine=T
          onestr='Autoregressive trend-cycle polynomial'
          n1=37
        case(13)
          sSummary=transLcad(1:nTransLcad)//' polynomial coefficients'
          lsSum=24+nTransLcad
          sH='PHIC'
          lsH=4
          lTop=T
          idstr=transLcad(1:nTransLcad)//'.polynomial.coefficients'
          n0=24+nTransLcad
          lEnd=F
          lCoeff=1
          lOneLine=T
          onestr='Stationary '//transLcad(1:nTransLcad)//' polynomial'
          n1=23+nTransLcad
        case(14)
          sH='DELC'
          lsH=4
          lTop=F
          lEnd=F
          lCoeff=2
          lOneLine=T
          onestr='Nonstationary '//transLcad(1:nTransLcad)//
     &           ' polynomial'
          n1=26+nTransLcad
        case(15)
          sH='PHIC'
          lsH=4
          lTop=F
          lEnd=T
          lCoeff=2
          lOneLine=T
          onestr='Autoregressive '//transLcad(1:nTransLcad)//
     &           ' polynomial'
          n1=27+nTransLcad
        case(31) 
          sSummary='Trend-Cycle model'
          lsSum=17
          sH='THP'
          lsH=3
          lTop=T
          idstr='trend.cycle.model'
          n0=17
          lEnd=F
          lCoeff=0
          lOneLine=T
          onestr='Numerator Coefficients ('//
     &           '<abbr title="moving average">MA</abbr> polynomial)'
          n1=75
        case(32) 
          sH='PHIPT'
          lsH=5
          lTop=F
          lEnd=F
          lCoeff=0
          lOneLine=T
          onestr='Denominator Coefficients ('//
     &           '<abbr title="autoregressive">AR</abbr> polynomial)'
          n1=77
        case(33)
          sSummary='Seasonal Model'
          lsSum=13
          sH='THS'
          lsH=3
          lTop=T
          idstr='seasonal.model'
          n0=13
          lEnd=F
          lCoeff=0
          lOneLine=T
          onestr='Numerator Coefficients ('//
     &           '<abbr title="moving average">MA</abbr> polynomial)'
          n1=75
        case(34)
          sH='PHIST'
          lsH=5
          lTop=F
          lEnd=F
          lCoeff=0
          lOneLine=T
          onestr='Denominator Coefficients ('//
     &           '<abbr title="autoregressive">AR</abbr> polynomial)'
          n1=77
        case(35)
          sSummary=transLcad(1:nTransLcad)//' model'
          lsSum=6+nTransLcad
          sH='THC'
          lsH=3
          lTop=T
          idstr=transLcad(1:nTransLcad)//'.model'
          n0=6+nTransLcad
          lEnd=F
          lCoeff=0
          lOneLine=T
          onestr='Numerator Coefficients ('//
     &           '<abbr title="moving average">MA</abbr> polynomial)'
          n1=75
        case(36)
          sH='PHICT'
          lsH=5 
          lTop=F
          lEnd=F
          lCoeff=0
          lOneLine=T
          onestr='Denominator Coefficients ('//
     &           '<abbr title="autoregressive">AR</abbr> polynomial)'
          n1=77
        case(37)
          sSummary='Seasonally Adjusted Series Model'
          lsSum=57
          sH='THN'
          lsH=3
          lTop=T
          idstr='seasonally.adjusted.series.model'
          n0=32
          lEnd=F
          lCoeff=0
          lOneLine=T
          onestr='Numerator Coefficients ('//
     &           '<abbr title="moving average">MA</abbr> polynomial)'
          n1=75
        case(38)
          sH='PHINT'
          lsH=5      
          lTop=F
          lEnd=F
          lCoeff=0
          lOneLine=T
          onestr='Denominator Coefficients ('//
     &           '<abbr title="autoregressive">AR</abbr> polynomial)'
          n1=77
        case(41)
          sH='VAR(TC)'
          lsH=7
          lTop=F
          lEnd=T
          lCoeff=0
          lOneLine=T
          onestr='INNOVATION VARIANCE in units of <abbr title="'//
     $           'variance">VAR</abbr>(A)'
          n1=69
        case(42)
          sH='VAR(S)'
          lsH=6
          lTop=F
          lEnd=T
          lCoeff=0
          lOneLine=T
          onestr='INNOVATION VARIANCE in units of <abbr title="'//
     $           'variance">VAR</abbr>(A)'
          n1=69
        case(43)
          sH='VAR(C)'
          lsH=6
          lTop=F
          lEnd=T
          lCoeff=0
          lOneLine=T
          onestr='INNOVATION VARIANCE in units of <abbr title="'//
     $           'variance">VAR</abbr>(A)'
          n1=69
        case(44)
          sH='VAR(SA)'
          lsH=7
          lTop=F
          lEnd=T
          lCoeff=0
          lOneLine=T
          onestr='INNOVATION VARIANCE in units of <abbr title="'//
     $           'variance">VAR</abbr>(A)'
          n1=69
        case(45)
          sSummary='Irregular model'
          lsSum=15
          sH='VAR(I)'
          lsH=6
          lTop=T
          idstr='irregular.variance'
          n0=18
          lEnd=T
          lCoeff=0
          lOneLine=T
          onestr='INNOVATION VARIANCE in units of <abbr title="'//
     $           'variance">VAR</abbr>(A)'
          n1=69
      end select          
c
c        do j=0,Ntab
      IF(lTop)THEN
        CALL makDivId(Nio,idstr(1:n0),'pol')
        CALL mkTableTag(Nio,'w30',sSummary(1:lsSum))
        CALL mkCaption(Nio,sSummary(1:lsSum))
      END IF
      IF(lCoeff.eq.1)THEN
        IF(lOneLine)THEN
          indCoef=indCoef+1
          WRITE(Nio,1001)indCoef
        ELSE
          WRITE(Nio,1011)
        END IF
      END IF
      IF(lOneLine)THEN
        indMdp=indMdp+1
        WRITE(Nio,1002)indMdp,onestr(1:n1)
      END IF
      DO j = 1, longDat
        IF(LCoeff.gt.0)THEN
          IF(lOneLine)THEN
            indMdl=indMdl+1
            write(nio,1003)indMdl,sH(1:lsH),j-1,indMdp,indMdl,indCoef,
     &                     wData(j)
          ELSE
            WRITE(Nio,1012)sH(1:lsH),j-1,wData(j)
          END IF
        ELSE
          indMdl=indMdl+1
          IF(icode.gt.40)THEN
            write(nio,1014)indMdl,sH(1:lsH),indMdp,indMdl,wData(j)
          ELSE
            write(nio,1004)indMdl,sH(1:lsH),j-1,indMdp,indMdl,wData(j)
          END IF
        END IF
      END DO
      IF(Lend)THEN
        CALL writTag(Nio,'</table></div>')
        CALL mkPOneLine(Nio,'@','&nbsp;')
      END IF
c        end do 
c-----------------------------------------------------------------------
 1001 format('<tr><th class="head">&nbsp;</th><th id="coef',i4.4,
     $       '">Coefficients</th></tr>') 
 1011 format('<tr><th class="head">&nbsp;</th><th scope="col">',
     $       'Coefficients</th></tr>')
 1002 format('<tr><th colspan="2" id="mdp',i4.4,'" class="left">',a,
     $       '</th></tr>')
 1012 format('<tr><th scope="row">',a,'(',i2,')</th><td class="right">',
     $       f8.4,'</td></tr>')
 1003 format('<tr><th id="mdlg',i4.4,'">',a,'(',i2,')</th>',
     $       '<td headers="mdp',i4.4,' mdlg',i4.4,' coef',i4.4,
     $       '" class="right">',f8.4,'</td></tr>')
 1004 format('<tr><th id="mdlg',i4.4,'">',a,'(',i2,')</th>',
     $       '<td headers="mdp',i4.4,' mdlg',i4.4,'" class="right">',
     $       f8.4,'</td></tr>')
 1014 format('<tr><th id="mdlg',i4.4,'">',a,'</th>',
     $       '<td headers="mdp',i4.4,' mdlg',i4.4,'" class="right">',
     $       f8.4,'</td></tr>')
c-----------------------------------------------------------------------
      return
      end
cc
c
cc
      subroutine plotWkFilters(wkp,wkn,wks,wkc,wki,nfilters,nio)
      IMPLICIT NONE
C.. Parameters ..
      INCLUDE 'srslen.prm'
      integer mp
      parameter (mp = POBS)
c Formal Arguments ..
      real*8 wkp(mp),wkn(mp),wks(mp),wkc(mp),wki(mp)
      integer nfilters,nio
c Local Variables
      integer i
      include 'transcad.i'
c
      CALL makDivId(Nio,'wiener.kolmogorov.filters','@')
      CALL mkTableTag(nio,'w60',
     &     'Wiener-Kolmogorov filters (one side) for the components')
      CALL mkCaption(nio,'Filters')
      CALL writTag(Nio,'<thead>')
      CALL writTag(Nio,'<tr>')
      write(nio,100)'TREND-CYCLE',
     $      '<acronym title="Seasonal Adjusted">SA</acronym> SERIES',
     &      'SEASONAL',transLCad(1:nTransLCad),'IRREGULAR'
 100  FORMAT('<td class="head">&nbsp;</td>',5('<th scope="col">',a,
     $       '</th>'))
      CALL writTag(Nio,'</tr>')
      CALL writTag(Nio,'</thead>')
      CALL writTag(Nio,'<tbody>')
      do i=1,nfilters
        write(nio,110)i-1,wkp(i),wkn(i),wks(i),wkc(i),wki(i)
 110    FORMAT('<tr><th scope="row">LAG ',i3,'</th>',
     $         5('<td class="center">',f7.4,'</td>'),'</tr>') 
      end do
      CALL writTag(Nio,'</tbody>')
      CALL writTag(Nio,'</table></div>')
      CALL mkPOneLine(Nio,'@','&nbsp;')
      end
cc
c
cc
c-----------------------------------------------------------------------
c  Icode 1 --> ASYMMETRIC TREND-CYCLE CONCURRENT ESTIMATOR FILTER
c  Icode 2 --> ASYMMETRIC SA CONCURRENT ESTIMATOR FILTER
c-----------------------------------------------------------------------
      Subroutine PlotAsymWeight(Weight,nio,icode)
      IMPLICIT NONE
C.. Parameters ..
      integer mx
      parameter (mx = 300)
c Formal Arguments ..
      real*8 Weight(0:mx*2)
      integer nio,icode
c Local Variables
      integer i,j
      character sEnd*2
      INCLUDE 'htmlout.cmn'
c     
c     
      if (icode.eq.2) then
        sEnd='_N'
        CALL makDivId(Nio,'sa.concurrent.filter.infinite','@')
      else if (icode.eq.1) then
        sEnd='_P'
        CALL makDivId(Nio,'trend.concurrent.filter.infinite','@')
      else if (icode.eq.4) then
        sEnd='_M'
        CALL makDivId(Nio,'sa.concurrent.filter.finite','@')
      else if (icode.eq.3) then
        sEnd='_O'
        CALL makDivId(Nio,'trend.concurrent.filter.finite','@')
      end if
      CALL mkTableTag(Nio,'w60','@')
      if (icode.eq.2) then
        CALL mkCaption(Nio,'WEIGHTS FOR ASYMMETRIC SA CONCURRENT'//
     $            ' ESTIMATOR FILTER'//Cbr//'<span class="em">'//
     $            '(semi-infinite realization)</span>')
      else if (icode.eq.1) then
        CALL mkCaption(Nio,'WEIGHTS FOR ASYMMETRIC TREND '//
     $        'CONCURRENT ESTIMATOR FILTER'//Cbr//'<span class="em">'//
     $        '(semi-infinite realization)</span>')
      else if (icode.eq.4) then
        CALL mkCaption(Nio,'WEIGHTS FOR ASYMMETRIC SA CONCURRENT'//
     $            ' ESTIMATOR FILTER'//Cbr//'<span class="em">'//
     $            '(finite realization)</span>')
      else if (icode.eq.3) then
        CALL mkCaption(Nio,'WEIGHTS FOR ASYMMETRIC TREND '//
     $        'CONCURRENT ESTIMATOR FILTER'//Cbr//'<span class="em">'//
     $        '(finite realization)</span>')
      end if
      write(Nio,1010) sEnd,sEnd,sEnd
 1010 FORMAT('<tr><th>exp(B)</th><th id="w_1',A2,'">WEIGHTS</th>',
     $           '<th>exp(B)</th><th id="w_2',A2,'">WEIGHTS</th>',
     $           '<th>exp(B)</th><th id="w_3',A2,'">WEIGHTS</th></tr>')
      i=0
      do while (i.lt.8)
       write(Nio,1020)(j,sEnd,j,j,sEnd,sEnd,Weight(j), j=i,i+2)
 1020  FORMAT('<tr>',
     $    '<td axis="exp(B)" id="ex',i1,A2,'" class="center">',I2,
     $    '</td><td headers="ex',i1,A2,' w_1',A2,'" class="center">',
     $       F9.6,'</td>',/,
     $    '<td axis="exp(B)" id="ex',i1,A2,'" class="center">',I2,
     $    '</td><td headers="ex',i1,A2,' w_2',A2,'" class="center">',
     $       F9.6,'</td>',/,
     $    '<td axis="exp(B)" id="ex',i1,A2,'" class="center">',I2,
     $    '</td><td headers="ex',i1,A2,' w_3',A2,'" class="center">',
     $       F9.6,'</td></tr>') 
       i=i+3
      end do
      write(Nio,1030) (sEnd,sEnd,sEnd,Weight(j), j=9,11)
 1030 FORMAT('<tr>',
     $       '<td axis="exp(B)" id="ex9',A2,'" class="center"> 9</td>',
     $       '<td headers="ex9',A2,' w_1',A2,'" class="center">',
     $       F9.6,'</td>',/,
     $       '<td axis="exp(B)" id="ex10',A2,'" class="center">10</td>',
     $       '<td headers="ex10',A2,' w_2',A2,'" class="center">',
     $       F9.6,'</td>',/,
     $       '<td axis="exp(B)" id="ex11',A2,'" class="center">11</td>',
     $       '<td headers="ex11',A2,' w_3',A2,'" class="center">',
     $       F9.6,'</td></tr>')
      i=i+3
      do while (i.lt.60)
       write(Nio,1040)(j,sEnd,j,j,sEnd,sEnd,Weight(j), j=i,i+2)
 1040  FORMAT('<tr>',
     $    '<td axis="exp(B)" id="ex',i2,A2,'" class="center">',I2,
     $    '</td><td headers="ex',i2,A2,' w_1',A2,'" class="center">',
     $       F9.6,'</td>',/,
     $    '<td axis="exp(B)" id="ex',i2,A2,'" class="center">',I2,
     $    '</td><td headers="ex',i2,A2,' w_2',A2,'" class="center">',
     $       F9.6,'</td>',/,
     $    '<td axis="exp(B)" id="ex',i2,A2,'" class="center">',I2,
     $    '</td><td headers="ex',i2,A2,' w_3',A2,'" class="center">',
     $       F9.6,'</td></tr>')
        i=i+3
       end do
       write (Nio,1050)sEnd,sEnd,sEnd,Weight(60)
 1050  FORMAT('<tr><td axis="exp(B)" id="ex60',A2,'" class="center">60',
     $   '</td><td headers="ex60',A2,' w_2',A2,'" class="center">',
     $    F9.6,'</td><td colspan="4">&nbsp;</td></tr>') 
       CALL writTag(Nio,'</table></div>')
       CALL mkPOneLine(Nio,'@','&nbsp;')
      end
c
c      
      subroutine writeF69Note(fileUnit)
      integer fileUnit
      logical T,F
      parameter(T=.true.,F=.false.)
      include 'htmlout.cmn'
c
c      write (fileUnit,'(''<p><br> mq=12:   *(1)= 2.1878 rad ,   '',
c     $             ''*(2)= 2.7143 rad<br>'')')
c      write (fileUnit,
c     $         '('' mq=4 :  *(1)= 0.2802 rad ,   *(2)= 0.5611 rad'',
c     $             ''</p>'')')
      CALL mkPOneLine(fileUnit,'@','mq=12:   TD= 2.1878 rad '//Cbr//
     $                ' mq=4 :  TD= 0.2802 rad ')
      CALL writln('AT : peaks detected in AR(30) and using Tukey '//
     $            'spectrum estimator'//Cbr,fileUnit,0,T,F)
      CALL writln('A- : only peaks detected in AR(30) spectrum '//
     $            'estimator'//Cbr,fileUnit,0,F,F)
      CALL writln('-T : only peaks detected using Tukey estimator '//
     $            'spectrum'//Cbr,fileUnit,0,F,F)
      CALL writln('-- : No peaks detected in AR(30) nor using Tukey '//
     $            'spectrum estimator)',fileUnit,0,F,T)
      end
c
*      subroutine openPeakHtm(u)
*      integer u
*      character filename*180
*      integer istrlen,ireturn
*      external istrlen
*      include 'dirs.i'
*      include 'stdio.i'
*c
*      filename = Cursrs(1:Nfilcr)//'_pks.html'
**       filename=Outdir(1:ISTRLEN(Outdir)) // '\peaks.htm'
*      call OPENDEVICE (filename,u,0,ireturn)
*      CALL mkHead(u,filename,'Spectral Diagnostics Matrix',F,1,1,F)
*      CALL mkTableTag(u,'w90','Spectral Diagnostics Matrix')
*      CALL writTag(u,'<thead>')
*      CALL writTag(u,'<tr>')
*      CALL mkHeaderCellScope(u,3,0,'col','@','nser')
*      CALL mkHeaderCellScope(u,3,0,'col','@','Title')
*      CALL mkHeaderCellScope(u,0,8,'colgroup',
*     &                       'Seasonally Adjusted Series','SA SERIES')
*      CALL mkHeaderCellScope(u,0,8,'colgroup','@','RESIDUALS')
*      CALL writTag(u,'</tr>')
*      CALL writTag(u,'<tr>')
*      CALL mkHeaderCellScope(u,0,6,'colgroup','@',
*     $                       'Seasonal frequencies (cycles per year')
*      CALL mkHeaderCellScope(u,0,2,'colgroup',
*     $                       'Trading day frequency (radians)',
*     $                       'TD freq.(rad.)')
*      CALL writTag(u,'</tr>')
*      CALL writTag(u,'<tr>')
*      write(u,1010)'one','two','three','four','five','six'
*      write(u,1010)'one','two','three','four','five','six'
* 1010 format(6('<th scope="col">',a,'</th>'))
*      write(u,1020)'TD','TD1'
* 1020 format(2('<th scope="col">',a,'</th>'))
*      CALL writTag(u,'</tr>')
*      CALL writTag(u,'</thead>')
*      CALL writTag(u,'<tbody>')
*      end
cc
c
cc
      subroutine openPeakHtm2(ifail)
      character filename*180
      integer istrlen,ifail
      external istrlen
      include 'dirs.i'
      include 'stdio.i'
      filename = Cursrs(1:Nfilcr)//'_pks.html'
*       filename=Outdir(1:ISTRLEN(Outdir)) // '\peaks.htm'
       call OPENDEVICE (filename,69,0,ifail) 
       call tablePeakHtm(69,12,'SA',1,Lxhtml)
      filename = Cursrs(1:Nfilcr)//'_pksI.html'
*       filename=Outdir(1:ISTRLEN(Outdir)) // '\peaksIr.htm'
       call OPENDEVICE (filename,72,0,ifail) 
       call tablePeakHtm(72,12,'Irregular',1,Lxhtml)
      filename = Cursrs(1:Nfilcr)//'_pksT.html'
*       filename=Outdir(1:ISTRLEN(Outdir)) // '\peaksTr.htm'
       call OPENDEVICE (filename,73,0,ifail)
       call tablePeakHtm(73,12,'Trend-Cycle',1,Lxhtml)  
      end
cc
c
cc
      subroutine ClosePeaksMatrix(fileUnit)
      implicit none
      integer fileUnit 
      logical bool
       inquire (unit=fileUnit,opened=bool)
       if (bool) then 
        CALL writTag(fileUnit,'</table>')
        CALL mkPOneLine(fileUnit,'@','&nbsp;')
        call writeF69Note(fileUnit)
        CALL writTag(fileUnit,'</body>')
        CALL writTag(fileUnit,'</html>')
        close(fileUnit)
       end if
      return
      end 
c
c
c
      subroutine wrLnTabPeaks(fileUnit,niter,matTitle,picos,IsTable)
      implicit none
C     INPUT PARAMETERS
      integer fileUnit,niter,IsTable
      character matTitle*180,picos(7)*2
c     LOCAL VARIABLES
      integer i,tmp
c     EXTERNAL 
      character cadTablePeaks*180,cadSummPeaks*180
c---------------------
      tmp= 6
      if (IsTable.gt.0) then
       write(cadTablePeaks,1000)
     $       '(''<tr><td scope="row">'',i4,''</td>'',',
     $       tmp+2,'(''<td>'',a,''</td>''),''</tr>'')'
       write(fileUnit,cadTablePeaks) 
     $               niter,mattitle(1:22),(picos(i),i=1,7)
      else
       write(cadSummPeaks,1000) '(''<tr>'',',tmp+1, 
     $     '(''<td>'',a,''</td>''),''</tr></tbody></table>'')'
       write(fileUnit,cadSummPeaks) (picos(i),i=1,7)
       CALL mkPOneLine(fileUnit,'@','&nbsp;')
      end if
      return
 1000 FORMAT(A,I1,A)
      end subroutine
c
c     
c rober: Esta rutina queda por modificar! La salida no ajusta bien y no estaba bien parametrizada
c        faltan en las cabeceras para peaks*.m n y nser
c
c     tableHeadPeaks: write the head of peaks tables in peaks*.m or in summary*.txt 
      subroutine tableHeadPeaks(io,MQ,CompName,isTable)
      implicit none
      integer io,MQ,isTable
      character CompName*(*)
c     LOCAL VARIABLES
      character*80 cadTableHead
      character*6 cad(6)
      integer i,skipPos,tmp
      data cad /'one   ','two   ','three ','four  ' ,'five  ','six   '/
c -------------------
      if (isTable.eq.1) then
        skipPos=25
        tmp=6
      else
        skipPos=2
        tmp=MQ/2
      end if
      write(cadTableHead,1010) skipPos,CompName
 1010 format('(',I2,'X,''Stochastic Component: ',A,''')') 
      write(io,cadTableHead)
      if (tmp.eq.6) then
          write (cadTableHead,1020) '(',skipPos,'x,5x,',
     $    '''Seasonal frequencies(cycles per year and TD freq.(rad.)'')'
 1020     format(A,I2,A,A)
          write(io,cadTableHead)
      else
          write (cadTableHead,1030)
     $          '(',skipPos,'x,5x,e''SEAS. freq. TD(rad.)'')'
 1030     format(A,I2,A)
          write(io,cadTableHead)
      end if
c
      write (cadTableHead,1040) '(',skipPos,'x,4x,',tmp*6,
     $                          '(''-''),2x,7(''-''))'
 1040 format(A,I2,A,I2,A)
      write (io,cadTableHead)
      write(cadTableHead,1050) '(',skipPos+5,'x,',tmp+1,'(A,x))'
 1050 format(A,I2,A,I1,A)
      write (io,cadTableHead)  (cad(i),i=1,tmp)," TD   " 
      end subroutine
c
c**********************************************  start here
c     tablePeakHtm: like tableHeadPeaks for case HTML=1
      subroutine tablePeakHtm(u,MQ,CompName,isTable,Lxhtml)
      integer u,MQ,isTable
      character CompName*(*)
      logical Lxhtml
      integer istrlen
      external istrlen
      include 'dirs.i'
c     LOCAL VARIABLES
      character*300 cad1
      character*6 cad(6)
      integer i
      data cad /'one  ','two  ','three','four ' ,'five ','six  '/
c -------------------
c
      if (isTable.eq.0) then
        write (u,'(''<table>'')')
        write (u,'("<caption>Spectral Diagnostics ",A
     $                       "</caption>")') CompName
      else
       call WriteDoctype(u,1)
       write(u,'(''<HTML lang="en-US">'')')
       write(u,'(''<HEAD>'')')
       write(u,'(''<META HTTP-EQUIV="Content-Type" '',
     &             ''CONTENT="text/html; charset=iso-8859-1">'')')
       write(u,'(''<META NAME="lang" CONTENT="en-US" >'')')
       write(u,'(''<TITLE>Spectral Diagnostics '',A,'' </TITLE>'')')
     $       CompName
       call writeCSS(u,1,Lxhtml)
       write (u,'(''</HEAD><BODY>'')') 
       write (u,'(''<table summary="Spectral Diagnostics '',A,''.">'')')
     $       CompName
      end if
      write (u,'(A)')  '<thead>'   
      if (isTable.eq.1) then      
       write (cad1,'(A,A,A,I1,A,A,A)')
     $        '(''<tr><th scope="col" rowspan="2">nser</th>',
     $        '<th scope="col" rowspan="2">Title</th>',
     $        '<th scope="colgroup" colspan="',
     $         MQ/2,
     $        '">Seasonal frequencies(cycles per ',
     $        'year)</th><th scope="col" rowspan="2">',
     $        'TD freq.(rad.)</th></tr>'')'
       write(u,cad1)
      else
       write (cad1,'(A,A,I1,A,A,A)')
     $        '(''<tr><th scope="colgroup" ',
     $        'colspan="',MQ/2,'">Seasonal frequencies(cycles per ',
     $        'year)</th><th scope="col" rowspan="2">',
     $        'TD freq.(rad.)</th></tr>'')'
       write(u,cad1)
      end if
      write(cad1,'(A,I1,A)') '(''<tr>'',',MQ/2,
     $     '(''<th scope="col">'',A,''</th>''),''</tr></thead>'')' 
      write (u,cad1) (cad(i),i=1,MQ/2)
      write(u,'(''<tbody>'')')
      end subroutine
c
c
c      
      subroutine warnPeaks(nio,picos,nameType,mq)
      implicit none
      integer nio,mq
      character picos(7)*2,nameType*20
      character auxwrPeak*5,lnPeak*40
      integer ipeaks
      integer ISTRLEN
      external ISTRLEN
c     LOCAL PARAMETERS
      integer i
c---------------------------------------
      if ((picos(7)(1:1).eq.'A') .or. (picos(7)(2:2).eq.'T')) then
        CALL mkPOneLine(nio,'em','Detected a Spectral peak in '//
     $                  nameType(1:ISTRLEN(nameType))//
     $                  ' for the TD frequency')
      end if
      ipeaks=0
      auxwrPeak=' '
      lnpeak=' '
      do i = 1,6
        if ((picos(i)(1:1).eq.'A').or.(picos(i)(2:2).eq.'T')) then
          ipeaks=ipeaks+1
          write(auxwrPeak,1000) i*12/MQ
 1000     format(I1,"PI/6")
          lnPeak=lnpeak(1:ISTRLEN(lnpeak))//' '//auxwrpeak
c          if (HTML .eq. 1) then
c           write (nio,'(''<p><em>There is a Spectral peak in '',A,
c     $             '' for the Seasonal frequency : '',I1,
c     $              ''PI/6</em></p>'')') 
c     $               nameType(1:ISTRLEN(nameType)) , i*12/MQ  
c          else
c           write (nio,'(4x,''There is a Spectral peak in '',A,
c     $              '' for the Seasonal frequency : '',I1,
c     $            ''PI/6'')')  nameType(1:ISTRLEN(nameType)) , i*12/MQ
c          end if
        end if       
      end do
      if (ipeaks.gt.0) then
       if (ipeaks.eq.1) then
        CALL mkPOneLine(nio,'em','There is a Spectral peak in '//
     $                  nameType(1:ISTRLEN(nameType))// 
     $                  ' for the Seasonal frequency : '//LnPeak(1:6))
       else
        CALL mkPOneLine(nio,'em','There is a Spectral peak in '//
     $                  nameType(1:ISTRLEN(nameType))// 
     $                  ' for the Seasonal frequencies : '//
     $                  LnPeak(1:ISTRLEN(LnPeak)))
       end if
      end if
      end
c
      subroutine wrHeadTGenSumS(nio)                 
      implicit none
c-----------------------------------------------------------------------
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      integer nio
c     
      CALL mkTableTag(nio,'x11','Decomposition : General')
      CALL mkCaption(nio,'Decomposition : General')
      write (nio,1010)Cbr,Cbr,Cbr,Cbr,Cbr,Cbr
 1010 FORMAT('<tr><th rowspan="2" scope="col">Preadjustment</th>',/,
     $       '<th rowspan="2" scope="col">Model',a,'Changed</th>',/,
     $       '<th rowspan="2" scope="col">Approx.',a,'to NA</th>',/,
     $       '<th colspan="7" scope="colgroup">Model</th>',/,
     $       '<th scope="col" rowspan="2">SD(a)</th>',/,
     $       '<th rowspan="2" scope="col">SEAS_NP(a)</th>',/,
     $       '<th rowspan="2" scope="col">Spectr.',a,'Factor</th>',/,
     $       '<th rowspan="2" scope="col">Check',a,'on ACF</th>',/,
     $       '<th rowspan="2" scope="col">Check',a,'on CCF</th>',/,
     $       '<th scope="colgroup" colspan="5">Determ.',a,
     $       'Comp. Modif.</th></tr>')
      write (nio,1020)
 1020 format('<tr><th scope="col">m</th><th scope="col">p</th>',/,
     $           '<th scope="col">d</th><th scope="col">q</th>',/,
     $           '<th scope="col">bp</th><th scope="col">bd</th>',/,
     $           '<th scope="col">bq</th><th scope="col">TC</th>',/,
     $           '<th scope="col">S</th><th scope="col">U</th>',/,
     $           '<th scope="col">TRANS</th><th scope="col">SA</th>',
     $       '</tr>')
c-----------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
cc
c
cc
      subroutine wrHeadTparISumS(nio,IsCloseToTD)
      implicit none
c-----------------------------------------------------------------------
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      integer nio
      logical IsCloseToTD
c      
      character auxS*6
c      
      CALL mkTableTag(nio,'x11','Decomposition : Standard Errors')
      CALL mkCaption(nio,'Decomposition : Standard Errors')
      write (nio,1010)Cbr,Cbr
 1010 FORMAT('<tr>',
     $       '<th rowspan="2" colspan="5" scope="colgroup">',
     &       'Standard Deviation(innovation)</th>',
     $       '<th colspan="2" rowspan="2" scope="colgroup">',
     $       'Standard Error Estimate',a,'(Concurrent)</th>',
     $       '<th colspan="2" rowspan="2" scope="colgroup">',
     $       'Standard Error Revison',a,'(Concurrent)</th>',
     $       '<th colspan="5" scope="colgroup">',
     $       'Standard Error : Rates of Growth</th></tr>')
      write (nio,1020)Cbr,Cbr
 1020 FORMAT('<tr><th colspan="2" scope="colgroup">SE T11',a,
     $       '(One Period)</th>',/,
     $       '<th colspan="3" scope="colgroup">SE T1Mq',a,
     $       '(Annual Centered)</th></tr>')
       if (IsCloseToTD) then
        auxS='stocTD'
       else
        auxS='TRANS '
       end if
      write (nio,1030)auxS
 1030 FORMAT('<tr><th scope="col">TC</th><th scope="col">S</th>',/,
     $       '<th scope="col">',a,'</th><th scope="col">U</th>',/,
     $       '<th scope="col">SA</th><th scope="col">TC</th>',/,
     $       '<th scope="col">SA</th><th scope="col">TC</th>',/,
     $       '<th scope="col">SA</th><th scope="col">TC</th>',/,
     $       '<th scope="col">SA</th><th scope="col">X</th>',/,
     $       '<th scope="col">TC</th><th scope="col">SA</th></tr>')
      end
c
      subroutine wrHeadTparIISumS(nio)
      implicit none
c-----------------------------------------------------------------------
      integer nio
c-----------------------------------------------------------------------
      CALL mkTableTag(nio,'x11','Decomposition : Properties')
      CALL mkCaption(nio,'Decomposition : Properties')
      write (nio,1010)
 1010 format('<thead><tr>',
     $     '<th colspan="4" scope="colgroup">Convergence (in %)</th>',/,
     $     '<th colspan="3" rowspan="2" scope="colgroup">',
     $     'Significant Stochastic Seasonality (95%)</th>',/,
     $     '<th colspan="2" rowspan="2" scope="colgroup">DAA</th></tr>')
      write (nio,1020)
 1020 format('<tr><th colspan="2" scope="colgroup">1Y</th>',
     $       '<th colspan="2" scope="colgroup">5Y</th></tr>')
      write (nio,1030)
 1030 format('<tr><th scope="col">TC</th><th scope="col">SA</th>',/,
     $       '<th scope="col">TC</th><th scope="col">SA</th>',/,
     $       '<th scope="col">Hist.</th><th scope="col">Prel.</th>',/,
     $       '<th scope="col">Fore.</th><th scope="col">TC</th>',/,
     $       '<th scope="col">SA</th></tr></thead>')
      end 
cc
c
cc
      subroutine tablaPicos(u,SA,TR,IR,mq,
     $                      totalSeasTR,totalSeasSA,totalSeasIR)
      implicit none
      logical T,F
      parameter(T=.true.,F=.false.)
      integer u,mq,totalSeasTR,totalSeasSA,totalSeasIR
      integer istrlen
      character SA(7)*2,TR(7)*2,IR(7)*2,srad*6
      logical SeasSpectCrit2,TDSpectCrit
      external SeasSpectCrit2,TDSpectCrit
      include 'units.cmn'
      include 'htmlout.cmn'

c     LOCAL VARIABLES
      character*7 cad(7)
      character*1 wchar
      integer i
      data cad /'  One  ','  Two  ',' Three ',
     $          ' Four  ',' Five  ','  Six  ','  TD   '/
c     
c
      if ((mq.ne.4) .and. (mq.ne.12)) return
      if (mq.eq.12) then
       sRad='2.1878'
      else
       sRad='0.2802'
      end if
       if (u.eq.Mt1) then
        CALL genSkip(1045)
       end if
       CALL mkTableTag(u,'w70','STOCHASTIC SEASONAL AND TRADING '//
     $                 'DAY SPECTRAL PEAKS')
       if (u.eq.Mt1) then 
        CALL mkCaption(u,'STOCHASTIC SEASONAL AND TRADING DAY '//
     $                 'SPECTRAL PEAKS')
       else
        CALL mkCaption(u,'Stochastic seasonal and trading day '//
     $                 'spectral peaks')     
       end if
       write(u,1010) int(mq/2),Cbr,sRad
 1010  format('<thead><tr><td rowspan="2" class="head"></td>',
     $   '<th id="fS_p" colspan="',i1, '">Frequency (cycles per yer)',
     $   '</th><th rowspan="2" id="fTD_p"><abbr title="Trading Day">TD',
     $   '</abbr>',a,'(',A6,'<abbr title="radians">rad</abbr>)</th>',
     $   '</tr>') 
       if (mq.eq.12) then
        write(u,1020) (i, cad(i), i=1,6)
 1020   format('<tr>',6('<th id="fS',i1,'_p">',a,'</th>'),'</tr>')
       else
        write(u,1060) (i, cad(i), i=1,2)
 1060   format('<tr>',2('<th id="fS',i1,'_p">',a,'</th>'),'</tr>')
       end if
       CALL writTag(u,'</thead>')
       CALL writTag(u,'<tbody>')
       if (mq.eq.12) then
        write(u,1030) 'SA_p', '<abbr title="Seasonally'//
     $         ' adjusted">SA</abbr> series', ('SA_p', i, SA(i), i=1,6), 
     $         'SA_p', SA(7)
        write(u,1030) 'TR_p', 'Trend-cycle', ('TR_p', i, TR(i), i=1,6),
     $         'TR_p', TR(7)
        write(u,1030) 'IR_p', 'Irregular', ('IR_p', i, IR(i), i=1,6), 
     $         'IR_p', IR(7)
 1030   format('<tr><th id="',a,'">',a,'</th>',/,
     $         2('<td class="center" headers="',a,' fS_p fS',i1,'_p" >',
     $         A2,'</td>'),/,
     $         2('<td class="center" headers="',a,' fS_p fS',i1,'_p" >',
     $         A2,'</td>'),/,
     $         2('<td class="center" headers="',a,' fS_p fS',i1,'_p" >',
     $         A2,'</td>'),/,
     $         '<td class="center" headers="',a,' fTD_p" >',A2,
     $         '</td></tr>') 
       else 
        write(u,1070) 'SA_p', '<abbr title="Seasonally'//
     $         ' adjusted">SA</abbr> series', ('SA_p', i, SA(i), i=1,2),  
     $         'SA_p', SA(7)
        write(u,1070) 'TR_p', 'Trend-cycle', ('TR_p', i, TR(i), i=1,2), 
     $         'TR_p', TR(7) 
        write(u,1070) 'IR_p', 'Irregular', ('IR_p', i, IR(i), i=1,2), 
     $         'IR_p', IR(7)
 1070   format('<tr><th id="',a,'">',a,'</th>',/,
     $         2('<td class="center" headers="',a,' fS_p fS',i1,'_p" >',
     $         A2,'</td>'),/,
     $         '<td class="center" headers="',a,' fTD_p" >',A2,
     $         '</td></tr>') 
       end if
       CALL writTag(u,'</tbody>')
       CALL writTag(u,'</table>')
       CALL mkPOneLine(u,'@','&nbsp;')
       write (u,1100)
 1100  format('<p class="w50"><strong> AT</strong> : peaks',
     $       ' detected in AR(30) and using Tukey spectrum estimator')
       write (u,1110)Cbr,'A-','only peaks','in AR(30)'
       write (u,1110)Cbr,'-T','only peaks','using Tukey'
       write (u,1110)Cbr,'--','No peaks','in AR(30) nor using Tukey'
 1110  format(a,' <strong>',a,'</strong> : ',a,' detected ',a,
     $     ' spectrum estimator')
       CALL writTag(u,'</p>')
cc  START FROM HERE NEXT TIME
       if (u.eq.16) then
        CALL writTagOneLine(u,'h3','@','RESIDUAL STOCHASTIC '//
     $                      'SEASONALITY: SPECTRAL EVIDENCE')
        CALL mkTableTag(u,'w70','spectral evidence table')
        CALL mkCaption(u,'Spectral Evidence Table')
        if (SeasSpectCrit2(SA,mq).or.totalSeasSA.ge.5) then      
         write(u,1120)'SEASONALLY ADJUSTED SERIES',
     $                'Evidence of residual seasonality',1
 1120    format('<tr><th scope="row">',a,
     $    '</th><td><abbr title="',a,'">',i1,'</abbr></td></tr>') 
        else
         write(u,1120)'SEASONALLY ADJUSTED SERIES',
     $   'No evidence of residual seasonality or evidence is too weak',0
        end if
        if (SeasSpectCrit2(TR,mq).or.totalSeasTR.ge.5) then
         write(u,1120)'TREND-CYCLE COMPONENT',
     $                'Evidence of residual seasonality',1
        else
         write(u,1120)'TREND-CYCLE COMPONENT',
     $   'No evidence of residual seasonality or evidence is too weak',0
        end if
        if (SeasSpectCrit2(IR,mq).or.totalSeasIR.ge.5) then
         write(u,1120)'IRREGULAR COMPONENT',
     $                'Evidence of residual seasonality',1
        else
         write(u,1120)'IRREGULAR COMPONENT',
     $   'No evidence of residual seasonality or evidence is too weak',0
        end if
        CALL writTag(u,'</table>')
        CALL mkPOneLine(u,'@','&nbsp;')
        CALL writln('<strong>1</strong> : EVIDENCE OF RESIDUAL '//
     $              'SEASONALITY.'//Cbr,u,0,T,F)
        CALL writln('<strong>0</strong> : NO EVIDENCE OF RESIDUAL '//
     $              'SEASONALITY OR EVIDENCE IS TOO WEAK',u,0,F,T)
c  tabla trading day effect
        CALL writTagOneLine(u,'h3','@','TRADING DAY EFFECT: '//
     $                      'SPECTRAL EVIDENCE')
        CALL mkTableTag(u,'w70','trading day spectral test')
        CALL mkCaption(u,'Trading Day Spectral Test')
        if (TDSpectCrit(SA)) then      
         write(u,1120)'SEASONALLY ADJUSTED SERIES',
     $    'Evidence of residual trading day effect detected',1
        else
         write(u,1120)'SEASONALLY ADJUSTED SERIES',
     $    'No evidence of residual trading day effect or evidence '//
     $    'is too weak',0
        end if
        if (TDSpectCrit(TR)) then
         write(u,1120)'TREND-CYCLE COMPONENT',
     $    'Evidence of residual trading day effect detected',1
        else
         write(u,1120)'TREND-CYCLE COMPONENT',
     $    'No evidence of residual trading day effect or evidence '//
     $    'is too weak',0
        end if
        if (TDSpectCrit(IR)) then
         write(u,1120)'IRREGULAR',
     $    'Evidence of residual trading day effect detected',1
        else
         write(u,1120)'IRREGULAR',
     $    'No evidence of residual trading day effect or evidence '//
     $    'is too weak',0
        end if
        CALL writTag(u,'</table>')
        CALL mkPOneLine(u,'@','&nbsp;')
        CALL writln('<strong>1</strong> : EVIDENCE OF RESIDUAL '//
     $              'TRADING DAY.'//Cbr,u,0,T,F)
        CALL writln('<strong>0</strong> : NO EVIDENCE OF RESIDUAL '//
     $              'TRADING DAY OR EVIDENCE IS TOO WEAK',u,0,F,T)
*        write(u,*) '<p>Note: With RSA=3, trading day effect can be'//
*     $             ' imposed by entering itrad=1,2,6 or 7.</p>'
       end if      
      end
cc
c
cc 
      subroutine wrTablaTestSeas(u,uidx,html,saS,trendS,irS)      
      implicit none
      logical T,F
      parameter(T=.true.,F=.false.)
      integer u,saS,trendS,irS,html,uidx
      include 'htmlout.cmn'
      CALL writTagOneLine(u,'h2','@',
     &                    'OVERALL TEST FOR RESIDUAL SEASONALITY')
      CALL mkTableTag(u,'w70','Overall Residual Seasonality test')
      CALL mkCaption(u,'Overall Residual Seasonality test')
      if (saS.eq.1) then        
        write(u,1010)'SEASONALLY ADJUSTED SERIES',
     $               'Identifiable seasonality detected',1
      else
        write(u,1010)'SEASONALLY ADJUSTED SERIES',
     $               'No identifiable seasonality is detected',0
      end if
      if (trendS.eq.1) then
         write(u,1010)'TREND-CYCLE COMPONENT',
     $                'Identifiable seasonality detected',1
      else
         write(u,1010)'TREND-CYCLE COMPONENT',
     $                'No identifiable seasonality is detected',0
      end if
      if (irS.eq.1) then
         write(u,1010)'IRREGULAR COMPONENT',
     $                'Identifiable seasonality detected',1
      else
         write(u,1010)'IRREGULAR COMPONENT',
     $                'No identifiable seasonality is detected',0
      end if
 1010 FORMAT('<tr><th scope="row">',a,'</th><td class="center">',
     $       '<abbr title="',a,'">',i1,'</abbr></td></tr>') 
      CALL writTag(u,'</table>')
      CALL mkPOneLine(u,'@','&nbsp;')
      CALL writln('<strong>1</strong> : EVIDENCE OF RESIDUAL '//
     $            'TRADING DAY.'//Cbr,u,0,T,F)
      CALL writln('<strong>0</strong> : NO EVIDENCE OF RESIDUAL '//
     $            'TRADING DAY OR EVIDENCE IS TOO WEAK',u,0,F,T)
      end
c
c     OutDenC1: escribe en la salida y en USRENTRY los denominadores de los componentes 
      subroutine OutDenC1(Out,Nio,Titleg,
     $                    p,d,q,bp,bd,bq,theta,nTh,Btheta,nBth,
     $                    phi,nPhi,Bphi,nBphi)
      implicit none
c     INPUT PARAMETERS
      integer Out,Nio,p,d,q,bp,bd,bq,nTh,nBth,nPhi,nBphi
      character Titleg*80
      real*8 theta(4),Btheta(25),phi(4),Bphi(13)
c     LOCAL PARAMETERS
      integer i
      character wformat*65
      include 'htmlout.cmn'
      include 'transcad.i'
c     ---------------
      if (Out .eq. 0) then
       CALL genSkip(1046)
       CALL writTagOneLine(Nio,'h2','@','PART 2 : '//
     $     'DERIVATION OF THE MODELS FOR THE COMPONENTS AND ESTIMATORS')
       CALL mkPOneLine(Nio,'@',
     $                  '<strong> SERIES TITLE:</strong> '//Titleg) 
       write (Nio,1000) p, d, q, bp, bd, bq
       CALL writTagOneLine(Nio,'h3','@','PARAMETER VALUES : '//
     $            'COEFFICIENTS OF POLYNOMIALS IN B OF THE MODEL '//
     $            '(TRUE SIGNS)')
*        itab=itab+1
       if (nth.gt.0.or.nbth.gt.0.or.nphi.gt.0.or.nbphi.gt.0) then
        CALL makDivId(Nio,'model.parameters.true.signs','pol')
        CALL mkTableTag(Nio,'w30',
     $     'COEFFICIENTS OF POLYNOMIALS IN B OF THE MODEL (TRUE SIGNS)')
        CALL mkCaption(Nio,
     $     'COEFFICIENTS OF POLYNOMIALS IN B OF THE MODEL (TRUE SIGNS)')
        indMdp=0
        indMdl=0
        indCoef=1
        WRITE(Nio,1001)indCoef
       end if
       if (nth.gt.0) then
        indMdp=indMdp+1
        WRITE(Nio,1002)indMdp,'Nonseasonal MA parameters'
        do i=1,nth
         indMdl=indMdl+1
         write (nio,1003)indMdl,i-1,indMdp,indMdl,indCoef,theta(i)
        end do
       end if
c
       if (nbth.gt.0) then
*        itab=itab+1
        indMdp=indMdp+1
        WRITE(Nio,1002)indMdp,'Seasonal MA parameters'
        indMdl=indMdl+1
        write(nio,1003)indMdl,0,indMdp,indMdl,indCoef,btheta(1)
        if (nbth.gt.1) then
         indMdl=indMdl+1
         write(nio,1003)indMdl,1,indMdp,indMdl,indCoef,btheta(nbth)
        end if
       end if
c
       if (nphi.gt.0) then
*        itab=itab+1
        indMdp=indMdp+1
        WRITE(Nio,1002)indMdp,'Nonseasonal AR parameters'
        do i=1,nphi
         indMdl=indMdl+1
         write (nio,1003)indMdl,i-1,indMdp,indMdl,indCoef,phi(i)
        end do
       end if
c
       if (nbphi.gt.0) then
c        itab=itab+1
        indMdp=indMdp+1
        WRITE(Nio,1002)indMdp,'Seasonal AR parameters'
        indMdl=indMdl+1
        write(nio,1003)indMdl,0,indMdp,indMdl,indCoef,bphi(1)
        if (nbphi.gt.1) THEN
         indMdl=indMdl+1
         write(nio,1003)indMdl,1,indMdp,indMdl,indCoef,bphi(nbphi)
        END IF
       end if
       if (nth.gt.0.or.nbth.gt.0.or.nphi.gt.0.or.nbphi.gt.0) then
        CALL writTag(Nio,'</table></div>')
        CALL mkPOneLine(Nio,'@','&nbsp;')
       end if
      end if
 1000 format('<p><strong> MODEL PARAMETERS :  </strong>',
     $       2('(',i1,',',i1,',',i1,')'),'</p>')
 1001 format('<tr><th class="head">&nbsp;</th><th id="coef',i4.4,
     $       '">Coefficients</th></tr>') 
 1002 format('<tr><th colspan="2" id="mdp',i4.4,'" class="left">',a,
     $       '</th></tr>')
 1003 format('<tr><th id="mdlg',i4.4,'">Lag ',i2,'</th>',
     $       '<td headers="mdp',i4.4,' mdlg',i4.4,' coef',i4.4,
     $       '" class="right">',f8.4,'</td></tr>')
      end
c
c
c
      subroutine OutDenCN(Out,
c     $                 Nidx,
     $                  Nio,
c     $                  Titleg,
     $                  init,pstar,
c     $                  p,d,q,bp,bd,bq,theta,nTh,Btheta,nBth,
c     $                  phi,nPhi,Bphi,nBphi,
     $                  ThStar,Qstar,
     $                  Chis,nChis,Chins,nChins,Chi,nChi,
     $                  Cycs,nCycs,Cycns,nCycns,Cyc,nCyc,
     $                  Psis,nPsis,Psins,nPsins,Psi,nPsi,
     $                  Adjs,nAdjs,Adjns,nAdjns,Chcyc,nChcyc,
     $                  Totden,nTot)
      implicit none
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
c     INPUT PARAMETERS
      integer Out,Nio,init,pstar,
c     $                  Nidx,
c     $                  p,d,q,bp,bd,bq,nTh,nBth,
c     $                  nPhi,nBphi,
     $                  Qstar,nChis,nChins,nChi,
     $                  nCycs,nCycns,nCyc,
     $                  nPsis,nPsins,nPsi,
     $                  nAdjs,nAdjns,nChcyc,
     $                  nTot
c     character Titleg*80
      real*8 ThStar(27),
c     $       theta(4),Btheta(25),phi(4),Bphi(13),
     $       Chis(17),Chins(8),Chi(8),
     $       Cycs(17),Cycns(5),Cyc(17),Psis(16),Psins(27),Psi(27),
     $       Adjs(17),Adjns(8),Chcyc(20),Totden(40)
c     LOCAL PARAMETERS
      integer i
      character wformat*65
*      include 'indhtml.i'
      include 'transcad.i'
      include 'htmlout.cmn'
c     ---------------
C
C PRINTOUT OF THE DENOMINATORS
C
      if (Out .eq. 0) then
c        itab=itab+1
        CALL writTagOneLine(Nio,'h4','@','NUMERATOR OF THE MODEL'//
     $       ' (TOTAL MOVING AVERAGE POLYNOMIAL)')
c        call WrTabHtmPol(Thstar,qstar,nio,1)
        call WrTabHtmPol(Thstar,qstar,nio,1)
        CALL writTagOneLine(Nio,'h3','@','FACTORIZATION OF THE '//
     $       'TOTAL AUTOREGRESSIVE POLYNOMIAL')
c        itab=itab+1
*        CALL writTagOneLine(Nio,'h4','pol',
*     $                      'STATIONARY AUTOREGRESSIVE TREND-CYCLE')
        call WrTabHtmPol(Chis,nchis,nio,10)
*        CALL makDivId(Nio,'stationary.ar.trend.coefficients','pol')
*        CALL genTableTag(Nio,'coefficients of stationary '//
*     $         'autoregressive trendcycle polynomial',nchis,.false.)
*        CALL writTag(Nio,'<tr>')
*        do i=1,nchis
*         write (nio,1010)'PHIP', i-1
* 1010    FORMAT('<th scope="col">',a,'(',i1,')</th>')
*        end do
*        CALL writTag(Nio,'</tr>')
*        CALL writTag(nio,'<tr>')
*        do i=1,nchis
*         write (nio,1020)Chis(i)
* 1020    FORMAT('<td class="center">',f8.4,'</td>')
*        end do
*        CALL writTag(Nio,'</tr>')
*        CALL writTag(Nio,'</table></div>')
       call USRENTRY(Chis,1,Nchis,1,17,1059)
c        itab=itab+1
*        CALL writTagOneLine(Nio,'h4','pol',
*     $                      'NON-STATIONARY AUTOREGRESSIVE TREND-CYCLE')
        call WrTabHtmPol(Chins,nchins,nio,11)
*        CALL makDivId(Nio,'nonstationary.ar.trend.coefficients','pol')
*        CALL genTableTag(Nio,'coefficients of non-stationary '//
*     $         'autoregressive trendcycle polynomial',nchins,.false.)
*        CALL writTag(Nio,'<tr>')
*        do i=1,nchins
*         write (nio,1010)'DELP',i-1
*        end do
*        CALL writTag(Nio,'</tr>')
*        CALL writTag(Nio,'<tr>')
*        do i=1,nchins
*         write (nio,1020)Chins(i)
*        end do
*        CALL writTag(Nio,'</tr>')
*        CALL writTag(Nio,'</table></div>')
       call USRENTRY(Chins,1,Nchins,1,8,1058)
c        iTab=iTab+1
*        CALL writTagOneLine(Nio,'h4','pol','AUTOREGRESSIVE TREND-CYCLE')
        call WrTabHtmPol(Chi,nchi,nio,12)
*        CALL makDivId(Nio,'ar.trend.coefficients','pol')
*        CALL genTableTag(Nio,'coefficients of autoregressive'//
*     $         ' trendcycle polynomial',nchi,.false.)
*        CALL writTag(Nio,'<tr>')
*        do i=1,nchi
*         write (nio,1010)'PHIPT', i-1
*        end do
*        CALL writTag(Nio,'</tr>')
*        CALL writTag(Nio,'<tr>')
*        do i=1,nchi
*         write (nio,1020)Chi(i)
*        end do
*        CALL writTag(Nio,'</tr>')
*        CALL writTag(Nio,'</table></div>')
       call USRENTRY(Chi,1,Nchi,1,8,1059)
       if ((init.ne.2) .and. (ABS(Chis(Nchis+1)-99.99).lt.1.d-12)) then
         call wWritln('Stationary Autoregressive Trend-Cycle '//
     &                'MAY HAVE UNIT ROOT.',Nio,0,T,T)
         CALL mkPOneLine(Nio,'@','&nbsp;')
       end if
       if ((init.ne.2) .and. (ABS(Chins(Nchins+1)-99.99).lt.1.d-12))
     $    then
         call wWritln('Non-Stationary Autoregressive Trend-Cycle '//
     &                'MAY HAVE UNIT ROOT.',Nio,0,T,T)
         CALL mkPOneLine(Nio,'@','&nbsp;')
       end if
c        iTab=iTab+1
*        CALL writTagOneLine(Nio,'h4','pol','STATIONARY '//
*     $         'AUTOREGRESSIVE '//transLCad(1:nTransLCad)//' COMPONENT')
        call WrTabHtmPol(Cycs,ncycs,nio,13)
*        CALL makDivId(Nio,'stationary.ar.'//TransCad(1:nTransCad)//
*     $         '.coefficients','pol')
*        CALL genTableTag(Nio,'coefficients of stationary '//
*     $         'autoregressive '//transLCad(1:nTransLCad)//
*     $         ' component polynomial',ncycs,.false.)
*        CALL writTag(Nio,'<tr>')
*        do i=1,ncycs
*         write (nio,1010)'PHIC', i-1
*        end do
*        CALL writTag(Nio,'</tr>')
*        CALL writTag(Nio,'<tr>')
*        do i=1,ncycs
*         write (nio,1020)Cycs(i)
*        end do
*        CALL writTag(Nio,'</tr>')
*        CALL writTag(Nio,'</table></div>')
       call USRENTRY(Cycs,1,Ncycs,1,17,1060)
c        iTab=iTab+1
*        CALL writTagOneLine(Nio,'h4','pol','NON-STATIONARY '//
*     $         'AUTOREGRESSIVE '//transLCad(1:nTransLCad)//' COMPONENT')
        call WrTabHtmPol(Cycns,ncycns,nio,14)
*        CALL makDivId(Nio,'nonstationary.ar.'//TransCad(1:nTransCad)//
*     $         '.coefficients','pol')
*        CALL genTableTag(Nio,'coefficients of nonstationary '//
*     $         'autoregressive '//transLCad(1:nTransLCad)//
*     $         ' component polynomial',ncycns,.false.)
*        CALL writTag(Nio,'<tr>')
*        do i=1,ncycns
*         write (nio,1010)'DELC', i-1
*        end do
*        CALL writTag(Nio,'</tr>')
*        CALL writTag(Nio,'<tr>')
*        do i=1,ncycns
*         write (nio,1020)Cycns(i)
*        end do
*        CALL writTag(Nio,'</tr>')
*        CALL writTag(Nio,'</table></div>')
       call USRENTRY(Cycns,1,Ncycns,1,5,1061)
c        iTab=iTab+1

*       CALL writTagOneLine(Nio,'h4','pol',
*     $         'AUTOREGRESSIVE '//transLCad(1:nTransLCad)//' COMPONENT')
       call WrTabHtmPol(Cyc,Ncyc,nio,15)
*       CALL makDivId(Nio,'ar.'//TransCad(1:nTransCad)//'.coefficients',
*     $         'pol')
*       CALL genTableTag(Nio,'coefficients of '//
*     $         'autoregressive '//transLCad(1:nTransLCad)//
*     $         ' component polynomial',Ncyc,.false.)
*       CALL writTag(Nio,'<tr>')
*       do i=1,Ncyc
*         write (nio,1010)'PHICT', i-1
*       end do
*       CALL writTag(Nio,'</tr>')
*       CALL writTag(Nio,'<tr>')
*       do i=1,Ncyc
*        write (nio,1020)Cyc(i)
*       end do
*       CALL writTag(Nio,'</tr>')
*       CALL writTag(Nio,'</table></div>')
       if ((init.ne.2) .and. (ABS(Cycs(Ncycs+1)-99.99).lt.1.d-12)) then
         call wWritln('Stationary Autoregressive '//
     $                TransLCad(1:nTransLcad)//
     &                ' MAY HAVE UNIT ROOT.',Nio,0,T,T)
         CALL mkPOneLine(Nio,'@','&nbsp;')
       end if
       if ((init.ne.2) .and. (ABS(Cycns(Ncycns+1)-99.99).lt.1.d-12))
     $    then
         call wWritln('Non-Stationary Autoregressive '//
     $                TransLCad(1:nTransLcad)//
     &                ' MAY HAVE UNIT ROOT.',Nio,0,T,T)
         CALL mkPOneLine(Nio,'@','&nbsp;')
       end if

*        iTab=iTab+1
*       CALL writTagOneLine(Nio,'h4','@',
*     $                   'STATIONARY AUTOREGRESSIVE SEASONAL COMPONENT')
       call WrTabHtmPol(Psis,Npsis,nio,2)
       call USRENTRY(Psis,1,Npsis,1,16,1151)

*        iTab=iTab+1
*       CALL writTagOneLine(Nio,'h4','@','NON-STATIONARY '//
*     $                     'AUTOREGRESSIVE SEASONAL COMPONENT')
       call WrTabHtmPol(Psins,Npsins,nio,3)
       call USRENTRY(Psins,1,Npsins,1,27,1150)

*       iTab=iTab+1
*       CALL writTagOneLine(Nio,'h4','@',
*     $                     'AUTOREGRESSIVE SEASONAL COMPONENT')
       call WrTabHtmPol(Psi,Npsi,nio,4)
       if ((init.ne.2) .and. (ABS(Psis(Npsis+1)-99.99).lt.1.d-12)) then
        call wWritln('Stationary Autoregressive Seasonal '//
     $               'MAY HAVE UNIT ROOT',Nio,0,T,T)
         CALL mkPOneLine(Nio,'@','&nbsp;')
       end if
       if ((init.ne.2) .and. (ABS(Psins(Npsins+1)-99.99).lt.1.d-12))
     $    then
        call wWritln('Non-Stationary Autoregressive Seasonal '//
     $               'MAY HAVE UNIT ROOT',Nio,0,T,T)
        CALL mkPOneLine(Nio,'@','&nbsp;')
       end if

*       iTab=iTab+1
*       CALL writTagOneLine(Nio,'h4','@','STATIONARY '//
*     $                   'AUTOREGRESSIVE SEASONALLY ADJUSTED COMPONENT')
       call WrTabHtmPol(Adjs,Nadjs,nio,5)
       call USRENTRY(Adjs,1,Nadjs,1,17,1152)

*       iTab=iTab+1
*       CALL writTagOneLine(Nio,'h4','@','Non-Stationary '//
*     $                   'AUTOREGRESSIVE SEASONALLY ADJUSTED COMPONENT')
       call WrTabHtmPol(Adjns,Nadjns,nio,6)
       call USRENTRY(Adjns,1,Nadjns,1,8,1153)

*        iTab=iTab+1
*       CALL writTagOneLine(Nio,'h4','@',
*     $                   'AUTOREGRESSIVE SEASONALLY ADJUSTED COMPONENT')
       call WrTabHtmPol(Chcyc,Nchcyc,nio,7)

*       iTab=iTab+1
*       CALL writTagOneLine(Nio,'h4','@','TOTAL DENOMINATOR '//
*     $                     '(TOTAL AUTOREGRESSIVE POLYNOMIAL)')
       call WrTabHtmPol(totden,Ntot,nio,8)
      end if
c------------------
      if (pstar .ne. Ntot) then
        call wWritln(Cbr//'DIMENSION PROBLEM',Nio,0,T,T)
      end if
      end
c
c
      subroutine OutAuto(OUT,Nio,Qstat,df,r,se,M,caption)
c     QSTAT<0 if we do not write Qstat Test
      implicit none
c     INPUT PARAMETERS
      integer OUT,Nio,df,M
      real*8 Qstat,r(50),se(50)
      character caption*(26)
c  i/o
*      include 'indhtml.i'
c
c     LOCAL PARAMETERS
      integer mr,mp,i,ie,k,iacf,ncap
      character*100 auxstr
      character*2 thisP
c     
c     external function
      integer ISTRLEN
      external ISTRLEN      
c     ---------------------------------
      if (out .eq. 0) then
       mr = 1
       mp = m / 12
       if (MOD(m,12) .eq. 0) then
        mr = 0
       end if
       mp = mp*12 + mr
       iacf=0
       ncap=istrlen(caption)
       do i=1,mp,12
         ie=i+11
         iacf=iacf+1
         write(thisP,1011)iacf
 1011    format(i2)
         CALL mkTableTag(Nio,'w80','AUTOCORRELATIONS OF '//
     &                   caption(1:ncap)//', Part'//thisP)
         CALL mkCaption(Nio,'AUTOCORRELATIONS OF '//
     &                  caption(1:ncap)//', Part'//thisP)
*          iTab=iTab+1
*          iId=iId+1
         CALL writTag(Nio,'<thead>')
         CALL writTag(Nio,'<tr>')
         CALL mkTableCell(Nio,'head','&nbsp;')
         DO k=i,ie
          WRITE(Nio,1020)k
 1020     FORMAT('<th scope="col"> Lag ',I6,'</th>')
         END DO
         CALL writTag(Nio,'</tr>')
         CALL writTag(Nio,'</thead>')
         CALL writTag(Nio,'<tbody>')
         CALL writTag(Nio,'<tr>')
         CALL mkHeaderCellScope(Nio,0,0,'row',
     &                          'Autocorrelation Function','ACF')
 6000    format (12('<td class="center">',f9.4,'</td>'))
         write (Nio,6000) (r(k), k = i,ie)
         CALL writTag(Nio,'</tr>')
         CALL writTag(Nio,'<tr>')
         CALL mkHeaderCellScope(Nio,0,0,'row','Standard Error','SE')
         write (Nio,6000) (se(k), k = i,ie)
         CALL writTag(Nio,'</tr>')
         CALL writTag(Nio,'</tbody></table>')
         CALL mkPOneLine(Nio,'@','&nbsp;')
        end do
c     -------------------------------
       if (QStat.gt.0.0) then
 6002    format ('<p class="em">THE LJUNG-BOX Q VALUE IS ',f10.2,
     $           ' AND IF RESIDUALS ARE RANDOM IT SHOULD BE',
     $           ' DISTRIBUTED AS CHI-SQUARE (',i2,')</p>')
         write (Nio,6002) qstat, df
       end if
      end if
      end
c
c     OutSeas: escribe en el fichero de salida:
c                     los residuos extendidos,
c                     Los residuos studentized (que distan mucho de 0)
c                     los test sobre residuos extendidos
c                     Las autocorrelaciones de los residuos extendidos
c                     Si algun test salio mal
c                     El test de RUNS
c                     Las autocorrelaciones de los residuos al cuadrado
c                     Escribe si hay evidencia de no linearidad
c                     Escribe los backward residuals
      Subroutine OutSeats(IOUT,Nio,Ndevice,
     $                 printBack,ba,sr,SQSTAT,SDF,SSE,m,MQ,
     $                 n_1,n0,tvalRUNS,
     $                Qstat,DF,Pstat1,spstat1,
     $                wnormtes,wsk,skewne,test1,wkk,rkurt,test,r,SEa,
     $                resid,flagTstu,it,iper,iyear,
     $                rmean,rstd,DW,KEN,RTVAL,SumSres,F0,Nyer1,Nper1,
     $                Pstar,Qstar,D,BD)
      implicit none
      INCLUDE 'srslen.prm'
c
      logical T,F
      parameter(T = .true.,F=.false.)
c
      include 'dimensions.i'
      include 'peaks.i'
      include 'sig.i'
      include 'sform.i'
*      include 'indhtml.i'
      include 'htmlout.cmn'
c     INPUT PARAMETERS
      logical printBack
      integer IOUT,Nio,m,mq,DF,SDF,thisDate(2)
      real*8 resid(mpkp),ba(mpkp),Qstat,Pstat1,spstat1
      real*8 sr(50),SQstat,SSE(50),tvalRUNS
      integer n_1,n0
      real*8 wnormtes,wsk,skewne,test1,wkk,rkurt,test,r(50),SEa(50)
      integer flagTstu,NDEVICE,IPER,IYEAR,it,Nper1,nYer1
      integer Pstar,Qstar,D,BD
      real*8 Rmean,Rstd,DW,KEN,RTVAL,F0,SumSres
c     EXTERNAL
      integer ISTRLEN
      real*8 KENDALLS
      external ISTRLEN,KENDALLS
      intrinsic MOD
c     LOCAL PARAMETERS
      real*8 sigq
      character buff*180,fname*30,subtitle*50,cstr*10
      integer i,ITT,ipos
      integer saveNZ,saveNper,saveNyer
C
C.. Local Arrays ..
      real*8 chi299(50)/6.6349,9.2103,11.3449,13.2767,15.0863,16.8119,
     &              18.4753,20.0902,21.666,23.2093,24.725,26.217,
     &              27.6882,29.1412,30.5779,31.9999,33.4087,34.8053,
     &              36.1909,37.5662,38.9322,40.2894,41.6384,42.9798,
     &              44.3141,45.6417,46.9629,48.2782,49.5879,50.8922,
     &              52.1914,53.4858,54.7755,56.0609,57.3421,58.6192,
     &              59.8925,61.1621,62.4281,63.6907,64.9501,66.2062,
     &              67.4593,68.7095,69.9568,71.2014,72.4433,73.6826,
     &              74.9195,76.1539/
      real*8 chi295(50)/3.8415,5.9915,7.8147,9.4877,11.0705,
     &                  12.5916,14.0671,15.5073,16.919,18.307,19.6751,
     &                  21.0261,22.362,23.6848,24.9958,26.2962,
     &                  27.5871,28.8693,30.1435,31.4104,32.6706,
     &                  33.9244,35.1725,36.415,37.6525,38.8851,
     &                  40.1133,41.3371,42.557,43.773,44.9853,46.1943,
     &                  47.3999,48.6024,49.8018,50.9985,52.1923,
     &                  53.3835,54.5722,55.7585,56.9424,58.124,
     &                  59.3035,60.4809,61.6562,62.8296,64.0011,
     &                  65.1708,66.3386,67.5048/
      real*8 a(kp+mp),dvec(1)
c --------------------
      saveNZ = Nz   
      saveNper=Nper
      saveNyer=Nyer             
      if (Out .eq. 0) then
        Nz = Na
        Nyer = nyer1
        Nper = nper1
        thisDate(1)=Nyer
        thisDate(2)=Nper
        CALL genSkip(1191)
        CALL writTagOneLine(nio,'h3','@','SEATS RESIDUALS')
        call prttbl(thisDate,Mq,resid,nz,'EXTENDED RESIDUALS',ndec,
     &              'extrsd.seats')
        Nz=saveNz
        Nper=saveNper
        Nyer=saveNyer
      end if
c --------------------
      Do i=1,NA
        a(i) = Resid(i) / Sqf
        it = i + Pstar - Qstar + D + Bd*Mq
        itt = it + Nper - 1
        iper = MOD(itt,Nfreq)
        iyear = itt / Nfreq
        iyear = Nyer + iyear
        if (iper .eq. 0) then
          iper = Nfreq
          iyear = iyear - 1
        end if
        if ((Out.eq.0) .and. (a(i).lt.-Sek.or.a(i).gt.Sek)) then
c            if (flagTstu.eq.0) then
c               call AddIdx(Nidx,Nio,'Studentized Residuals','0012',0,12)
c              flagTstu=1
c           end if
c 6043       format ('<p><em>STUDENTIZED EXTENDED RESIDUAL OF ',f8.4,
c     $             '  AT T=',i3,4x,'(',i2,1x,i4,')</em></p>')
c            write (Nio,6043) a(i), it, iper, iyear
          if (flagTstu.eq.0) then
            CALL genSkip(1192)
            CALL writTagOneLine(Nio,'h3','@',
     &                          'STUDENTIZED EXTENDED RESIDUALS')
            CALL mkTableTag(Nio,'w70','STUDENTIZED EXTENDED RESIDUALS')
            CALL writTag(Nio,'<tr>')
            CALL mkHeaderCellScope(Nio,0,0,'col','Observation Number',
     &                             '#OBSERVATION')
            CALL mkHeaderCellScope(Nio,0,0,'col','@','DATE')
            CALL mkHeaderCellScope(Nio,0,0,'col','@','VALUE')
            CALL writTag(Nio,'</tr>')
            flagTstu=1
          end if
          CALL writTag(Nio,'<tr>')
          ipos=1
          CALL itoc(it,cstr,ipos)
          CALL mkHeaderCellScope(Nio,0,0,'row','Observation Number '//
     &                           cstr(1:(ipos-1)),'#'//cstr(1:(ipos-1)))
          write(nio,7043)iper,iyear,a(i)
 7043     format('<td class="center">',i2,'-',i4,'</td>',/,
     &           '<td class="center">',f8.4,'</td>')
          CALL writTag(Nio,'</tr>')
        end if        
      end do
      if (flagTstu.eq.1) then
        CALL writTag(Nio,'</table>')
        CALL mkPOneLine(Nio,'@','&nbsp;')
      end if
      if (Nio .eq. ndevice) then
        dvec(1)=rmean
        call USRENTRY(dvec,1,1,1,1,1040)
        dvec(1)=rstd               
        call USRENTRY(dvec,1,1,1,1,1041)
        dvec(1)=skewne             
        call USRENTRY(dvec,1,1,1,1,1045)
        dvec(1)=test1              
        call USRENTRY(dvec,1,1,1,1,1046)
        dvec(1)=rkurt              
        call USRENTRY(dvec,1,1,1,1,1042)
        dvec(1)=test               
        call USRENTRY(dvec,1,1,1,1,1043)
        dvec(1)=wnormtes           
        call USRENTRY(dvec,1,1,1,1,1044)
        dvec(1)=Sqf                
        call USRENTRY(dvec,1,1,1,1,1047)
        dvec(1)=dw                 
        call USRENTRY(dvec,1,1,1,1,1048)
        if (MQ.gt.1) then
          dvec(1)=ken                 
          call USRENTRY(dvec,1,1,1,1,1049)
        end if
      end if
      if (Out .eq. 0) then                 
        CALL genSkip(1058)
        CALL mkTableTag(Nio,'w70',
     $                 'test-statistics on extended residuals')
        CALL mkCaption(Nio,'Test-statistics on extended residuals')
        write (Nio,1010)'MEAN ',rmean
        write (Nio,1010)'STANDARD DEVIATION OF MEAN = ',rstd
        write (Nio,1020)'T-VALUE ',rtval
        write (Nio,1030)'NORMALITY TEST ',wnormtes
        write (Nio,1040)'SKEWNESS = ',skewne, test1
        write (Nio,1040)'KURTOSIS = ',rkurt, test
        write (Nio,1010)'SUM OF SQUARES ',sumSres
        write (Nio,1010)'DURBIN-WATSON ',dw
        write (Nio,1010)'STANDARD DEVIATION OF RESIDUALS ',Sqf
        write (Nio,1010)'VARIANCE OF RESIDUALS = ',f0
        CALL writTag(Nio,'</table>')
        CALL mkPOneLine(Nio,'@','&nbsp;')
 1010   format('<tr><th scope="row">',a,'</th><td>',d12.4,'</td></tr>')
 1020   format('<tr><th scope="row">',a,'</th><td>',f8.4,'</td></tr>')
 1030   format('<tr><th scope="row">',a,'</th><td>',g14.4,'</td></tr>')
 1040   format('<tr><th scope="row">',a,'</th><td>',f8.4,
     $     ' ( <em>SE</em> = ',f8.4,')</td></tr>')
c   añadir una nota con: (*) Chi-square(2)
        if (MQ.gt.1) then
          CALL genSkip(1059)
          CALL writln('<strong>NON-PARAMETRIC TEST FOR RESIDUAL '//
     &                'SEASONALITY (FRIEDMAN)',Nio,0,T,F)
          write (Nio,1050)ken,MQ-1
 1050     format(' SEAS_NP = ',f9.2,' ASYMPTOTICALLY ',
     &           'DISTRIBUTED AS CHI-SQUARE(',i2,')</strong>') 
          write (Nio,1060)Cbr,'99',chi299(MQ-1)
          write (Nio,1060)Cbr,'95',chi295(MQ-1)
 1060     format(a,'Critical value ',a,'%: ',f9.2)
          CALL writTag(Nio,'</p>')
        end if
c --------------------
        CALL genSkip(1047)
        Call OutAuto(IOUT,Nio,Qstat,df,r,sea,M,
     $               'EXTENDED RESIDUALS        ')
c     ------------------------------------------------------------------
        sigq = SQRT(2.0d0*DF)
        if (Qstat .gt. DF+6*sigq) then
          CALL mkPOneLine(Nio,'@','<strong>EVIDENCE OF EXTENDED '//
     $       'RESIDUALS CORRELATION : </strong> LARGE')
        end if
        if ((Qstat.gt.DF+3*sigq) .and.(Qstat.le.DF+6*sigq)) then
          CALL mkPOneLine(Nio,'@','<strong>EVIDENCE OF EXTENDED '//
     $       'RESIDUALS CORRELATION : </strong> MODERATE')
        end if
        if (wnormtes .gt. 9.0d0) then
          CALL mkPOneLine(Nio,'ub','EVIDENCE OF NON-NORMALITY')
        end if
        wsk = skewne / test1
        if (wsk .gt. 3.0d0) then
          CALL mkPOneLine(Nio,'bold','EVIDENCE OF POSITIVE ASYMETRY')
        end if
        if (wsk .lt. -3.0d0) then
          CALL mkPOneLine(Nio,'bold','EVIDENCE OF NEGATIVE ASYMETRY')
        end if
        wkk = (rkurt-3) / test
        if (wkk .gt. 3.0d0) then
          CALL mkPOneLine(Nio,'bold','EVIDENCE OF EXCESS KURTOSIS')
        end if
*        if ((Pg.eq.0) .and. (iter.eq.0)) then
*          fname = 'AUTORES.T2'
*          subtitle = 'ACF OF EXTENDED RESIDUALS'
*          call PLOTACF(fname,subtitle,r,M,1,Na)
*        end if
 6000   format ('<tr><th scope="row">NUMBER OF DATA',
     $          '</th><td>',i4,'</td></tr>',
     $          '<tr><th scope="row">NUMBER OF (+)</th>',
     $          '<td>',i4,'</td></tr>',
     $          '<tr><th scope="row">NUMBER OF (-) </th>',
     $          '<td>',i4,'</td></tr>')
        CALL mkTableTag(Nio,'w70','test of runs on extended residuals')
        CALL mkCaption(Nio,'APPROXIMATE TEST OF RUNS ON EXTENDED '//
     $                     'RESIDUALS')
*          iId=iId+1
*          iTab=Itab+1
        write (Nio,6000) na, n_1, n0
 6001   format ('<tr><th scope="row">T-VALUE = </th><td>',g16.3,
     $          '</td></tr>')
        write (Nio,6001) tvalRUNS
        CALL writTag(Nio,'</table>')
        CALL mkPOneLine(Nio,'@','&nbsp;')
c     ----------------------------------------------------
        if ((mq.eq.4).or.(mq.eq.12)) then
          call warnPeaks(nio,picosRes,'Residuals           ',mq)
        end if
      end if
c     ----------------------------------------------------
      CALL genSkip(1048)
      Call OutAuto(OUT,Nio,sQstat,sDF,sr,sSE,M,
     $             'SQUARED EXTENDED RESIDUALS')
      if (Out .eq. 0) then
        sigq = SQRT(2.0d0*sDF)
        buff = ' '
        if (sQstat .gt. Qstat+2.0d0) then
          if (sQstat .gt. sDF+6*sigq) then
            buff = 'LARGE'
          end if
          if ((sQstat.lt.sDF+6*sigq) .and.
     $        (sQstat.gt.sDF+3*sigq)) then
            buff = 'MODERATE'
          end if
        else if (sQstat .gt. sDF+6*sigq) then
          buff = 'YES'
        end if
        if (ISTRLEN(buff) .gt. 1) then
          CALL mkPOneLine(Nio,'@',
     $           '<strong>EVIDENCE OF NON-LINEARITY : </strong> '//buff)
        end if
        buff = ' '
        if (Pstat1 .gt. spstat1) then
          if (Pstat1 .ge. 9.5d0) then
            buff = 'LARGE'
          end if
          if ((Pstat1.ge.7.5d0) .and. (Pstat1.lt.9.5d0)) then
            buff = 'MODERATE'
          end if
        else if (Pstat1 .gt. 9.5d0) then
          buff = 'YES'
        end if
        if (ISTRLEN(buff) .gt. 1) then
          CALL mkPOneLine(Nio,'@',
     $                    '<strong>EVIDENCE OF SEASONAL NON-'//
     $                    'LINEARITY : </strong> '//buff)
        end if
C
C Comment the next 5 lines for TSW
C
CUNX#ifdef DOS
!DEC$ IF DEFINED (DOS)
*        if ((Pg.eq.0) .and. (Out.eq.0).and.(iter.eq.0)) then
*                 fname = 'AUTOSRES.T2'
*                 subtitle = 'ACF OF SQD EXTENDED RESIDUALS'
*                 call PLOTACF(fname,subtitle,sr,M,0,0)
*        end if
CUNX#end if
!DEC$ end if
c     ----------------------------------------------------
        if (printBack) then
          NZ=Na
c        Nper=Nper1
c       Nyer=Nyer1
*          if ((smtr .eq. 0)) then
          if (Out.eq.0) then
            thisDate(1)=Nyer
            thisDate(2)=Nper
            CALL genSkip(1164)
            call prttbl(thisDate,Mq,ba,nz,'BACKWARD RESIDUALS',ndec,
     &                  'backrsd.seats')
          end if
          NZ=saveNZ
        end if
c       Nper=saveNper
c       Nyer=SaveNyer
      end if
      end
c
c
c
      subroutine OutPara(nio,niter,mattitle,NAiter,mean,
     $                   p,d,q,bp,bd,bq,phi,bphi,nbphi,
     $                   theta,btheta,nbth,qstat,wm,inicio)
      implicit none
c     INPUT PARAMETERS
      integer nio,niter,NAiter,p,d,q,bp,bd,bq,
     $        nbphi,nbth,mean,inicio
      character mattitle*180
      real*8 qstat,wm
      real*8 phi(*),bphi(*),theta(*),btheta(*)
      integer nOutPar
      common /outPar/ nOutPar
c     LOCAL PARAMETERS
      character PHIo(3)*7,bphio*7,tho(3)*7,btho*7
      integer i
c  --------------------------------
      if (nOutPar.eq.niter) then
        return
      else
        nOutPar=niter
      end if
c  --------------------------------
      do i=1,p
        write(phio(i),1010) phi(i+inicio)
 1010   format(f7.4)
      enddo
      do i=p+1,3
        write(phio(i),1020)
 1020   format(3x,"0",3x)
      enddo
      if (bp.gt.0) then
        write(bphio,1010) bphi(nbphi)
      else
        write(bphio,1020)
      end if
      do i=1,q
        write(tho(i),1010) theta(i+inicio)
      enddo
      do i=q+1,3
        write(tho(i),1020)
      enddo
      if (bq.gt.0) then
        write(btho,1010) Btheta(nbth)
      else
        write(btho,1020)
      end if
      write(nio,1030)
     $        niter,mattitle(1:22),NAiter,qstat,
     $        phio(1)(1:7),phio(2)(1:7),phio(3)(1:7),bphio(1:7),
     $        mean,p,d,q,bp,bd,bq,
     $        tho(1)(1:7),tho(2)(1:7),tho(3)(1:7),btho(1:7),wm
 1030 format('<tr><td scope="row">',i4,'</td><td>',a,
     $       '</td><td>',i2,'</td><td>',f12.5,'</td>',
     $       4('<td>',a,'</td>'),3('<td>',i1,'</td>'),'<td>',i2,'</td>',
     $       3('<td>',i1,'</td>'),4('<td>',a,'</td>'),'<td>',g12.4,
     $       '</td></tr>')
      end  
c
c     OutNoPar
c
      subroutine OutNoPar(nio,niter,mattitle)
      implicit none
c     INPUT PARAMETERS
      integer nio,niter
      character mattitle*180
      integer nOutPar
      common /outPar/ nOutPar
c  --------------------------------
      if (nOutPar.eq.niter) then
        return
      else
        nOutPar=niter
      end if
      write(nio,1000)niter,mattitle(1:22),-1,-1,0,0,0,0,
     $        -1,-1,-1,-1,-1,-1,-1,0,0,0,0,-1
 1000 FORMAT('<tr><td scope="row">',i4,'</td><td>',a,
     $       '</td>', 18('<td>',i2,'</td>'),'</tr>')
      end
cc
c
cc
cc
c
cc
      character*60 function PeriodH(idx,freq)
C 
C.. Implicits .. 
       implicit none
C 
C.. Formal Arguments .. 
       integer idx,freq
C 
C.. Local Variables .. 
      character*4 null
C 
C.. Local Arrays .. 
       character*60 Month(12),Per(12)
       data Month/
     & '<abbr title="January">Jan</abbr>',
     & '<abbr title="February">Feb</abbr>',
     & '<abbr title="March">Mar</abbr>',
     & '<abbr title="April">Apr</abbr>',
     & 'May',
     & '<abbr title="June">Jun</abbr>',
     & '<abbr title="July">Jul</abbr>',
     & '<abbr title="August">Aug</abbr>',
     & '<abbr title="September">Sep</abbr>',
     & '<abbr title="October">Oct</abbr>',
     & '<abbr title="November">Nov</abbr>',
     & '<abbr title="December">Dec</abbr>'/
       data Per/
     & '<abbr title="First Period">1st</abbr>',
     & '<abbr title="Second Period">2nd</abbr>',
     & '<abbr title="Third Period">3th</abbr>',
     & '<abbr title="Fourth Period">4th</abbr>',
     & '<abbr title="Fifth Period">5th</abbr>',
     & '<abbr title="Sixth Period">6th</abbr>',
     & '<abbr title="Seventh Period">7th</abbr>',
     & '<abbr title="Eighth Period">8th</abbr>',
     & '<abbr title="Nineth Period">9th</abbr>',
     & '<abbr title="Tenth Period">10th</abbr>',
     & '<abbr title="Eleventh Period">11th</abbr>',
     & '<abbr title="Twelveth Period">12th</abbr>'/
       null=' '
       if ((idx.le.0).or.(idx.gt.12)) then
        PeriodH = null
       end if
       if ((idx .gt. freq).or.(freq.gt.12)) then
        PeriodH=null
       end if
       if (freq.eq.12) then
        PeriodH = Month(idx)
       else
        PeriodH = Per(idx)
       end if
       return
      end
c
c
c
      subroutine OutRoots(nio,nroots,rez,imz,modul,ar,pr,cad)
      implicit none
c
c..   FORMAL PARAMETERS
      integer nio,nRoots
      real*8 rez(*),imz(*),modul(*),ar(*),pr(*)
      character cad*(41)
c     
      CALL mkPOneLine(Nio,'ub','ROOTS OF '//cad//' POLYNOMIAL')
      call OutRPQ(Nio,nroots,rez,imz,modul,ar,pr)
      end
c
c
c
      subroutine OutARIMA(Nio,init,p,bp,q,bq,wm,PHI,TH,BPHI,BTH,
     $                    sePHI,seBPHI,seTH,seBTH)
      implicit none
c
c.. INPUT PARAMETERS
      integer Nio,init,p,bp,q,bq
      real*8 wm
      real*8 PHI(3),TH(3),BPHI(3),BTH(3)
      real*8 sePHI(3),seBPHI(3),seTH(3),seBTH(3)
c
c.. Local parameters
      integer i,narma
c
        if (Init .eq. 2) then
          CALL mkTableTag(Nio,'w40','Mean')
          write (Nio,1010) wm
          CALL writTag(Nio,'</table>')
          CALL mkPOneLine(Nio,'@','&nbsp;')
        end if
        narma=p+bp+q+bq
        if(narma.le.2)THEN
         CALL mkTableTag(Nio,'w40','ARMA PARAMETERS')
        else if(narma.ge.6)THEN
         CALL mkTableTag(Nio,'w80','ARMA PARAMETERS')
        else
         CALL mkTableTag(Nio,'w60','ARMA PARAMETERS')
        END IF
        CALL mkCaption(Nio,'<abbr title="autoregressive moving '//
     &                 'average">ARMA</abbr> PARAMETERS')
        CALL writTag(Nio,'<tr>')
        if (p.gt.0) THEN
         do i=1,p
           write(nio,1020)'PHI',i
         end do
        END IF
        if (bp.eq.1) then 
           write(nio,1020)'BPHI',1
        end if
        if (q.gt.0) THEN
         do i=1,q
           write(nio,1020)'THETA',i
         end do
        end if
        if (bq.eq.1) then 
          write(nio,1020)'BTHETA',1
        end if
        CALL writTag(Nio,'</tr>')
        CALL writTag(Nio,'<tr>')
c       
        if ((p.gt.0) .or. (bp.gt.0)) then                              
          if (P .ne. 0) then
            if (Init .eq. 2) then
                do i=1,p
                  write (Nio,1030) -Phi(i)
                end do           
            else   !.....Init<>2
                do i=1,p
                  write (Nio,1040)-Phi(i), sePHI(i)
                end do
           end if   ! of init =2
         end if     ! of p<>0
c
         if (Bp .ne. 0) then
           if (Init .eq. 2) then
               write (nio,1030) -Bphi(1)
           else
               write (nio,1040) -Bphi(1), seBPHI(1)
           end if
         end if           
       end if 
c                
       if ((q.gt.0) .or. (bq.gt.0)) then            
         if (Q .ne. 0) then
           if (Init .eq. 2) then
               do i=1,q
                 write (Nio,1030) -Th(i)
               end do           
           else   !.....Init<>2
               do i=1,q
                 write (Nio,1040) -Th(i), seTH(i)
               end do
           end if   ! of init =2
         end if     ! of p<>0
c
         if (Bq .ne. 0) then
           if (Init .eq. 2) then
               write (nio,1030) -Bth(1)
           else
               write (nio,1040) -Bth(1), seBTH(1)
           end if
         end if 
       end if 
       CALL writTag(Nio,'</tr>')
       CALL writTag(Nio,'</table>')
       CALL mkPOneLine(Nio,'@','&nbsp;')
c-----------------------------------------------------------------------
 1010 FORMAT('<tr><th scope="row">MEAN =</th><td class="center">',g16.6,
     $        '</td></tr>',
     $       '<tr><th scope="row"><abbr title="standard error">',
     $       'SE</abbr> =</th><td class="center">*******</td></tr>')
 1020 FORMAT('<th scope="col">',a,'(',i1,')</th>')
 1030 FORMAT('<td class="center">',f10.4,'</td>')
 1040 FORMAT('<td class="center">',f10.4,
     $       ' (<abbr title="standard error">SE</abbr>=',f7.4 ,')</td>')
c-----------------------------------------------------------------------
      end
c
c
c
      subroutine showFirstNA(nio,InputModel,p,d,q,bp,bd,bq,theta,
     $                Btheta,nbth,phi,Bphi,nbphi,imeanout,tramo)
      implicit none
c    INPUT PARAMETERS
      integer nio,InputModel, p,d,q,bp,bd,bq,nbth,nbphi,
     $        imeanout,tramo
      real*8 theta(*),Btheta(*),phi(*),Bphi(*)
c
      integer i
c    EXTERNAL
      character gettmcs
      EXTERNAL gettmcs
c
      include 'htmlout.cmn'
c ---------------------------------------------------------------
      if (InputModel.eq.1) then
        if ((getTmcs().eq.'Y').or.(getTmcs().eq.'y' )) then
         write(nio,1010)Cbr,Cbr,'FIRST MODEL THAT ENTERS THE '//
     $       'DECOMPOSITION: '
        else
         if (tramo.ne.0) then
          write(nio,1010)Cbr,Cbr,'ARIMA MODEL SELECTED BY REGARIMA: ' 
         else
          write(nio,1010)Cbr,Cbr,'ARIMA MODEL SELECTED: ' 
         end if
        end if 
        write(nio,1020)p,d,q,bp,bd,bq
        if (imeanout.eq.0) then
         write(nio,*) 'with mean.</em></p>'
        else
         write(nio,*) 'without mean.</em></p>'
        endif
        CALL mkTableTag(nio,'w80','ARMA Parameters')
        CALL mkCaption(nio,'<abbr title="autoregressive moving '//
     &         'average">ARMA</abbr> Parameters')
        CALL writTag(Nio,'<tr>')
        do i=1,p
         write(nio,1030) 'PHI',i
        end do
        if (bp.eq.1) then 
         write(nio,1040)'BPHI'
        end if
        do i=1,q
         write(nio,1030) 'THETA',i
        end do
        if (bq.eq.1) then 
         write(nio,1040)'BTHETA'
        end if
        CALL writTag(Nio,'</tr><tr>')
        do i=2,p+1
         write(Nio,1050) phi(i)
        end do  
        if (bp.eq.1) then
         write(Nio,1050) Bphi(nbphi)
        endif
        do i=2,q+1
         write(Nio,1050) theta(i)
        end do  
        if (bq.eq.1) then
         write(Nio,1050) Btheta(nbth)
        endif             
        CALL writTag(Nio,'</tr>')
        CALL writTag(Nio,'</table>')
        CALL mkPOneLine(Nio,'@','&nbsp;')
      end if
 1010 FORMAT('<p><em>',a,a,a)
 1020 FORMAT('(',i1,',',i1,',',i1,')(',i1,',',i1,',',i1,')')
 1030 FORMAT('<th scope="col">',a,'(',i1,')</th>')
 1040 FORMAT('<th scope="col">',a,'</th>')
 1050 FORMAT('<td>',f10.4,'</td>')
      end
cc
c
cc
      subroutine OutCorr(nio,nx,cMatrix)
      implicit none
C
      integer n10
      parameter(n10=10)
c
c.. INPUT PARAMETERS
      integer nio,nx
      real*8 cMatrix(n10,n10)
c.. LOcal PARAMETERS
      integer i,j
c
      CALL mkPOneLine(Nio,'ub','CORRELATION MATRIX')
      CALL mkTableTag(Nio,'w90','@')
      
      CALL writTag(Nio,'<tr>')
      CALL mkTableCell(Nio,'head','&nbsp;')
      do i = 1,nx
       write(Nio,1010)'col',i
 1010  format('<th scope="',a,'">ARMA Parameter ',i3,'</th>')
      end do
      CALL writTag(Nio,'</tr>')
      do i = 1,nx
       CALL writTag(Nio,'<tr>')
       write(Nio,1010)'row',i
       do j = 1, i
        write (Nio,1020) cMatrix(i,j)
 1020   format('<td class="center">',f6.3,'</td>')
       end do
       if (i.lt.nx) then
        do j = i+1, nx
         CALL mkTableCell(Nio,'@','&nbsp;')
        end do
       END IF
       CALL writTag(Nio,'</tr>')
      end do

      CALL writTag(Nio,'</table>')
      CALL mkPOneLine(Nio,'@','&nbsp;')

      end
c
c
c
      subroutine OutMean(nio,tst,Wm,seMEan)
      implicit none
c
c.. Input Parameters
      integer nio,tst
      real*8 Wm,seMEan
c
      if (tst .gt. 0) then
        CALL mkTableTag(Nio,'w50','Mean')
        write (Nio,7018) wm
 7018   format('<tr><th scope="row">MEAN =</th><td class="center">',
     $         g16.6,'</td></tr>',/,'<tr><th scope="row">',
     $         '<abbr title="Mean standard error">SE</abbr> =</th>',
     $         '<td class="center"> ******* </td></tr>')
        CALL writTag(Nio,'</table>')
      else
        CALL mkTableTag(Nio,'w50','Mean and Mean standard error')
        write (Nio,7019) wm,seMean
 7019   format('<tr><th scope="row">MEAN =</th><td class="center">',
     $         g16.6,'</td></tr>',/,'<tr><th scope="row">',
     $         '<abbr title="Mean standard error">SE</abbr> =</th>',
     $         '<td class="center"> ',g16.6,' </td></tr>')
        CALL writTag(Nio,'</table>')
      end if
      CALL mkPOneLine(Nio,'@','&nbsp;')
      end
c
c
c
      Subroutine OutModel(nio,noserie,p,d,q,bp,bd,bq,mq,model)
      implicit none
c
      logical T
      parameter(T=.true.)
c
c.. INPUT PARAMETERS
      integer nio,noserie,p,d,q,bp,bd,bq,mq,model
C ..INPUT/OUTPUT
*      include 'indhtml.i'
      include 'htmlout.cmn'
c
       if ((noserie.eq.0)) then
         CALL genSkip(1061)
 6013    format ('<p><em>NONSEASONAL: P=',i2,' D=',i2,' Q=',i2,a)
         write (Nio,6013) P, D, Q, Cbr
       else if ((noserie.eq.1)) then
 6014    format ('<p class="ub">MODEL</p>',/,
     $           '<p><em>NONSEASONAL: P=',i2,' D=',i2,' Q=',i2,a)
         write (Nio,6014) P, D, Q, Cbr
       end if
       if (Bp+Bd+Bq.ne.0) then
 6015    format ('SEASONAL: BP=',i2,' BD=',i2,' BQ= ',i2,a)
         write (Nio,6015) Bp, Bd, Bq, Cbr
       end if
       CALL writTag(Nio,'</em></p>')
 6016  format ('<p><em>PERIODICITY    MQ=</em> ',i3,' </p>')
       write (Nio,6016) Mq
C
C INITIALIZE DETPRI
C
       if (model .eq. 1) then
         call setTmcs('Y')
         call nWritln('ARIMA MODEL FROM TRAMO HAS BEEN '//
     $                'MODIFIED TO SATISFY SEATS CONSTRAINTS',
     $                Nio,0,T,T)
       end if
      end
c
c
c
      subroutine OutPart(nio,nAutocorr,serie,Partial,sePart)
      implicit none
c
      integer n10
      parameter(n10=10)
c
c.. Input Parameters
      integer nio,nAutocorr
      real*8 sePart,Partial(5*n10)
      character*(17) serie 
c
c.. Input/Output
*      include 'indhtml.i'
c
c.. External functions
      integer istrlen
      external istrlen
c
c.. Local Parameters
      integer mr,ml,i,ie,k,ipcf,nser
      character*2 thisP
c
       mr = 1
       ml = nAutocorr / 12
       if (MOD(nAutocorr,12) .eq. 0) then
        mr = 0
       end if
       ml = ml*12 + mr
       ipcf=0
       nser=istrlen(serie)
        do i=1,ml,12
         ipcf=ipcf+1
         ie=i+11
         WRITE(thisP,1011)ipcf
 1011    FORMAT(i2)
         CALL mkTableTag(Nio,'w80','PARTIAL AUTOCORRELATIONS OF '//
     &                   serie(1:nser)//', Part '//thisP)
         CALL mkCaption(Nio,'PARTIAL AUTOCORRELATIONS OF '//
     &                  serie(1:nser)//', Part '//thisP)

*         iTab=iTab+1
*         iId=iId+1
         CALL writTag(Nio,'<thead>')
         CALL writTag(Nio,'<tr>')
         CALL mkTableCell(Nio,'head','&nbsp;')
         DO k=i,ie
          WRITE(Nio,1020)k
 1020     FORMAT('<th scope="col"> Lag ',I6,'</th>')
         END DO
         CALL writTag(Nio,'</tr>')
         CALL writTag(Nio,'</thead>')
         CALL writTag(Nio,'<tbody>')
         CALL writTag(Nio,'<tr>')
         CALL mkHeaderCellScope(Nio,0,0,'row',
     &                        'Partial Autocorrelation Function','PACF')
         write (Nio,1030) (Partial(k), k = i,ie)
 1030    format(12('<td class="center">',f9.4,'</td>')) 
         CALL writTag(Nio,'</tr>')
         CALL writTag(Nio,'<tr>')
         CALL mkHeaderCellScope(Nio,0,0,'row','Standard Error','SE')
         write (Nio,1030) (sePart, k = i,ie)
         CALL writTag(Nio,'</tr>')
         CALL writTag(Nio,'</tbody></table>')
         CALL mkPOneLine(Nio,'@','&nbsp;')
        end do
      end
c
c
c     OutSerAc: Output of transformed series and autocorrelations
      subroutine OutSerAc(nio,z,nz,Lam,Imean,noserie,Pg,Out,Ndec,iter,
     $                    D,BD,Nper,Nyer,mq,Wdif,
     $                    WdifCen,nwDif,WmDifXL,Zvar,VdifXL,
     $                    QstatXL,df,rXL,seRxl,M,partACF,sePartACF)
      implicit none
      integer n10
      parameter(n10=10)
      INCLUDE 'srslen.prm'
      INCLUDE 'dimensions.i'
c   INPUT
      integer nio,nz,Lam,Imean,noserie,Pg,Out,Ndec,iter,D,BD,Nper,Nyer,
     $        mq,nwDif                  
      real*8 z(*),Wdif(*),WdifCen(*),WmDifXL,Zvar,VdifXL
      real*8 QstatXL,rXL(5*n10),seRxl(5*n10),partACF(5*n10),sePartACF
      integer df,M
C    OUTPUT
*      integer Itab,Iid
c    LOCAL
      integer nz1,nyer2,nper2,thisDate(2)
      character htmtit*120,fname*30,subtitle*50
c
      thisDate(1) = Nyer
      thisDate(2) = Nper

*       if ((Lam.ne.1).and. (noserie.eq.0)) then
*         if ((Pg.eq.0) .and. (Out.lt.2).and.(iter.eq.0)) then
*                 fname = 'TSERIE.T'
*                 subtitle = 'LINEARIZED SERIES (LOGS)'
*                 call PLOTLSERIES(fname,subtitle,z,Nz,1,0.0d0)
*         end if
*       end if
*       if ((Lam.eq.1) .and. (noserie.eq.0) .and.
*     $    (iter.eq.0).and.(Pg.eq.0) .and. (Out.lt.2)) then
*         call grSerie(z,nz,Nper,nYer,mq)
*       end if
c
       if ((Lam.ne.1).and. (noserie.eq.0)) then
         if (Out .eq. 0) then
            CALL genSkip(1165)
             if (Ndec.eq.0) THEN
               call prttbl(thisDate,Mq,z,nz,'TRANSFORMED SERIES',2,
     $                     'trn.seats')
             ELSE
               call prttbl(thisDate,Mq,z,nz,'TRANSFORMED SERIES',Ndec,
     $                     'trn.seats')
             END IF
         end if
       end if
c
       if (Out .eq. 0) then
*           iTab=iTab+1
*           iId=iId+1
           CALL genSkip(1063)
           CALL mkTableTag(nio,'w70','Differences')
           CALL mkCaption(nio,'Differences')
           write (nio,1000)D, Bd
 1000      format('<tr><th scope="row" abbr="D">',
     $       'NONSEASONAL DIFFERENCING     D=</th><td>',i2,
     $       '</td></tr>',/,'<tr><th scope="row" abbr="BD">',
     $       'SEASONAL DIFFERENCING    BD=</th><td>',i2,
     $       '</td></tr></table>')
           CALL mkPOneLine(Nio,'@','&nbsp;')
       end if
c
       if ((D.ne.0.or.Imean.ne.0) .and. (D+Bd).ne.0) then
         nz1 = Nz
         nyer2 = Nyer
         nper2 = Nper
         Nz = NwDif
         Nper = Nper + Bd*Mq + D
         do while (Nper.gt.mq .and. mq.ne.0)
           Nper = Nper - mq
           Nyer = Nyer + 1
         end do
         if (Out .eq. 0) then
             thisDate(1) = Nyer
             thisDate(2) = Nper
             CALL genSkip(1166)
             call prttbl(thisDate,Mq,Wdif,NwDif,'DIFFERENCED SERIES',
     &                   Ndec,'diff.srs')
         end if  
         call USRENTRY(Wdif,1,NwDif,1,mpkp,3001)
       end if    
c
       if (Imean.ne.0 .or. D.ne.0) then
         if (Imean .ne. 0) then
           if (Out .eq. 0) then
             htmtit = 'SERIES HAS BEEN MEAN CORRECTED'
           end if
           if ((D+Bd) .ne. 0) then
             if (Out .eq. 0) then
               if (Lam .eq. 1) then
                 htmtit = 'DIFFERENCED AND CENTERED SERIES'
                 CALL genSkip(1167)
               else
                 htmtit = 'DIFFERENCED AND CENTERED'//
     $                    ' TRANSFORMED SERIES'
                 CALL genSkip(1168)
               end if
               thisDate(1) = Nyer
               thisDate(2) = Nper
               call prttbl(thisDate,Mq,WdifCen,NwDif,htmtit,Ndec,
     &                     'diff.srs.cen')
             end if
           end if
         end if
*         if ((Pg.eq.0) .and. (Out.lt.2).and.(iter.eq.0)) then
*           if ((d+bd).ne.0.or.Imean.eq.0) then
*             fname = 'DIFFER.T'
*             subtitle = 'DIFFERENCED SERIES'
*             if ((Imean.ne.0).and.((D+BD).ne.0))then
*               call PLOTSERIES(fname,subtitle,WdifCen,NwDif,1,0.0d0)
*             else
*               call PLOTSERIES(fname,subtitle,Wdif,NwDif,1,0.0d0)
*             end if
*           end if
*         end if
       end if   
       if ((D.ne.0.or.Imean.ne.0) .and. (D+Bd).ne.0) then
         Nyer = nyer2
         Nz = nz1  !restauramos antiguo valor de nz
         Nper = nper2
       end if   
c
       if (Out .eq. 0) then
*           iTab=iTab+1
           write (Nio,1010) wmDifXL
 1010      FORMAT('<p><em>MEAN OF DIFFERENCED SERIES:</em> ',d12.4,
     $            '</p>') 
       end if
       if (Imean .eq. 0) then
         if (Out .eq. 0) then
           CALL mkPOneLine(Nio,'em','MEAN SET EQUAL TO ZERO')
         end if
       end if
       if (Out .eq. 0) then
           write (Nio,1020) Zvar
 1020      FORMAT('<p><em>VARIANCE OF Z SERIES:</em> ',d12.4,'</p>') 
       end if
       if (((D+Bd).ne.0) .and. (Out.eq.0)) then
           write (Nio,1030) VdifXL
 1030      FORMAT('<p><em>VARIANCE OF DIFFERENCED:</em> ',d14.4,'</p>') 
       end if
c
       CALL genSkip(1049)
       Call OutAuto(OUT,Nio,QstatXL,df,rXL,seRxl,M,
     $              'STATIONARY SERIES         ')
CUNX#ifdef DOS
!DEC$ IF DEFINED (DOS)
*       if ((Pg.eq.0) .and. (Out.eq.0).and.(iter.eq.0)) then
*                 fname = 'DAUTO.T2'
*                 subtitle = 'ACF OF DIFFERENCED SERIES'
*                 call PLOTACF(fname,subtitle,rXL,M,0,0)
*       end if
CUNX#end if
!DEC$ end if
       if (out .eq. 0) then
           CALL genSkip(1062)
           call OutPart(nio,m,'STATIONARY SERIES',partAcf,SEpartAcf)
       end if
      end
c
c
c
*      subroutine grSerie(z,nz,Nper,nYer,mq)
*      implicit none
*      integer mp,kp
*      include 'srslen.prm'
*      parameter(kp = PFCST, mp = POBS)
*c   INPUT
*      real*8 z(*)
*      integer nz,nPer,nYer,mq
*c   Local
*      real*8 bz(mp+2*kp)
*      integer Nper2,nyer2,i
*      character fname*30,subtitle*50
*c
*         fname = 'GSERIE.T'
*         subtitle = 'PERIOD-TO-PERIOD SERIES GROWTH'
*         do i = 2,Nz
*           bz(i-1) = z(i) - z(i-1)
*         end do
*         nyer2 = Nyer
*         nper2 = Nper
*         Nper=Nper+1
*         if (Nper .gt. Mq) then
*           Nper = 1
*           Nyer = Nyer + 1
*         end if
*         call PLOTRSERIES(fname,subtitle,bz,Nz-1,1,0.0d0)
*         Nyer = nyer2
*         Nper = nper2
*      end
c
c
c
      Subroutine OutPart2(nio,z,nz,Lam,Imean,noserie,Pg,Out,Ndec,
     $                    iter,p,D,q,bp,BD,bq,Nper,Nyer,mq,
     $                    Wdif,WdifCen,nwDif,WmDifXL,Zvar,VdifXL,
     $                    QstatXL,df,rXL,seRxl,M,partACF,sePartACF,
     $                    model,PicosXL,init,tstmean,Wm,seMean,nx,
     $                    Cmatrix,PHI,TH,BPHI,BTH,sePHI,seTH,seBPHI,
     $                    seBTH,MArez,MAimz,MAmodul,MAar,MApr,
     $                    rez,imz,modul,ar,pr,THstar,isVa0)
      implicit none
      integer n10,n1,n12
      parameter(n1=1,n10=10,n12=12)
c   INPUT
      integer nio,nz,Lam,Imean,noserie,Pg,Out,Ndec,iter,
     $        p,D,q,bp,BD,bq,Nper,Nyer,mq,nwDif,model
      real*8 z(*),Wdif(*),WdifCen(*),WmDifXL,Zvar,VdifXL
      real*8 QstatXL,rXL(5*n10),seRxl(5*n10),partACF(5*n10),sePartACF
      integer df,M
      character PicosXL(7)*2
      integer init,tstmean,nx
      logical isVa0
      real*8 Wm,seMean,Cmatrix(n10,n10),THstar(27),
     $       PHI(3*n1),TH(3*n1),BPHI(3*n1),BTH(3*n1),
     $       sePHI(n10),seTH(n10),seBPHI(n10),seBTH(n10),
     $    MArez(5*n12+n12/3),MAimz(5*n12+n12/3),MAmodul(5*n12+n12/3),
     $    MAar(5*n12+n12/3),MApr(5*n12+n12/3),
     $    rez(5*n12+n12/3),imz(5*n12+n12/3),modul(5*n12+n12/3),
     $    ar(5*n12+n12/3),pr(5*n12+n12/3)
c   OUTPUT
*      integer Itab,Iid
C
      if (noserie.ne.1) then
        call OutSerAc(nio,z,nz,Lam,Imean,noserie,Pg,Out,Ndec,
     $                iter,D,BD,Nper,Nyer,mq,Wdif,
     $                WdifCen,nwDif,wmDifXL,Zvar,VdifXL,
     $                QstatXL,df,rXL,seRxl,M,partACF,sePartACF)
      end if
C
C WRITE DESCRIPTION OF MODEL
C
      if ((Out.eq.0)) then
        call OutModel(nio,noserie,p,d,q,bp,bd,bq,mq,model)
c
        if (noserie.ne.1) then
          if (init.ne.2) then
            CALL genSkip(1064)
            CALL mkPOneLine(Nio,'ub','PARAMETER ESTIMATES')
            call OutMean(nio,tstMean,Wm,seMEan)
c            call OutCorr(nio,nx,cMatrix)
          end if 
        end if 
c
       if (isVa0) then
        call OutARIMAva0(Nio,init,p,bp,wm,PHI,BPHI)
       else
        call OutARIMA(Nio,init,p,bp,q,bq,wm,
     $         PHI,TH,BPHI,BTH,sePHI,seBPHI,seTH,seBTH)
        if (Q.gt.1) then
          call OutRoots(nio,q,MArez,MAimz,MAmodul,MAar,MApr,
     $                  '<abbr title="moving average">MA</abbr>(Q)')
        end if
        if ((noserie.ne.1).and.((mq.eq.4).or.(mq.eq.12))) then
          call warnPeaks(nio,picosXl,'Linealized Series   ',mq)
        end if
       end if
c
       if (p.gt.1) then
          call OutRoots(nio,p,rez,imz,modul,ar,pr,
     $                  '<abbr title="autoregression">AR</abbr>(P)')
       end if                            
      end if
      end
*c
*c
*c
*c     ErrorLog: añade una línea indicando para cada serie el error o warning encontrado.
*      subroutine ErrorLog(HTML,Description,onlyFirst)
*c  parámetros entrada:
*c      HTML: 1=>salida en fichero html ErrorLog.htm, 0=>Salida en fichero de salida plano.
*c      Description: descripción del error encontrado,
*c                   Cuidado: si el parámetro de entrada es de diferente longitud, puede escribir basura al final de la línea.
*c      onlyFirst=1 solo escribirá el error si es la primera llamada a ErrorLog que se ha hecho en la serie actual,
*c                  para eso usa la variable global haveErrors que cada vez que se empieza a ejecutar una nueva
*c                  serie se pone a 0, y se pone a 1 cuando se llama a ErrorLog.
*c      OnlyFirst=0 escribirá el error siempre, las columnas de número de serie y nombre de serie se dejaran en 
*c                  blanco si ya se reportaron errores para esa serie(haveErrors>0).
*c Otras variables globales:
*c        haveError: si ya se ha escrito en ErrorLog un error asociado con la serie y modelo actual.
*c        countError: indica cuantas series con error se han encontrado previo a llamar a ErrorLog,
*c                   si countError=0 =>Errorlog crea el fichero ErrorLog.{txt ó htm} y escribe la cabecera.
*c        OutDir:directorio de salida.
*c        Iserie: numero de serie que estamos procesando.
*c        mattitle(1:matlen):nombre de serie que estamos procesando.
*      implicit none
*c   INPUT PARAMETERS
*      include 'sername.i'
*      include 'seatserr.i'
*      include 'dirs.i'
*      integer HTML,onlyFirst
*      character*500 description
*c   EXTERNAL
*      integer ISTRLEN
*      external ISTRLEN
*c   LOCAL
*      character*180 filename,HTMLtitle
*      integer Ifail,lDescription
*c ---------------------------------------------------------
*      if (haveError.ne.0.and.onlyFirst.ne.0) then
*        return
*      end if
*      if (CountError.eq.0) then
*        if (HTML.eq.1) then
*          filename=OutDIR(1:istrlen(OutDir)) // '\ErrorLog.htm'
*        else
*          filename=OutDir(1:ISTRLEN(OUTDIR)) //'\ErrorLog.txt'
*        end if
*        call openDevice(filename,76,0,Ifail)
*        if (Html.eq.1) then
*c          htmlTitle='Errors Log File'
*          call writeDocType(76,1)
*          write(76,'(''<HTML lang="en-US">'')')
*          write(76,'(''<HEAD>'')')
*          write(76,'(''<META HTTP-EQUIV="content-type" '',
*     $           ''CONTENT="text/html;charset=iso-8859-1">'')')
*          write(76,'(''<META NAME="lang" CONTENT="en-US">'')')
*          write(76,'(''<TITLE>Seats ErrorLog</TITLE>'')')
*          call writeCSS(76,1)
*          write(76,'(''</HEAD><BODY>'')')
*          write(76,'(''<table>'')')
*          write(76,'(''<tr><th scope="col">n</th>'',
*     $           ''<th scope="col">TITLE</th>'',
*     $           ''<th scope="col">Description</th></tr>'')')
*        else
*          write(76,'(4x,"n",5x,"TITLE",23x,"Description")')
*        end if
*      end if
*      countError=countError+1
*      lDescription=ISTRLEN(description)
*      if (haveError.ne.0) then
*        if (HTML.eq.1) then
*          write(76,'(''<tr><td scope="row"></td>'',
*     $           ''<td></td>'',
*     $           ''<td>'',a,''</td></tr>'')') 
*     $           description(1:lDescription)
*        else
*          write(76,'(33x,a)') description(1:ldescription)
*        end if
*      else
*        if (HTML.eq.1) then
*          write(76,'(''<tr><td scope="row">'',i7,''</td>'',
*     $           ''<td>'',a,''</td>'',
*     $           ''<td>'',a,''</td></tr>'')') 
*     $           niter,mattitle(1:22),description(1:lDescription)
*        else
*          write(76,'(i7,2x,a22,4x,a)') niter,mattitle(1:22),
*     $           description(1:ldescription)
*        end if
*      end if
*      haveError=1
*      end
c
c
c
      subroutine shCloseTD(nio,InputModel,p,d,q,bp,bd,bq)
      implicit none
      logical T
      parameter(T=.true.)
c    INPUT PARAMETERS
      integer nio,InputModel,p,d,q,bp,bd,bq
c    EXTERNAL
      character*7 OrderName
      external OrderName
c ---------------------------------------------------------------
      INCLUDE 'htmlout.cmn'
c ---------------------------------------------------------------
      if (InputModel.gt.5) then
        return
      end if
      if (InputModel.gt.1) then
        write(nio,1010) orderName(InputModel),Cbr,p,d,q,bp,bd,bq  
      else
        write(nio,1020) p,d,q,bp,bd,bq  
      end if
 1010 FORMAT('<p>',a,' model has changed.',a,
     $       'The model is approximated to (',i1,',',i1,',',i1,')(',i1,
     $       ',',i1,',',i1,')</p>')
 1020 FORMAT('<p> Model changed to (',i1,',',i1,',',i1,')(',i1,
     $       ',',i1,',',i1,')</p>')
      end
c
c
c
      subroutine ShowFirstModel(nio,p,d,q,bp,bd,bq,th,
     $                Bth,phi,Bphi,imean,tramo,init)
      implicit none
c    INPUT PARAMETERS
      integer nio,p,d,q,bp,bd,bq,imean,tramo,init
      real*8 th(*),Bth(*),phi(*),Bphi(*)
c
      integer i
c    EXTERNAL
      character*7 OrderName
      external OrderName
c
      INCLUDE 'htmlout.cmn'
c ---------------------------------------------------------------
      if (tramo.eq.0) then     
       write(nio,1010)Cbr,Cbr,'ARIMA MODEL SELECTED BY TRAMO: '
      else
       write(nio,1010)Cbr,Cbr,'SEATS ARIMA MODEL INPUT: '
      end if
 
      write(nio,1020)p,d,q,bp,bd,bq
      if (imean.eq.0) then
        write(nio,*) 'with mean.</em></p>'
      else
        write(nio,*) 'without mean.</em></p>'
      endif
      if (init.eq.2) then
        CALL mkTableTag(nio,'w80','ARMA Parameters')
        CALL mkCaption(nio,'<abbr title="autoregressive moving '//
     &         'average">ARMA</abbr> Parameters')
        CALL writTag(Nio,'<tr>')
        do i=1,p
          write(nio,1030)'PHI',i
        end do
        if (bp.eq.1) then 
          write(nio,1040)'BPHI'
        end if
        do i=1,q
          write(nio,1030)'THETA',i
        end do
        if (bq.eq.1) then 
          write(nio,1040)'BTHETA'
        end if
        CALL writTag(nio,'</tr><tr>') 
        do i=1,p
          write(Nio,1050) -phi(i)
        end do  
        if (bp.eq.1) then
          write(Nio,1050) -Bphi(1)
        endif
        do i=1,q
          write(Nio,1050) -th(i)
        end do  
        if (bq.eq.1) then
          write(Nio,1050) -Bth(1)
        end if             
        CALL writTag(nio,'</tr></table>') 
      end if
 1010 FORMAT('<p><em>',a,a,a)
 1020 FORMAT('(',i1,',',i1,',',i1,')(',i1,',',i1,',',i1,')')
 1030 FORMAT('<th scope="col">',a,'(',i1,')</th>')
 1040 FORMAT('<th scope="col">',a,'</th>')
 1050 FORMAT('<td>',f10.4,'</td>')
      end
cc
c
cc
      subroutine showNA(nio,InputModel)
      implicit none
      logical T
      parameter(T=.true.)
c    INPUT PARAMETERS
      integer nio,InputModel
      integer i
c    EXTERNAL
      character*7 OrderName
      external OrderName
c ---------------------------------------------------------------
      if ((InputModel.ne.1).and.(InputModel.le.5)) then
       call wWritln(orderName(InputModel)//
     $             ' model has no admissible decomposition.',Nio,0,T,T)
      end if
      end
c
c
c
      character*7 function orderName(Index)
      implicit none
c    INPUT PARAMETERS
      integer Index
c    LOCAL ARRAYS
      character ordenes(10)*7,masOrdenes*7
      data ordenes /'First  ','Second ','Third  ','Fourth ','Fifth  ',
     $         'Sixth  ','Seventh','Eighth','ninth','tenth'/
c --------------------------------------------------------------
      if (Index.le.10) then
        masOrdenes=ordenes(Index)
      else
        write(masOrdenes,'(I5,"TH")')Index
      end if
      orderName=masOrdenes
      return
      end
c
c
c     OutSearch: escribe la salida de Search cuando todo va bien.
      subroutine OutSearch(nio,out,itn,ifn,fi,x,nx,e)
      implicit none
c    INPUT PARAMETERS
      integer nio,itn,out,ifn,nx,e(*)
      real*8 fi,x(*)
c    LOCAL PARAMETERS
      integer j,fixed
      include 'htmlout.cmn'
c -----------------------------------------------------------------
      if (itn.eq.0) then 
         return
      end if
*      if (out.eq.0) then
* 6019   format ('<p><em>CONVERGED AFTER </em>',i2,
*     $          '<em> ITERATIONS AND </em>',i3,
*     $          '<em> FUNCTION VALUES    F = </em>',e17.8,
*     $          6('<br>',e20.6))
*        write (Nio,6019) itn, Ifn, fi, (x(j), j = 1,nx)
*          CALL writTag(Nio,'</p>')
*      end if
      
      fixed=0
      do j = 1,nx
        if (e(j) .eq. 1) then
           fixed=fixed+1
        end if
      enddo
      if ((fixed.gt.0).and.(out.eq.0)) then
 6020    format ('<p><strong>PARAMETERS FIXED</strong>')
         write (Nio,6020)
       do j = 1,nx
        if (e(j) .ne. -1) then
         if (e(j) .eq. 1) then
 6021       format (a,i6)
            write (Nio,6021) Cbr, j
         end if
        end if
       end do
       CALL writTag(Nio,'</p>')
      end if
      end
cc
c
cc
      subroutine m_statSA(nio)
      implicit none
      logical T
      parameter(T=.true.)
      integer nio
c 
      include 'htmlout.cmn'
c 
      call nWritln('SEASONALITY IS STATIONARY (EVERY PERIOD HAS '//
     $    'ZERO MEAN) AND MODEL MAY YIELD AN ERRATIC SEASONAL '//
     $    'COMPONENT.'//Cbr//'SEASONAL ADJUSTMENT MAY BE IMPROVED BY'//
     $    ' SETTING "STATSEAS=1".',Nio,0,T,T)
      end
cc
c
cc
      subroutine m_statSB(nio)
      implicit none
      logical T
      parameter(T=.true.)
      integer nio
c
      call nWritln('THE MODEL EVIDENCES VERY WEAK SEASONALITY. '//
     $             'ITS ESTIMATION WOULD BE ERRATIC AND THE '//
     $             'EFFECT IS CAPTURED AS A TRANSITORY COMPONENTS.',
     $             Nio,0,T,T)
          
      end
cc
c
cc
      subroutine m_statSC(nio)
      implicit none
      logical T
      parameter(T=.true.)
      integer nio
c
      call nWritln('IN AN ATTEMPT TO IMPROVE SEASONAL ADJUSTMENT, '//
     $   'NON-STATIONARITY HAS BEEN IMPOSED ON THE SEASONAL COMPONENT.',
     $             Nio,0,T,T)
          
      end
cc
c m_statO:antiguo mensaje (hasta rel 433) que se sacaba con STATSEAS=0 cuando BP=1
cc   
      subroutine m_statO(nio)
      implicit none
      logical T
      parameter (T=.true.)
      integer nio
c
      include 'htmlout.cmn'
c
      call nWritln('INPUT MODEL HAS A STATIONARY '//
     $      'SEASONAL STRUCTURE '//
     $      'INAPPROPRIATE FOR SEASONAL ADJUSTMENT.'//Cbr//
     $      'SEATS HAS CHANGED THE SEASONAL ORDERS TO :'//
     $      '(0, 1, 1) - This may affect forecasting.',Nio,0,T,T)
     
      end
cc
c
cc
      subroutine m_vc_is0(nio)
      implicit none
      logical T
      parameter (T=.true.)
      integer nio
c
      call nWritln('Transitory innovation variance is very small. '//
     $        'Transitory component can be ignored',Nio,0,T,T)
      end
cc
c
cc
      subroutine writeSumS(baseName,nBase,numser,noTratadas,wSposBphi,
     $           wSstochTD,wSstatseas,wSrmod,wSxl)
      implicit none
      logical T,F
      parameter (T=.true.,F=.false.)
      include 'stdio.i'
      include 'sums.i'
      include 'dirs.i'  
      integer numser,noTratadas,wSposBphi,nBase,
     $            wSstochTD,wSstatseas
      real*8 wSrmod,wSxl
      character cstr*(10),baseName*(PFILCR)
c
      integer wio,ireturn,ipos
      integer date_time (8)
      character*12 real_clock (3)
      character fname*180
      character sposBphi*2,sstochTD*2,sstatseas*2,srmod*8,sxl*8,sur*8
      integer NTratadas
c
      integer ISTRLEN
      external ISTRLEN
      include 'htmlout.cmn'
      include 'build.prm'
*      include 'build.i'
c abrir fichero
c
c
      nTratadas=NumSer-noTratadas
      if (nTratadas.le.0) then
        nTratadas=1
      endif
      wio=2
      ireturn=0
      fname = baseName(1:nBase)//'_sumS.html'
      call OPENDEVICE(fname,wio,0,ireturn)
      if (ireturn .ne. 0) then
       return
      end if
      call DATE_AND_TIME (REAL_CLOCK(1),REAL_CLOCK(2),REAL_CLOCK(3),
     &                    DATE_TIME)  
      CALL mkHead(wio,baseName(1:nBase)//'_sumS.html Summary Seats',
     &            'Summary of SEATS output',T,1,7,F)
      CALL writTagOneLine(wio,'h1','@','SUMMARY SEATS')
*      CALL mkPOneLine(wio,'@',
*     &                'Input File : '//infil(1:ISTRLEN(infil)))
*      CALL writln('Revision : '//revision//Cbr,wio,0,T,F)
      CALL writln('Date : '// 
     &        real_clock(1)(1:4) // '-' // real_clock(1)(5:6) // '-' //
     &        real_clock(1)(7:8) // ' ' //real_clock(2)(1:2) // ':' //
     &        real_clock(2)(3:4) // ':' // real_clock(2)(5:6)//Cbr,
     &        wio,0,F,F)
      ipos=1
      CALL itoc(numser,cstr,ipos)
      CALL writln('Series in file: ' //cstr(1:(ipos-1))//Cbr,
     &            wio,0,F,F)
      IF(tSEATS.gt.0)THEN
       ipos=1
       CALL itoc(tSEATS,cstr,ipos)
       CALL writln('Series processed with SEATS: '//cstr(1:(ipos-1))//
     &             Cbr,wio,0,F,F)
      END IF
      IF(tX11.gt.0)THEN
       ipos=1
       CALL itoc(tX11,cstr,ipos)
       CALL writln('Series processed with X-11: '//cstr(1:(ipos-1))//
     &             Cbr,wio,0,F,F)
      END IF
      IF(tNSA.gt.0)THEN
       ipos=1
       CALL itoc(tNSA,cstr,ipos)
       CALL writln('Series not seasonally adjusted: '//
     &             cstr(1:(ipos-1))//Cbr,wio,0,F,F)
      END IF
      ipos=1
      CALL itoc(numser-noTratadas,cstr,ipos)
      CALL writln('Total number of series processed: '//
     &            cstr(1:(ipos-1)),wio,0,F,T) 
c
      CALL mkTableTag(Wio,'w70','Summary Seats Input parameters')
      CALL mkCaption(Wio,'Summary Seats Input parameters')
      CALL writTag(Wio,'<tr>')
      if (wSrmod.eq.-9.99) then
        write (wio,1010)'sRmod','RMOD','sRmod'
      else
        write (wio,1020)'sRmod','RMOD','sRmod',wSrmod
      end if
      if (wSxl.eq.-9.99) then
        write (wio,1010)'sxl','XL','sxl'
      else
        write (wio,1020)'sxl','XL','sxl',wSxl
      end if
      if (wSposbphi.eq.-9) then
        write (wio,1010)'sposbphi','POSBPHI','sposbphi'
      else
        write (wio,1021)'sposbphi','POSBPHI','sposbphi',wSposbphi
      end if
      CALL writTag(Wio,'</tr>')
      CALL writTag(Wio,'<tr>')
      if (wSstochtd.eq.-9) then
        write (wio,1010)'sstochtd','STOCHTD','sstochtd'
      else
        write (wio,1021)'sstochtd','STOCHTD','sstochtd',wSstochtd
      end if
      if (wSstatseas.eq.-9) then
        write (wio,1010)'sstatseas','STATSEAS','sstatseas'
      else
        write (wio,1021)'sstatseas','STATSEAS','sstatseas',wSstatseas
      end if
      CALL writTag(Wio,'</tr>')
      CALL writTag(Wio,'</table>')
      CALL mkPOneLine(Wio,'@','&nbsp;')
 1010 format('<th id="',a,'">',a,'=</th><td headers="',a,'">*</td>') 
 1020 format('<th id="',a,'">',a,'=</th><td headers="',a,'">',f8.2,
     &       '</td>')
 1021 format('<th id="',a,'">',a,'=</th><td headers="',a,'">',i8,
     &       '</td>')
c      
      CALL writTagOneLine(wio,'h2','@','TABLE A: GENERAL')
      CALL mkTableTag(Wio,'w80','TABLE A: GENERAL')
      CALL writTag(wio,'<tr>')
      CALL mkTableCell(wio,'head','&nbsp;')
      CALL mkHeaderCellScope(wio,0,0,'col','Number of Series',
     &                       '# of Series')
      CALL mkHeaderCellScope(wio,0,0,'col','Percent of Series','%')
      CALL writTag(wio,'</tr>')
      write(wio,1030)'Model Changed by SEATS', 
     &      tTMCS, DBLE(tTMCS)/DBLE(nTratadas)*100.0d0
      write(wio,1030)'Approximate (NA decomposition)',
     &      tANA, DBLE(tANA)/DBLE(nTratadas)*100.0d0 
      write(wio,1030)'With seasonal component',
     &      tScomp, DBLE(tScomp)/DBLE(nTratadas)*100.0d0 
      write(wio,1030)'With Transitory Component',
     &      tCycComp, DBLE(tCycComp)/DBLE(nTratadas)*100.0d0 
      write(wio,1030)'With Stochastic TD',
     &      tStocTD, DBLE(tStocTD)/DBLE(nTratadas)*100.0d0 
      CALL writTag(Wio,'</table>')
      CALL mkPOneLine(Wio,'@','&nbsp;')
 1030 format('<tr><th scope="row">',a,'</th>','<td>',i7,'</td><td>',
     &       f6.2,'</td></tr>') 
c
      CALL writTagOneLine(wio,'h2','@','TABLE B: CHECKS')
      CALL mkTableTag(Wio,'w80','TABLE B: CHECKS')
      CALL writTag(wio,'<tr>')
      CALL mkTableCell(wio,'head','&nbsp;')
      CALL mkHeaderCellScope(wio,0,0,'col','Number of Series',
     &                       '# of Series')
      CALL mkHeaderCellScope(wio,0,0,'col','Percent of Series','%')
      CALL writTag(wio,'</tr>')
      write(wio,1030)'Fail Spectral ',
     &      tSpecFac, DBLE(tSpecFac)/DBLE(nTratadas)*100.0d0
      write(wio,1030)'Fail check on ACF',
     &      tACF, DBLE(tACF)/DBLE(nTratadas)*100.0d0
      write(wio,1030)'Fail check on CCF',
     &      tCCF, DBLE(tCCF)/DBLE(nTratadas)*100.0d0
      write(wio,1030)'Unstable seasonality'//Cbr//
     &      '(too large innovation variance)',
     &      tUnstSa,DBLE(tUnstSa)/DBLE(nTratadas)*100.0d0
      write(wio,1030)'Unreliable estimation of seasonality '//Cbr//
     &      '(too large estimation variance)',
     &      tUnrSa,DBLE(tUnrSa)/DBLE(nTratadas)*100.0d0
      write(wio,1030)'Revisions in SA series are too large', 
     &      tRevSa,DBLE(tRevSa)/DBLE(nTratadas)*100.0d0
      write(wio,1030)'Seasonality detected but not significant',
     &      tSeasNoSig,DBLE(tSeasNoSig)/DBLE(nTratadas)*100.0d0 
      write(wio,1030)'Bias in level of SA series is too large',
     &      tBias,DBLE(tBias)/DBLE(nTratadas)*100.0d0 
      CALL writTag(Wio,'</table>')
      CALL mkPOneLine(Wio,'@','&nbsp;')
c
      if (tCrQs.ne.-1) then
        CALL writTagOneLine(wio,'h2','@',
     &                     'TABLE C: RESIDUAL SEASONALITY IN SA SERIES')
        CALL mkTableTag(Wio,'w80',
     &                  'TABLE C: RESIDUAL SEASONALITY IN SA SERIES')
        CALL writTag(wio,'<tr>')
        CALL mkTableCell(wio,'head','&nbsp;')
        CALL mkHeaderCellScope(wio,0,0,'col','Number of Series',
     &                       '# of Series')
        CALL mkHeaderCellScope(wio,0,0,'col','Percent of Series','%')
        CALL writTag(wio,'</tr>')
        write(wio,1030)'Autocorrelation function evidence',
     &        tCrQs,DBLE(tCrQs)/DBLE(nTratadas)*100.0d0 
        write(wio,1030)'Non-Parametric evidence',
     &        tCrSNP,DBLE(tCrSNP)/DBLE(nTratadas)*100.0d0 
        write(wio,1030)'Spectral evidence',
     &        tCrPeaks,DBLE(tCrPeaks)/DBLE(nTratadas)*100.0d0 
        CALL writTag(Wio,'</table>')
        CALL mkPOneLine(Wio,'@','&nbsp;')
      end if
      CALL writTag(Wio,'</body></html>')  
      close(wio)
      return
      end 
C 
c     PhaseDia writes the Concurrent estimator:phase Diagram
      subroutine PhaseDia(nio,phaseDp,phaseDs,mq)
      implicit none
      integer mw
      parameter (mw=1200)
c    INPUT PARAMETERS
      integer nio,mq
      real*8 phaseDp(0:mw),phaseDs(0:mw)
c    LOCAL PARAMETERS
      integer i
      intrinsic INT
c ----------------------------------------------------------------------
      CALL makDivId(Nio,'concurrent.estimator.phase','@')
      CALL mkTableTag(Nio,'w50','CONCURRENT ESTIMATOR : PHASE DIAGRAM')
      CALL mkCaption(Nio,'CONCURRENT ESTIMATOR : PHASE DIAGRAM')
      CALL makColgroup(Nio,0)
      CALL makColgroup(Nio,2)
      if (MQ.eq.12) then
         write(nio,1010)'months'
      else
         write(nio,1010)'time periods'
      end if
      write(nio,1020)
     &   '<abbr title="seasonally adjusted">SA</abbr> series',
     &   'trend-cycle'
      CALL writTag(nio,'<tbody>')
      write(nio,1030)'INFINITY',phaseDs(0),phaseDp(0)
      write(nio,1030)'20 years cycle',
     $        phaseDs(INT(2*mw/(20*MQ))),phaseDp(INT(2*mw/(20*MQ)))
      write(nio,1030)'10 years cycle',
     $        phaseDs(INT(2*mw/(10*MQ))),phaseDp(INT(2*mw/(10*MQ)))
      write(nio,1030)'5 years cycle',
     $        phaseDs(INT(2*mw/(5*MQ))),phaseDp(INT(2*mw/(5*MQ)))
      write(nio,1030)'2 years cycle',
     $        phaseDs(INT(2*mw/(2*MQ))),phaseDp(INT(2*mw/(2*MQ)))
      CALL writTag(nio,'</tbody>')
      CALL writTag(nio,'</table></div>')
      CALL mkPOneLine(nio,'@','&nbsp;')
c ----------------------------------------------------------------------
 1010 format('<thead><tr><th rowspan="2">period of cycle',
     $       '</th><th colspan="2" scope="colgroup">',
     $       'Delay(in ',a,')</th></tr>')
 1020 format('<tr>',2('<th scope="col">',a,'</th>'),'</tr></thead>')
 1030 format('<tr><th scope="row">',a,'</th>',/,
     $       2('<td class="center">',F6.1,'</td>'),/,'</tr>')
c ----------------------------------------------------------------------
      end
C 
c     PhaseDia writes the Concurrent estimator:phase Diagram
      subroutine Phas2Dia(nio,phaseDp,phaseDs,FDelayp,FDelaySA,mq)
      implicit none
      integer mw
      parameter (mw=1200)
c    INPUT PARAMETERS
      integer nio,mq
      real*8 phaseDp(0:mw),phaseDs(0:mw),FDelayp(0:mw),FDelaySA(0:mw)
c    LOCAL PARAMETERS
      integer i
      intrinsic INT
c -------------------------------------------------------------
      CALL makDivId(Nio,'concurrent.estimator.phase','@')
      CALL mkTableTag(Nio,'w50','CONCURRENT ESTIMATOR : PHASE DIAGRAM')
      CALL mkCaption(Nio,'CONCURRENT ESTIMATOR : PHASE DIAGRAM')
      CALL makColgroup(Nio,0)
      CALL makColgroup(Nio,2)
      if (MQ.eq.12) then
          write(nio,1010)'months'
      else
          write(nio,1010)'time periods'
      end if
      write(nio,1020)
      write(nio,1030)
      CALL writTag(nio,'<tbody>')
      write(nio,1040)'INF',
     $        phaseDs(0),FdelaySA(0),phaseDp(0),FdelayP(0)
      write(nio,1040)'20years cycle',
     $        phaseDs(INT(2*mw/(20*MQ))),FdelaySA(INT(2*mw/(20*MQ))),
     $        phaseDp(INT(2*mw/(20*MQ))),FDelayP(INT(2*mw/(20*MQ)))
      write(nio,1040)'10years cycle',
     $        phaseDs(INT(2*mw/(10*MQ))),FdelaySA(INT(2*mw/(10*MQ))),
     $        phaseDp(INT(2*mw/(10*MQ))),FdelayP(INT(2*mw/(10*MQ)))
      write(nio,1040)'5years cycle',
     $        phaseDs(INT(2*mw/(5*MQ))),FdelaySA(INT(2*mw/(5*MQ))),
     $        phaseDp(INT(2*mw/(5*MQ))),FdelayP(INT(2*mw/(5*MQ)))
      write(nio,1040)'2years cycle',
     $        phaseDs(INT(2*mw/(2*MQ))),FdelaySA(INT(2*mw/(2*MQ))),
     $        phaseDp(INT(2*mw/(2*MQ))),FdelayP(INT(2*mw/(2*MQ)))
      CALL writTag(nio,'</tbody>')
      CALL writTag(nio,'</table></div>')
      CALL mkPOneLine(nio,'@','&nbsp;')
c ----------------------------------------------------------------------
 1010 format('<thead><tr><th rowspan="3">period of cycle',
     $       '</th><th colspan="4" scope="colgroup">',
     $       'Delay(in ',a,')</th></tr>')
 1020 format('<tr><th colspan="2" scope="colgroup">SA series</th>',
     $       '<th colspan="2" scope="colgroup">trend-cycle</th></tr>')
 1030 format('<tr>',2('<th scope="col">semi-infinite</th>',
     $        '<th scope="col">finite</th>'),'</tr></thead>')
 1040 format('<tr><th scope="row">',a,'</th>',4('<td>',F6.1,'</td>'),
     $       '</tr>')
c ----------------------------------------------------------------------
      end
c
c     ModelEst: writes the table "ARIMA MODEL FOR ESTIMATOR"
c
      subroutine ModelEst(MQ,d,bd,isCloseToTD,varwnp,Hp,lHp,Vrp,Ep,lEp,
     $              varwns,Hs,lHs,Vrs,Es,lEs,varwnc,Hc,lHc,Vrc,Ec,lEc,
     $              varwna,Ha,lHa,Vra,Ea,lEa,Qt1,Hu,lHu,Vru,Eu,lEu)
      implicit none
      real*8 diffInt
      parameter (diffInt=1.0D-6)
      include 'stream.i'
      include 'polynom.i'
      include 'models.i'
c    INPUT PARAMETERS
      logical isCloseToTD 
      integer MQ,d,bd,lHp,lEp,lHs,lEs,lHc,lEc,
     $       lHa,lEa,lHu,lEu
      real*8 varwnp,Hp(60-1),Ep(0:60-1),varwns,Hs(60-1),Es(0:60-1),
     $       varwnc,Hc(60-1),Ec(0:60-1),varwna,Ha(60-1),Ea(0:60-1),
     $       Qt1,Hu(60-1),Eu(0:60-1),Vrp,Vrs,Vrc,Vra,Vru
c    LOCAL VARIABLES
      integer i,j,cont
      real*8 eTHstar(40),ePHI(40)
      character strPol*(MaxStrLength),line*(maxLineLength),
     $          strTH*(maxStrLength),lineTH*(maxLineLength),
     $          cadEstimator*(4),nameComp*(20),cstr*(10)
      integer nAR,ARdim(maxPolDim),nMA,MAdim(maxPolDim),il,iL2,ipos
      real*8 AR(maxPol,maxPolDim),MA(maxPol,maxPolDim)
      integer istrlen
      external istrlen
c -----------------------------------------------------------------------
c  Initialize
c -----------------------------------------------------------------------
      DO i=1,maxPolDim
       ARdim(i)=0
       MAdim(i)=0
       DO j=1,maxPol
        AR(j,i)=0D0
        MA(j,i)=0D0
       END DO
      END DO
c -----------------------------------------------------------------------
      CALL genSkip(1065)
      CALL writTagOneLine(Nio,'h3','@','ARIMA MODEL FOR ESTIMATORS')
      CALL writTag(Nio,'<ul>')
      CALL writTagOneLine(Nio,'li','@',
     $                 'Innovation are these in observed series (a(t))')
      CALL writTag(Nio,'</ul>')
      eTHstar(1)=1.0d0;
      do i=1,qstar0
        eTHstar(i+1)=-THstr0(i);
      enddo
      call strPolyn('F    ',eTHstar,qstar0+1,diffInt,strTH,lineTH)
      cont=0
c   SA Estimator
      cont=cont+1
      ipos=1
      CALL itoc(cont,cstr,ipos)
      CALL writTagOneLine(Nio,'h4','@',cstr(1:(ipos-1))//
     &                    '. SA SERIES [n(t)]')
      nAR=0
      nMA=0
      call AddBJpols(MA,MAdim,nMA,thadj,nthadj)
      call AddBJpols(MA,MAdim,nMA,Psis,nPsis)
      call AddBJpols(AR,ARdim,nAR,chis,nchis)
      if (isCloseToTD) then
        call AddBJpols(MA,MAdim,nMA,Cycs,nCycs)
      else
        call AddBJpols(AR,ARdim,nAR,Cycs,nCycs)
      end if
      call tableEstM(MQ,strTH,lineTH,AR,ARdim,nAR,d+bd,0,0,'N   ',
     $       thadj,nthadj,MA,MAdim,nMA,0,0,bd,varwna,Ha,lHa,Vra,Ea,lEa)
c   Estimator of Trend
      if ((d+bd).gt.0 .or. nChis.gt.0) then
        cont=cont+1 
        ipos=1
        CALL itoc(cont,cstr,ipos)
        CALL writTagOneLine(Nio,'h4','@',cstr(1:(ipos-1))//
     $       '. TREND-CYCLE COMPONENT [P(t)]')
        nAR=0
        nMA=0
        call AddBJpols(MA,MAdim,nMA,thetp,nthetp)
        call AddBJpols(MA,MAdim,nMA,Psis,nPsis)
        call AddBJpols(MA,MAdim,nMA,Cycs,nCycs)
        call AddBJpols(AR,ARdim,nAR,chis,nchis)
        call tableEstM(MQ,strTH,lineTH,AR,ARdim,nAR,d+bd,0,0,'P   ',
     $      thetp,nthetp,MA,MAdim,nMA,0,0,bd,varwnp,Hp,lHp,Vrp,Ep,lEp)
      end if
c   Estimator of Seasonal
      if (bd.gt.0 .or. nPsis.gt.0) then
        cont=cont+1 
        ipos=1
        CALL itoc(cont,cstr,ipos)
        CALL writTagOneLine(Nio,'h4','@',cstr(1:(ipos-1))//
     $       '. SEASONAL COMPONENT [S(t)]')
        nAR=0
        nMA=0
        call AddBJpols(MA,MAdim,nMA,thets,nthets)
        call AddBJpols(MA,MAdim,nMA,chis,nChis)
        call AddBJpols(MA,MAdim,nMA,Cycs,nCycs)
        call AddBJpols(AR,ARdim,nAR,Psis,nPsis)
        call tableEstM(MQ,strTH,lineTH,AR,ARdim,nAR,0,0,bd,'S   ',
     $      thets,nthets,MA,MAdim,nMA,d+bd,0,0,varwns,Hs,lHs,Vrs,Es,lEs)
      end if
c   Estimator of Transitory o TD.stochastic
      if ((nthetc.gt.0) .or. (ncycs.gt.0)) then
        cont=cont+1 
        ipos=1
        CALL itoc(cont,cstr,ipos)
        if (isCloseToTD) then
          CadEstimator='TDs'
          NameComp='TD.stochastic'
        else
          CadEstimator='C'
          NameComp='TRANSITORY'
        end if
        il=ISTRLEN(NameComp)
        iL2=ISTRLEN(CadEstimator)
        CALL writTagOneLine(Nio,'h4','@',cstr(1:(ipos-1))//
     $       '. '//NameComp(1:il)//' ['//CadEstimator(1:il2)//'(t)]')
        nAR=0
        nMA=0
        call AddBJpols(MA,MAdim,nMA,thetc,nthetc)
        call AddBJpols(MA,MAdim,nMA,chis,nChis)
        call AddBJpols(MA,MAdim,nMA,Psis,nPsis)
        call AddBJpols(AR,ARdim,nAR,Cycs,nCycs)
        call tableEstM(MQ,strTH,lineTH,AR,ARdim,nAR,0,0,0,
     $                cadEstimator,thetc,nthetc,
     $                MA,MAdim,nMA,d,bd,0,varwnc,Hc,lHc,Vrc,Ec,lEc)
      end if
c   Irregular Estimator
      if (qt1.ne.0.d0) then
       cont=cont+1 
       ipos=1
       CALL itoc(cont,cstr,ipos)
       CALL writTagOneLine(Nio,'h4','@',cstr(1:(ipos-1))//
     $     '. IRREGULAR COMPONENT [U(t)]')
       nAR=0
       nMA=0
       call AddBJpols(MA,MAdim,nMA,chis,nChis)
       call AddBJpols(MA,MAdim,nMA,Psis,nPsis)
       call AddBJpols(MA,MAdim,nMA,Cycs,nCycs)
       call tableEstM(MQ,strTH,lineTH,AR,ARdim,nAR,0,0,0,'U   ',
     $       thetc,0,MA,MAdim,nMA,d,bd,0,Qt1,Hu,lHu,Vru,Eu,lEu)
      end if
      end subroutine
c
c   TableEstM: escribe los modelos de cada ARIMA MODEL ESTIMATOR
c
      subroutine tableEstM(MQ,strTH,lineTH,AR,ARdim,nAR,dc,bdc,NSc,
     $                CadEstimator,THc,lTHc,MA,MAdim,nMA,dnc,bdnc,NSnc,
     $                Vc,Hc,lHc,Vrc,Ec,lEc)
      implicit none
      real*8 diffInt 
      parameter (diffInt=1.0D-6)
      include 'stream.i'
      include 'polynom.i'
c   INPUT PARAMETERS
      integer MQ,ARdim(MaxPolDim),nAR,MAdim(MaxPolDim),nMA,dc,BDc,NSc,
     $        lTHc,Dnc,BDnc,NSnc,lHc,lEc
      real*8 AR(maxPol,MaxPolDim),MA(maxPol,maxPolDim),Vc,Hc(60-1),
     $       Ec(0:60-1),THc(*),Vrc
      character strTH*(maxStrLength),lineTH*(maxLineLength),
     $          CadEstimator*(4)
c   LOCAL PARAMETERS
      character strPol*(maxStrLength),line*(maxLineLength),
     $          strPHI*(maxStrlength),linePHI*(maxLineLength),
     $          strTmp*(maxStrLength),lineTmp*(maxLineLength)
      integer i,il,dummInt
      real*8 ePol(maxPolDim),Kcc,rRoots(60),iRoots(60),
     $      mRoots(60),arRoots(60),pRoots(60)
      integer IstrLen
      external IstrLen
c --------------------------------------------------------------------
      call getStrPols('B    ',AR,ARdim,nAR,Dc,MQ,BDc,NSc,strPHI,linePHI)
      strPol=''
      line=''
      dummInt=3
      il=istrlen(CadEstimator)
      call appendStr(strPHI,linePHI,strPol,line)
      call appendStrRight(strTH,lineTH,strPol,line)
      strTmp=''
      write(linetmp,1010)
     $     CadEstimator(1:il),CadEstimator(1:il)
 1010 format(A,'(t)=K<sub>',A,'</sub>') 
      call AppendStr(strTmp,lineTmp,strPol,line)
      if (lTHc.gt.0) then
        ePol(1)=1.0d0
        do i=1,lTHc
          ePol(i+1)=-THc(i)
        enddo
        call strPolyn('B    ',ePol,lTHc+1,diffInt,strTmp,lineTmp)
        call appendStr(strTmp,lineTmp,strPol,line)
      end if
      call getStrPols('F    ',MA,MAdim,nMA,Dnc,MQ,BDnc,NSnc,strTmp,
     &                LineTmp)
      call AppendStr(strtmp,lineTmp,strPol,line)
      strTmp=''
      lineTmp='a(t)'
      call appendStr(strTmp,lineTmp,strPol,line)
      call appendLine(strPol,line)
      CALL writTagOneLine(nio,'h5','@','(1) HISTORICAL ESTIMATOR')
      CALL mkPOneLine(nio,'@',strPol(1:istrlen(strPol)))
      write(nio,1020) 'K',CadEstimator(1:il),Vc
 1020 format('<p>',a,'<sub>',A,'</sub>= ',F9.6,'</p>')
      kcc=Ec(0)*Vc;
      ePol(1)=1.0d0
      do i=1,lEc
        ePol(i+1)=Ec(i)/Ec(0)
      enddo
      nMA=0
      call AddPols(MA,MAdim,nMA,ePol,lEc+1)
      strPol=''
      write(line,1030) CadEstimator(1:il),CadEstimator(1:il)
 1030 format(A,'(t|t)=Kc<sub>',A,'</sub>')
      call getStrPols('B    ',MA,MAdim,nMA,0,MQ,0,0,strtmp,lineTmp)
      call AppendStr(strTmp,LineTmp,strPol,line)
      strTmp=''
      lineTmp='a(t)'
      call AppendStr(strTmp,lineTmp,strPol,line)
      call AppendStrRight(strPHI,linePHI,strPol,line)
      call AppendLine(strPol,line)
      CALL writTagOneLine(nio,'h5','@','(2) CONCURRENT ESTIMATOR['//
     $                    CadEstimator(1:il)//'(t|t)]')
      CALL mkPOneLine(nio,'@',strPol(1:istrlen(strPol)))
      write(nio,1020) 'Kc',CadEstimator(1:il),Kcc
      call RPQ(ePol,lEc+1,Rroots,iRoots,mRoots,arRoots,pRoots,1,dummInt)
      if (lEc.gt.0) then
        call showRoots(Rroots,iRoots,mRoots,arRoots,lEc,
     $                 'MA ROOTS of concurrent estimator',
     $                 '<abbr title="moving average">MA</abbr> '//
     $                 'ROOTS of concurrent estimator',
     $                 'ma.roots.'//CadEstimator(1:il))
      end if
      strPol=''
      write(line,1040) CadEstimator(1:il)
 1040 format('R(t|t)=Kr<sub>',A,'</sub> F')
      call AppendStrRight(strTH,lineTH,strPol,line)
      if (lHc.gt.0) then
        nMA=0
        call AddBJPols(MA,MAdim,nMA,Hc,lHc)
        call getStrPols('F    ',MA,MAdim,nMA,0,MQ,0,0,StrTmp,lineTmp)
        call AppendStr(strTmp,lineTmp,strPol,line)
      end if
      strTmp=''
      lineTmp='a(t)'
      call AppendStr(strTmp,lineTmp,strPol,line)
      call AppendLine(strPol,line)
      CALL writTagOneLine(nio,'h5','@',
     $                  '(3) REVISION IN CONCURRENT ESTIMATOR [R(t|t)]')
      CALL mkPOneLine(nio,'@',strPol(1:istrlen(strPol)))
      write(nio,1020) 'Kr',CadEstimator(1:il),Vrc
      end subroutine
c
c
c
      subroutine showRoots(Rroots,iRoots,mRoots,arRoots,nRoots,
     $                     Summ,Caption,thisId)
      implicit none
      character blan*4,two*4
      real*8 tol,pi
      parameter (tol=1.0d-5,pi =3.14159265358979d0,blan=' -  ',
     &           two='2.0 ')
      include 'stream.i'
*      include 'indhtml.i'
c  INPUT PARAMETERS
      real*8 rRoots(60),iRoots(60),mRoots(60)
      integer nRoots
      character summ*(*),caption*(*),thisId*(*)
c  INPUT/OUTPUT
c    arRoots(In radians)=>arRoots(In degrees)
      real*8  arRoots(60)
c  LOCAL PARAMETERS
      real*8  pRoots(60)
      character per(64)*4
      integer i,lenCaption,lenSumm,contR
      integer istrlen
      external istrlen
c--------------------------------------------------------------------   
      do i = 1,nroots
       arRoots(i)=arRoots(i)*pi/180.0d0
       if ((iRoots(i).gt.tol) .or. (iRoots(i).lt.-tol)) then
        pRoots(i) = 2.0d0 * pi / arRoots(i)
        per(i)=blan
       else
        pRoots(i) = 999.99
        per(i) = blan
        if (rRoots(i) .lt. 0.0d0) then
         per(i) = two
        end if
       end if
       arRoots(i) = 180.0d0 * arRoots(i) / pi
      end do
      lenSumm=istrlen(Summ)
      lenCaption=istrlen(Caption)
 6000 format ('<tr><td class="head">&nbsp;</td>',
     $        5('<th scope="col">',a,'</th>'),'</tr>')
      CALL makDivId(nio,thisId,'@')
      CALL mkTableTag(nio,'w70',summ(1:lenSumm))
      CALL mkCaption(nio,caption(1:lenCaption))
*        itab=itab+1
      write (Nio,6000)'REAL PART','IMAGINARY PART','MODULUS',
     $                'ARGUMENT','PERIOD'
      contR=0
      do i=1,nroots
         if (iRoots(i) .ge. -tol) then 
           contR=contR+1 
           if (ABS(pRoots(i)-999.99) .lt. 1.d-12) then
 6001        format ('<tr><th scope="row">root ',i2,'</th>',/,
     $               4('<td class="center">',f11.3,'</td>'),/,
     $               '<td class="center">',a4,'</td></tr>')
             write(Nio,6001) contR,rRoots(i),iRoots(i),mRoots(i),
     $             arRoots(i),per(i)
           else
 6002        format ('<tr><th scope="row">root ',i2,'</th>',/,
     $               5('<td class="center">',f11.3,'</td>'),/,'</tr>')
             write(Nio,6002) contR,rRoots(i),iRoots(i),mRoots(i),
     $             arRoots(i),pRoots(i)
           end if
         end if
      end do
      CALL writTag(Nio,'</table></div>')
      CALL mkPOneLine(Nio,'@','&nbsp;')
      end subroutine

      SUBROUTINE mkHrTable(Nio,vhr,nhr,thisCapCode)
      implicit none
      character thisStyle*(3)
      real*8 vhr(*)
      integer thisCapCode,Nio,nhr,i

      if (nhr.eq.0) RETURN

      if (nhr.gt.8) then
       thisStyle='w80'
      else if (nhr.gt.4) then
       thisStyle='w60'
      else if (nhr.eq.1) then
       thisStyle='w20'
      else
       thisStyle='w40'
      end if
      IF (thisCapCode.eq.1) THEN
        CALL mkTableTag(Nio,thisStyle,'Harmonic function F(X)')
        CALL mkCaption(Nio,'F(X)')
      ELSE IF (thisCapCode.eq.2) THEN
        CALL mkTableTag(Nio,thisStyle,'Harmonic function T(X)')
        CALL mkCaption(Nio,'T(X)')
      ELSE IF (thisCapCode.eq.3) THEN
        CALL mkTableTag(Nio,thisStyle,'Harmonic function C(X)')
        CALL mkCaption(Nio,'C(X)')
      ELSE IF (thisCapCode.eq.4) THEN
        CALL mkTableTag(Nio,thisStyle,'Harmonic function S(X)')
        CALL mkCaption(Nio,'S(X)')
      ELSE IF (thisCapCode.eq.5) THEN
        CALL mkTableTag(Nio,thisStyle,
     &                  'N(X), FORMED FROM THE PRODUCT T(S)C(X)')
        CALL mkCaption(Nio,'N(X), FORMED FROM THE PRODUCT T(S)C(X)')
      ELSE IF (thisCapCode.eq.6) THEN
        CALL mkTableTag(Nio,thisStyle,
     $          'H(X), FORMED FROM THE PRODUCT T(X)C(X)S(X) = N(X)S(X)')
        CALL mkCaption(Nio,
     $          'H(X), FORMED FROM THE PRODUCT T(X)C(X)S(X) = N(X)S(X)')
      ELSE IF (thisCapCode.eq.7) THEN
        CALL mkTableTag(Nio,thisStyle,'Harmonic function U(x)')
        CALL mkCaption(Nio,'U(X)')
      ELSE IF (thisCapCode.eq.8) THEN
        CALL mkTableTag(Nio,thisStyle,'Harmonic function V(x)')
        CALL mkCaption(Nio,'V(x)')
      ELSE IF (thisCapCode.eq.9) THEN
        CALL mkTableTag(Nio,thisStyle,        
     $                  'DUM(X) = RT(X) - U(X)S(X) - V(X)N(X).'//
     $                  ' THIS SHOULD BE ZERO')
        CALL mkCaption(Nio,'DUM(X) = RT(X) - U(X)S(X) - V(X)N(X).'//
     $                 ' THIS SHOULD BE ZERO')
      ELSE IF (thisCapCode.eq.10) THEN
        CALL mkTableTag(Nio,thisStyle,'Harmonic function UT(x)')
        CALL mkCaption(Nio,'UT(X)')
      ELSE IF (thisCapCode.eq.11) THEN
        CALL mkTableTag(Nio,thisStyle,'Harmonic function UC(x)')
        CALL mkCaption(Nio,'UC(x)')
      ELSE IF (thisCapCode.eq.12) THEN
        CALL mkTableTag(Nio,thisStyle,        
     $                  'DUM(X) = U(X) - UT(X)C(X) - UC(X)T(X).'//
     $                  ' THIS SHOULD BE ZERO')
        CALL mkCaption(Nio,'DUM(X) = U(X) - UT(X)C(X) - UC(X)T(X).'//
     $                 ' THIS SHOULD BE ZERO')
      ELSE IF (thisCapCode.eq.13) THEN
        CALL mkTableTag(Nio,thisStyle,        
     $                  'DUM(X) = RT(X) - V(X)C(X) - UC(X)S(X).'//
     $                  ' THIS SHOULD BE ZERO')
        CALL mkCaption(Nio,'DUM(X) = RT(X) - V(X)C(X) - UC(X)S(X).'//
     $                 ' THIS SHOULD BE ZERO')
      ELSE IF (thisCapCode.eq.14) THEN
        CALL mkTableTag(Nio,thisStyle,        
     $                  'DUM(X) = RT(X) - UT(X)S(X) - V(X)T(X).'//
     $                  ' THIS SHOULD BE ZERO')
        CALL mkCaption(Nio,'DUM(X) = RT(X) - UT(X)S(X) - V(X)T(X).'//
     $                 ' THIS SHOULD BE ZERO')
      ELSE IF (thisCapCode.eq.15) THEN
        CALL mkTableTag(Nio,thisStyle,        
     &                  'DUM(X) =  F(X) - V(X)T(X)C(X) - '//
     &                  'UT(X)S(X)C(X) - UC(X)S(X)T(X).  '//
     &                  'THIS SHOULD BE ZERO')
        CALL mkCaption(Nio,'DUM(X) =  F(X) - V(X)T(X)C(X) - '//
     &                 'UT(X)S(X)C(X) - UC(X)S(X)T(X).  '//
     &                 'THIS SHOULD BE ZERO')
      end if
      CALL writTag(Nio,'<tr>')
      DO i=1,nhr
         write (Nio,7003) vhr(i)
      END DO
      CALL writTag(Nio,'</tr>')
      CALL writTag(Nio,'</table>')
      CALL mkPOneLine(Nio,'@','&nbsp;')
 7003 format ('<td class="center">',f11.4,'</td>')

      end subroutine

      subroutine mkTable56Row(Nio, rowLabel, nlastp, nlasty, Mq,
     &                        qx1, qx1se, qx1tse, qx2, qx2se, qx2tse)
      implicit none
      character rowLabel*(*)
      character mth(12)*4,amth(12)*9,srt(8)*4,asrt(8)*7
      real*8 qx1, qx1se, qx1tse, qx2, qx2se, qx2tse
      integer Nio,nlastp, nlasty, ipos, Mq
c ----------------------------------------------------------------------
      integer ISTRLEN
      external ISTRLEN
c ----------------------------------------------------------------------
      data mth/
     $     'JAN ','FEB ','MAR ','APR ','MAY ','JUN','JUL','AUG ','SEP',
     $     'OCT ','NOV ','DEC '/
      DATA amth/'January  ','February ','March    ','April    ',
     &          '@        ','June     ','July     ','August   ',
     &          'September','October  ','November ','December '/
      data srt/'1ST','2ND','3RD','4TH','5TH','6TH','7TH','8TH'/
      data asrt/'first  ','second ','third  ','fourth ',
     &          'fifth  ','sixth  ','seventh','eighth '/
c ----------------------------------------------------------------------
      if (Mq.eq.12) then
            if(nlastp.eq.5)then
             write(nio,1170)rowLabel,
     $            mth(nlastp), nlasty, qx1, qx1se, qx1tse
            else
             ipos=istrlen(amth(nlastp))
             write(nio,1171)rowLabel, amth(nlastp)(1:ipos), 
     $            mth(nlastp), nlasty, qx1, qx1se, qx1tse
            end if
            if (nlastp .eq. 6) then
             write (nio,1180)
     $             mth(nlastp-1), nlasty, qx2, qx2se, qx2tse
            else if (nlastp .gt. 1) then
             ipos=istrlen(amth(nlastp-1))
             write (nio,1181)amth(nlastp-1)(1:ipos), 
     $             mth(nlastp-1), nlasty, qx2, qx2se, qx2tse
            else
             ipos=istrlen(amth(mq))
             write (nio,1181) amth(mq)(1:ipos), 
     $             mth(mq), nlasty, qx2, qx2se, qx2tse
            end if
      else
            ipos=istrlen(asrt(nlastp))
            write(nio,1171) rowLabel, asrt(nlastp)(1:ipos),
     $            srt(nlastp), nlasty, qx1, qx1se, qx1tse
            if (nlastp .gt. 1) then
             ipos=istrlen(asrt(nlastp-1))
             write (nio,1181) asrt(nlastp-1)(1:ipos),
     $             srt(nlastp-1), nlasty, qx2, qx2se, qx2tse
            else
             ipos=istrlen(asrt(mq))
             write (nio,1181) amth(mq)(1:ipos), 
     $             srt(mq), nlasty, qx2, qx2se, qx2tse
            end if
      end if
 1170 FORMAT('<tr><th rowspan="2" scope="rowgroup">',a,'</th>',/,
     &       '<th scope="row">',a3,'-',i4,'</th>',/,
     $       3('<td>',g11.3,'</td>'),/,'</tr>') 
 1171 FORMAT('<tr><th rowspan="2" scope="rowgroup">',a,'</th>',/,
     $       '<th scope="row"><abbr title="',a,'">',a3,'</abbr>-',
     $       i4,'</th>',/,
     $       3('<td class="center">',g11.3,'</td>'),/,'</tr>') 
 1180 FORMAT('<tr><th scope="row">',a3,'-',i4,'</th>',/,
     $       3('<td class="center">',g11.3,'</td>'),/,'</tr>') 
 1181 FORMAT('<tr><th scope="row"><abbr title="',a,'">',a3,'</abbr>-',
     $       i4,'</th>',/,
     $       3('<td class="center">',g11.3,'</td>'),/,'</tr>') 
      return
      end subroutine

      subroutine genTableTag(Nio,sSummary,nCol,Prtsum)
      implicit none
      integer Nio,nCol
      logical Prtsum
      character sSummary*(*)

      if (nCol.le.3) then
       CALL mkTableTag(Nio,'w30',sSummary)
      else if (nCol.ge.9) then
       CALL mkTableTag(Nio,'w90',sSummary)
      else
       CALL mkTableTag(Nio,'w60',sSummary)
      end if
      IF(Prtsum)CALL mkCaption(Nio,sSummary)

      return
      end

      subroutine OutARIMAva0(Nio,init,p,bp,wm,PHI,BPHI)
      implicit none
      integer n10,n1
      parameter (n10=10,n1=1)
      include 'srslen.prm'
      include 'dimensions.i'
c
c.. INPUT PARAMETERS
      integer Nio,init,p,bp,q
      real*8 wm
      real*8 PHI(3*N1),BPHI(3*N1)
C ..INPUT/OUTPUT
*      include 'indhtml.i'
c
c.. Local parameters
      integer i
c
      if (Init .eq. 2) then
        CALL mkTableTag(Nio,'w40','Mean')
        write(Nio,1010) wm
        CALL writTag(Nio,'</table>')
        CALL mkPOneLine(Nio,'@','&nbsp;')
      end if
c tabla AR         
      if ((p.gt.0) .or. (bp.gt.0)) then  
        CALL mkTableTag(Nio,'w60','AR PARAMETERS')
        CALL mkCaption(Nio,'<abbr title="autoregressive">AR</abbr> '//
     $                     'PARAMETERS')
        CALL writTag(Nio,'<tr>')
        if (p.gt.0) THEN
         do i=1,p
           write(nio,1020)'PHI',i
         end do
        END IF
        if (bp.eq.1) write(nio,1020)'BPHI',1
        CALL writTag(Nio,'</tr>')
        CALL writTag(Nio,'<tr>')
        if (p.gt.0) THEN
         do i=1,p
          write(Nio,1030) -Phi(i)
         end do
        end if
        if (Bp .gt. 0) write (nio,1030) -Bphi(1)
        CALL writTag(Nio,'</tr>')
c     revisa este cierre
        CALL writTag(Nio,'</table>')
        CALL mkPOneLine(Nio,'@','&nbsp;')
      end if
C-----------------------------------------------------------------------
 1010 FORMAT('<tr><th scope="row">MEAN =</th><td class="center">',g16.6,
     $       '</td></tr>')
 1020 FORMAT('<th scope="col">',a,'(',i1,')</th>')
 1030 FORMAT('<td class="center">',f10.4,'</td>')
C-----------------------------------------------------------------------
      return
      end
C-----------------------------------------------------------------------
      subroutine OutQS(Nio,Iagr,QSori,QSori2,QSrsd,QSsadj,QSsadj2,QSirr,
     &                 QSirr2,chdr,nhdr,LpOri,Lplog,Lqchk)
      implicit none
C-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'htmlout.cmn'
C-----------------------------------------------------------------------
      CHARACTER chdr*(30)
      INTEGER Nio,Iagr,nhdr
      DOUBLE PRECISION QSori,QSori2,QSrsd,QSsadj,QSsadj2,QSirr,QSirr2
      LOGICAL LpOri,Lplog,Lqchk
C-----------------------------------------------------------------------
      LOGICAL dpeq
      DOUBLE PRECISION chisq
      EXTERNAL dpeq,chisq
c-----------------------------------------------------------------------
      IF(Lqchk)THEN
       CALL mkTableTag(Nio,'w60',
     &                 'QS Statistics for (quarterly) seasonality '//
     &                 chdr(1:nhdr))
       CALL mkCaption(Nio,'QS statistic for (quarterly) seasonality '//
     &                chdr(1:nhdr))
      ELSE
       CALL mkTableTag(Nio,'w60','QS Statistics for seasonality '//
     &                 chdr(1:nhdr))
       CALL mkCaption(Nio,'QS statistic for seasonality '//
     &                chdr(1:nhdr))
      END IF
      CALL writTag(Nio,'<tr>')
      CALL writTagOneLine(Nio,'td','head','&nbsp;')
      CALL mkHeaderCellScope(Nio,0,0,'col','QS seasonality diagnostic',
     &                        'QS')
      CALL mkHeaderCellScope(Nio,0,0,'col','@','p-value')
      CALL writTag(Nio,'</tr>')
      IF(LpOri)THEN
       IF(.not.dpeq(QSori,DNOTST))THEN
        IF(Lplog)THEN
         WRITE(Nio,1020)'log(Original Series)',QSori,chisq(QSori,2)
        ELSE
         WRITE(Nio,1020)'Original Series',QSori,chisq(QSori,2)
        END IF
       END IF
       IF(.not.dpeq(QSori2,DNOTST))THEN
        IF(Lplog)THEN
         WRITE(Nio,1020)'log(Original Series (extreme value adjusted))',
     &                   QSori2,chisq(QSori2,2)
        ELSE
         WRITE(Nio,1020)'Original Series (extreme value adjusted)',
     &                   QSori2,chisq(QSori2,2)
        END IF
       END IF
       IF(.not.dpeq(QSrsd,DNOTST))
     &     WRITE(Nio,1020)'Residuals',QSrsd,chisq(QSrsd,2)
      END IF
      IF(.not.dpeq(QSsadj,DNOTST))THEN
       IF(Lplog)THEN
        IF(Iagr.eq.4)THEN
         WRITE(Nio,1020)'log(Indirect Seasonally Adjusted Series)',
     &                  QSsadj,chisq(QSsadj,2)
        ELSE
         WRITE(Nio,1020)'log(Seasonally Adjusted Series)',QSsadj,
     &                  chisq(QSsadj,2)
        END IF
       ELSE
        IF(Iagr.eq.4)THEN
         WRITE(Nio,1020)'Indirect Seasonally Adjusted Series',QSsadj,
     &                  chisq(QSsadj,2)
        ELSE
         WRITE(Nio,1020)'Seasonally Adjusted Series',QSsadj,
     &                  chisq(QSsadj,2)
        END IF
       END IF
      END IF
      IF(.not.dpeq(QSsadj2,DNOTST))THEN
       IF(Lplog)THEN
        IF(Iagr.eq.4)THEN
         WRITE(Nio,1020)'log(Indirect Seasonally Adjusted Series '//
     &                  '(extreme value adjusted))',QSsadj2,
     &                  chisq(QSsadj2,2)
        ELSE
         WRITE(Nio,1020)'log(Seasonally Adjusted Series '//
     &                  '(extreme value adjusted))',QSsadj2,
     &                  chisq(QSsadj2,2)
        END IF
       ELSE
        IF(Iagr.eq.4)THEN
         WRITE(Nio,1020)'Indirect Seasonally Adjusted Series '//
     &                  '(extreme value adjusted)',QSsadj2,
     &                  chisq(QSsadj2,2)
        ELSE
         WRITE(Nio,1020)'Seasonally Adjusted Series (extreme value '//
     &                  'adjusted)',QSsadj2,chisq(QSsadj2,2)
        END IF
       END IF
      END IF
      IF(.not.dpeq(QSirr,DNOTST))THEN
        IF(Iagr.eq.4)THEN
         WRITE(Nio,1020)'Indirect Irregular Series',QSirr,chisq(QSirr,2)
        ELSE
         WRITE(Nio,1020)'Irregular Series',QSirr,chisq(QSirr,2)
        END IF
      END IF
      IF(.not.dpeq(QSirr2,DNOTST))THEN
        IF(Iagr.eq.4)THEN
         WRITE(Nio,1020)'Indirect Irregular Series (extreme value '//
     &                  'adjusted) ',QSirr2,chisq(QSirr2,2)
        ELSE
         WRITE(Nio,1020)'Irregular Series (extreme value adjusted)',
     &                  QSirr2,chisq(QSirr2,2)
        END IF
      END IF
      CALL writTag(Nio,'</table>')
      CALL mkPOneLine(Nio,'@','&nbsp;')
c-----------------------------------------------------------------------
 1020 FORMAT('<tr><th scope="row">',a,'</th><td>',f16.2,'</td><td>',
     &       f10.4,'</td></tr>')
C-----------------------------------------------------------------------
      return
      end
C-----------------------------------------------------------------------
      subroutine OutNP(Nio,Iagr,NPsadj,NPsadj2,chdr,nhdr,Lplog)
      implicit none
C-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      include 'htmlout.cmn'
C-----------------------------------------------------------------------
      CHARACTER chdr*(30),str*(3)
      INTEGER Nio,Iagr,nhdr,NPsadj,NPsadj2,nchr
      LOGICAL Lplog
c-----------------------------------------------------------------------
      CHARACTER YSNDIC*5
      INTEGER ysnptr,PYSN
      PARAMETER(PYSN=2)
      DIMENSION ysnptr(0:PYSN)
      PARAMETER(YSNDIC='noyes')
c-----------------------------------------------------------------------
      DATA ysnptr/1,3,6/
c-----------------------------------------------------------------------
      CALL mkTableTag(Nio,'w60',
     &                'NP Statistic for residual seasonality '//
     &                chdr(1:nhdr))
      CALL mkCaption(Nio,'NP statistic for residual seasonality '//
     &                chdr(1:nhdr))
      CALL writTag(Nio,'<tr>')
      CALL writTagOneLine(Nio,'td','head','&nbsp;')
      CALL mkHeaderCellScope(Nio,0,0,'col','@','Residual Seasonality?')
      CALL writTag(Nio,'</tr>')
c-----------------------------------------------------------------------
      IF(.not.(NPsadj.eq.NOTSET))THEN
       CALL getstr(YSNDIC,ysnptr,PYSN,NPsadj+1,str,nchr)
       IF(Lplog)THEN
        IF(Iagr.eq.4)THEN
         WRITE(Nio,1020)'log(Indirect Seasonally Adjusted Series)',
     &                  str(1:nchr)
        ELSE
         WRITE(Nio,1020)'log(Seasonally Adjusted Series)',str(1:nchr)
        END IF
       ELSE
        IF(Iagr.eq.4)THEN
         WRITE(Nio,1020)'Indirect Seasonally Adjusted Series',
     &                  str(1:nchr)
        ELSE
         WRITE(Nio,1020)'Seasonally Adjusted Series',str(1:nchr)
        END IF
       END IF
      END IF
      IF(.not.(NPsadj2.eq.NOTSET))THEN
       CALL getstr(YSNDIC,ysnptr,PYSN,NPsadj2+1,str,nchr)
       IF(Lplog)THEN
        IF(Iagr.eq.4)THEN
         WRITE(Nio,1020)'log(Indirect Seasonally Adjusted Series '//
     &                  Cbr//'(extreme value adjusted))',str(1:nchr)
        ELSE
         WRITE(Nio,1020)'log(Seasonally Adjusted Series '//
     &                  Cbr//'(extreme value adjusted))',str(1:nchr)
        END IF
       ELSE
        IF(Iagr.eq.4)THEN
         WRITE(Nio,1020)'Indirect Seasonally Adjusted Series '//
     &                  '(extreme value adjusted)',str(1:nchr)
        ELSE
         WRITE(Nio,1020)'Seasonally Adjusted Series (extreme value '//
     &                  'adjusted)',str(1:nchr)
        END IF
       END IF
      END IF
      CALL writTag(Nio,'</table></div>')
      CALL mkPOneLine(Nio,'@','&nbsp;')
c-----------------------------------------------------------------------
 1020 FORMAT('<tr><th scope="row">',a,'</th><td class="center">',a,
     &       '</td></tr>')
C-----------------------------------------------------------------------
      return
      end
      
