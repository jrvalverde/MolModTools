
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 nextov"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "nextov.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "nextov.web"
      integer function nextov(JUMP)
      implicit none
      integer a,fclose,fdelnk,fdelov,i,Ian,Icharg,Idum,Idump,Ilsw1,In,In
     &fo,Iop,Iout,iprint,iprt,Ipunch,irwinf,irwiop,irwlnk
      integer irwmol,irwntr,Iunits,j,Jop,JUMP,l,Ll,Lnk,lrwinf,lrwiop,lrw
     &lnk,lrwmol,lrwntr,Maxpos,Multip,Nae,Natoms,Nbasis,Nbe
      integer Nchain,Ne,newch,newcrd,newov,Nlink,Ntrsta,Nutits,offset,ol
     &dcrd,oldov,Pad
      double precision Atmchg,C
      real Twait
      logical Print,Syncs
      real Tstart,Tstop,Elapsd,cputim
      dimension a(8)
      common/iop/Iop(50)
      common/ilsw1/Ilsw1(2)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/info/Info(10)
      common/tmprte/Nchain,Ll,Nlink,Pad,Lnk(200),Jop(50,50)
      common/io/In,Iout,Ipunch
      common/fidump/Idump
      common/ntr/Twait(3,4),Ntrsta(3,4),Iunits(4),Nutits,Maxpos(4),Print
     &(4),Syncs(4),Idum
      common/clcks/Tstart,Tstop,Elapsd
      data irwntr/992/,lrwntr/21/
      data irwiop/996/,lrwiop/25/
      data irwmol/997/,lrwmol/454/
      data irwinf/993/,lrwinf/5/
      data irwlnk/999/,lrwlnk/102/
      data fdelov/8/,fdelnk/7/
      data fclose/10/
      data oldov,oldcrd/0,0/
      
      
      
      
      
      
      
      
      
      
      iprint=Iop(33)
      
      call ilsw(2,20,iprt)
      
      if(iprt.EQ.0)then
      do 50 i=1,Nutits
      if(Ntrsta(2,i).NE.0)write(Iout,99001)Iunits(i),(Ntrsta(j,i),j=2,3)
     &,Maxpos(i)
50    continue
      
99001 format(' UNIT=',i2,' HAD ',i7,' QIO CALLS,',i12,' 4-B WORDS TRANSF
     &., ',i6,' RECS USED ')
      endif
      
      do 200 i=1,Nutits
      do 100 j=1,3
      Twait(j,i)=0
      Ntrsta(j,i)=0
100   continue
200   continue
      
      
      call tread(irwlnk,Nchain,lrwlnk,1,lrwlnk,1,0)
      Nchain=1
      if(Nlink.NE.0)then
      Nchain=mod(Lnk(Nlink),10000)
      oldov=mod(Lnk(Nlink),10000)/100
      oldcrd=mod(Lnk(Nlink),1000000)/10000
      endif
      
      if(iprt.EQ.0.OR.iprint.GE.2)write(Iout,99002)Nchain,Ilsw1(1)
      
99002 format(' <<<< LEAVE LINK ',i4,', STATUS = ',i12,' >>>>')
      
      call newlnk(Lnk,Nlink,JUMP)
      
      newch=mod(Lnk(Nlink),10000)
      newov=mod(Lnk(Nlink),10000)/100
      newcrd=mod(Lnk(Nlink),1000000)/10000
      
      call twrite(irwlnk,Nchain,lrwlnk,1,lrwlnk,1,0)
      
      call fileio(fdelnk,0,0,0,0)
      if(newov.NE.oldov)call fileio(fdelov,0,0,0,0)
      
      if(oldcrd.NE.newcrd)then
      offset=lrwlnk+lrwiop*(newcrd-1)
      call fileio(2,-irwlnk,lrwiop,Iop,offset)
      call twrite(irwiop,Iop,lrwiop,1,lrwiop,1,0)
      Idump=Iop(35)
      endif
      
      call twrite(irwmol,Natoms,lrwmol,1,lrwmol,1,0)
      call twrite(irwinf,Info,lrwinf,1,lrwinf,1,0)
      call twrite(irwiop,Iop,lrwiop,1,lrwiop,1,0)
      call twrite(irwntr,Twait(1,1),lrwntr,1,lrwntr,1,0)
      call fileio(fclose,l,l,l,l)
      
      nextov=newch
      return
      
      end
C* :1 * 
      
