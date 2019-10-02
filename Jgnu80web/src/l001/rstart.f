
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rstart"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rstart.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "rstart.web"
      subroutine rstart
      implicit none
      integer fclose,fopen,i,Ian,Ibfdum,ibl,ic,Icard,Icharg,Idum,Idump,I
     &lsw1,In,Info,Io,ioccur,ioilsw,Iop,Iout,Ipunch
      integer Irtcrd,irwibf,irwinf,irwiop,irwlbl,irwlnk,irwmol,irwntr,ir
     &wphy,Ismode,Istat,Ititle,Iux,Jop,jqint,l,Labl,Last,len,linkno
      integer Ll,Lnk,lprin,lrwiop,lrwjop,lrwlnk,max,Maxpos,Mode,Multip,n
     &,Nae,Natoms,Nbasis,Nbe,nch,Nchain,Ne,newcrd,Nlink
      integer Ntx,Nunits,offset,Pad,Units,Wait
      double precision C,Phycon,Atmchg
      logical Print,Syncs
      common/label/Labl(1000),Ititle(100),Irtcrd(100)
      common/ertcrd/Icard(80)
      common/io/In,Iout,Ipunch
      common/iop/Iop(50)
      common/tmprte/Nchain,Ll,Nlink,Pad,Lnk(200),Jop(50,50)
      common/phycon/Phycon(30)
      common/ilsw1/Ilsw1(2)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/info/Info(10)
      common/fidump/Idump
      common/ntr/Wait(3,4),Io(3,4),Units(4),Nunits,Maxpos(4),Print(4),Sy
     &ncs(4),Idum
      common/ibf/Ismode,Mode,Istat,Last,Ntx,Iux(5),Ibfdum(20)
      data irwlnk/999/,lrwlnk/102/,lrwjop/1352/
      data irwibf/508/
      data irwntr/992/
      data irwinf/993/
      data irwphy/994/
      data irwiop/996/,lrwiop/25/
      data irwmol/997/
      data irwlbl/502/
      data ioilsw/998/
      data fclose/10/,fopen/9/
      data max/80/
      data ibl/' '/,l/'L'/,lprin/'('/
      
      
      call fileio(fopen,l,l,l,l)
      call inicom
      call tread(irwphy,Phycon,30,1,30,1,0)
      call tread(ioilsw,Ilsw1,1,1,1,1,0)
      call tquery(irwlbl,len)
      if(len.NE.0)call tread(irwlbl,Labl,len,1,len,1,0)
      call tquery(irwmol,len)
      if(len.NE.0)call tread(irwmol,Natoms,len,1,len,1,0)
      call tquery(irwinf,len)
      if(len.NE.0)call tread(irwinf,Info,len,1,len,1,0)
      call tquery(irwntr,len)
      if(len.NE.0)call tread(irwntr,Wait(1,1),len,1,len,1,0)
      ic=8
      call tquery(irwibf,len)
      if(len.NE.0)call tread(irwibf,Ismode,len,1,len,1,0)
      if(len.NE.0)then
      if(Istat.GT.1)then
20    ic=ic+1
      if(ic.LE.max)then
      if(Icard(ic).EQ.ibl)goto 20
      if(Icard(ic).EQ.l)goto 20
      
      
      linkno=0
30    linkno=10*linkno+jqint(Icard(ic))
      ic=ic+1
      if(ic.GT.max)call lnk1e
      if(Icard(ic).EQ.ibl)then
      
      n=1
      else
      if(Icard(ic).NE.lprin)goto 30
      
      ic=ic+1
      n=jqint(Icard(ic))
      endif
      if(linkno.NE.1)then
      
      call tread(irwlnk,Nchain,lrwlnk,1,lrwlnk,1,0)
      ioccur=0
      do 35 i=1,150
      if(mod(Lnk(i),10000).EQ.linkno)then
      ioccur=ioccur+1
      if(ioccur.EQ.n)then
      Nlink=i
      call twrite(irwlnk,Nchain,101,1,101,1,0)
      goto 40
      endif
      endif
      
35    continue
      else
      
      write(Iout,99003)Iux(2)
      call idef(Iux(2),Iop(36))
      return
      endif
      endif
      
40    call tread(irwlnk,Nchain,lrwjop,1,lrwjop,1,0)
      call prtrte(Iout,Lnk,Jop)
      nch=Lnk(Nlink)
      nch=mod(nch,10000)
      newcrd=mod(Lnk(Nlink),1000000)/10000
      write(Iout,99001)nch
      if(n.GT.1)write(Iout,99002)n
      
99001 format(' RESTARTING AT LINK L',i3)
99002 format('+                       (',i2,')')
      offset=lrwlnk+lrwiop*(newcrd-1)
      call fileio(2,-irwlnk,lrwiop,Iop,offset)
      call twrite(irwiop,Iop,lrwiop,1,lrwiop,1,0)
      write(Iout,99003)Iux(2)
      
99003 format(' REOPENING INTEGRAL FILE ON UNIT NR ',i2)
      
      call idef(Iux(2),Iop(36))
      endif
      endif
      call fileio(fclose,l,l,l,l)
      Idump=Iop(35)
      call chain(nch)
      stop 11
      
      end
C* :1 * 
      
