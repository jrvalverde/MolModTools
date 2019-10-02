
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fillrt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fillrt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 67 "fillrt.web"
      subroutine fillrt(IFUNC,VAL,NOP)
      implicit none
      integer altdir,i,icard,IFUNC,In,Iout,ipr,Ipunch,irwlnk,j,Jop,jump,
     &Ll,Lnk,lrwlnk,Nchain,Nlink,NOP,ovrlay,Pad
      integer spec,VAL
      save
      common/io/In,Iout,Ipunch
      common/tmprte/Nchain,Ll,Nlink,Pad,Lnk(200),Jop(50,50)
      data irwlnk/999/,lrwlnk/1352/
      
      if(IFUNC.EQ.0)then
      
      icard=0
      Nlink=0
      do 50 i=1,200
      Lnk(i)=0
50    continue
      do 100 i=1,50
      do 60 j=1,50
      Jop(j,i)=0
60    continue
100   continue
      ovrlay=0
      altdir=0
      jump=0
      return
      
      elseif(IFUNC.EQ.2)then
      
      altdir=0
      icard=icard+1
      ovrlay=VAL
      if(jump.EQ.0)return
      Nlink=Nlink+1
      Lnk(Nlink)=jump
      jump=0
      return
      elseif(IFUNC.EQ.3)then
      
      Jop(NOP,icard)=VAL
      return
      elseif(IFUNC.EQ.4)then
      
      spec=VAL+ovrlay*100+icard*10000+altdir*1000000
      Nlink=Nlink+1
      Lnk(Nlink)=spec
      return
      elseif(IFUNC.EQ.5)then
      
      jump=VAL
      return
      elseif(IFUNC.EQ.6)then
      
      if(jump.NE.0)then
      Nlink=Nlink+1
      Lnk(Nlink)=jump
      endif
      else
      
      altdir=VAL
      return
      endif
      Nlink=Nlink+1
      Lnk(Nlink)=99*100+(icard+1)*10000+99
      Nlink=Nlink-1
      if(VAL.EQ.0)Nlink=0
      Nchain=1
      call twrite(irwlnk,Nchain,lrwlnk,1,lrwlnk,1,0)
      call ilsw(2,21,ipr)
      if(ipr.EQ.0)call prtrte(Iout,Lnk,Jop)
      return
      
      end
C* :1 * 
      
