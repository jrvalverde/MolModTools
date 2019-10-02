
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 bdump"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "bdump.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "bdump.web"
      subroutine bdump(MODE)
      implicit none
      double precision A,C1,C2,C3,C4,Exx,X,Y,Z
      integer i,Ia,ii,In,Iout,Ipunch,Jan,jj,k,l1,l2,l2m1,MAXPRM,MAXS21,M
     &AXSH1,MAXSHL,Maxtyp,MODE,Nshell
      integer Shella,Shelln,Shellt,Shellc,Aos,Aon,Shladf
      character*6 nw,nw15
      dimension nw(14)
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      dimension A(MAXSHL,15)
      dimension Ia(MAXSHL,7)
      dimension C4(MAXSHL),Shladf(MAXSHL)
      equivalence(C4(1),C3(MAXSH1)),(Shladf(1),C3(MAXS21))
      common/io/In,Iout,Ipunch
      equivalence(A(1,1),Exx(1))
      equivalence(Ia(1,1),Jan(1))
      data nw/6HEXX   ,6HC1    ,6HC2    ,6HC3+C4 ,6HX     ,6HY     ,6HZ 
     &    ,6HJAN   ,6HSHELLA,6HSHELLN,6HSHELLT,6HSHELLC,6HAOS   ,6HAON  
     & /
      data nw15/6HSHLADF/
      data l1/7/,l2/7/
      
      
      
      
      
      
99001 format(//1x,a6)
99002 format(8H0NSHELL=,i6,10x,7HMAXTYP=,i6/)
99003 format(7(i4,e13.7))
99004 format(10(2H (,i2,i4,1H)))
99005 format(10(2H (,i2,a6,1H)))
      
      if(MODE.NE.0)then
      if(MODE.NE.2)goto 200
      endif
      jj=-2
      do 100 i=1,l1
      if(i.LT.5)then
      jj=jj+3
      k=240
      elseif(i.EQ.5)then
      
      jj=jj+3
      k=80
      else
      
      jj=jj+1
      k=80
      endif
      if(i.EQ.4)k=160
      write(Iout,99001)nw(i)
      write(Iout,99003)(ii,A(ii,jj),ii=1,k)
100   continue
200   if(MODE.NE.0)then
      jj=0
      l2m1=l2-1
      do 250 i=1,l2m1
      jj=jj+1
      write(Iout,99001)nw(i+7)
      write(Iout,99004)(ii,Ia(ii,jj),ii=1,80)
250   continue
      write(Iout,99001)nw(14)
      write(Iout,99005)(i,Aon(i),i=1,80)
      write(Iout,99001)nw15
      write(Iout,99004)(i,Shladf(i),i=1,MAXSHL)
      write(Iout,99002)Nshell,Maxtyp
      endif
      return
      
      end
C* :1 * 
      
