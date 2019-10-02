
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 msprnt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "msprnt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "msprnt.web"
      subroutine msprnt(NVAR,ISTEP,NSTEP,X,XLAST,XDISPL,G,S,XNAME)
      implicit none
      double precision d,G,h,S,X,XDISPL,XLAST,XNAME
      integer i,iad1,iad2,iblank,icur,In,Iout,Ipunch,is,ISTEP,istr,itmp,
     &j,len,ncur,NSTEP,NVAR
      integer getchr
      dimension X(*),XLAST(*),XDISPL(*),G(*),XNAME(*),S(50,50)
      dimension h(50,50),is(100),iad1(50),iad2(50),d(50)
      dimension itmp(20),istr(20)
      common/io/In,Iout,Ipunch
      data iblank/1H /
      
      
      
      
      
      
99001 format(1x,'STEP NUMBER: ',i3,' OUT OF A MAXIMUM OF ',i3/1x,'ALL QU
     &ANTITIES PRINTED IN INTERNAL UNITS ','(HARTREES-BOHRS-RADIANS)')
99002 format(1H0,'VARIABLE',5x,4x,'X',5x,5x,3x,'DE/DX',2x,5x,3x,'DELTA X
     &',2x,5x,1x,'NEW X'/)
99003 format((1x,1x,8A1,1x,5x,4(f10.6,5x)))
99004 format(/1x,'THE SECOND DERIVATIVE MATRIX:'/)
99005 format(/)
      
      
      write(Iout,99001)ISTEP,NSTEP
      write(Iout,99002)
      
      ncur=0
      do 200 i=1,NVAR
      call getb(2,itmp,len,XNAME,ncur)
      do 50 j=1,8
      istr(j)=iblank
50    continue
      icur=0
      len=min0(len,8)
      do 100 j=1,len
      istr(j)=getchr(itmp,icur)
100   continue
      write(Iout,99003)(istr(j),j=1,8),XLAST(i),G(i),XDISPL(i),X(i)
200   continue
      write(Iout,99004)
      
      
      do 300 i=1,NVAR
      do 250 j=i,NVAR
      h(i,j)=S(i,j)
      h(j,i)=S(i,j)
250   continue
300   continue
      call inv(h,NVAR,is,iad1,iad2,d,50)
      call matprt(h,50,50,NVAR,NVAR,1,1,XNAME,XNAME,1,0,0)
      write(Iout,99005)
      
      return
      
      end
C* :1 * 
      
