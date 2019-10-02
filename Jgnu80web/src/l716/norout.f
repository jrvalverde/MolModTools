
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 norout"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "norout.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "norout.web"
      subroutine norout(A,EIG,MNDIM,MDIM,IAN,ISYM)
      implicit none
      double precision A,blank,EIG,t,temp
      integer i,IAN,iatom,ifin,In,ind,Iout,Ipunch,ist,ist1,ISYM,j,len,ma
     &x,max1,MDIM,min,MNDIM,n,nc
      integer ncur,ndim
      dimension A(MNDIM),EIG(MDIM)
      dimension IAN(100)
      dimension ISYM(MDIM)
      dimension temp(30),t(5)
      common/io/In,Iout,Ipunch
      data blank/1H /
      
      
      
99001 format(34H HARMONIC FREQUENCIES (CM**-1) AND,1x,29HNORMALIZED NORM
     &AL COORDINATES)
99002 format(' FREQUENCIES ----',5x,10F10.4)
99003 format(20H COORD ATOM ELEMENT )
99004 format(1x,i3,1x,i5,1x,i5,5x,10F10.5)
99005 format(18x,10I10)
      ndim=MNDIM/MDIM
      nc=5
      ncur=0
      min=1
      max=MDIM
      ifin=0
      write(Iout,99001)
100   max1=min+nc-1
      if(max.LE.max1)then
      max1=max
      ifin=1
      endif
      write(Iout,99005)(i,i=min,max1)
      
      do 200 i=1,30
      temp(i)=blank
200   continue
      ist=26
      do 300 i=min,max1
      call getb(2,t,len,ISYM,ncur)
      ist1=ist-len/2
      call putb(t,len,temp,ist1)
      ist=ist+10
300   continue
      call strout(Iout,temp,ist1,1)
      write(Iout,99002)(EIG(i),i=min,max1)
      write(Iout,99003)
      do 400 i=1,ndim
      n=max1-min+1
      iatom=(i-1)/3+1
      do 350 j=1,n
      ind=ndim*(min+j-2)+i
      temp(j)=A(ind)
350   continue
      write(Iout,99004)i,iatom,IAN(iatom),(temp(j),j=1,n)
400   continue
      min=min+nc
      if(ifin.NE.1)goto 100
      return
      
      end
C* :1 * 
      
