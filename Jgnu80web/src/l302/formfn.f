
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 formfn"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "formfn.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "formfn.web"
      subroutine formfn(KOP,MAXDIM,F,N,X)
      implicit none
      double precision F,fmt,X
      integer i,ick,In,ind,Iout,Ipunch,j,KOP,MAXDIM,N,nfill
      dimension F(MAXDIM,MAXDIM),fmt(20)
      common/io/In,Iout,Ipunch
      
      
      
      
      nfill=N+1
      call fmtgen(fmt,X,2*N+1,ick)
      
      do 100 j=1,nfill
      do 50 i=1,nfill
      ind=i+j-1
      F(i,j)=fmt(ind)
50    continue
100   continue
      
      
      return
      
      end
C* :1 * 
      
