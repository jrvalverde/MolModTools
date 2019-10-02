
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fvmul"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fvmul.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "fvmul.web"
      subroutine fvmul(N,F,V,T,S)
      implicit none
      double precision F,S,sum,T,V,zero
      integer i,icol,ijt,ijv,indf1,indf2,irow,irp,k,N
      dimension F(*),V(*),T(*),S(*)
      data zero/0.0D0/
      
      
      
      
      
      
      
      
      
      
      indf1=0
      ijt=0
      
      do 200 irow=1,N
      
      do 50 i=1,irow
      S(i)=F(indf1+i)
50    continue
      indf1=indf1+irow
      
      if(irow.LT.N)then
      irp=irow+1
      indf2=indf1+irow
      do 60 i=irp,N
      S(i)=F(indf2)
      indf2=indf2+i
60    continue
      endif
      
      ijv=0
      do 100 icol=1,N
      
      sum=zero
      do 80 k=1,N
      ijv=ijv+1
      sum=sum+S(k)*V(ijv)
80    continue
      
      ijt=ijt+1
      T(ijt)=sum
100   continue
200   continue
      
      
      call trspn2(N,T)
      
      return
      
      end
C* :1 * 
      
