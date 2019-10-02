
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 udms"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "udms.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "udms.web"
      subroutine udms(X,NVAR)
      implicit none
      double precision Anames,Fpvec,Values,X
      integer i,Intvec,iozsub,NVAR
      dimension X(*)
      common/zsubst/Anames(50),Values(50),Intvec(50),Fpvec(50)
      data iozsub/570/
      
      
      
      
      
      
      
      
      call tread(iozsub,Anames,175,1,175,1,0)
      do 100 i=1,NVAR
      Values(i)=X(i)
100   continue
      call twrite(iozsub,Anames,175,1,175,1,0)
      
      return
      
      end
C* :1 * 
      
