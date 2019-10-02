
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 udbs"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "udbs.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "udbs.web"
      subroutine udbs(X)
      implicit none
      double precision Alpha,Anames,Beta,Bl,Fpvec,Values,X
      integer i,Ianz,Intvec,iozsub,Iz,Lalpha,Lbeta,Lbl,Nvarrd,Nz
      dimension X(*)
      common/zmat/Ianz(50),Iz(50,4),Bl(50),Alpha(50),Beta(50),Lbl(50),La
     &lpha(50),Lbeta(50),Nz,Nvarrd
      common/zsubst/Anames(50),Values(50),Intvec(50),Fpvec(50)
      data iozsub/570/
      
      
      
      
      
      
      
      
      call tread(iozsub,Anames,175,1,175,1,0)
      do 100 i=1,Nvarrd
      Values(i)=X(i)
100   continue
      call twrite(iozsub,Anames,175,1,175,1,0)
      
      return
      
      end
C* :1 * 
      
