
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 subvar"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "subvar.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "subvar.web"
      subroutine subvar(BL,ALPHA,BETA,LBL,LALPHA,LBETA,NZ,NVAR)
      implicit none
      double precision ALPHA,Anames,BETA,BL,Fpvec,one,sign,Values
      integer i,idx,In,Intvec,Iout,iozsub,Ipunch,j,LALPHA,LBETA,LBL,NVAR
     &,NZ
      dimension BL(*),ALPHA(*),BETA(*)
      dimension LBL(*),LALPHA(*),LBETA(*)
      common/io/In,Iout,Ipunch
      common/zsubst/Anames(50),Values(50),Intvec(50),Fpvec(50)
      data iozsub/570/
      data one/1.0D0/
      
      
      
      
      
      
      
99001 format(1x,'VARIABLE INDEX OF ',i4,' ON CARD ',i3,' IS OUT OF ''RAN
     &GE.  NVAR = ',i3)
      
      
      
      if(NVAR.EQ.0)return
      call tread(iozsub,Anames,175,1,175,1,0)
      
      
      do 100 i=1,NZ
      j=i
      if(LBL(i).NE.0)then
      idx=iabs(LBL(i))
      if(idx.LE.0.OR.idx.GT.NVAR)goto 200
      sign=one
      if(LBL(i).LT.0)sign=-one
      BL(i)=sign*Values(idx)
      endif
      if(LALPHA(i).NE.0)then
      idx=iabs(LALPHA(i))
      if(idx.LE.0.OR.idx.GT.NVAR)goto 200
      sign=one
      if(LALPHA(i).LT.0)sign=-one
      ALPHA(i)=sign*Values(idx)
      endif
      if(LBETA(i).NE.0)then
      idx=iabs(LBETA(i))
      if(idx.LE.0.OR.idx.GT.NVAR)goto 200
      sign=one
      if(LBETA(i).LT.0)sign=-one
      BETA(i)=sign*Values(idx)
      endif
100   continue
      return
      
      
200   write(Iout,99001)idx,j,NVAR
      call lnk1e
      stop 13
      
      end
C* :1 * 
      
