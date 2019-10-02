
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 initms"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "initms.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 31 "initms.web"
      subroutine initms(IOP,XNAME,X,NVAR,ISTEP,NSTEP,S,THRESH,CALCFC,TOA
     &NG,FRCNST)
      implicit none
      double precision Alpha,Anames,Beta,Bl,blank,defalt,Fpvec,FRCNST,gf
     &loat,pt001,S,THRESH,TOANG,Values,X,XNAME,zero
      integer i,Ianz,icur,Intvec,IOP,iout,iozmat,iozsub,ISTEP,itmp,itype
     &,Iz,Lalpha,Lbeta,Lbl,len,lzmat,lzsub,ncur,NSTEP
      integer NVAR,Nvarrd,Nz
      logical CALCFC
      dimension IOP(50)
      dimension itmp(20),itype(50)
      dimension S(50,50),XNAME(*),X(*),FRCNST(*)
      common/zmat/Ianz(50),Iz(50,4),Bl(50),Alpha(50),Beta(50),Lbl(50),La
     &lpha(50),Lbeta(50),Nz,Nvarrd
      common/zsubst/Anames(50),Values(50),Intvec(50),Fpvec(50)
      data blank/1H /
      data zero/0.0D0/,defalt/0.0003D0/
      data iozmat,lzmat,iozsub,lzsub/507,351,570,175/
      
      
      
      
      
      
      
99001 format(1x,'NVAR OUT OF VALID RANGE IN LINK 105, NVAR= ',i10)
      
      
      
      call tread(iozmat,Ianz,lzmat,1,lzmat,1,0)
      if(Nvarrd.GT.0.AND.Nvarrd.LE.50)then
      
      call tread(iozsub,Anames,lzsub,1,lzsub,1,0)
      
      
      do 50 i=1,100
      XNAME(i)=blank
50    continue
      icur=0
      ncur=0
      do 100 i=1,Nvarrd
      call getb(2,itmp,len,Anames,ncur)
      call putb(itmp,len,XNAME,icur)
      call putdel(2,XNAME,icur)
      X(i)=Values(i)
100   continue
      NVAR=Nvarrd
      
      
      ISTEP=0
      NSTEP=min0(20,NVAR+10)
      if(IOP(6).NE.0)NSTEP=IOP(6)
      THRESH=defalt
      if(IOP(7).NE.0)THRESH=pt001/gfloat(IOP(7))
      CALCFC=IOP(10).EQ.4
      if(.NOT.CALCFC)then
      
      call initfc(IOP,S,Intvec,Fpvec,NVAR,itype,XNAME,FRCNST)
      else
      do 120 i=1,NVAR
      itype(i)=4
      Fpvec(i)=zero
120   continue
      endif
      else
      write(iout,99001)Nvarrd
      call lnk1e
      stop
      endif
      
      
      call prmtbl(0,XNAME,X,itype,Fpvec,NVAR,Lbl,Nz,TOANG)
      
      return
      
      end
C* :1 * 
      
