
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 zmatch"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "zmatch.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 52 "zmatch.web"
      subroutine zmatch(OK,SYMBLS,NAME,LEN,VALUE,IFVAR,NSYMB)
      implicit none
      double precision Alpha,Anames,Beta,Bl,Fpvec,str,SYMBLS,VALUE,Value
     &s
      integer i,Ianz,In,Intvec,Iout,Ipunch,Iz,Lalpha,Lbeta,Lbl,LEN,lenst
     &r,NAME,ncur,NSYMB,Nvar,Nz
      logical OK,streqc,IFVAR
      dimension str(10),SYMBLS(*),NAME(*)
      common/zmat/Ianz(50),Iz(50,4),Bl(50),Alpha(50),Beta(50),Lbl(50),La
     &lpha(50),Lbeta(50),Nz,Nvar
      common/zsubst/Anames(50),Values(50),Intvec(50),Fpvec(50)
      common/io/In,Iout,Ipunch
      
99001 format('  SYMBOL NOT FOUND IN Z-MATRIX.')
      
      ncur=0
      OK=.FALSE.
      
      do 100 i=2,Nz
      
      
      call getb(2,str,lenstr,SYMBLS,ncur)
      
      if(lenstr.EQ.LEN)then
      if(streqc(str,NAME,LEN))then
      
      OK=.TRUE.
      NSYMB=NSYMB-1
      if(IFVAR)Lbl(i)=isign(Nvar,Lbl(i))
      if(.NOT.(IFVAR))then
      Bl(i)=VALUE
      if(Lbl(i).LT.0)Bl(i)=-Bl(i)
      Lbl(i)=0
      endif
      endif
      endif
      
      
      if(i.NE.2)then
      call getb(2,str,lenstr,SYMBLS,ncur)
      
      if(lenstr.EQ.LEN)then
      if(streqc(str,NAME,LEN))then
      
      OK=.TRUE.
      NSYMB=NSYMB-1
      if(IFVAR)Lalpha(i)=isign(Nvar,Lalpha(i))
      if(.NOT.(IFVAR))then
      Alpha(i)=VALUE
      if(Lalpha(i).LT.0)Alpha(i)=-VALUE
      Lalpha(i)=0
      endif
      endif
      endif
      
      
      if(i.NE.3)then
      call getb(2,str,lenstr,SYMBLS,ncur)
      
      if(lenstr.EQ.LEN)then
      if(streqc(str,NAME,LEN))then
      
      OK=.TRUE.
      NSYMB=NSYMB-1
      if(IFVAR)Lbeta(i)=isign(Nvar,Lbeta(i))
      if(.NOT.(IFVAR))then
      Beta(i)=VALUE
      if(Lbeta(i).LT.0)Beta(i)=-VALUE
      Lbeta(i)=0
      endif
      endif
      endif
      endif
      endif
100   continue
      
      if(OK)return
      write(Iout,99001)
      call strout(Iout,NAME,LEN,1)
      call fferr(0,0)
      call lnk1e
      return
      
      end
C* :1 * 
      
