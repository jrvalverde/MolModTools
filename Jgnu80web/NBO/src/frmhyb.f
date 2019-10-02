
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 frmhyb"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "frmhyb.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "frmhyb.web"
      subroutine frmhyb(HYB,THYB,COEF,HYCOEF,KL,KU,NHYB)
      implicit none
      double precision COEF,HYB,HYCOEF,one,temp,thresh,THYB,zero
      integer i,ih,ihyb,Ispin,k,KL,KU,Lfnao,Lfnarc,Lfndaf,Lfndef,Lfndm,L
     &fnin,Lfnmo,Lfnnab,Lfnnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna
      integer Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lfnpr,Munit,Mxao,Mxaolm,Mxbo,N
     &atoms,Nbas,Ndim,NHYB
      
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      dimension HYB(1),THYB(Ndim,Ndim),HYCOEF(Ndim)
      
      data zero,one,thresh/0.0D0,1.0D0,1.0D-4/
      
      
      
      if(dabs(COEF).LT.thresh)return
      do 100 ihyb=1,NHYB
      temp=zero
      ih=0
      do 50 k=KL,KU
      ih=ih+1
      temp=temp+HYB(ih)*THYB(k,ihyb)
50    continue
      if(dabs(dabs(temp)-one).LT.thresh)return
      if(dabs(temp).GT.thresh)then
      write(Lfnpr,99001)NHYB+1,temp,ihyb
      stop
      endif
100   continue
      
      
      NHYB=NHYB+1
      if(NHYB.GT.Nbas)stop 'Too many hybrids'
      do 200 i=1,Nbas
      THYB(i,NHYB)=zero
200   continue
      ih=0
      do 300 i=KL,KU
      ih=ih+1
      THYB(i,NHYB)=HYB(ih)
300   continue
      HYCOEF(NHYB)=COEF
      if(NHYB.NE.Nbas)return
      call svtnho(THYB)
      return
      
99001 format(/1x,'Hybrid ',i3,' has a ','non-negligible overlap of ',f8.
     &5,' with hybrid ',i3,'.')
      end
C* :1 * 
      
