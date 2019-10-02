
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rydiag"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rydiag.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 35 "rydiag.web"
      subroutine rydiag(T,S,TPNAO,DMBLK,SBLK,OCC,EVAL,EVECT,EVAL2,IORB,N
     &C,NM,NSTART,NRYDC,LARC,LIST,IRPNAO)
      implicit none
      double precision DMBLK,dmsum,EVAL,EVAL2,EVECT,OCC,S,SBLK,ssum,T,tk
     &i,tkj,tli,TPNAO,zero
      integer i,ii,inao,IORB,IRPNAO,Ispin,j,jc,jj,jnao,k,km1,kp1,l,LARC,
     &LIST,m,Munit,Mxao,Mxaolm
      integer Mxbo,Natoms,Nbas,NC,Ndim,NM,NRYDC,NSTART
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      dimension T(Ndim,Ndim),S(Ndim,Ndim),TPNAO(Ndim,Ndim),OCC(Nbas),DMB
     &LK(NRYDC,NRYDC),SBLK(NRYDC,NRYDC),EVAL(Nbas),EVECT(NRYDC,NRYDC),LA
     &RC(NRYDC),LIST(NRYDC),EVAL2(Nbas)
      data zero/0.0D0/
      
      
      ii=0
      do 100 i=1,NRYDC
      do 50 j=1,NRYDC
      DMBLK(i,j)=zero
      SBLK(i,j)=zero
50    continue
100   continue
      do 300 i=NSTART,NC
      ii=ii+1
      do 200 m=1,NM
      inao=IORB+(i-1)+(m-1)*NC
      do 140 k=1,Nbas
      dmsum=zero
      ssum=zero
      km1=k-1
      do 110 l=1,km1
      tli=T(l,inao)
      dmsum=dmsum+tli*S(l,k)
      ssum=ssum+tli*S(k,l)
110   continue
      tki=T(k,inao)
      dmsum=dmsum+tki*S(k,k)
      ssum=ssum+tki
      kp1=k+1
      do 120 l=kp1,Nbas
      tli=T(l,inao)
      dmsum=dmsum+tli*S(k,l)
      ssum=ssum+tli*S(l,k)
120   continue
      EVAL(k)=dmsum
      EVAL2(k)=ssum
140   continue
      jj=0
      do 160 j=NSTART,i
      jj=jj+1
      jnao=IORB+(j-1)+(m-1)*NC
      dmsum=zero
      ssum=zero
      do 150 k=1,Nbas
      tkj=T(k,jnao)
      dmsum=dmsum+EVAL(k)*tkj
      ssum=ssum+EVAL2(k)*tkj
150   continue
      DMBLK(ii,jj)=DMBLK(ii,jj)+dmsum
      SBLK(ii,jj)=SBLK(ii,jj)+ssum
160   continue
200   continue
      do 250 jj=1,ii
      DMBLK(ii,jj)=DMBLK(ii,jj)/NM
      DMBLK(jj,ii)=DMBLK(ii,jj)
      SBLK(ii,jj)=SBLK(ii,jj)/NM
      SBLK(jj,ii)=SBLK(ii,jj)
250   continue
300   continue
      call atdiag(NRYDC,DMBLK,SBLK,EVAL,EVECT)
      call rank(EVAL,NRYDC,NRYDC,LARC)
      do 400 j=1,NRYDC
      jc=LARC(j)
      do 350 i=1,NRYDC
      SBLK(i,j)=EVECT(i,jc)
350   continue
400   continue
      do 500 m=1,NM
      jj=0
      do 450 j=NSTART,NC
      jj=jj+1
      jnao=IORB+(j-1)+(m-1)*NC
      OCC(jnao)=EVAL(jj)
      LIST(jj)=jnao
450   continue
      call limtrn(T,LIST,SBLK,DMBLK,Ndim,Nbas,NRYDC,NRYDC,1)
500   continue
      
      if(IRPNAO.EQ.0)return
      
      
      call symort(EVECT,SBLK,DMBLK,NRYDC,NRYDC,EVAL)
      do 600 m=1,NM
      jj=0
      do 550 j=NSTART,NC
      jj=jj+1
      LIST(jj)=IORB+(j-1)+(m-1)*NC
550   continue
      call limtrn(TPNAO,LIST,SBLK,DMBLK,Ndim,Nbas,NRYDC,NRYDC,1)
600   continue
      return
      end
C* :1 * 
      
