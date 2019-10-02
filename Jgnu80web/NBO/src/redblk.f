
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 redblk"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "redblk.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 30 "redblk.web"
      subroutine redblk(T,TPNAO,IL,DM,BLK,EVAL,C,NF,IORB,NC,IRANK,IRPNAO
     &)
      implicit none
      double precision ave,BLK,C,DM,EVAL,sum,T,TPNAO,zero
      integer i,IL,inao,IORB,IRANK,IRPNAO,Ispin,j,jc,jnao,Larc,Lbl,Ldeg,
     &Lorb,Lorbc,Lstemt,Lstocc,m,MAXATM,MAXBAS
      integer Munit,Mxao,Mxaolm,Mxbo,Naoctr,Naol,Natoms,Nbas,NC,Ndim,NF,
     &nm
      parameter(MAXATM=99,MAXBAS=500)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbbas/Ldeg(MAXBAS,6),Naoctr(MAXBAS),Naol(MAXBAS),Lstocc(MAX
     &BAS),Lstemt(MAXBAS),Larc(MAXBAS),Lbl(MAXBAS),Lorbc(MAXBAS),Lorb(MA
     &XBAS)
      dimension DM(Ndim,Ndim),BLK(NC,NC),C(NC,NC),EVAL(Ndim),T(Ndim,Ndim
     &),TPNAO(Ndim,Ndim),IRANK(Nbas)
      data zero/0.0D0/
      
      
      nm=IL*2+1
      do 100 j=1,NC
      do 50 i=1,j
      sum=zero
      do 20 m=1,nm
      inao=IORB+i-1+(m-1)*NC
      jnao=IORB+j-1+(m-1)*NC
      sum=sum+DM(inao,jnao)
20    continue
      ave=sum/nm
      BLK(i,j)=ave
      BLK(j,i)=ave
50    continue
100   continue
      call jacobi(NC,BLK,EVAL,C,NC,NC,1)
      call rank(EVAL,NC,NC,Larc)
      do 200 j=1,NC
      jc=Larc(j)
      do 150 i=1,NC
      BLK(i,j)=C(i,jc)
150   continue
200   continue
      do 300 m=1,nm
      do 250 j=1,NC
      NF=NF+1
      IRANK(j)=NF
250   continue
      call limtrn(T,IRANK,BLK,C,Ndim,Nbas,NC,NC,1)
      call limtrn(DM,IRANK,BLK,C,Ndim,Nbas,NC,NC,0)
      if(IRPNAO.EQ.1)call limtrn(TPNAO,IRANK,BLK,C,Ndim,Nbas,NC,NC,1)
300   continue
      
      return
      end
C* :1 * 
      
