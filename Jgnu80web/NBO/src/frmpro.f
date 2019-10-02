
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 frmpro"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "frmpro.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "frmpro.web"
      subroutine frmpro(P,IA,Q,NK,PK,VK,PI)
      implicit none
      integer i,IA,Iatcr,Iatno,icol,Ill,Ino,Ispin,Iul,Iznuc,j,k,l,MAXATM
     &,MAXBAS,Munit,Mxao,Mxaolm,Mxbo,Natoms
      integer nb,Nbas,Ndim,NK,Norbs
      double precision one,P,PI,PK,Q,VK,zero
      
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ill(MAXATM),
     &Iul(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      dimension P(Mxao,Mxao),VK(Mxao),PI(Mxao),Q(Mxao,Ndim),PK(Mxao,Mxao
     &)
      data zero,one/0.0D0,1.0D0/
      nb=Norbs(IA)
      do 100 j=1,nb
      do 50 i=1,j
      P(i,j)=zero
      P(j,i)=zero
      if(i.EQ.j)P(i,j)=one
50    continue
100   continue
      if(NK.LE.0)return
      do 300 k=1,NK
      icol=Ill(IA)+k-1
      do 150 i=1,nb
      VK(i)=Q(i,icol)
150   continue
      do 200 j=1,nb
      do 160 i=1,j
      PK(i,j)=-VK(i)*VK(j)
      PK(j,i)=PK(i,j)
      if(i.EQ.j)PK(i,j)=PK(i,j)+one
160   continue
200   continue
      do 250 i=1,nb
      do 220 j=1,nb
      PI(j)=zero
      do 210 l=1,nb
      PI(j)=PI(j)+P(i,l)*PK(l,j)
210   continue
220   continue
      do 240 j=1,nb
      P(i,j)=PI(j)
240   continue
250   continue
300   continue
      return
      end
C* :1 * 
      
