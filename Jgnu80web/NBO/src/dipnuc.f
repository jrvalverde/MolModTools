
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dipnuc"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dipnuc.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "dipnuc.web"
      subroutine dipnuc(DX,DY,DZ,ATCOOR,ETA,NOCC)
      implicit none
      double precision ATCOOR,Charge,DX,DY,DZ,ETA,x,Xdip,y,Ydip,z,Zdip,z
     &ero
      integer i,iat,Ibxm,Ispin,j,Label,Larc,Lbl,Lorb,Lorbc,Lstocc,MAXATM
     &,MAXBAS,Munit,Mxao,Mxaolm,Mxbo,Natoms,Nbas,Nbotyp
      integer Nbouni,nctr,Ndim,NOCC
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbdxyz/Xdip,Ydip,Zdip,Charge(MAXATM)
      common/nbbas/Label(MAXBAS,6),Nbouni(MAXBAS),Nbotyp(MAXBAS),Lstocc(
     &MAXBAS),Ibxm(MAXBAS),Larc(MAXBAS),Lbl(MAXBAS),Lorbc(MAXBAS),Lorb(M
     &AXBAS)
      dimension DX(Ndim,Ndim),DY(Ndim,Ndim),DZ(Ndim,Ndim),ATCOOR(3,Natom
     &s)
      
      data zero/0.0D0/
      
      
      call fecoor(ATCOOR)
      
      
      do 100 i=1,NOCC
      nctr=mod(Nbotyp(i),10)
      x=zero
      y=zero
      z=zero
      do 50 j=1,nctr
      iat=Label(Ibxm(i),j+3)
      x=x+ATCOOR(1,iat)
      y=y+ATCOOR(2,iat)
      z=z+ATCOOR(3,iat)
      Charge(iat)=Charge(iat)-ETA/nctr
50    continue
      x=ETA*x/nctr
      y=ETA*y/nctr
      z=ETA*z/nctr
      DX(i,i)=DX(i,i)+x
      DY(i,i)=DY(i,i)+y
      DZ(i,i)=DZ(i,i)+z
100   continue
      return
      end
C* :1 * 
      
