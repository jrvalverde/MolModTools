
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 drvsrd"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "drvsrd.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "drvsrd.web"
      subroutine drvsrd(NATOMS,NBASIS,FXYZ,D,DD,IATMS,IDUMP)
      implicit none
      double precision a,aa,D,DD,FXYZ,scftrc
      integer i,IATMS,ideriv,iderv1,iderv2,Idrv1,IDUMP,Ifill,In,index,in
     &t,Iout,Ipunch,Irwpt,Irww,ixyz,j,nat3,NATOMS,NBASIS
      integer nbs,ntt
      dimension FXYZ(*),D(*),DD(*),IATMS(*)
      common/io/In,Iout,Ipunch
      common/irw716/Irww,Irwpt,Idrv1,Ifill(2)
      
      
      
      
      
      
      
      
99001 format(' DUMP OF ONE ELECTRON INTEGRAL CONTRIBUTION TO FORCES')
99002 format(' DUMP OF KINETIC AND V(NE) CONTRIBUTION TO THE FORCES')
99003 format(3F20.8)
      
      nbs=(NBASIS+1)/2
      ntt=(NBASIS*(NBASIS+1))/2
      nat3=3*NATOMS
      call tread(Irwpt,D,ntt,1,ntt,1,0)
      call fileio(2,-Idrv1,nbs,IATMS,0)
      
      call fileio(2,-Idrv1,0,0,nbs+9*ntt)
      do 100 i=1,nat3
      call fileio(2,Idrv1,ntt,DD,0)
      FXYZ(i)=FXYZ(i)+scftrc(D,DD,NBASIS,1)
100   continue
      
      
      do 300 ixyz=1,3
      
      if(ixyz.NE.1)call tread(Irwpt,D,ntt,1,ntt,1,0)
      
      index=nbs+ntt*(ixyz+2)
      call fileio(2,-Idrv1,ntt,DD,index)
      int=0
      do 150 i=1,NBASIS
      do 120 j=1,i
      int=int+1
      ideriv=3*(IATMS(i)-1)+ixyz
      FXYZ(ideriv)=FXYZ(ideriv)+DD(int)*(D(int)+D(int))
120   continue
      FXYZ(ideriv)=FXYZ(ideriv)-DD(int)*D(int)
150   continue
      
      index=index+3*ntt
      call fileio(2,-Idrv1,ntt,DD,index)
      int=0
      do 200 i=1,NBASIS
      do 160 j=1,i
      int=int+1
      ideriv=3*(IATMS(j)-1)+ixyz
      FXYZ(ideriv)=FXYZ(ideriv)+DD(int)*(D(int)+D(int))
160   continue
      FXYZ(ideriv)=FXYZ(ideriv)-DD(int)*D(int)
200   continue
      
      if(IDUMP.GT.1)then
      write(Iout,99002)
      write(Iout,99003)(FXYZ(i),i=1,nat3)
      endif
      
      index=nbs+ntt*(ixyz-1)
      call fileio(2,-Idrv1,ntt,DD,index)
      call tread(Irww,D,ntt,1,ntt,1,0)
      int=0
      do 250 i=1,NBASIS
      do 220 j=1,i
      int=int+1
      a=DD(int)
      aa=a+a
      iderv1=3*(IATMS(i)-1)+ixyz
      iderv2=3*(IATMS(j)-1)+ixyz
      FXYZ(iderv1)=FXYZ(iderv1)+D(int)*aa
      FXYZ(iderv2)=FXYZ(iderv2)-D(int)*aa
220   continue
      FXYZ(iderv1)=FXYZ(iderv1)-D(int)*a
      FXYZ(iderv2)=FXYZ(iderv2)+D(int)*a
250   continue
      
300   continue
      
      if(IDUMP.GT.1)then
      write(Iout,99001)
      write(Iout,99003)(FXYZ(i),i=1,nat3)
      endif
      
      return
      
      end
C* :1 * 
      
