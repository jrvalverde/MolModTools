
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 symlbl"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "symlbl.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "symlbl.web"
      subroutine symlbl(IUHF,SYM,NBASIS,NOSYM)
      implicit none
      integer i,Iprmut,irwrep,irwsa,irwsb,IUHF,j,l,Lblrep,lc,len,lim,lrw
     &rep,MAXBAS,MAXSHL,NBASIS,nc,NOSYM,Nreps,Nsymop
      integer orbsym,SYM,tmp
      parameter(MAXBAS=150,MAXSHL=100)
      double precision Chrtbl,Symops
      dimension orbsym(MAXBAS),tmp(5),SYM(*)
      common/repcom/Nsymop,Nreps,Lblrep(32),Chrtbl(10,16),Symops(9,10),I
     &prmut(MAXSHL,10)
      data irwrep/562/,lrwrep/767/
      data irwsa/563/,irwsb/564/
      
      
      
      NOSYM=1
      call tquery(irwsa,len)
      if(len.LE.0)return
      call tquery(irwrep,len)
      if(len.NE.lrwrep)return
      if(IUHF.EQ.1)call tquery(irwsb,len)
      if(IUHF.EQ.1.AND.len.LE.0)return
      nc=0
      l=(NBASIS-1)/2+1
      if(IUHF.EQ.0)call tread(irwsa,orbsym,l,1,l,1,0)
      if(IUHF.EQ.1)call tread(irwsb,orbsym,l,1,l,1,0)
      
      call tread(irwrep,Nsymop,lrwrep,1,lrwrep,1,0)
      
      do 100 i=1,NBASIS
      lc=0
      if(orbsym(i).EQ.0)return
      if(orbsym(i).NE.1)then
      
      lim=orbsym(i)-1
      do 20 j=1,lim
      call skip(2,Lblrep,lc)
20    continue
      endif
      
      call getb(2,tmp,len,Lblrep,lc)
      
      call putchr('(',SYM,nc)
      call putb(tmp,len,SYM,nc)
      call putchr(')',SYM,nc)
      call putdel(2,SYM,nc)
100   continue
      NOSYM=0
      
      return
      
      end
C* :1 * 
      
