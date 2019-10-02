
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 mulpop"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "mulpop.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 68 "mulpop.web"
      subroutine mulpop(JUMP)
      implicit none
      double precision core
      double precision A,Aa,Atmchg,B,Bb,C,Fill,goc,Phycon,Toang,Toe,Tokg
     &,zero
      integer i,i1,i2,Ian,Icharg,Icmp,idumb,Ifcond,Ifdens,Ifdist,Ifgros,
     &Ifmo,Ifoorc,Ifpop,Ifspin,In,Iolbl,Iop,Iout,Ipunch
      integer Irtcrd,Irwca,Irwcb,Irweig,Irwgen,irwlbl,irwmap,Irwpa,Irwpb
     &,Irwpt,Irws,Irwsc1,Irwscr,Irwx,Irwy,Irwz,Ititle,j,j1,j2
      integer JUMP,k,l,labcol,Label,lc,line,lrwlbl,lrwmap,lsyma,lsymb,Ma
     &pper,Maprot,MAXATM,MAXBAS,MAXMEM,Mdim,Mnchrg,mode,Multip,Nae
      integer Namax,Natoms,Nbasis,Nbe,ncols,Ne,nosym,Nrot
      integer icontr
      parameter(MAXMEM=50000)
      parameter(MAXBAS=150,MAXATM=100)
      dimension goc(MAXBAS,6),line(80)
      dimension lsyma(1000),lsymb(1000)
      dimension core(MAXMEM)
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/phycon/Toang,Tokg,Toe,Phycon(27)
      common/iopt/Ifoorc,Ifdist,Ifmo,Ifdens,Ifpop,Ifgros,Mnchrg,Ifcond,I
     &fspin,Icmp
      common/io/In,Iout,Ipunch
      common/memry/Aa(MAXBAS),Bb(MAXBAS),A(MAXBAS,MAXBAS),B(MAXBAS,MAXBA
     &S),Fill(4700)
      common/label/Label(1000),Ititle(100),Irtcrd(100)
      common/md601/Mdim,Namax
      common/irw601/Irweig,Irwca,Irwcb,Irwpa,Irwpb,Irws,Iolbl,Irwx,Irwy,
     &Irwz,Irwpt,Irwgen,Irwscr,Irwsc1
      common/maps/Nrot,Maprot(MAXBAS),Mapper(MAXATM)
      equivalence(core(1),aa(1))
      data irwmap/559/,lrwmap/125/
      data irwlbl/502/,lrwlbl/600/
      data zero/0.0D0/
      
      
      
      
      
      
      
      
      
      
99001 format(4x,47H BETA MOLECULAR ORBITAL COEFFICIENTS (IMAGINARY,7H PA
     &RT).)
99002 format(4x,37HBETA DENSITY MATRIX (IMAGINARY PART).)
99003 format(4x,34HFULL MULLIKEN POPULATION ANALYSIS.)
99004 format(5x,22HGROSS ORBITAL CHARGES.)
99005 format(10x,34HCONDENSED TO ATOMS (ALL ELECTRONS))
99006 format(10x,22HATOMIC SPIN DENSITIES.)
99007 format(10x,27HDISTANCE MATRIX (ANGSTROMS))
99008 format(10x,21HTOTAL ATOMIC CHARGES.)
      
      call drum
      call rgdvo(Iop)
      if((iop(40).GE.2).OR.(iop(41).NE.0))go to 900
      
      
      ncols=max0(Nae+5,Nbe+5)
      ncols=min0(ncols,Nbasis)
      
      
      call tread(irwlbl,Label,lrwlbl,1,lrwlbl,1,0)
      
      call tread(irwmap,Nrot,lrwmap,1,lrwmap,1,0)
      Mapper(Natoms+1)=Nbasis+1
      
      mode=0
      if(Ifoorc.NE.0)mode=1
      
      if(Ifdist.NE.0)then
      write(Iout,99007)
      call dismat(Natoms,Ian,C,2,5,Iout,idumb,1,Toang)
      
      i=Nbasis
      endif
      call symasg(A,B,Aa,Bb,goc,Nbasis,Nbasis,Nae,Nbe,Natoms,0)
      call ecnfig(Multip,Nae,Nbe,Nbasis,Aa)
      call symlbl(0,lsyma,Nbasis,nosym)
      if(Ifoorc.NE.0)call symlbl(1,lsymb,Nbasis,nosym)
      labcol=2
      if(nosym.EQ.1)labcol=0
      
      
      if(Ifmo.NE.0)then
      if(Ifoorc.EQ.0)then
      
      call tread(Irweig,Aa,Nbasis,1,Nbasis,1,0)
      else
      call tread(Irweig,Aa,2*Nbasis,1,2*Nbasis,1,0)
      endif
      
      call tread(Irwca,A,Mdim,Mdim,Nbasis,Nbasis,0)
      
      lc=0
      call pad(line,lc,4,1H )
      if(Ifoorc.NE.0)call putbc('ALPHA',5,line,lc)
      call putbc(' MOLECULAR ORBITAL COEFFICIENTS',31,line,lc)
      if(Icmp.NE.0)call putbc(' (REAL PART)',12,line,lc)
      call strout(Iout,line,lc,1)
      
99009 format(' ',25A4)
      
      call matprt(A,Mdim,Mdim,Nbasis,ncols,2,labcol,Label,lsyma,0,Aa,1)
      if(Icmp.NE.0)then
      
      call tread(Irwca+1,A,Mdim,Mdim,Nbasis,Nbasis,0)
      
      lc=0
      call pad(line,lc,4,1H )
      if(Ifoorc.NE.0)call putbc('ALPHA',5,line,lc)
      call putbc(' MOLECULAR ORBITAL COEFFICIENTS',31,line,lc)
      call putbc(' (IMAGINARY PART).',18,line,lc)
      call strout(Iout,line,lc,1)
      call matprt(A,Mdim,Mdim,Nbasis,ncols,2,labcol,Label,lsyma,0,Aa,1)
      endif
      if(Ifoorc.NE.0)then
      
      call tread(Irwcb,A,Mdim,Mdim,Nbasis,Nbasis,0)
      
      lc=0
      call putbc('    BETA MOLECULAR ORBITAL COEFFICIENTS',39,line,lc)
      if(Icmp.NE.0)call putbc(' (REAL PART)',12,line,lc)
      call putchr('.',line,lc)
      call strout(Iout,line,lc,1)
      
      call matprt(A,Mdim,Mdim,Nbasis,ncols,2,labcol,Label,lsymb,0,Aa(Nba
     &sis+1),1)
      if(Icmp.NE.0)then
      call tread(Irwcb+1,A,Mdim,Mdim,Nbasis,Nbasis,0)
      write(Iout,99001)
      call matprt(A,Mdim,Mdim,Nbasis,ncols,2,labcol,Label,lsymb,0,Aa(Nba
     &sis+1),1)
      endif
      endif
      endif
      
      
      call tread(Irwpa,A,Mdim,Mdim,Nbasis,Nbasis,1)
      if(Ifdens.NE.0)then
      
      lc=0
      call pad(line,lc,4,1H )
      if(Ifoorc.NE.0)call putbc('ALPHA',5,line,lc)
      call putbc(' DENSITY MATRIX',15,line,lc)
      if(Icmp.NE.0)call putbc(' (REAL PART)',12,line,lc)
      call putchr('.',line,lc)
      call strout(Iout,line,lc,1)
      
      call matprt(A,Mdim,Mdim,Nbasis,Nbasis,2,0,Label,0,1,0,0)
      if(Icmp.NE.0)then
      call tread(Irwpa+1,B,Mdim,Mdim,Nbasis,Nbasis,1)
      
      lc=0
      call pad(line,lc,4,1H )
      if(Ifoorc.EQ.0)call putbc('ALPHA',5,line,lc)
      call putbc(' DENSITY MATRIX',15,line,lc)
      call putbc(' (IMAGINARY PART).',18,line,lc)
      call strout(Iout,line,lc,1)
      
      call matprt(B,Mdim,Mdim,Nbasis,Nbasis,2,0,Label,0,1,0,0)
      endif
      if(Ifoorc.NE.0)then
      
      call tread(Irwpb,B,Mdim,Mdim,Nbasis,Nbasis,1)
      
      lc=0
      call pad(line,lc,4,1H )
      call putbc('BETA DENSITY MATRIX',19,line,lc)
      if(Icmp.NE.0)call putbc(' (REAL PART)',12,line,lc)
      call putchr('.',line,lc)
      call strout(Iout,line,lc,1)
      
      call matprt(B,Mdim,Mdim,Nbasis,Nbasis,2,0,Label,0,1,0,0)
      if(Icmp.NE.0)then
      call tread(Irwpb+1,B,Mdim,Mdim,Nbasis,Nbasis,1)
      write(Iout,99002)
      call matprt(B,Mdim,Mdim,Nbasis,Nbasis,2,0,Label,0,1,0,0)
      endif
      endif
      endif
      
      
      
100   call tread(Irws,B,Mdim,Mdim,Nbasis,Nbasis,1)
      do 200 i=1,Nbasis
      do 150 j=1,i
      A(i,j)=A(i,j)*B(i,j)
      A(j,i)=A(i,j)
150   continue
200   continue
      if(Ifpop.NE.0.AND.mode.EQ.0)then
      write(Iout,99003)
      call matprt(A,Mdim,Mdim,Nbasis,Nbasis,2,0,Label,0,1,0,0)
      endif
      
      
      do 300 i=1,Nbasis
      Aa(i)=zero
      do 250 j=1,Nbasis
      Aa(i)=Aa(i)+A(i,j)
250   continue
300   continue
      if(mode.EQ.1)then
      do 350 i=1,Nbasis
      goc(i,2)=Aa(i)
350   continue
      endif
      if(mode.NE.2)then
      
      do 400 i=1,Nbasis
      goc(i,1)=Aa(i)
400   continue
      else
      do 450 i=1,Nbasis
      goc(i,3)=Aa(i)
      goc(i,4)=goc(i,2)-goc(i,3)
450   continue
      endif
      
      if(Mnchrg.NE.0)call orbtyp(Aa)
      
      
      do 600 i=1,Natoms
      i1=Mapper(i)
      i2=Mapper(i+1)-1
      do 500 j=1,Natoms
      B(i,j)=zero
      j1=Mapper(j)
      j2=Mapper(j+1)-1
      do 480 k=i1,i2
      do 460 l=j1,j2
      B(i,j)=B(i,j)+A(k,l)
460   continue
480   continue
500   continue
600   continue
      
      if(mode.EQ.1)then
      call twrite(Irwsc1,B,Mdim,Mdim,Natoms,Natoms,1)
      mode=2
      call tread(Irwpb,A,Mdim,Mdim,Nbasis,Nbasis,1)
      goto 100
      
      elseif(mode.NE.2)then
      
      if(Ifgros.NE.0)then
      if(Ifoorc.EQ.1)then
      
      write(Iout,99004)
      lc=0
      call putbc('TOTAL',5,line,lc)
      call putdel(2,line,lc)
      call putbc('ALPHA',5,line,lc)
      call putdel(2,line,lc)
      call putbc('BETA',4,line,lc)
      call putdel(2,line,lc)
      call putbc('SPIN',4,line,lc)
      call putdel(2,line,lc)
      call matprt(goc,Mdim,4,Nbasis,4,2,1,Label,line,0,0,0)
      else
      write(Iout,99004)
      call matprt(goc,Mdim,4,Nbasis,1,2,0,Label,0,0,0,0)
      endif
      endif
      
      if(Ifcond.NE.0)then
      write(Iout,99005)
      call atompr(Natoms,Ian,B,Mdim,Mdim,Natoms)
      if(Natoms.NE.1)then
      do 610 j=1,Natoms
      do 605 i=2,Natoms
      B(j,1)=B(j,1)+B(j,i)
605   continue
610   continue
      write(Iout,99008)
      call atompr(Natoms,Ian,B,Mdim,Mdim,1)
      endif
      if(Ifoorc.NE.0)then
      call tread(Irwsc1,A,Mdim,Mdim,Natoms,Natoms,1)
      write(Iout,99006)
      call atompr(Natoms,Ian,A,Mdim,Mdim,Natoms)
      endif
      endif
      else
      mode=0
      call tread(Irwsc1,A,Mdim,Mdim,Natoms,Natoms,1)
      do 650 i=1,Natoms
      do 620 j=1,Natoms
      A(i,j)=A(i,j)-B(i,j)
620   continue
650   continue
      call twrite(Irwsc1,A,Mdim,Mdim,Natoms,Natoms,1)
      call tread(Irwpt,A,Mdim,Mdim,Nbasis,Nbasis,1)
      goto 100
      endif
      
      
      call dq(Natoms,Nbasis,Atmchg,Icharg,C,Mdim,A,B,Toe,Toang)
      
      if(Ifoorc.EQ.1)call frmpop(Natoms,Ian,Nbasis,Mdim,Mdim,A,B,Aa)
      
      
900   CONTINUE
      icontr=82
      call runnbo(core,MAXMEM,iop,icontr)
      JUMP=0
      
      return
      
      end
C* :1 * 
      
