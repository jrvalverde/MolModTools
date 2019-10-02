
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 atompr"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "atompr.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "atompr.web"
      subroutine atompr(NATOMS,IAN,E,MDIM,NDIM,NCOLS)
      implicit none
      double precision E
      integer i,ia,IAN,iflag,ilower,In,Iout,Ipunch,iupper,j,MDIM,NATOMS,
     &NCOLS,NDIM
      
      dimension E(MDIM,NDIM)
      dimension IAN(*)
      character*4 iel(87)
      common/io/In,Iout,Ipunch
      data iel/'  BQ','   H','  He','  Li','  Be','   B','   C','   N','
     &   O','   F','  Ne','  Na','  Mg','  Al','  Si','   P','   S','  C
     &l','  Ar','   K','  Ca','  Sc','  Ti','   V','  Cr','  Mn','  Fe',
     &'  Co','  Ni','  Cu','  Zn','  Ga','  Ge','  As','  Se','  Br','  
     &Kr','  Rb','  Sr','   Y','  Zr','  Nb','  Mo','  Tc','  Ru','  Rh'
     &,'  Pd','  Ag','  Cd','  In','  Sn','  Sb','  Te','   I','  Xe',' 
     &  Cs','  Ba','  La','  Ce','  Pr','  Nd','  Pm','  Sm','  Eu','  G
     &d','  Tb','  Dy','  Ho','  Er','  Tm','  Yb','  Lu','  Hf','  Ta',
     &'   W','  Re','  Os','  Ir','  Pt','  Au','  Hg','  Tl','  Pb','  
     &Bi','  Po','  At','  Rn'/
      
      
      
99001 format(4x,11I11)
99002 format(i3,a4,11F11.6)
      
      iflag=1
      ilower=1
100   iupper=ilower+5
      if(iupper.GE.NCOLS)then
      iupper=NCOLS
      iflag=0
      endif
      write(Iout,99001)(i,i=ilower,iupper)
      do 200 i=1,NATOMS
      ia=IAN(i)+1
      write(Iout,99002)i,iel(ia),(E(i,j),j=ilower,iupper)
200   continue
      ilower=iupper+1
      if(iflag.NE.0)goto 100
      
      return
      
      end
C* :1 * 
      
