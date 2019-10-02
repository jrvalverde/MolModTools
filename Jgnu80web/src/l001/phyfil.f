
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 phyfil"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "phyfil.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "phyfil.web"
      subroutine phyfil(PHYCON)
      implicit none
      double precision avog,boltz,hartre,PHYCON,planck,slight,toang,toe,
     &tokg,tomet
      integer irwphy
      double precision jpcal
      dimension PHYCON(30)
      data toang/0.52917706D00/
      data tokg/1.6605655D-27/
      data toe/4.803242D-10/
      data planck/6.626176D-34/
      data avog/6.022045D+23/
      data jpcal/4.184D00/
      data tomet/5.2917706D-11/
      data hartre/4.359814D-18/
      data slight/2.99792458D+10/
      data boltz/1.380662D-23/
      data irwphy/994/
      
      
      
      
      
      
      
      
      
      
      
      
      
      call aclear(30,PHYCON)
      
      PHYCON(1)=toang
      PHYCON(2)=tokg
      PHYCON(3)=toe
      PHYCON(4)=planck
      PHYCON(5)=avog
      PHYCON(6)=jpcal
      PHYCON(7)=tomet
      PHYCON(8)=hartre
      PHYCON(9)=slight
      PHYCON(10)=boltz
      
      call twrite(irwphy,PHYCON,30,1,30,1,0)
      return
      
      end
C* :1 * 
      
