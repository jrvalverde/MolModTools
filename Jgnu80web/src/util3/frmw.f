
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 frmw"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "frmw.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 30 "frmw.web"
      subroutine frmw(NBASIS,NAE,NBE,IRWCA,IRWCAI,IRWCB,IRWEIG,IRWW,W,E,
     &C,IPR,IOUT)
      implicit none
      double precision C,E,W
      integer IOUT,IRWCA,IRWCAI,IRWCB,IRWEIG,IRWW,itemp,NAE,NBASIS,NBE,n
     &tt
      logical open,complx,IPR
      dimension C(*),E(*),W(*)
      
      
      
      
      
      
      
      
      
99001 format(5x,'ENERGY-WEIGHTED DENSITY MATRIX')
      
      call ilsw(2,1,itemp)
      open=.FALSE.
      complx=.FALSE.
      if(itemp.EQ.1)open=.TRUE.
      if(itemp.EQ.2)complx=.TRUE.
      
      itemp=NBASIS
      if(open)itemp=NBASIS+NBASIS
      call tread(IRWEIG,E(1),itemp,1,itemp,1,0)
      
      ntt=(NBASIS*(NBASIS+1))/2
      call aclear(ntt,W(1))
      
      itemp=2
      if(open)itemp=1
      call winc(NAE,NBASIS,itemp,IRWCA,E(1),W,C)
      if(complx)call winc(NAE,NBASIS,itemp,IRWCAI,E(1),W,C)
      if(open)call winc(NBE,NBASIS,itemp,IRWCB,E(1+NBASIS),W,C)
      
      if(IPR)then
      write(IOUT,99001)
      call ltoutd(NBASIS,W,0)
      endif
      
      call twrite(IRWW,W(1),ntt,1,ntt,1,0)
      
      return
      
      end
C* :1 * 
      
