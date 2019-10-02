
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dimens"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dimens.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "dimens.web"
      subroutine dimens(NBASIS)
      implicit none
      double precision disc,gfloat,gsqrt
      integer icon,In,Iout,ipart,Ipunch,Locav,Mdim,Mdsq,Mshifs,Mtt,NBASI
     &S,Ntt
      double precision c
      common/memlng/Locav
      common/maxdm/Mdim,Mtt,Ntt,Mdsq,Mshifs
      common/io/In,Iout,Ipunch
      
      
99001 format(///' NBASIS:',i3,' EXCEEDS MDIM:',i3///)
      
      call ilsw(2,1,icon)
      ipart=icon/2+1
      disc=gsqrt(1.0D00+gfloat(4*Locav/ipart))
      Mdim=int((disc-1.0D00)/2.0D00)
      if(NBASIS.LE.Mdim)then
      Mtt=Mdim*(Mdim+1)/2
      Ntt=NBASIS*(NBASIS+1)/2
      Mdsq=Mdim*Mdim
      Mshifs=Locav/2
      return
      endif
      
      write(Iout,99001)NBASIS,Mdim
      call lnk1e
      stop 13
      
      end
C* :1 * 
      
