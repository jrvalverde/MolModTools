
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 putsmt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "putsmt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "putsmt.web"
      subroutine putsmt(IRWF,ISUB,A)
      implicit none
      double precision A
      integer i,In,Indsmt,Iout,ipos,Ipunch,IRWF,Irwsmt,isave,ISUB,len,Ma
     &xsmt,Ninsmt
      dimension A(*)
      common/smt/Indsmt,Maxsmt,Ninsmt(10),Irwsmt(10)
      common/io/In,Iout,Ipunch
      
      
      
      
99001 format(' PUTMST:  UNABLE TO MAP IRWF=',i5/'   ISUB=',i6/'   NINSMT
     &, IRWSMT:'/(1x,i2,2I6))
      
      do 100 i=1,Indsmt
      isave=i
      if(IRWF.EQ.Irwsmt(i))goto 200
100   continue
      write(Iout,99001)IRWF,ISUB,Indsmt,(i,Ninsmt(i),Irwsmt(i),i=1,Indsm
     &t)
      call lnk1e
      
200   len=Ninsmt(isave)
      
      ipos=(ISUB-1)*len
      call fileio(+1,-IRWF,len,A(1),ipos)
      
      return
      
      end
C* :1 * 
      
