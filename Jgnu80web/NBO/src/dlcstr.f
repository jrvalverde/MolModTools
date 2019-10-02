
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dlcstr"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dlcstr.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "dlcstr.web"
      subroutine dlcstr(IBO,IL,NL,LIST,ML,ISTR)
      implicit none
      integer i,IBO,icomma,ihtyp,IL,ileft,iright,Ispin,ISTR,LIST,MAXCHR,
     &MAXD,ML,Munit,Mxao,Mxaolm,Mxbo,Natoms,Nbas,nd
      integer Ndim,NL
      
      parameter(MAXCHR=28,MAXD=4)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      
      dimension LIST(Ndim),ISTR(80)
      integer ik(MAXD)
      
      data icomma,ileft,iright/',','(',')'/
      
      
      ML=0
100   IL=IL+1
      if(IL.LE.NL)then
      call idigit(LIST(IL),ik,nd,MAXD)
      if(ML+nd+4.LE.MAXCHR)then
      if(ML.NE.0)then
      ML=ML+1
      ISTR(ML)=icomma
      endif
      do 120 i=1,nd
      ML=ML+1
      ISTR(ML)=ik(i)
120   continue
      ML=ML+1
      ISTR(ML)=ileft
      ML=ML+1
      ISTR(ML)=ihtyp(IBO,LIST(IL))
      ML=ML+1
      ISTR(ML)=iright
      goto 100
      endif
      endif
      
      IL=IL-1
      return
      end
C* :1 * 
      
