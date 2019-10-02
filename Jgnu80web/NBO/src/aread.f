
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 aread"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "aread.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "aread.web"
      subroutine aread(A,MR,NR,NC,JOB,LFN,ERROR)
      implicit none
      double precision A
      integer i,ialfa,ibeta,idash,istr,itemp,j,JOB,LFN,MR,NC,NR
      dimension A(MR,1),JOB(20)
      dimension itemp(20)
      logical ERROR
      
      common/nbflag/Rohf,Uhf,Ci,Open,Complex,Alpha,Beta,Mcscf,Auhf,Ortho
      logical Rohf,Uhf,Ci,Open,Complex,Alpha,Beta,Mcscf,Auhf,Ortho
      
      data idash,ialfa,ibeta/4H----,4HALPH,4HBETA/
      
      
      
      
      
      
      
      
      if(Alpha.OR..NOT.Open)read(LFN,99002,end=300)JOB
      if(.NOT.Open)istr=idash
      if(Alpha)istr=ialfa
      if(Beta)istr=ibeta
      
100   read(LFN,99002,end=300)itemp
      if(itemp(1).NE.istr)goto 100
      
      
      if(abs(NR).EQ.abs(NC).AND.NR.LT.0)then
      read(LFN,99001,end=300)((A(i,j),i=1,j),j=1,abs(NR))
      do 150 j=1,abs(NR)-1
      do 120 i=j+1,abs(NR)
      A(i,j)=A(j,i)
120   continue
150   continue
      else
      do 200 j=1,abs(NC)
      read(LFN,99001,end=300)(A(i,j),i=1,abs(NR))
200   continue
      endif
      ERROR=.FALSE.
      return
      
300   ERROR=.TRUE.
      return
      
99001 format(1x,5F15.9)
99002 format(1x,20A4)
      end
C* :1 * 
      
