
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 awrite"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "awrite.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "awrite.web"
      subroutine awrite(A,MR,NR,NC,TITLE,LFN)
      implicit none
      double precision A
      integer i,j,job,LFN,Lfnao,Lfnarc,Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo,L
     &fnnab,Lfnnao,Lfnnbo,Lfnnho,Lfnnlm,lfnout,Lfnpna,Lfnpnb,Lfnpnh
      integer Lfnpnl,Lfnppa,Lfnpr,MR,NC,NR
      dimension A(MR,1)
      character*80 TITLE
      dimension job(20)
      
      common/nbflag/Rohf,Uhf,Ci,Open,Complex,Alpha,Beta,Mcscf,Auhf,Ortho
      logical Rohf,Uhf,Ci,Open,Complex,Alpha,Beta,Mcscf,Auhf,Ortho
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      
      
      lfnout=abs(LFN)
      if(lfnout.EQ.Lfnpr)write(lfnout,99001)
      if(Alpha.OR..NOT.Open.OR.lfnout.EQ.Lfnpr)then
      call fetitl(job)
      write(lfnout,99002)job
      write(lfnout,99003)TITLE(1:79)
      endif
      if(Alpha)write(lfnout,99004)
      if(Beta)write(lfnout,99005)
      
      
      if(abs(NR).EQ.abs(NC).AND.NR.LT.0)then
      write(lfnout,99006)((A(i,j),i=1,j),j=1,abs(NR))
      else
      do 50 j=1,abs(NC)
      write(lfnout,99006)(A(i,j),i=1,abs(NR))
50    continue
      endif
      return
      
99001 format(/1x)
99002 format(1x,19A4,a3)
99003 format(1x,a79,/1x,79('-'))
99004 format(1x,'ALPHA SPIN')
99005 format(1x,'BETA  SPIN')
99006 format(1x,5F15.9)
      end
C* :1 * 
      
