
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 nbodel"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "nbodel.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "nbodel.web"
      subroutine nbodel(A,MEMORY,IDONE)
      
      
      
      
      implicit none
      double precision A,occchg
      integer i,iat,Iatno,ib,ibas,Ibxm,ich,IDONE,ii,inam,Iscr1,Iscr2,isp
     &,Ispin,itype,k,l3c,Label,lbd,lbl
      integer lblnk1,lblnk2,Lfnao,Lfnarc,Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo
     &,Lfnnab,Lfnnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lf
     &nppa,Lfnpr
      integer lhyp,MAXATM,MAXBAS,MEMORY,Munit,Mxao,Mxaolm,Mxbo,n1,n2,n3,
     &n4,n5,n6,n7,nameat,Natoms,Nbas,Nbotyp,Nbouni
      integer nctr,ndel,Ndim,nelec,nend,nmoocc,nsq,ntrunc
      logical done
      dimension A(MEMORY),ich(3,2),inam(3),isp(3)
      
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbbas/Label(MAXBAS,6),Nbouni(MAXBAS),Nbotyp(MAXBAS),Iatno(M
     &AXBAS),Ibxm(MAXBAS),Iscr1(2*MAXBAS),Iscr2(2*MAXBAS)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      data lbd/2HBD/,l3c/2H3C/,lblnk2/2H  /,lblnk1/1H /,lhyp/1H-/
      
      
      
      
      nsq=Ndim*Ndim
      n1=1
      n2=n1+Ndim
      n3=n2+Ndim
      n4=n3+nsq
      n5=n4+nsq
      n6=n5+nsq
      n7=n6+Ndim
      nend=n7+nsq/2+1
      if(nend.GT.MEMORY)then
      
      write(Lfnpr,99006)nend,MEMORY
      IDONE=1
      return
      
99001 format(/1x,'Occupations of bond orbitals:')
99002 format(7x,8F7.4)
99003 format(/7x,'Orbital',19x,'No deletions   This deletion   Change',/
     &,1x,78('-'))
99004 format(1x,i3,'. ',a2,a1,'(',i2,')',3(a2,3A1),9x,f7.5,8x,f7.5,3x,f8
     &.5)
99005 format(/)
99006 format(/1x,'Insufficient memory in subroutine NBODEL:',/5x,'Memory
     & needed: ',i10,'   Memory available: ',i10,/1x,'Deletions halted!'
     &)
      else
      call fenbo(A(n3),A(n1),A(n5),nelec)
      call fefnbo(A(n4))
      
      
      
      call delete(A(n4),A(n5),Ndim,A(n7),nsq,itype,ndel,ntrunc,done,Ispi
     &n)
      
      
      if(.NOT.(done))then
      
      call jacobi(ntrunc,A(n5),A(n2),A(n4),Ndim,Ndim,0)
      
      
      nmoocc=nelec
      if(Ispin.EQ.0)nmoocc=nelec/2
      call newdm(A(n5),A(n4),A(n2),Ndim,A(n7),nsq,ndel,itype,nmoocc,Ispi
     &n)
      
      
      call trnspo(A(n3),Ndim,Ndim)
      call simltr(Ndim,Ndim,A(n5),A(n3),A(n4),A(n6),1)
      call svnewd(A(n5))
      
      write(Lfnpr,99001)
      write(Lfnpr,99003)
      do 20 ibas=1,Ndim
      ib=Ibxm(ibas)
      lbl=Label(ib,1)
      nctr=1
      if(lbl.EQ.lbd)nctr=2
      if(lbl.EQ.l3c)nctr=3
      do 10 i=1,3
      iat=Label(ib,i+3)
      call convrt(iat,ich(i,1),ich(i,2))
      inam(i)=lblnk2
      if(iat.GT.0)inam(i)=nameat(Iatno(iat))
      isp(i)=lhyp
      if(i.GE.nctr)isp(i)=lblnk1
10    continue
      i=n1-1+ibas
      ii=n2-1+ibas
      occchg=A(ii)-A(i)
      write(Lfnpr,99004)ibas,(Label(ib,k),k=1,3),(inam(k),ich(k,1),ich(k
     &,2),isp(k),k=1,3),A(i),A(ii),occchg
20    continue
      IDONE=0
      return
      endif
      endif
      
      IDONE=1
      return
      end
C* :1 * 
      
