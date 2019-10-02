
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 wrbas"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "wrbas.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "wrbas.web"
      subroutine wrbas(SCR,ISCR,LFN)
      
      
      implicit none
      integer i,i1,i2,i3,i4,i5,i6,i7,i8,i9,Iatcr,Iatno,Ino,ISCR,Ispin,Iz
     &nuc,j,j1,j2,j3
      integer j4,k,Lang,Lctr,LFN,Lfnao,Lfnarc,Lfndaf,Lfndef,Lfndm,Lfnin,
     &Lfnmo,Lfnnab,Lfnnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh
      integer Lfnpnl,Lfnppa,Lfnpr,Ll,Lu,MAXATM,MAXBAS,Munit,Mxao,Mxaolm,
     &Mxbo,Natoms,Nbas,ncomp,Ndim,nexp,Norbs,nprim,nptr,nshell
      double precision SCR
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),L
     &u(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbao/Lctr(MAXBAS),Lang(MAXBAS)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      
      dimension SCR(1),ISCR(1)
      
      
      call febas(nshell,nexp,ISCR)
      
      
      if(nshell.EQ.0)then
      write(Lfnpr,99001)
      return
      endif
      
      
      
      
      
      i1=3
      i2=i1+nshell
      i3=i2+nshell
      i4=2+(3*nshell+1)/2
      i5=i4+nexp
      i6=i5+nexp
      i7=i6+nexp
      i8=i7+nexp
      i9=i8+nexp
      
      
      call fetitl(SCR(i9))
      
      
      write(LFN,99002)(SCR(i9+i),i=0,9)
      write(LFN,99003)Natoms,nshell,nexp
      
      
      call fecoor(SCR(i9))
      
      
      j=0
      do 100 i=1,Natoms
      write(LFN,99004)Iatno(i),(SCR(i9+j+k),k=0,2)
      j=j+3
100   continue
      write(LFN,99005)
      
      
      
      
      
      
      
      j1=1
      j2=i1
      j3=i3
      j4=i2
      do 200 i=1,nshell
      ncomp=ISCR(j2)
      nprim=ISCR(j3)
      nptr=ISCR(j4)
      write(LFN,99006)Lctr(j1),ncomp,nprim,nptr
      write(LFN,99006)((Lang(j1+j)),j=0,ncomp-1)
      j1=j1+ncomp
      j2=j2+1
      j3=j3+1
      j4=j4+1
200   continue
      write(LFN,99005)
      
      
      write(LFN,99007)(SCR(i4+i),i=0,nexp-1)
      write(LFN,99008)
      write(LFN,99007)(SCR(i5+i),i=0,nexp-1)
      write(LFN,99008)
      write(LFN,99007)(SCR(i6+i),i=0,nexp-1)
      write(LFN,99008)
      write(LFN,99007)(SCR(i7+i),i=0,nexp-1)
      write(LFN,99008)
      write(LFN,99007)(SCR(i8+i),i=0,nexp-1)
      return
      
99001 format(/1x,'No basis set information is stored on the NBO direct',
     &' access file.',/1x,'Thus, no `AOINFO'' file can be written.')
99002 format(1x,9A8,a7,/1x,'Basis set information needed for plotting ',
     &'orbitals',/1x,75('-'))
99003 format(1x,3I6,/1x,75('-'))
99004 format(1x,i4,3(2x,f12.9))
99005 format(1x,75('-'))
99006 format(1x,10I6)
99007 format(2x,4E18.9)
99008 format(1x)
      end
C* :1 * 
      
