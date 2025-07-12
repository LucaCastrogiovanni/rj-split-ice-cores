c
c ----------------------------------------------------------------------------
c
      subroutine write_mean_synthetics
c
c ----------------------------------------------------------------------------
c
c  Input/output: (none) -- all quantities are read from common blocks
c
c ----------------------------------------------------------------------------
c


      include '../trans_dim.para'
      include '../common.obs_syn'
      include '../common.obs_syn2'
      include '../common.recipe'


c Scratch variables
      integer id
      integer tot
      real*4 mean1, std1, mean2, std2, x
      character filename3*(namelen)
c
c
c

       write(*,*)
       write(*,*) '++++++++++++++++++++++++++++++++++++++++++++++'
       write(*,*) '   WRITE MEAN SYNTHETICS for DT and THETA '
       write(*,*) '++++++++++++++++++++++++++++++++++++++++++++++'
       write(*,*)
c
c
         tot=int((ntot-mod_burn_in)/nsample)*nchains
         write(*,*) ' Total collected models [(NTOT-BURN_IN)/NSAMPLE] :', tot
c
c
c
c
         filename3='output/mean_syn.001.dat'
         write(*,*) ' Writing file: ',filename3,' ...'
         open(iounit1,file=filename3,status='unknown')
         write(iounit1,'(a)') '# MEAN SYNTHETICS for  CO2 '
         do id=1, ndata
            mean1=mean_syn_D(id,1)/tot
            x=(mean_syn_D2(id,1)/tot)-mean1**2
            if(x.lt.1e-8)x=1.e-8
            std1=sqrt(x)
            write(iounit1,'(i5,4f18.6)')id, obs_D(id,1), obs_D(id,2),  mean1, std1 
         enddo
         close(iounit1)





      return
      end

c
