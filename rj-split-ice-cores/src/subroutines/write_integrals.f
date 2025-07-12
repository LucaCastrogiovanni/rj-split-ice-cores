c
c ----------------------------------------------------------------------------
c
      subroutine write_integrals
c
c ----------------------------------------------------------------------------
c
c  Input/output: (none) -- all quantities are read from common blocks
c
c ----------------------------------------------------------------------------
c


      include '../trans_dim.para'
      include '../common.integral'
      include '../common.param'
      include '../common.obs_syn'
      include '../common.recipe'


c Scratch variables
      integer iq, ic, id, idiv, ii, ix
      integer tot_mod
      real*4 v(max_n_grid), cur, freq
      real*4 smean, std(max_n_grid)
      real*4 min_p, max_p, mean, diff
      real*4 delta_x
      real*4 x(max_n_grid)
      character*3 q_char, id_char
      character filename3*(namelen)
c
c
c

      write(*,*)
      write(*,*) '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
      write(*,*) '   OUTPUT OF INTEGRALS of DT and THETA'
      write(*,*) '<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<'
      write(*,*)








c
c MEAN and STD of BETA, GAMMA and Density
c

      delta_x=(t_max-t_min)/(nstep_map-1)

      iq=1

         write(q_char,'(i3)') iq
         do ic=1,3
            if(q_char(ic:ic).eq.' ') q_char(ic:ic)='0'
         enddo

      do id=1,2

         write(id_char,'(i3)') id
         do ic=1,3
            if(id_char(ic:ic).eq.' ') id_char(ic:ic)='0'
         enddo

c COMPUTE AVERAGE MODELS

         do ix=1,nstep_map

           x(ix)=t_min+(ix-1)*delta_x

           mean=0.0
           tot_mod=0
           min_p=bound_grid(ix,id,1)
           max_p=bound_grid(ix,id,2)

           do ii=1, ndiv
             cur=min_p+(1.d0*ii-0.5d0)/(1.d0*ndiv)*(max_p-min_p)
             mean=mean+1.d0*int_grid(ix,id,ii)*cur
             tot_mod=tot_mod+int_grid(ix,id,ii)
           enddo
           mean=mean/(1.d0*tot_mod)


           v(ix)=mean


           smean=0.0
           min_p=bound_grid(ix,id,1)
           max_p=bound_grid(ix,id,2)

           do ii=1, ndiv
             diff=v(ix)-( min_p+(1.d0*ii-0.5d0)/(1.d0*ndiv)*(max_p-min_p) )
             smean = smean +
     &               1.d0 * int_grid(ix,id,ii) * (diff**2)
           enddo

           std(ix)=1.d0*sqrt(smean/(1.d0*tot_mod))


         enddo

c WRITE MODELS

         filename3=
     &   'output/mean'//q_char//'.'//id_char//'.out'
c         write(*,*) ' Writing file: ',filename3,' ...'
         open(iounit1,file=filename3,status='unknown')
         write(iounit1,'(3i4)')iq, id
         do ix=1, nstep_map
           write(iounit1,'(4f20.8)' ) x(ix), v(ix), std(ix)
         enddo
        
         close(iounit1)

      enddo



      return
      end

c
