c
c ----------------------------------------------------------------------------
c
      subroutine write_accepted_model(n,param,ichain,imod,move_value,accepted,lppd0)
c
c write_statistics -- write MOVE statistics to file
c
c
c

      include '../trans_dim.para'
      include '../common.rmcmc_stat'
      include '../common.recipe'
      include '../common.param'

      integer n, ichain, imod, move_value, accepted
      integer ic, id
      integer itime_sorted(nc_max)

      real*4  lppd0, param(nc_max,nd_max)
      real*4  hparam(nhparam_max)
      real*4  time_cp(nc_max)
      real*4  param_sorted(nc_max,nd_max)


c
c    Sorting changepoints
c
c      do ic = 1, n
c        time_cp(ic)=param(ic,1)
c      enddo

c      call indexx(n,time_cp,itime_sorted)

c      do ic=1,n
c        do id=1,nd
c          param_sorted(ic,id)=param(itime_sorted(ic),id)
c        enddo
c      enddo


      write(98,'(A)')' MODfromPPD:: +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
      write(98,'(A,i6,i10,2i8)') ' MODfromPPD:: STAT: ',ichain, imod, move_value, accepted
      write(98,'(A,i6,i10,4f16.4)') ' MODfromPPD:: STAT: ',ichain, imod, lppd0
      write(98,'(A,i6,i10,3i12)') ' MODfromPPD:: STAT: ',ichain, imod, n
      do ic=1,n
        write(98,'(A,i6,i12,i10,20f20.6)') ' MODfromPPD:: PARA: ',ichain, imod, ic, (param(ic,id),id=1,nd)
      enddo
      write(98,'(A)')' MODfromPPD:: +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'

      return
      end
c
c

