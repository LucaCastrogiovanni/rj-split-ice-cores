c
c
c	subroutine FORWARD
c
c--------------------------------------------------------------------
c
c INPUT:
c         n --   number of complexities
c         param -- parameter values for each complexity
c
c OUTPUT:
c         (none) -- syn_data is stored in a COMMON BLOCK
c
c--------------------------------------------------------------------
c
c
        subroutine forward(n,param)


        include '../trans_dim.para'


        include '../common.obs_syn'
        include '../common.param'
        include '../common.recipe'


c
c MAIN variables

        integer n
        real*4  param(nc_max,nd_max)



c
c Scratch variables
 
        integer ic, id, ic_nearest
        real*4  dist, dist_min
        real*4 beta, gamma


          if(info)then
          write(*,'(a)') ' ++RjSPLIT:: MODEL for FORWARD: '
          do ic=1,n
            write(*,'(a,i4,3f16.4)') ' ++RjSPLIT:: NUCLEUS: ',ic,(param(ic,id), id=1,nd)
          enddo
          write(*,*)
          endif




        do id=1,ndata

c Find the "nearest neighbour" of id-th data

           ic_nearest=-1
           dist_min=10e20

           do ic=1,n

             dist=abs(param(ic,1)-obs_D(id,1))

             if(dist.lt.dist_min)then
               dist_min=dist
               ic_nearest=ic
             endif
         
           enddo


           beta = param(ic_nearest,2)
           gamma = param(ic_nearest,3)

           syn_D(id,1) = gamma - beta*temp(id,2)
c            syn_D(id,1) = gamma*temp(id,2) - beta


c
c END DO-LOOP over NDATA
c
        enddo

        if(info)then
          write(*,*)
          write(*,'(a)') ' ++RjSPLIT::  FORWARD:'
          write(*,'(a)') ' ++RjSPLIT::  Obs_D and syn_D (first 20 values) '
          write(*,'(a,20f8.3)') 'CO2-SYN:',(syn_D(id,1), id=1,20)
          write(*,'(a,20f8.3)') 'CO2-OBS:',(obs_D(id,2), id=1,20)
          write(*,*)
        endif


        return
        end
