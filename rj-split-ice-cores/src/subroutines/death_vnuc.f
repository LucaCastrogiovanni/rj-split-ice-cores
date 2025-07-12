        subroutine death_vnuc(ic_sele,param0,n,param)


        include '../trans_dim.para'


        include '../common.obs_syn'
        include '../common.param'
        include '../common.recipe'
        include '../common.randomtype'


c
c MAIN variables
c
        integer ic_sele, n
        real*4  param(nc_max,nd_max), param0(nc_max,nd_max)


c
c Scratch variables
c
        integer ic, id

c
c Re-ordering PARAM array

        do ic=ic_sele,n
          do id=1,nd
            param(ic,id)=param0(ic+1,id)
          enddo
        enddo


        return
        end







