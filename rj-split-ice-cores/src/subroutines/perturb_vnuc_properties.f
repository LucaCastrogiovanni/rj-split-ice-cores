

        subroutine perturb_vnuc_properties(id_sele,ic_sele,param0,param)


        include '../trans_dim.para'


        include '../common.obs_syn'
        include '../common.param'
        include '../common.recipe'
        include '../common.randomtype'


c
c MAIN variables
c
        integer id_sele,ic_sele
        real*4  param(nc_max,nd_max), param0(nc_max,nd_max)


c
c Scratch variables
c
        real*4 x0, x_new, xmin, xmax, sc


        x0=param0(ic_sele,id_sele)
        xmin=apriori_info(id_sele,1)
        xmax=apriori_info(id_sele,2)
        sc=sc_rmcmc(id_sele)

        call pick_cand_value_uniform(x0,xmin,xmax,sc,x_new)

        param(ic_sele,id_sele)=x_new



        return
        end







