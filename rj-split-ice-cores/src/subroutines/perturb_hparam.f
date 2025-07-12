

        subroutine perturb_hparam(ip_sele,hparam0,hparam)


        include '../trans_dim.para'


        include '../common.obs_syn'
        include '../common.param'
        include '../common.recipe'
        include '../common.randomtype'


c
c MAIN variables
c
        integer ip_sele
        real*4  hparam(nhparam_max), hparam0(nhparam_max)

c
c Scratch variables
c
        real*4 x0, x_new, xmin, xmax, sc


        x0=hparam0(ip_sele)
        xmin=apriori_info_hparam(ip_sele,1)
        xmax=apriori_info_hparam(ip_sele,2)
        sc=sc_hparam(ip_sele)

        call pick_cand_value_uniform(x0,xmin,xmax,sc,x_new)

        hparam(ip_sele)=x_new

     
        return
        end 
             

