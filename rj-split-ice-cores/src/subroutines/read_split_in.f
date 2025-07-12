

        subroutine read_split_in


c Read parameter file: input/MDL/prior.info and store value in COMMON BLOCK

        include '../trans_dim.para'

        include '../common.param'
        include '../common.recipe'
        include '../common.obs_syn'


        integer yesno
c


        open(12,file='input/MDL/prior.info',status='old')

        read(12,*)
        read(12,*)
        read(12,*)
        read(12,*)

c Read MIN/MAX number of complexities    
        read(12,*) min_c, max_c
       
c Read MIN-DIST between complexities    (WARN:: this must be in days )
        read(12,*)min_dist_nuc
c Read the NUMBER OF DIMENSIONS of each complexity
        read(12,*)nd

        read(12,*)

c Read MIN/MAX/SCALE for TIME dimension 
        read(12,*)apriori_info(1,1),apriori_info(1,2),sc_rmcmc(1)
c Read MIN/MAX/SCALE for BETA  dimension       
        read(12,*)apriori_info(2,1),apriori_info(2,2),sc_rmcmc(2)
c Read MIN/MAX/SCALE for GAMMA dimension
        read(12,*)apriori_info(3,1),apriori_info(3,2),sc_rmcmc(3)

        read(12,*)
        read(12,*)

c Read MIN/MAX/SCALE for the h-param -- SCALE for Errors on dCdt
        read(12,*)apriori_info_hparam(1,1),apriori_info_hparam(1,2),sc_hparam(1)



c Read if noise has to be added to synthetics

        added_noise=.false.
        read(12,*)
        read(12,*)
        read(12,*)yesno
        if(yesno.eq.1)added_noise=.true.

        close(12)







        return
        end


