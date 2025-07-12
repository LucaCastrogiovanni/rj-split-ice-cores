        subroutine birth_vnuc(n0,param0,n,param)


        include '../trans_dim.para'

        include '../common.obs_syn'
        include '../common.param'
        include '../common.recipe'
        include '../common.randomtype'


c
c MAIN variables
c
        integer n0, n
        real*4  param(nc_max,nd_max), param0(nc_max,nd_max)


c
c Scratch variables
c
        integer it, ic
        real*4 dist, x_new
        logical too_close

c
c RAN3 variables
c
        real*4 p, ran3



c A new complexity is created

c Select a new position

        do it=1,1000

          p=ran3(iseed0)
          x_new=apriori_info(1,1)+p*(apriori_info(1,2)-apriori_info(1,1))

          too_close=.false.
          do ic=1,n0
            dist=abs(param0(ic,1)-x_new)
            if(dist.lt.min_dist_nuc) too_close=.true.
          enddo

          if(.not.too_close)then
            param(n,1)=x_new
            goto 202
          endif

        enddo

 202    continue

c Select new values for DT and THETA from PRIOR

 
        p=ran3(iseed0)
        param(n,2)= apriori_info(2,1)+p*(apriori_info(2,2)-apriori_info(2,1))
        p=ran3(iseed0)
        param(n,3)= apriori_info(3,1)+p*(apriori_info(3,2)-apriori_info(3,1))




        return
        end






