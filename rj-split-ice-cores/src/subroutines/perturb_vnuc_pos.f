

        subroutine perturb_vnuc_pos(ic_sele,n0,param0,param)


        include '../trans_dim.para'


        include '../common.obs_syn'
        include '../common.param'
        include '../common.recipe'
        include '../common.randomtype'


c
c MAIN variables
c
        integer ic_sele, n0
        real*4  param(nc_max,nd_max), param0(nc_max,nd_max)


c
c Scratch variables
c
        integer it, ic
        real*4 x0, x_new, xmin, xmax, sc
        real*4 dist
        logical too_close



c
c RAN3
c
        real*4 p, ran3





c Perturb VNUC position

          do it=1,1000

            xmin=apriori_info(1,1)
            xmax=apriori_info(1,2)


            p=ran3(iseed0)
            if(p.lt.0.5)then

              if(info)write(*,*) ' ++RjSPLIT:: PERTURB VNUC POS -- STRONG PERTURB '
                p=ran3(iseed0)
                x_new=xmin+p*(xmax-xmin)

            else

              if(info)write(*,*) ' ++RjSPLIT:: PERTURB VNUC POS -- WEAK PERTURB '
              x0=param0(ic_sele,1)
              sc=sc_rmcmc(1)
              call pick_cand_value_uniform(x0,xmin,xmax,sc,x_new)
              param(ic_sele,1)=x_new

            endif

            too_close=.false.
            do ic=1,n0
              dist=10.0*min_dist_nuc
              if(ic.ne.ic_sele)dist=abs(param(ic,1)-param(ic_sele,1))
              if(dist.lt.min_dist_nuc) too_close=.true.
            enddo

            if(too_close)write(*,'(a,i4)') 'WARN:: TOO_CLOSE -- NUCLEUS CAN NOT BE MOVED ... ', it

            if(.not.too_close) goto 203

          enddo

 203      continue



          return
          end






