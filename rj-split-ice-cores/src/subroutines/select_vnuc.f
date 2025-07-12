

        subroutine select_vnuc(n,ic_sele)


        include '../trans_dim.para'


        include '../common.obs_syn'
        include '../common.param'
        include '../common.recipe'
        include '../common.randomtype'


c
c MAIN variables
c
        integer n, ic_sele
c
c Scratch variables
c
        integer ic
        real*4 min_prob, max_prob, delta_prob
c
c RAN3 variables
c
        real*4 p, ran3


c Randomly select a complexity

        p=ran3(iseed0)
        min_prob=0.0
        max_prob=0.0
        ic_sele=-1
        delta_prob=1.0/(1.0*n)

        do ic=1, n
          min_prob=max_prob
          max_prob=min_prob+delta_prob
          if(min_prob.lt.p.and.p.le.max_prob.and.ic_sele.lt.0)then
            ic_sele=ic
            if(info) write(*,*) ' ++ RjSPLIT:: CAND-SELE -- SELECTED NUC:', ic_sele
          endif
        enddo

        if(ic_sele.lt.0) write(*,*) ' WARNING ------------------- ic_sele = -1  ----- ADJUSTED'
        if(ic_sele.lt.0) ic_sele=n

        return
        end




