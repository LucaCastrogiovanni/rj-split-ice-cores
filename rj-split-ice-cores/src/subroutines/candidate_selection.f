c
c
c       subroutine CANDIDATE_SELECTION
c
c---------------------------------------------------------------------------------
c
c INPUT:
c         n0 --   number of complexities in curent model
c         param0 -- parameter values for each complexity in curent model
c         hparam0 -- scaling factor for noise
c
c OUTPUT:
c         n --   number of complexities in candidate model
c         param -- parameter values for each complexity in candidate model
c         hparam -- scaling factor for noise
c         move_value -- selected MOVE in the recipe
c
c---------------------------------------------------------------------------------
c
c RECIPE includes seven moves, each diffrent probability:
c
c (1) Perturb DT hyper-parameter  (move_value=1)
c (2) Perturb TIME of one complexity (move_value=2)
c (3) Perturb BETA of one complexity (move_value=3)
c (4) Perturb GAMMA of one complexity (move_value=4)
c (5) Birth of a new complexity (move_value=5)
c (6) Death of a new complexity (move_value=6)
c
c---------------------------------------------------------------------------------
c
c
        subroutine candidate_selection(n0,param0,hparam0,n,param,hparam,move_value)


        include '../trans_dim.para'


        include '../common.obs_syn'
        include '../common.param'
        include '../common.recipe'
        include '../common.randomtype'


c
c MAIN variables
c
        integer n, n0, move_value
        real*4  param(nc_max,nd_max), param0(nc_max,nd_max)
        real*4  hparam(nhparam_max), hparam0(nhparam_max)


c
c Scratch variables
c 
        integer imove,  ic, ip, id, ic_sele, n_cand
        real*4 min_prob, max_prob


c
c RAN3 variables
c
        real*4 p, ran3




c
c 1. CANDIDATE MODEL IS EQUAL TO CURRENT (and after, it is perturbed)
c 
        n=n0
        do ic=1,n
          do id=1,nd
            param(ic,id)=param0(ic,id)
          enddo
        enddo
c        do ip=1,nhparam_max
c        hparam(ip)=hparam0(ip)
         hparam = hparam0
c        enddo

        if(info)then

          write(*,*) 
          write(*,*)
          write(*,'(a)') ' ++RjSPLIT:: CANDIDATE SELECTION -- '
          write(*,'(a)') ' ++RjSPLIT:: CURRENT MODEL: '
          do ic=1,n
            write(*,'(a,i4,3f16.4)') ' ++RjSPLIT:: NUCLEUS: ',ic,(param(ic,id), id=1,nd)
          enddo
c          write(*,'(a,1f16.4)')' ++RjSPLIT:: HPARA: ',(hparam(1))
          write(*,'(a,1f16.4)')' ++RjSPLIT:: HPARA: ',(hparam)
          write(*,*)

        endif




c
c 2. SELECT A MOVE
c
        p=ran3(iseed0)

        min_prob=0.0
        max_prob=0.0
        move_value=-1

        do imove=1, 5
          min_prob=max_prob
          max_prob=min_prob+move_prob(imove)
          if(min_prob.lt.p.and.p.le.max_prob.and.move_value.lt.0)then
            if(info) write(*,*) ' ++ RjSPLIT:: CAND-SELE -- Selected move:', imove
            move_value=imove
          endif
        enddo





c
c
c 3. PERTURB CANDIDATE
c
c

c
c
c MOVE_VALUE=1 --  HPARAM PERTURB

        if(move_value.eq.1)then

          call perturb_hparam(1,hparam0,hparam)


c
c
c MOVE_VALUE=2 -- TIME perturb

        else if(move_value.eq.2)then

          call select_vnuc(n,ic_sele)
          call perturb_vnuc_pos(ic_sele,n,param0,param)

c
c
c MOVE_VALUE=3 -- BETA perturb

        else if(move_value.eq.3)then

          call select_vnuc(n,ic_sele)
          call perturb_vnuc_properties(2,ic_sele,param0,param)

c
c
c MOVE_VALUE=4 -- GAMMA perturb

        else if(move_value.eq.4)then

          call select_vnuc(n,ic_sele)
          call perturb_vnuc_properties(3,ic_sele,param0,param)

c
c
c MOVE_VALUE=5/6 -- Birth/Death of a complexity

        else if(move_value.eq.5)then

          call pick_cand_n_value_uniform(n0,min_c,max_c,n_cand)
          n=n_cand
         
          if(n.lt.n0)then

            move_value=6
          
            if(info) write(*,'(2(a,i4))') ' ++ RjSPLIT:: DEATH MOVE:', n0, ' --> ', n

            call select_vnuc(n0,ic_sele)
            call death_vnuc(ic_sele,param0,n,param)

          endif

          if(n.gt.n0)then

             move_value=5

             if(info) write(*,'(2(a,i4))') ' ++ RjSPLIT:: BIRTH: MOVE:', n0, ' --> ', n

             call birth_vnuc(n0,param0,n,param)

          endif

          if(n.eq.n0)then
            if(info) write(*,'(a,i4)') ' ++ RjSPLIT:: MODEL IS NOT PURTURBED -- move_value=',move_value
          endif


        endif





        if(info)then

          write(*,*)
          write(*,'(a)') ' ++RjSPLIT:: CANDIDATE MODEL: '
          do ic=1,n
            write(*,'(a,i4,3f16.4)') ' ++RjSPLIT:: NUC: ',ic,(param(ic,id), id=1,nd)
          enddo
c          write(*,'(a,f16.4)')' ++RjSPLIT:: HPARA: ',hparam(1)
          write(*,'(a,f16.4)')' ++RjSPLIT:: HPARA: ',hparam
          write(*,*)


        endif






        return
        end
