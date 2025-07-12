c
c
c       subroutine GET_INITIAL_MODEL
c
c---------------------------------------------------------------------------------
c
c INPUT:
c         (none)
c
c OUTPUT:
c         n0 --   number of complexities in curent model
c         param0 -- parameter values for each complexity in curent model
c         hparam0 -- NOISE LEVEL
c
c---------------------------------------------------------------------------------
c
c GET_INITIAL_MODEL:
c
c (1) N will be posed at a minimum
c (2) PARAM0 will be sampled randomly
c (3) HPARAM0 will be sampled randomly
c
c---------------------------------------------------------------------------------
c
c
        subroutine get_initial_model(n0,param0,hparam0)


        include '../trans_dim.para'


        include '../common.obs_syn'
        include '../common.param'
        include '../common.recipe'
        include '../common.randomtype'

c
c MAIN variables
c
        integer n0
        real*4  param0(nc_max,nd_max)
        real*4  hparam0(nhparam_max)


c
c Scratch variables
c 
        integer ic, id, ip

c
c RAN3 variables
c
        real*4 p, ran3


c
c Check for a imposed starting model
c

	open(10,file='start_model.dat',status='old',ERR=102)

        read(10,*)
        read(10,*)
        read(10,*)
        read(10,*) n0
        do ic=1,n0
          read(10,*) (param0(ic,id),id=1,nd)
        enddo
        read(10,*)
        read(10,*) (hparam0(ip), ip=1,nhparam_max)

        close (10)

        goto 104


 102    continue

c
c
c RAMDOM STARTING MODEL
c
c
c  (1) N0=1


c        n0=min_c
        p=ran3(iseed0)
        n0 = min_c + int(p*(max_c - min_c))
        

c
c  (2) PARAM0 from APRIORI_INFO

        do ic=1,n0
          p=ran3(iseed0)
          param0(ic,1)=apriori_info(1,1)+p*(apriori_info(1,2)-apriori_info(1,1))
          p=ran3(iseed0)
          param0(ic,2)=apriori_info(2,1)+p*(apriori_info(2,2)-apriori_info(2,1))
          p=ran3(iseed0)
          param0(ic,3)=apriori_info(3,1)+p*(apriori_info(3,2)-apriori_info(3,1))
        enddo

c
c (3) HYPER-PARAMETERS

        p=ran3(iseed0)
        hparam0(1)=apriori_info_hparam(1,1)+p*(apriori_info_hparam(1,2)-apriori_info_hparam(1,1))


 104    continue



          write(*,*)
          write(*,'(a)') ' ++RjSPLIT:: STARTING MODEL -- '
          do ic=1,n0
            write(*,'(a,i4,3f16.4)') ' ++RjSPLIT:: NUCLEUS: ',ic,(param0(ic,id), id=1,nd)
          enddo
          write(*,'(a,f16.4)')' ++RjSPLIT:: HPARA:',hparam0,(hparam0(ip), ip=1,nhparam_max)
          write(*,*)



        return
        end
