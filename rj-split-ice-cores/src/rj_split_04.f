c--------------------------------------------------------------------------------------------
c
c  RJ-SPLIT::
c
c   This algorithm performs:
c    --> Classical McMC
c    --> Trans-D sampling
c    --> Hierarchical Bayes 
c    --> Joint inversion
c
c--------------------------------------------------------------------------------------------
c
c  The Observed data:
c
c         obs_D(i,1)= TIME of the i-th event
c         obs_D(i,2)= dCdt of the i-th event
c         obs_D(i,3)= error on dCdt for the i-th event
c
c--------------------------------------------------------------------------------------------
c
c  The forward problem considers a Zero-order (i.e. constant value for the entire surface)
c  simple interpolation of the Voronoi ensamble over a 1D-space. The nearest Voronoi nucleus 
c  to a given point determines the β and γ in that point, i.e.:
c
c    syn_D(i,1)=dCdt_near, where dCdt_near is the dCdt for the nearest nucleus to obs(i,1)
c
c---------------------------------------------------------------------------------------------
c
c  The model:
c
c     n - number of 1D Voronoi nuclei
c
c     param -- time, β, γ: t- coord, β and γ are splitting params for each nucleus
c     hparam -- Scale parameter for the standard deviation (one data-set, one value)
c
c---------------------------------------------------------------------------------------------
c
c
c   FLOW-CHART
c
c 
c   1. User initialization:
c   1.1 Read parameters for the random walk
c   1.2 Read parameters for the model space
c   1.3 Read observed data
c   1.4 Initialize integrals quantities

c   (start multi-chain inversion)
c    
c   1.5 Get initial model (model Zero)
c
c ------------------> this is the main Loop
c
c   2. Perturb the current model (or model Zero)
c
c   3. Compute Forward prediction
c
c   4. Compute Misfit between observed and predicted β and γ
c
c   5. Metropolis's rule: substitute current model with candidate or keep it!
c
c   6. Write integrals (after BURN-IN phase)
c
c   7. Go back to point (2)
c
c ------------------> end of the main Loop
c
c   (end multi-chain inversion)
c
c   8. Finalize and exit
c
c

c----------------------------------------------------------------------------------------------



         program RJ_SPLIT


         include 'trans_dim.para'

         include 'common.obs_syn'
         include 'common.param'
         include 'common.randomtype'
         include 'common.misfit'
         include 'common.recipe'



c
c MAIN variables

         integer n, n0
         real*4  param(nc_max,nd_max), param0(nc_max,nd_max)
         real*4  hparam(nhparam_max), hparam0(nhparam_max)




c
c Scratch variables

         integer accepted, move_value
         integer ic, id, im, ichain
         real*4  lh_norm, lppd





c 
c   1. User initialization: 

c   1.1 Read parameters for the random walk

         write(*,*) ' ++RjSPLIT:: Read rmcmc params ...'
         call read_rmcmc_in

c   1.2 Read observed data

         write(*,*) ' ++RjSPLIT:: Read OBSERVED data ...'
         call read_data

c   1.3 Read parameters for the model space

         write(*,*) ' ++RjSPLIT:: Read PARAMETER space info ...'
         call read_split_in

c   1.4 Initialize integrals quantities

         write(*,*) ' ++RjSPLIT:: initialize integrals ...'
         call integrals_bound



c
c MAIN LOOP 
c

         write(*,*) 
         write(*,*) ' ++RjSPLIT::' 
         write(*,*) ' ++RjSPLIT:: STARTING THE MAIN INVERSION LOOP'
         write(*,*) ' ++RjSPLIT:: '
         write(*,*)

c
c Start serial, multi-chain inversion
c 

         do ichain = 1, nchains

        

c   1.5 Get initial model (model Zero)

         write(*,*) ' ++RjSPLIT:: ramdomly pick the initial model ...'
         call get_initial_model(n0,param0,hparam0)


         do im=1, ntot

           if(im.eq.1.or.mod(im,100).eq.0) write(*,'(2(a,i10),a,i5)')' ++RjSPLIT:: MODEL:',im,' out of ',ntot, ' chain:', ichain

           if(im.eq.1)then
c Model Zero becomes candidate model
             n=n0
             do ic=1,n
               do id=1,nd
                 param(ic,id)=param0(ic,id)
               enddo
             enddo
             hparam(1)=hparam0(1)
             move_value=1
           else

c
c   2. Perturb the current model (or model Zero)
c
             if(info) write(*,'(a)') ' ++RjSPLIT:: CANDIDATE SELECTION ...'
             call candidate_selection(n0,param0,hparam0,n,param,hparam,move_value)

           endif
c
c   3. Compute Forward prediction
c
           if(info) write(*,'(a)') ' ++RjSPLIT:: FORWARD MODELLING ... '
           call forward(n,param)

c
c   4. Compute Misfit between observed and predicted time-series 
c
           if(info) write(*,'(a)') ' ++RjSPLIT:: MISFIT COMPUTATION ... '
           call misfit(hparam,lh_norm,lppd)

c
c   5. Metropolis's rule: substitute current model with candidate or keep it!
c
           if(info) write(*,'(a)') ' ++RjSPLIT:: Apply METROPOLIS rule ... '
           call metropolis(im,lh_norm,lppd,accepted)

           if(accepted.eq.0)then
c Candidate has been rejected -- current model becomes candidate model for integrals computation
             n=n0
             do ic=1,n
               do id=1,nd
                 param(ic,id)=param0(ic,id)
               enddo
               hparam(1)=hparam0(1)
             enddo
           endif
     
c
c   6. Write integrals (after BURN-IN phase) -- with chain thinning
c
           if(mod(im,nsample).eq.0)write(99,202) 'MOD:',ichain,im, move_value, accepted,lppd,lppd0,lh_norm,lh_norm0,n,hparam(1)
           if(mod(im,nsample).eq.0)call write_accepted_model(n,param,ichain,im,move_value,accepted,lppd0)
           



 202       format(a,i3,i10,i5,i18, 4f24.8,i6,f15.5)

           if(im.ge.mod_burn_in)then

             if(mod(im,nsample).eq.0)then

               call save_synthetics(accepted)
c               call acceptance_statistics(move_value,accepted)
               call compute_integrals(n,param,hparam)

             endif

           endif

c Candidate model becomes current model

           n0=n
           do ic=1,n0
             do id=1,nd
               param0(ic,id)=param(ic,id)
             enddo
           enddo
           hparam0(1)=hparam(1)

c
c   7. Go back to point (2)
c
         enddo


c
c End serial, multi-chain inversion
c
         enddo




        
c        
c   8. Finalize and exit
c

        call write_mean_synthetics
        call write_last_synthetics(hparam)
        call write_statistics
        call write_integrals


        stop
        end
