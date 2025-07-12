
      subroutine integrals_bound
c
c integrals_bound_1d -- computes min/max values for integrals 
c
      
      include '../trans_dim.para'
      include '../common.param'
      include '../common.integral'
      include '../common.recipe'





c
c Scratch variables
      integer ix, ip


c
c
c INITIALIZE INT for HYPER-PARAMETERS
c
c


        do ip=1,nhparam_max
          bound_hparam(ip,1)=apriori_info_hparam(ip,1)
          if(info) write(*,201)' H-PARAM: ',ip,' between:', bound_hparam(ip,1)
        enddo

 201  format(a,i3,a,f9.3)
        



c
c
c  number of complexities for nuclei quality
c
      bound_nc(1)=min_c
      bound_nc(2)=max_c

      if(info) write(*,204)' Number of NUCLEI: Int. between:', bound_nc(1), bound_nc(2)

 204  format(a,2i6)

c
c Position of the boundary b/w Voronoi Nuclei              
c

      bound_pos(1)= apriori_info(1,1)
      bound_pos(2)= apriori_info(1,2)




c
c BETA, GAMMA and Nuclei density Properties  
c


      do ix=1, nstep_map

        bound_grid(ix,1,1)= apriori_info(2,1)
        bound_grid(ix,1,2)= apriori_info(2,2)
        bound_grid(ix,2,1)= apriori_info(3,1)
        bound_grid(ix,2,2)= apriori_info(3,2)

      enddo




      return
      end


c----------------------------------------------------------------------------------------
c
c
c
c

c
      subroutine compute_integrals(n,param,hparam)
c
c compute_integrals -- computes integrals 
c

      
      include '../trans_dim.para'
      include '../common.param'
      include '../common.recipe'
      include '../common.obs_syn'
      include '../common.integral'



      integer n
      real*4  param(nc_max,nd_max), hparam(nhparam_max)
c
c
c
c Scratch variables
      integer ic, ix, idiv, cur_n_c, ip, ib
      integer ic_nearest
      real*4  x_nuc_unsort(nc_max), x_nuc(nc_max)
      real*4  cur, min_p, max_p, delta_x, x, ib_pos
      real*4  dist, dist_min, beta, gamma


c
c HYPER-PARAMETERS
c

        do ip=1,nhparam_max

          cur=hparam(ip)
          min_p=bound_hparam(ip,1)
          max_p=bound_hparam(ip,2)

          idiv=int(ndiv*(cur-min_p)/(max_p-min_p))+1
          if(idiv.le.0)idiv=1
          if(idiv.gt.ndiv)idiv=ndiv

          int_hparam(ip,idiv)=int_hparam(ip,idiv)+1      

        enddo


c
c
c  number of complexities for each quality
c
c WARNING: in this case, a discrete variable, integral is recovered for each discrete value
c
         
        cur_n_c=n
        int_nc(cur_n_c)=int_nc(cur_n_c)+1


c
c compute boundaries position
c
c N. of boundaries should be N-1

        do ic  = 1, n
          x_nuc_unsort(ic)=param(ic,1)
        enddo
   

        call sort(x_nuc_unsort,n,x_nuc)



        do ib=2, n

          ib_pos=0.5*(x_nuc(ib)+x_nuc(ib-1))

          min_p=bound_pos(1)
          max_p=bound_pos(2)

          idiv=int(nstep_map*(ib_pos-min_p)/(max_p-min_p))+1

          if(idiv.le.0)idiv=1
          if(idiv.gt.nstep_map)idiv=nstep_map

          int_pos(idiv)=int_pos(idiv)+1


        enddo






c
c BETA and GAMMA
c

      delta_x=1.0*((t_max-t_min)/(nstep_map-1))

      do ix=1, nstep_map

           x=t_min+(ix-1)*delta_x

c Nearest complexity

           ic_nearest=-1
           dist_min=10e20

           do ic=1,n

             dist=abs(param(ic,1)-x)

             if(dist.lt.dist_min)then
               dist_min=dist
               ic_nearest=ic
             endif

           enddo

c Local BETA

           beta=param(ic_nearest,2)
           min_p=bound_grid(ix,1,1)
           max_p=bound_grid(ix,1,2)

           idiv=int(ndiv*(beta-min_p)/(max_p-min_p))+1

           if(idiv.le.0)idiv=1
           if(idiv.gt.ndiv)idiv=ndiv

           int_grid(ix,1,idiv)=int_grid(ix,1,idiv)+1

c Local GAMMA

           gamma=param(ic_nearest,3)
           min_p=bound_grid(ix,2,1)
           max_p=bound_grid(ix,2,2)

           idiv=int(ndiv*(gamma-min_p)/(max_p-min_p))+1

           if(idiv.le.0)idiv=1
           if(idiv.gt.ndiv)idiv=ndiv

           int_grid(ix,2,idiv)=int_grid(ix,2,idiv)+1


      enddo


      return
      end

