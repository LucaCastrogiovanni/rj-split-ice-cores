
      subroutine pick_cand_n_value_uniform(n0,min,max,n_cand)

      implicit none

      include '../common.randomtype'

      integer n0, min, max, n_cand

c RAN3 variables
      real*4 p, ran3

c Scratch variables
      integer n_neigh0, n_neigh1
      real alpha


      if(min.eq.max)then
        if(n0.ne.min)then
          write(*,*) ' ERROR - pick_cand_n_value_unif ... exit'
          STOP
        endif
        n_cand=n0
        return
      endif

c Found the number of neighour of point n0 (which is always 2, but for boundary values of n, where it is equal to 1)

      n_neigh0=2
      if(n0.eq.min.or.n0.eq.max)n_neigh0=1

c Choose candidate point

      if(n0.ne.min.and.n0.ne.max)then
         p=ran3(iseed0)
         if(p.le.0.5)n_cand=n0-1
         if(p.gt.0.5)n_cand=n0+1
      else if(n0.eq.min)then
         n_cand=n0+1
      else if(n0.eq.max)then
         n_cand=n0-1
      endif

c Found the number of neighour of point n_cand

      n_neigh1=2
      if(n_cand.eq.min.or.n_cand.eq.max)n_neigh1=1

c Transition probability

      alpha=float(n_neigh0)/float(n_neigh1)

c Pick candidate

      p=ran3(iseed0)
      if(p.gt.alpha)n_cand=n0
      if(p.le.alpha)n_cand=n_cand

      return
      end



