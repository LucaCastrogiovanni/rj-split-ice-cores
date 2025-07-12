

      subroutine save_synthetics(iacc)

c
c ------------------------------------------------------------------------------------
c This subroutine saves the values of the synthetic Depth and their square
c to compute mean posterior values.
c ------------------------------------------------------------------------------------
c
c   Input:
c         iacc      -- ACCEPTED flag
c
c   Output:
c        (none)     -- SYN phases are stored in a common block
c
c ------------------------------------------------------------------------------------
c


      include '../trans_dim.para'
      include '../common.obs_syn'
      include '../common.obs_syn2'

c Main variables
      integer iacc

c Scratch variables
        integer id



        if(iacc.ne.0)then

           do id = 1, ndata
              syn_D0(id,1)=syn_D(id,1)
           enddo

        endif

        do id = 1, ndata
              mean_syn_D(id,1)=mean_syn_D(id,1)+syn_D0(id,1)
              mean_syn_D2(id,1)=mean_syn_D2(id,1)+syn_D0(id,1)**2
        enddo

      return
      end
c
c
c

c
c ----------------------------------------------------------------------------

