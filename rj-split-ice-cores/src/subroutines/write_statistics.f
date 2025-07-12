c
c ----------------------------------------------------------------------------
c
      subroutine write_statistics
c
c write_statistics -- write MOVE statistics to file
c
c
c

      include '../trans_dim.para'
      include '../common.rmcmc_stat'

      integer iacc, imove
      integer n_tot(20)
      integer n_acc_1, n_reject, n_acc_2
      integer lu_move

c Compute TOT values

      do imove=1,5
      n_tot(imove)=0
      do iacc=-1,1
         n_tot(imove)=n_tot(imove)+move_stat(imove,iacc)
      enddo
      enddo



      n_acc_1=0
      n_reject=0
      n_acc_2=0

      do imove=1,5
         n_acc_1=n_acc_1+move_stat(imove,-1)
         n_reject=n_reject+move_stat(imove,0)
         n_acc_2=n_acc_2+move_stat(imove,1)
      enddo

      lu_move=999
      open(lu_move,file='output/move.stat',status='unknown')

      write(lu_move,*) '#'
      write(lu_move,*) '# -------------------------------------------------------------'
      write(lu_move,*) '#'
      write(lu_move,*) '#      MOVE  <-- ACCEPTED VALUES (-1;0;1 ) -->'

      do imove=1,5
        write(lu_move,'(i7,4i12)') imove, (move_stat(imove,iacc),iacc=-1,1), n_tot(imove)
      enddo

      write(lu_move,'(a,3i12)') '          ', n_acc_1, n_reject, n_acc_2
      write(lu_move,*)

      close(lu_move)

      return
      end
c
c

