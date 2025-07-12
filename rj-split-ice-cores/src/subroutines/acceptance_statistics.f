
      subroutine acceptance_statistics(move_value,accepted)

c
c acceptance_statistics -- update MCMC statistics
c


      include '../trans_dim.para'
      include '../common.rmcmc_stat'

      integer accepted, move_value


c
c Upgrade MOVE statistics
      move_stat(move_value,accepted)=move_stat(move_value,accepted)+1
c
c
      if(accepted.eq.-1) n_acc_rules=n_acc_rules+1
      if(accepted.eq.0)  n_rej=n_rej+1
      if(accepted.eq.1) n_acc=n_acc+1

      return
      end
