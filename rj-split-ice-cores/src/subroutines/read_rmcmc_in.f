

        subroutine read_rmcmc_in


c Read parameter file: rmcmc.in and store value in COMMON BLOCK

        include '../trans_dim.para'

        include '../common.recipe'
        include '../common.randomtype'

c
c Scratch Variables

        integer yesno


        open(12,file='rmcmc.in',status='old')

        read(12,*)
c Read NUMBER OF INDEPENDENT CHAINS
        read(12,*)nchains
c Read NUMBER OF MODELS IN THE McMC walk
        read(12,*)ntot
c Read NUMBER OF MODELS IN THE BURN-IN phase
        read(12,*)mod_burn_in
c Read THINNING CHAIN parameter (i.e. only 1/nsample models are saved to inegrals)
        read(12,*)nsample
c Read if POSTERIOR or PRIOR is sampled
        posterior=.false.
        read(12,*)yesno
        if(yesno.eq.0)posterior=.true.
c Read RANDOMSEED
        read(12,*) iseed0
c Read LEVEL of OUTPUT (1- all; 0- nothing)
        info=.false.
        read(12,*)yesno
        if(yesno.eq.1)info=.true.

        read(12,*)
c Read MOVE PROB for the 5 moves in the McMC walk (WARN:: BIRTH/DEATH of a complexity is just ONE prob)
        read(12,*)move_prob(1)
        read(12,*)move_prob(2)
        read(12,*)move_prob(3)
        read(12,*)move_prob(4)
        read(12,*)move_prob(5)

        read(12,*)
c Read PARAMETER for INTEGRATION
        read(12,*)nstep
        read(12,*)nstep_map
        ndiv=nstep

        close(12)


c
c OPEN log file
c
        open(99,file='output/chain.log',status='unknown')
        write(99,'(a)') 'MOD:   CHAIN   MOVE      ACC      LPPD           LPPD0             LHNORM         LHNORM0  N  HPARAM'
 

c
c OPEN PPD model file
c
        open(98,file='output/samples.dat',status='unknown')

        return
        end


