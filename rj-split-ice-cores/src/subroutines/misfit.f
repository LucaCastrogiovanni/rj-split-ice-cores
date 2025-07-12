c
c
c	subroutine MISFIT
c
c-------------------------------------------------------------------------
c
c INPUT:
c        hparam  --  scaling factor for the STD
c
c OUTPUT:
c         lppd -- logarithmic value of the PPD (i.e Chi-squared value)
c
c-------------------------------------------------------------------------
c
c
        subroutine misfit(hparam,lh_norm,lppd)

        

        include '../trans_dim.para'


        include '../common.obs_syn'
        include '../common.param'


c
c MAIN variables

        real*4  lh_norm, lppd, hparam(nhparam_max)



c
c Scratch variables

        integer id
        real*4  diff

       
c
c COMPUTE FIT AS A CHI-SQUARE VALUE. COVARIANCE ERROR MATRIX IS CONSIDERED DIAGONAL
c


        diff=0.0


c        open(32, file = 'output/log-like-distribution.log', status = 'unknown')

        do id=1,ndata

c Misfit over CO2

            diff = diff  + ((obs_D(id,2)-syn_D(id,1))/(10**(hparam(1))*obs_D(id,3)))**2
c            write(32, '(f16.4)') diff

        enddo



       lh_norm= 1.0*ndata*hparam(1)*2.302585e0

        lppd=diff

c        close(32)

        return
        end
