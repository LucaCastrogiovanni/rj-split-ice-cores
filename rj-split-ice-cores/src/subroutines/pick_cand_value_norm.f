
      subroutine pick_cand_value_normal(x,xmean,xstd,sc,x_new)

      implicit none

      real*4 x,xmean,xstd,sc,xcand, x_new
      real*4 dx, dxcand, exparg, probacc, dev


      include '../common.randomtype'

c RAN3 variables
      real*4 ran3


      x_new=x

      if(xstd.gt.0.1e-5)then

      call sampleNormal(0.0,1.0,dev)

      xcand=x+sc*xstd*dev
      dx=x-xmean
      dxcand=xcand-xmean
      exparg=(dxcand*dxcand-dx*dx)/(2.0*xstd*xstd)
      probacc=exp(-1.0*exparg)
      dev=ran3(iseed0)
      if(probacc.ge.dev)then
        x_new=xcand
      endif

      endif


      return
      end

