
      subroutine pick_cand_value_uniform(x,xmin,xmax,sc,x_new)

      implicit none

      include '../common.randomtype'

      real*4 x,xmin,xmax,sc,xcand, x_new
      real*4 halfwidth, xcandhigh, xcandlow
      real*4 probacc, xhigh, xlow
      real*4 maxdev, dev


c RAN3 variables
      real*4 ran3


      x_new=x
      halfwidth=0.5*sc*(xmax-xmin)
      xcandhigh=x+halfwidth
      if(xcandhigh.gt.xmax)xcandhigh=xmax
      xcandlow=x-halfwidth
      if(xcandlow.lt.xmin)xcandlow=xmin
      maxdev=xcandhigh-xcandlow
      dev=ran3(iseed0)
      xcand=xcandlow+dev*maxdev
      xhigh=xcand+halfwidth
      if(xhigh.gt.xmax)xhigh=xmax
      xlow=xcand-halfwidth
      if(xlow.lt.xmin)xlow=xmin
      probacc=(xcandhigh-xcandlow)/(xhigh-xlow)
      dev=ran3(iseed0)
      if(probacc.ge.dev)then
        x_new=xcand
      endif


c -------------------------------------------------------------- WARNING:: THIS IS A PURE MC -- ONLY FOR TEST
c                                                                     (see Mosegaard and Tarantola, 1995)
c      dev=ran3(iseed0)
c      halfwidth=xmax-xmin
c      x_new=xmin+halfwidth*dev
c --------------------------------------------------------------------------------------------------


      return
      end

