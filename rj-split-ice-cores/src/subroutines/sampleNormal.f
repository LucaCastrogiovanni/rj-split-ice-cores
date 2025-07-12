
      subroutine sampleNormal(mean,std,x)

      implicit none

      include '../common.randomtype'

      real*4 mean, std, x
      real*4 r1

      integer ii
      real*4 ran3, dev

      r1=mean
      if(std.gt.0.1e-5)then
      do ii=1,12
        dev=ran3(iseed0)
        r1=r1+(dev-0.5)*std
      enddo
      endif
      x=r1

      return
      end

