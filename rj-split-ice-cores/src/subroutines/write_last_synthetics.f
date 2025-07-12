c
c ----------------------------------------------------------------------------
c
      subroutine write_last_synthetics(hparam)
c
c ----------------------------------------------------------------------------
c


      include '../trans_dim.para'
      include '../common.obs_syn'
      include '../common.obs_syn2'
      include '../common.recipe'

c Main variables

      real*4  hparam(nhparam_max)


c Scratch variables

      integer id, ic, iadd
      real*4 mean1, std1, mean2, std2, std, x
      character filename3*(namelen)
      character*6 id_lab
c
c
c

       write(*,*)
       write(*,*) '----------------------------------------------'
       write(*,*) ' WRITE SYNTHETICS from the LAST sampled model '
       write(*,*) '----------------------------------------------'
       write(*,*)

	write(*,*)
        write(*,*) ' WARNING:: Synthetics are created using white noise. '
        write(*,*) ' WARNING:: Standard deviation in file data.obs are   '
        write(*,*) ' WARNING:: scaled using the hparam(1) '
        write(*,*) 
c
c
c
c
c
c


         filename3='output/syn.001.dat'
         write(*,*) ' Writing file: ',filename3,' ...'
         open(iounit1,file=filename3,status='unknown')
       
	 write(iounit1,*) ' # SYNTHETICS FOR CO2        '
	 write(iounit1,*)  '# of the CO2 data'
	 write(iounit1,*) t_min,       t_max, '# Time-window (X): MIN/MAX'
         write(iounit1,*) ndata,   '      # Number of data points'
         write(iounit1,*) '#'
	  write(iounit1,*) '# Time(yearsBP)      CO2 (ppmv)     Sigma'


         iadd=0
         do id=1, ndata
            write(id_lab,'(i6)')id
            do ic=1,6
              if(id_lab(ic:ic).eq.' ')id_lab(ic:ic)='0'
            enddo
            mean1=syn_D0(id,1)
            std1=obs_D(id,3)

            if(added_noise)then
              std=std1*(10**(hparam(1)))
              call sampleNormal(mean1,std,x)
c              if(x.lt.0.0)then
c                iadd=iadd+1
c                x=0.0
c              endif
              mean1=x
            endif



            write(iounit1,*) obs_D(id,1), mean1,  std1
	
 
	     


         enddo
         close(iounit1)

         if(iadd.gt.0)then
           write(*,*)
           write(*,'(a,i6)') ' --> DATA MODIFIED FOR BOUND COND VIOLATION: ', iadd
           write(*,*)
         endif





      return
      end

c
