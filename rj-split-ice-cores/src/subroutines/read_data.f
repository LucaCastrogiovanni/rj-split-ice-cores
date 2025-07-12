

        subroutine read_data


c Read data file: input/OBS/data.obs and store value in COMMON BLOCK

        include '../trans_dim.para'


        include '../common.obs_syn'
        include '../common.param'

c
c Scratch Variables 

        integer  id
       


        open(12,file='input/OBS/data.obs.001',status='old')

        read(12,*)
        read(12,*)
c Read time MIN and time MAX
        read(12,*)t_min, t_max
c Read NUMBER OF DATA
        read(12,*) ndata

        read(12,*)
        read(12,*) 
        do id=1,ndata
          read(12,*)  obs_D(id,1),obs_D(id,2),obs_D(id,3)
        enddo



        close(12)



c Read SUPPORT T data

        open(15,file='input/OBS/temperature.dat',status='old')
        read(15,*)
        read(15,*) ndata_temp
        read(15,*)

c Read ndata_temp
        
         do id=1,ndata_temp
           read(15,*)  temp(id,1), temp(id,2)
        enddo



c
c Compute derivative of temperature
c
        do id=1,ndata_temp
          if(id.ne.1.and.id.ne.ndata_temp)then
            d_temp(id)= 0.5 * ( temp(id+1,2) - temp(id-1,2) ) / (2.0) 
          else
            d_temp(id)=0.0
          endif
        enddo



        close(15)

c
c -----------------------------------------------------------------------
c WRITE OUT OBSERVED DATA for REFERENCE
c
        open(13,file='output/obs.data',status='unknown')

        do id=1,ndata
          write(13,101) 'OBS DATA:', id, obs_D(id,1),obs_D(id,2),obs_D(id,3)
        enddo

 101    format(a,i6,3f16.4)
c
c -----------------------------------------------------------------------
c




        return
        end


