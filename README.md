# MtStreamMetab

Place to organize stream metab data as well as covariate data that might explain metabolism patterns for streams in the Tahoe Basin. 

This repo is just for: 
1. Organizing and QAQC of raw miniDOT DO files "streamDO_24.R".
	A. Here we clean outlier if the they are 3 standard deviations out of range of a 5 observation rolling average of DO or temperature. 
	B. Bring in climate data from NLDAS for barometric pressure (https://disc.gsfc.nasa.gov/datasets/GLDAS_NOAH025_3H_2.1/summary) as well as light (Short wave radiation - https://disc.gsfc.nasa.gov/datasets/NLDAS_FORA0125_H_2.0/summary). See repo X for climate data processing. 
	C. Bring in hourly flow data from the USGS using dataRetrieval() and the station ID for each gaged stream reach
	D. Infill observations to have 15 minute resolution of DO, temp, pressure, light, flow, and depth.
	
2. Prepare the data for streamMetabolizer() "SM_modelprep.R"
	A. Where we estimate mean reach depth by using a rating curve of depth measurements and USGS observations for gage height. 
	B. calculate streamMetabolizer parameters: for light -- calc_solar_time(), calc_light(), and DO saturation calc_DO_sat()
	C. select data for model imputs
	
3. Operate metabolism models using "SM_model.R"
	A. test model on short - 1 to 2 week data chunk
	B. customize model parameters "bayes_specs_new()" and set priors 
	C. run model and save output 

