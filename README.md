# Sleep_on_BCI

This repository contains all code I have used to produce the results in my Master's thesis: "Comparative sleep study of neotropical mammals". 

We used accelerometry to calculate Vectorial Dynamic Body Acceleration (VeDBA) to measure overall body movements, which we used to classify sleep in four different mammalian species on Barro Colorafo Island, Panama. We investigated the influence of temperature on sleep and tested for an effect of sleep compensation by napping during the active period. 

The accelerometry data is obtained from two Movebank projects: “Food for thought” and "Sleep on BCI new" on [Movebank](https://www.movebank.org/). 
This research focuses on data of the capuchin monkeys (*Cebus capucinus*), spider monkeys (*Ateles geoffroyi*), coatis (*Nasua narica*), and kinkajous (*Potos flavus*). 
The weather data is provided by the Smithsonian Research Insitute on Barro Colorado Island in Panama and can be downloaded on their website: [Temperature](https://doi.org/10.25573/data.10042451.v32) and [Rain](https://doi.org/10.25573/data.10042463.v28). 

To reproduce the results, run the code in the numbered order, as following codes take in produced dataframes from previous codes.

`01_XXX.R`: Calculates VeDBA from the raw Accelerometer measurements for each species and from each project seperately. In the case of the data from the "Sleep on BCI" project it downsamples the samling schedule. 

`02_XXX.R`: Performes the sleep detection based on the threshold for each species. In the second script we evaluated visually which threshold to choose for each species. 

`3_XXX.R`: Are the scripts necessary to evaluate the sleep detection threshold in the Capuchin monkeys with video observation data. 

`04_XXX.R`: Merges the data for all species together into 1 dataframe with all varaiables necessary to proceed toward the statistical analysis. 

`05_XXX.R`: Are the scripts from the statistical analysis and contain all models per species for each hypothesis seperately. 
