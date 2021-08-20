## Recreating the cartogram 
  
This subdirectory uses the [`targets` package](https://docs.ropensci.org/targets/) to build the chart. Run `tar_make()` to execute `_targets.R` . This will produce: ![image](https://user-images.githubusercontent.com/17803537/130290186-b4dbbc92-6e1f-41d8-88bc-8e76d0523a50.png)


Finishing touches, namely the title, text, and logo annotations were done using a vector graphics editor. Final product:
![image](https://user-images.githubusercontent.com/17803537/130290245-877fb4a9-d4b9-4d41-97da-cdb22f1f8f18.png)

## Data  
This viz piggybacks on the [`gage-conditions-gif`] data processing pipeline (https://github.com/USGS-VIZLAB/gage-conditions-gif) that depends on an internal pipelining package. The data file used corresponds to `scipiper::scmake("2_process/out/dv_stats.rds.ind", "2_process.yml")` generated for July 2021 in the pipeline.

These data are daily streamflow values from currently active USGS streamgages, provided as a percentile relative to the historic record. Percentiles were binned categorically as shown below.
![image](https://user-images.githubusercontent.com/17803537/130290107-86ed5579-8050-4ba3-bdd3-88c9acf3c13c.png)

