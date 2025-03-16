# ECMA31330_MLProject

This Github Repo is for our final project submission for ECMA 31330. In this project, we are trying to answer the research question of "How do external monetary policies influence U.S. Federal Reserve decisions and does this influence strengthen in post-crisis periods?" This capitalizes on observation of increased implicit coordination between monetary policies across countries. 

The structure of this repo is as follows:

In the [Code](https://github.com/nourabdelbaki/ECMA31330_MLProject/tree/main/Code) Folder:
1. [database.R](https://github.com/nourabdelbaki/ECMA31330_MLProject/blob/main/Code/database.R): merging our data sources and creating the main dataframe. This produces the following two datasets: [1990_G7_US.csv](https://github.com/nourabdelbaki/ECMA31330_MLProject/blob/main/1990_G7_US.csv) and [1998_G7_US.csv](https://github.com/nourabdelbaki/ECMA31330_MLProject/blob/main/1998_G7_US.csv). They can be found in the main foolder. We end up using the 1998 one as the most complete. 
2. [CV_RandomForests.R](https://github.com/nourabdelbaki/ECMA31330_MLProject/blob/main/Code/CV_RandomForests.R): corresponding to section 4.2.1 Cross-validating RFs in the paper; uses CV to find the best model within each period, then saves the model objects into folder Models.
3. [coefficients_analysis.R](https://github.com/nourabdelbaki/ECMA31330_MLProject/blob/main/Code/coefficients_analysis.R): corresponds to the plots of feature importance and partial dependence + section 4.2.4, coefficient calculation. The data with the calculated coefficients can be found under [data_08_all.csv](https://github.com/nourabdelbaki/ECMA31330_MLProject/blob/main/data_08_all.csv) and [data_covid_all.csv](https://github.com/nourabdelbaki/ECMA31330_MLProject/blob/main/data_covid_all.csv) found in the main folder. 
4. [Eval_RandomForests.R](https://github.com/nourabdelbaki/ECMA31330_MLProject/blob/main/Code/Eval_RandomForests.R): corresponds to section 4.2.4- Evaluating Coefficients
5. [CV_kmeans.R](https://github.com/nourabdelbaki/ECMA31330_MLProject/blob/main/Code/CV_kmeans.R): code for section 4.3. The clusters created and saved can be found under the [inflation_clustered.csv](https://github.com/nourabdelbaki/ECMA31330_MLProject/blob/main/inflation_clustered.csv). 
6. [final_reg.R](https://github.com/nourabdelbaki/ECMA31330_MLProject/blob/main/Code/final_reg.R): code behind our results in section 5

In the [Models](https://github.com/nourabdelbaki/ECMA31330_MLProject/tree/main/Models) Folder is the saved model objects from [CV_RandomForests.R](https://github.com/nourabdelbaki/ECMA31330_MLProject/blob/main/Code/CV_RandomForests.R); 1 for all of the data, 1 for pre-2008, 1 for post-2008, 1 for pre-covid, and 1 for post-covid. 

In the [RawData](https://github.com/nourabdelbaki/ECMA31330_MLProject/tree/main/RawData) Folder is the set of all datasets as defined in section 4.1 of the paper. The original links to the datasets can be found under [links_to_data.xlsx](https://github.com/nourabdelbaki/ECMA31330_MLProject/blob/main/links_to_data.xlsx) in the main folder. 

In the [Visualizations](https://github.com/nourabdelbaki/ECMA31330_MLProject/tree/main/Visualizations) Folder is a saved image of all the visualization found in the final write-up. 
