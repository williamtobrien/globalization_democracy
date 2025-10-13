This is a repository for the code used to produce all results in my honors BA thesis in political science at the University of Chicago. Those who wish to examine the data cleaning and diagnostics process can start by looking at the `data_cleaning_diagnostics` folder. However, all the results are reproducible individually in each `.r` file using the provided `.csv` file.

# Required Packages
`library(tidyverse)`  
`library(ggplot2)`  
`library(plm)`  
`library(lmtest)`  
`library(stargazer)`  
`library(MASS)`  

# Required Data
For my code to execute properly, you must have the data folder in your working directory. If you wish to clean the data yourself, or walk through my process, you will use the provided `wdi.csv` and `vdem.csv` files to create the dataset `full_panel.csv`. For every other `.r` file, you must load in `full_panel.csv`, a process for which code is always given. 

The World Bank data is loaded from the `wdi.csv` file and contains World Development Indicators data on foreign direct investment inflows, foreign direct investment outflows, trade openness as a percentage of GDP, weighted mean applied tariff rates, GDP per capita, GDP annual growth rates, inflation rates, population size, and annual population growth rates. The unit of analysis is the country-year. The V-Dem `.csv` file includes the three outcome variables used in my primary analysis--_v2xel_frefair_, _v2xcl_rol_, and _v2xeg_eqaccess_ (which I code as _Free Fair_, _Equal Law_, and _Equal Access_ respectively--as well as the electoral and liberal democracy indices used to produce Figure 2, all presented with the country-year as the unit of analysis. 

# Structure & Process
All R scripts are inside the `code` folder and are categorized into either `data_cleaning_and_diagnostics`, `tables`, or `figures`. 

## Data Cleaning & Diagnostics
To clean the data, examine diagnostics, and test for unit root issues, navigate to the `data_cleaning_and_diagnostics` folder.

R script `1-Data_Cleaning.r` will load, clean, and merge the World Bank and V-Dem data into a tidy dataset called `full_panel.csv`, which contains both the World Bank and V-Dem data. Coding mismatches are addressed, and I remove excess columns. Last, I add coding for developmental levels and create lags for all explanatory variables. The final dataframe is exported to `full_panel.csv`.

R script `2-Univariate_Distributions_and_Unit_Root_Tests.r` loads the new `full_panel.csv` file and examines the univariate distributions of the variables to determine possible transformations. Because unit root non-stationarity poses serious threats in panel data settings, I also run diagnostics testing for possible unit root issues. 

## Tables & Figures
To simply reproduce the results in my thesis, you need only run the `.r` files in `tables` and `figures`. Figures 1 and 6 were created in LaTeX and are not included here.

The figures and tables appear in the paper in the following order:

Figure 2: R script `Figure_2.r` will create a plot of V-Dem's electoral democracy and liberal democracy indices from 2017 to 2024. 

Table 1: R script `Table_1.r` creates a table of variable descriptions using the 'stargazer' package

Table 2: R script `Table_2.r` iterates over all variables in my analysis to create descriptive statistics. A table is then made with the 'stargazer' package.

Figure 3: R scripts `Figure_3A.r` and `Figure_3B.r` produce respective coefficient plots for two outcome variables: _Free Fair_ and _Equal Law_. For each figure, I estimate my preferred model specification with standardized variables. I then plot the coefficients with 95% confidence intervals, with solid points indicating statistical significance at the 5% level. 

Table 3: R script `Table_3.r` produces a table of the main results for my project. The table presents coefficients, standard errors, and significance levels for the effect of each globalization variable on each dependent variable (multiplied by 100 for presentation). The same statistics are presented for the lagged dependent variables. P-values for tests for serial correlation and the number of observations are included at the bottom of the table. There are a total of 12 models, 6 using two-way fixed effects, and 6 using the Arellano-Bond GMM estimator. Half the models use one lag of democracy, and the other half use two. 

Figure 4: R script `Figure_4.r` creates a dynamic simulation depicting the effect of a one-time annual 1-standard deviation increase in _Trade_ on _Free Fair_ over 30 years. 95% confidence intervals are constructed via a parametric bootstrap of 1,000 draws from the estimated variance-covariance matrix. 

Figure 5: R script `Figure_5.r` does the same as `Figure_4.r` but for the effect of _FDI Outflows_ on _Equal Law_.

Table 4: R script `Table_4.r` does the same as `Table_3.r` but excludes the economic control variables (GDP per capita, GDP growth rate, and inflation) as a robustness check.

Table 5: R script `Table_5.r` creates a table of globalization's effect on democracy across subsamples. The three subsamples, created in `1-Data_Cleaning.r`, consist of 1) least developed countries (LDCs), 2) developing countries, and 3) Organization of Economic Co-operation and Development countries (OECDs). There are a total of 9 models, all of which include two lags of the dependent variable and two-way fixed effects.

Table A1: R script `Table_A1.r` produces Table A1 in the appendix. This presents results for globalization's effect on democracy after including controls for log of population size and the annual growth rate of population size as a robustness check. It contains 12 columns, as in Tables 3 and 4. 

Table A2: R script `Table_A2.r`  produces Table A2 in the appendix, which excludes outliers either with a standardized residual above or below 1.96 or with a Cook's distance above four divided by the number of observations.











