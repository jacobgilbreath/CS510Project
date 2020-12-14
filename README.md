# CS510Project

Before Running Code
1. Install "ggplot2", "dplyr", and "reshape2" packages.
2. Download "owid-covid-data.csv", "AAPL.csv", and "MSFT.csv"
3. Change path in lines 7, 13 and 19 to where the data is stored locally


By running the entire project, 2 graphs will be produce. The first will show the percentage in closing prices of Apple and Microsoft stocks respectively, and the second will show the percentage change in new cases. There is then a linear analysis between the percent of new cases and the closing price of each stock, Apple and Microsoft. 

The R-Squared values for these graphs are substantially low, both less than 0.02. The next analysis in this project shows the same linear regression model, except it is now predicting the closing prices for the following day.
