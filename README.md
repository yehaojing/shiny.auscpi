# [shiny.auscpi](https://github.com/yehaojing/shiny.auscpi)
An R Shiny dashboard to visualise the Australian Consumer Price Index (CPI), inspired by [Statistics Canada's "Consumer Price Index Data Visualization Tool"](https://www150.statcan.gc.ca/n1/pub/71-607-x/2018016/cpi-ipc-eng.htm)

## Running the dashboard
You can either clone the repository and run the app through the R Studio interface or alternatively:
```
shiny::runGitHub(repo = "shiny.auscpi",
                 username = "yehaojing",
                 ref = "main")
```

## Using the dashboard
* You can select the quarter of interest and region, which will populate the hierarchical breakdown of the Australian CPI.
	* By default, the breakdown is presented as a treemap, but it can be switched to a sunburst diagram
	* The "All Groups CPI" is selected by default.
* Selecting any of the children within the hierarchy diagram will filter the other graphs.
	* The bar graph shows whichever value has been selected ('% Quarter' or quarterly percentage movement by default).
	* The line graph shows a time series plot of the Index value of the selected series for all 8 capital cities and the weighted average of the 8 capitals.
		* An adjusted time series of the Index can be rendered by selecting the reference quarter and swapping to the "Adjusted" option.
		* One can isolate a trace by double clicking the legend, and Ctrl clicking to add more traces.
	* The heatmap shows the quarterly percentage movement over time by capital.
	* The map shows the geographical location of the 8 capitals.
	* For both the line graph and heatmap, one can slice to the desired time frame using the subplot below each main plot.
