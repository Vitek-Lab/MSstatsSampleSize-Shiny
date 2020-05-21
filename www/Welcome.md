# Welcome to `MSstats - Sample Size Calculation`


## Introduction
`MSstats - Sample Size Calculation` is a web-based graphical user interface that provides a 
user-friendly way to visualize and report the results from 
<a href = "https://bioconductor.org/packages/release/bioc/html/MSstatsSampleSize.html" target = "_blank">`MSstatsSampleSize`</a> package

<a href = "https://bioconductor.org/packages/release/bioc/html/MSstatsSampleSize.html" target = "_blank">`MSstatsSampleSize`</a> is an open-source R package, available in Bioconductor.
It allows researchers to design optimal MS-based proteomics experiments in terms
of statistical power and the use of resources. In particular, `MSstatsSampleSize`
uses protein-level data from a prior MS-based proteome investigation as a basis
to plan future experiments with similar methodologies. The software therefore
significantly contributes to the design of biological and clinical studies that 
involve replicates.

`MSstats - Sample Size Calculation` was created using <a href = "https://shiny.rstudio.com/" target = "_blank"> `R-Shiny` </a>, a Web Application Framework for R. It enables researchers
without experience with R/programming to utilize the functions from MSstatsSampleSize.


## Get Started

Begin by importing your dataset from the *Import Data* submenu in the sidebar. 
Accepted data format includes.  
* Protein abundance/summarised spectral counts
* Corresponding sample annotation file in a delimiter-separated format.  
  
Example datasets from <a href = "https://bioconductor.org/packages/release/data/experiment/html/MSstatsBioData.html" target = "_blank">`MSstatsBioData`</a> are also available from within the app.