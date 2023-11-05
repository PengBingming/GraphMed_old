# GraphMed
A Shiny Web Application to Analyze and Visualize Medical Data

This application is free at website: [https://shiny.chcmu.com.cn/graphmed/](https://shiny.chcmu.com.cn/graphmed/)

Mutil-language file is placed in the GraphMed/lang/info/ directory

Tools code are placed in the GraphMed/module directory

Demo data for each visualization function (files are placed in the GraphMed/www/ directory)
> deseq2/exp.csv and group.csv for DESeq2  
> heatmap/heatmap.csv for heatmap.  
> ......  

install needed packages, such as:
install.packages(c("shiny","ggplot2","DESeq2","RColorBrewer","DT","pheatmap","reshape"))

If you were ready for all packages, You may now run the shiny app with just one command in R:
shiny::runGitHub("GraphMed_old","PengBingming")
