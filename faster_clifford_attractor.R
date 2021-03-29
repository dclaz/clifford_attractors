##########################################################################################
# Load packages
library(Rcpp)
library(viridis)
library(scattermore)
library(png)

# Clean workspace
rm(list=ls())
gc()

# Load in awesome functions 
setwd("D:\\Dropbox\\Experiments\\Clifford_Attractors\\")
source("code/functions.R")



n_plots <- 100

# Set Seed
#set.seed(1234561)

output_loc <- "D:/dump/clifford/"
n_iter = 50*10^6
alpha = 0.025
res = 2*c(2560, 1440)


##########################################################################################



# Generate random set of params
params <- matrix(
    round(runif(4*n_plots, -3, 3), 4),
    ncol = 4
)

params = matrix(c(1.359, 1.126, -1.793, -1.336), nrow=1)

# Loop that saves down png's of the clifford attractos
for (i in 1:n_plots){
    # Print status
    cat(
        as.character(Sys.time()),
        paste0("plot: ", i,"\tparameters: ", paste0(params[i,], collapse=", ")),
        "\n"
    )
    
    colour_option = sample(c("A", "B", "C", "D", "E", "bw"), 1)
    
    # Obtain matrix of points
    # Print status
    cat(
        as.character(Sys.time()),
        paste0("\tGenerating points"),
        "\n"
    )
    cliff_points <- cliff_rcpp(
        n_iter, params[1], params[2], params[3], params[4]
    )
    
    # Calculate angle between successive points
    cliff_angle <- atan2(
        (cliff_points[,1] - c(cliff_points[-1,1],0)), 
        (cliff_points[,2] - c(cliff_points[-1,2],0))
    )
    
    # Obtain colours for points
    cat(
        as.character(Sys.time()),
        paste0("\tObtaining colours"),
        "\n"
    )
    if (colour_option == "bw"){
        cliff_cols = rgb(0, 0, 0, alpha = alpha)
        cliff_cols_mat = drop(cliff_cols_mat)
    } else {
        available_cols <- viridis(
            n = 1024*8, 
            alpha = alpha, 
            begin = 0, 
            end = 1, 
            direction = 1,
            option = colour_option
        )
        cliff_cols <- map2color(
            cliff_angle, 
            c(available_cols)#, rev(available_cols))
        )
        # convert to matrix
        cliff_cols_mat = col2rgb(cliff_cols, alpha = TRUE)[,-1]
    }
    

    
    # Name of output file
    output_file <- paste0(
        output_loc,
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        "_clifford_",
        params[1],"_",params[2],"_",params[3],"_",params[4],"_",n_iter/10^6,
        ".png"
    )
    
    # Output image directly to disk
    cat(
        as.character(Sys.time()),
        paste0("\tPlotting\t1")
    )
    png(
        output_file,
        width = res[1],
        height = res[2],
        pointsize = 1,
        bg = "white",
        antialias = "cleartype"
        
    )
    cat("\t2")
    # NEW method
    plot(scattermore(
        xy = cliff_points[-1,],
        size = res,
        rgba = cliff_cols_mat,
        cex = 1,
        output.raster = TRUE
    ))
    cat("\tDone")
    dev.off()

}



