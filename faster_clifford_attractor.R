################################################################################
# Load packages
library(Rcpp)
library(viridis)
library(scattermore)
library(stringr)

################################################################################
# Clean workspace
rm(list=ls())
gc()

# Load in awesome functions 
source("code/functions.R")

################################################################################
# Settings
n_plots <- 1000

output_loc <- "D:/dump/clifford/"
n_iter <- 100*10^6
alpha <- 0.025
res <- 2*c(2560, 1440)
pointsize <- 1
background_options <- c("black", "white") #black or white or both

# Set Seed
#set.seed(1234561)

##########################################################################################
# Generate random set of params
params <- matrix(
    round(runif(4*n_plots, -3, 3), 4),
    ncol = 4
)

# CUSTOM SET
#params = matrix(c(1.359, 1.126, -1.793, -1.336), nrow=1)
#colour_option = "V"



##########################################################################################
# Loop that saves down png's of the clifford attractos
for (i in 1:n_plots){
    
    current_params = params[i,]
    # Print status
    cat(
        as.character(Sys.time()),
        paste0("plot: ", i,"\tparameters: ", paste0(current_params, collapse=", ")),
        "\n"
    )
    
    colour_option = sample(c("A", "B", "C", "D", "E", "BW", "V"), 1) # cividis is meh
    background = sample(background_options, 1)
    # Obtain matrix of points
    # Print status
    cat(
        as.character(Sys.time()),
        paste0("\tGenerating points"),
        "\n"
    )
    cliff_points <- cliff_rcpp(
        n_iter,
        current_params[1],
        current_params[2],
        current_params[3],
        current_params[4]
    )
    
    # Calculate angle between successive points
    cliff_angle <- atan2(
        (cliff_points[,1] - c(cliff_points[-1,1],0)), 
        (cliff_points[,2] - c(cliff_points[-1,2],0))
    )
    
    # Obtain colours for points
    cat(
        as.character(Sys.time()),
        paste0("\tObtaining colours:\t", colour_option),
        "\n"
    )
    
    if (colour_option == "BW"){
        
        if (background == "black"){
            cliff_cols = rgb(1, 1, 1, alpha = alpha)
        } else if (background == "white"){
            cliff_cols = rgb(0, 0, 0, alpha = alpha)
        }

    } else if (colour_option == "V"){
        available_cols = vapor_pal_1(1024*16)
        available_cols = paste0(
            available_cols,
            str_pad(as.hexmode(round(alpha*255)), width=2, pad="0", side="left")
        )
        cliff_cols <- map2color(
            cliff_angle, 
            c(available_cols, rev(available_cols))
        )
        cliff_cols <- cliff_cols[-length(cliff_cols)]
    } else {
        available_cols <- viridis(
            n = 1024*16, 
            alpha = alpha, 
            begin = 0, 
            end = 1, 
            direction = 1,
            option = colour_option
        )
        cliff_cols <- map2color(
            cliff_angle, 
            c(available_cols, rev(available_cols))
        )
        cliff_cols <- cliff_cols[-length(cliff_cols)]

    }
    

    # Only bother plotting if there is a good number of points
    n_unique_points_in_sample = nrow(unique(cliff_points[1:1000000,]))
    cat(
        as.character(Sys.time()),
        paste0("\tUnique points in sample:\t", n_unique_points_in_sample),
        "\n"
    )
    
    if (n_unique_points_in_sample > 10000){
    
        # Name of output file
        output_file <- paste0(
            output_loc,
            format(Sys.time(), "%Y%m%d_%H%M%S"),
            "_clifford_", colour_option, "_", pointsize, "_",
            current_params[1],"_",current_params[2],"_",current_params[3],"_",current_params[4],"_",n_iter/10^6,
            ".jpg"
        )
        
        # Output image directly to disk
        cat(
            as.character(Sys.time()),
            paste0("\tPlotting.")
        )
        
        jpeg(
            output_file,
            width = res[1],
            height = res[2],
            pointsize = 1,
            bg = background,
            antialias = "cleartype",
            quality = 100
    
        )
        cat(".")

        cliff_points_1 <- cliff_points[-1,1]
        cliff_points_2 <- cliff_points[-1,2]
        rm(cliff_points)
        gc()
    
        scattermoreplot(
            cliff_points_1,
            cliff_points_2,
            bg = background,
            cex = pointsize,
            col = cliff_cols
        )
    
        cat(".")
        dev.off()
        cat(". Done\n")
        
    } else {
        cat(
            as.character(Sys.time()),
            paste0("\tSkipping plot.\n")
        )
    }


}




