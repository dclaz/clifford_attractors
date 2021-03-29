# C++ function for fast calculation of points
cliff_rcpp <- cppFunction("
                         NumericMatrix cliff(int nIter, double A, double B, double C, double D) {
                         NumericMatrix x(nIter, 2);
                         for (int i=1; i < nIter; ++i) {
                         x(i,0) = sin(A*x(i-1,1)) + C*cos(A*x(i-1,0));
                         x(i,1) = sin(B*x(i-1,0)) + D*cos(B*x(i-1,1));
                         }
                         return x;
                         }")


# Function for mapping a point to a colour
map2color <- function(x,pal,limits=NULL){
    if(is.null(limits)) limits=range(x)
    pal[findInterval(x,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
}


# Plot Clifford Attractor
plot_cliff <- function(
    n_iter, params, alpha, res=c(2560, 1440), colours="plasma", output_loc=""
){
    
    # Obtain matrix of points
    cliff_points <- cliff_rcpp(
        n_iter, params[1], params[2], params[3], params[4]
    )
    
    # Calculate angle between successive points
    cliff_angle <- atan2(
        (cliff_points[,1] - c(cliff_points[-1,1],0)), 
        (cliff_points[,2] - c(cliff_points[-1,2],0))
    )
    
    # Obtain colours for points
    if (colours == "plasma"){
        available_cols <- plasma(1024, alpha = alpha, begin = 0, end = 1, direction = 1)
        cliff_cols <- map2color(
            cliff_angle, 
            c(available_cols, rev(available_cols))
        )
    } else if (colours == "viridis"){
        available_cols <- viridis(1024, alpha = alpha, begin = 0, end = 1, direction = 1)
        cliff_cols <- map2color(
            cliff_angle, 
            c(available_cols, rev(available_cols))
        )
    } else if (colours == "magma"){
        available_cols <- magma(1024, alpha = alpha, begin = 0, end = 1, direction = 1)
        cliff_cols <- map2color(
            cliff_angle, 
            c(available_cols, rev(available_cols))
        )
    } else {
        cliff_cols = rgb(0, 0, 0, alpha = alpha)
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
    png(
        output_file,
        width = res[1],
        height = res[2],
        pointsize = 1,
        bg = "white",
        antialias = "cleartype"
        
    )
        plot(
            cliff_points[-1,],
            bg = "white",
            pch = ".",
            col = cliff_cols
        )
    dev.off()
}

