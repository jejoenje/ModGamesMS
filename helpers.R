list.to.df = function(l) {
  return(data.frame(matrix(unlist(l), nrow=length(l), byrow=T)))
}

### Extracts JUST the resource positions for each element of a gmse sims list,
###  i.e. returns a list of lists, of resource positions per time step per sim:
get_res_pos = function(gmse_sims_list) {
  lapply(gmse_sims_list, function(x) { lapply(x$resource, function(y) y[,c(1,5,6)]) } )
}

### http://wiki.cbr.washington.edu/qerm/sites/qerm/images/1/16/Filled.contour3.R
filled.contour3 <-
  function (x = seq(0, 1, length.out = nrow(z)),
            y = seq(0, 1, length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
            ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
            levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
            col = color.palette(length(levels) - 1), plot.title, plot.axes, 
            key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
            axes = TRUE, frame.plot = axes,mar, ...) 
  {
    # modification by Ian Taylor of the filled.contour function
    # to remove the key and facilitate overplotting with contour()
    # further modified by Carey McGilliard and Bridget Ferris
    # to allow multiple plots on one page
    
    if (missing(z)) {
      if (!missing(x)) {
        if (is.list(x)) {
          z <- x$z
          y <- x$y
          x <- x$x
        }
        else {
          z <- x
          x <- seq.int(0, 1, length.out = nrow(z))
        }
      }
      else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
      y <- x$y
      x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
      stop("increasing 'x' and 'y' values expected")
    # mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    # on.exit(par(par.orig))
    # w <- (3 + mar.orig[2]) * par("csi") * 2.54
    # par(las = las)
    # mar <- mar.orig
    plot.new()
    # par(mar=mar)
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
      stop("no proper 'z' matrix specified")
    if (!is.double(z)) 
      storage.mode(z) <- "double"
    .filled.contour(as.double(x), as.double(y), z, as.double(levels), 
                    col = col)
    if (missing(plot.axes)) {
      if (axes) {
        title(main = "", xlab = "", ylab = "")
        Axis(x, side = 1)
        Axis(y, side = 2)
      }
    }
    else plot.axes
    if (frame.plot) 
      box()
    if (missing(plot.title)) 
      title(...)
    else plot.title
    invisible()
  }

### http://wiki.cbr.washington.edu/qerm/sites/qerm/images/2/25/Filled.legend.R
filled.legend <-
  function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
                                                         length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
            ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
            levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
            col = color.palette(length(levels) - 1), plot.title, plot.axes, 
            key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
            axes = TRUE, frame.plot = axes, ...) 
  {
    # modification of filled.contour by Carey McGilliard and Bridget Ferris
    # designed to just plot the legend
    if (missing(z)) {
      if (!missing(x)) {
        if (is.list(x)) {
          z <- x$z
          y <- x$y
          x <- x$x
        }
        else {
          z <- x
          x <- seq.int(0, 1, length.out = nrow(z))
        }
      }
      else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
      y <- x$y
      x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
      stop("increasing 'x' and 'y' values expected")
    #  mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    #  on.exit(par(par.orig))
    #  w <- (3 + mar.orig[2L]) * par("csi") * 2.54
    #layout(matrix(c(2, 1), ncol = 2L), widths = c(1, lcm(w)))
    #  par(las = las)
    #  mar <- mar.orig
    #  mar[4L] <- mar[2L]
    #  mar[2L] <- 1
    #  par(mar = mar)
    # plot.new()
    plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
                yaxs = "i")
    rect(0, levels[-length(levels)], 1, levels[-1L], col = col)
    if (missing(key.axes)) {
      if (axes) 
        axis(4)
    }
    else key.axes
    box()
  }
#
#    if (!missing(key.title)) 
#        key.title
#    mar <- mar.orig
#    mar[4L] <- 1
#    par(mar = mar)
#    plot.new()
#    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
#    if (!is.matrix(z) || nrow(z) <= 1L || ncol(z) <= 1L) 
#        stop("no proper 'z' matrix specified")
#    if (!is.double(z)) 
#        storage.mode(z) <- "double"
#    .Internal(filledcontour(as.double(x), as.double(y), z, as.double(levels), 
#        col = col))
#    if (missing(plot.axes)) {
#        if (axes) {
#            title(main = "", xlab = "", ylab = "")
#            Axis(x, side = 1)
#            Axis(y, side = 2)
#        }
#    }
#    else plot.axes
#    if (frame.plot) 
#        box()
#    if (missing(plot.title)) 
#        title(...)
#    else plot.title
#    invisible()
#}

scale_cols = function(nl,x) {
  xl = seq(min(x),max(x),(max(x)-min(x))/nl)
  
  colpal = "RdBu"
  if(sum(xl<0)==length(xl)) {
    colpal = "Reds"
  }
  if(sum(xl>0)==length(xl)) {
    colpal = "Purples"
  }
  
  zero_pos =(1:length(xl))-which.min(abs(xl)); zero_pos
  cols = hcl.colors(max(abs(zero_pos))*2,colpal)
  mid_pos = floor(length(cols)/2)
  return(cols[zero_pos+mid_pos])
}

get_set_pars = function(x) {
  paras = as.data.frame(NULL)
  for(i in 1:length(x)) {
    paras = rbind(paras, as.data.frame(x[[i]]$paras))
  }
  return(paras)
}

get_set_data = function(set, paras) {
  ### Calculate extinctions and length of sim "run" (i.e. number of years achieved)
  paras$EXT_PROP = NA
  paras$MEAN_TIME = NA
  paras$MEAN_R = NA
  paras$MEAN_R0 = NA
  for(i in 1:length(set)) {
    dat_i = set[[i]]$data
    
    sim_length = NULL
    n0 = paras[i, "RESOURCE_INI"] 
    time_max = paras[i, "TIME_MAX"] 
    ### All resource runs for sim i:
    res_i = lapply(dat_i, function(x) x$resources[,2])
    ### Durations for each sim:
    duration_i = unlist(lapply(res_i,length))
    ### Exinct runs in sim i:
    ext_i = duration_i!=time_max
    ### Final N for each sim:
    nX = unlist(lapply(res_i, function(x) tail(x,1)))
    nX0 = nX
    nX0[ext_i] = 0
    
    ### Proportions extinct:
    paras[i, "EXT_PROP"] = sum(ext_i)/length(set)
    ### Mean duration:
    paras[i, "MEAN_TIME"] = mean(duration_i)
    
    ### Mean pop growth rates.
    ### R is N(final)-N(initial)/N(initial), R0 is the same but using zero for N(final) if the pop went extinct.
    paras[i, "MEAN_R"] = mean((nX-paras[i,"RESOURCE_INI"])/paras[i,"RESOURCE_INI"])
    paras[i, "MEAN_R0"] = mean((nX0-paras[i,"RESOURCE_INI"])/paras[i,"RESOURCE_INI"])
    
  }
  return(paras)
}
