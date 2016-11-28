# code from http://www.colbyimaging.com/wiki/statistics/color-bars

# Function to plot color bar
color.bar <- function(lut, min, max, nticks=11, ticks=seq(min, max, len=nticks), title='') {
  scale = (length(lut)-1)/(max-min)
  
  plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
  axis(2, ticks, las=1)
  for (i in 1:(length(lut)-1)) {
    y = (i-1)/scale + min
    rect(0,y,10,y+1/scale, col=lut[i], border=NA)
  }	
}


# function to plot spatial object with specified breaks
# from Cari's class
ploteqc <- function(spobj, z, breaks, ...){
  pal <- tim.colors(length(breaks)-1)
  fb <- classIntervals(z, n = length(pal), 
                       style = "fixed", fixedBreaks = breaks)
  col <- findColours(fb, pal)
  plot(spobj, col = col, ...)
  image.plot(legend.only = TRUE, zlim = range(breaks), col = pal)
}

# function to add a scalebar to a base-graphics plot
# adapted from
# http://stackoverflow.com/questions/23784943/how-to-add-a-scale-bar-in-r
myScalebar = function(units_label,text_label, xleft=NULL, yadj=1.5) {
  
  # Get plot coordinates
  pc = par("usr") 
  
  # Position scale line between last two major x-axis tick marks
  # and 1/10th of the total y-range above the lower y-axis coordinate
  if(is.null(xleft)) xleft <- floor(pc[2]-units_label)
  lines(c(xleft,xleft+units_label),     
        rep(pc[3] + 0.1*(pc[4] - pc[3]), 2),lwd=2)
  
  # Place the units label at the midpoint of and just below the scale line
  text(x=mean(c(xleft,xleft+units_label)), 
       y=pc[3] + 0.1*(pc[4] - pc[3]),
       label=text_label, adj=c(0.5, yadj),
       font=2) # for bold
}