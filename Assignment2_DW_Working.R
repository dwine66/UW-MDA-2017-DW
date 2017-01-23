#UW Data Science - Winter 2017
#Assignment #1
#Dave Wine 8430191

# Import packages
require(ggplot2)
require(car)
require(gridExtra)

# File read function
read.bldg = function(file = 'EnergyEfficiencyData.csv'){

bldg.data <- read.csv(file, header=TRUE, stringsAsFactors=FALSE)

factcols <- c('Orientation','Glazing.Area.Distribution')

bldg.data[, factcols]<-lapply(bldg.data[,factcols], as.numeric)

bldg.data[complete.cases(bldg.data),]
}

# 2D Scatterplot function
#This works OK but I haven't figured out how to pass or infer labels yet....
plots.2DS <- function(x,y,c){ 
ggplot(bldg.data, aes(x, y)) + geom_point(aes(color = factor(c))) 
  #+ 
  #xlab(x) + ylab(y) + 
  #ggtitle('Relationship between'+ x +' and ' + y +' \n with '+c+' Shown')
}

# normalization function
norm <- function(x){
  return((x-min(x))/(max(x)-min(x)))
#  return (min(x))
}

# denormalization function
denorm <- function(x){
  #TBD
}
# Read data in
bldg.data <- read.bldg()

# View dataset and summary statistics
str(bldg.data)
summary(bldg.data)

# Create Factors from appropriate variables
factOrient <- cut(bldg.data$Orientation,breaks=4,labels=1:4)
#factOrient <- lapply(factOrient, as.character)
factDist <- cut(bldg.data$Glazing.Area.Distribution,breaks=6,labels=1:6)
#factDist <- lapply(factDist, as.character)
factHeight <- cut(bldg.data$Overall.Height,breaks=2,labels=1:2)

# Some subsets that seemed useful:
Tall<-subset(bldg.data,Overall.Height==7)
Short<-subset(bldg.data,Overall.Height==3.5)

# Test normalization function
SA.n <- norm(bldg.data$Surface.Area)
WA.n <- norm(bldg.data$Wall.Area)
RA.n <- norm(bldg.data$Roof.Area)
HL.n <- norm(bldg.data$Heating.Load)
CL.n <- norm(bldg.data$Cooling.Load)

#OR try lapply for normalization
bldg.data.n <- lapply(bldg.data,norm)

# Main Analysis

# (See Word Doc for analysis and conclusions)
# Try various plots for Heating Load

#First, look at the overall correlation plots (not normalized)
options(repr.plot.width=8, repr.plot.height=8)
scatterplotMatrix(~ Relative.Compactness + Surface.Area + Wall.Area + Roof.Area + 
Overall.Height + Orientation + Glazing.Area + Glazing.Area.Distribution + Heating.Load + Cooling.Load, data = bldg.data)

#Then look at the overall correlation plots (normalized)
options(repr.plot.width=8, repr.plot.height=8)
scatterplotMatrix(~ Relative.Compactness + SA.n + WA.n + RA.n + 
Overall.Height + Orientation + Glazing.Area + Glazing.Area.Distribution + HL.n + CL.n, data = bldg.data)

# plot them next to each other with gridExtra
# grid.arrange(spm,spm.n,ncol=2)

# Then look at likely factors in more detail

ggplot(bldg.data, aes(x = factor(Orientation), y = Heating.Load)) + geom_boxplot() + 
  xlab('Orientation') + ggtitle('Heating Load by Orientation')

ggplot(bldg.data, aes(x = factor(Glazing.Area.Distribution), y = Heating.Load)) + geom_boxplot() + 
  xlab('Orientation') + ggtitle('Heating Load by Glazing Area Distribution')

#Violin Plot
ggplot(bldg.data, aes(x = factor(Overall.Height), y = Heating.Load)) + 
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75)) + xlab('Height')  + 
  ggtitle('Heating Load by Height')

#2D KDP
ggplot(bldg.data, aes(Surface.Area, Heating.Load)) + geom_point() + 
  geom_density2d() +
  xlab('Surface Area') + ylab('Heating Load') +
  ggtitle('Relationship between Surface Area and Heating Load')

# 2D Scatterplot
ggplot(bldg.data, aes(Surface.Area, Heating.Load)) + geom_point(aes(color = factor(Overall.Height))) + 
  xlab('Surface Area') + ylab('Heating Load') + 
  ggtitle('Relationship between Heating Load and Surface Area, \n with Overall Height Shown')

#Single Factor Analysis
ggplot(bldg.data, aes(x = Orientation, y = Heating.Load)) + geom_point() + 
  xlab('Orientation') + ylab('Heating.Load') + 
  ggtitle('Relationship between Orientation and Heating Load')

ggplot(Short, aes(x = Surface.Area, y = Heating.Load)) + geom_point() + 
  xlab('Surface Area') + ylab('Heating.Load') + 
  ggtitle('Relationship between Surface Area and Heating Load')

#Area Relationships
ggplot(bldg.data, aes(x = Roof.Area, y = Wall.Area)) + geom_point() + 
  xlab('Roof Area') + ylab('Wall.Area') + 
  ggtitle('Relationship between Roof Area and Wall Area')

ggplot(bldg.data, aes(Roof.Area, Surface.Area)) + geom_point(aes(color = factor(Overall.Height))) + 
  xlab('Roof Area') + ylab('Surface Area') + 
  ggtitle('Relationship between Roof Area and Surface Area, \n with heights shown')

# Short Building Analysis

ggplot(Short, aes(Surface.Area, Heating.Load)) + geom_point(aes(color = factor(Glazing.Area))) + 
  xlab('Surface Area') + ylab('Heating Load') + 
  ggtitle('Relationship between Surface Area and Heating Load for Short Buildings, \n with Glazing Area shown')

ggplot(Short, aes(Glazing.Area.Distribution, Heating.Load)) + geom_point(aes(color = factor(Glazing.Area))) + 
  xlab('Glazing Area Distribution') + ylab('Heating Load') + 
  ggtitle('Relationship between Glazing Area Distribution and Heating Load for Short Buildings, \n with Glazing Area shown')

ggplot(Short, aes(Orientation, Heating.Load)) + geom_point(aes(color = factor(Glazing.Area))) + 
  xlab('Orientation') + ylab('Heating Load') + 
  ggtitle('Relationship between Orientation and Heating Load, \n with Glazing Area shown')

#Violin Plot
ggplot(Short, aes(x = factor(Glazing.Area), y = Heating.Load)) + 
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75)) + xlab('Height')  + 
  ggtitle('Heating Load by Glazing Area for Short Buildings')

# Histogram 1
h1 <- ggplot(bldg.data, aes(Heating.Load)) + geom_histogram(binwidth = 2) + 
  xlab('Heating.Load') + ylab('Count')+ ggtitle('Histogram of Heating Load for Short Buildings')

# Histogram 2
h2 <- ggplot(Short, aes(Cooling.Load)) + geom_histogram(binwidth = 1) + 
  xlab('Cooling.Load') + ylab('Count')+ ggtitle('Histogram of Cooling Load for Short Buildings')

grid.arrange(h1,h2,ncol=2)

#Scatterplot Grid
options(repr.plot.width=8, repr.plot.height=11)
ggplot(bldg.data, aes(Surface.Area, Heating.Load)) + 
  geom_point(aes(color = Glazing.Area,  shape = factor(Glazing.Area.Distribution)), alpha = 0.3) +
  facet_grid(factor(Overall.Height) ~ factor(Orientation)) +
  xlab('Surface Area') + ylab('Heating Load') + 
  ggtitle('Relationship between Surface Area and Heating Load, \n with Height, 
              \n and shape showing Orientation')

# Tall Buildings
options(repr.plot.width=8, repr.plot.height=8)
scatterplotMatrix(~ Relative.Compactness + Surface.Area + Wall.Area + Roof.Area + 
                      Orientation + Glazing.Area + Glazing.Area.Distribution + Heating.Load, data = Tall)

ggplot(Tall, aes(x = Glazing.Area, y = Heating.Load)) + geom_point() + 
  xlab('Glazing.Area') + ylab('Heating.Load') + 
  ggtitle('Relationship between Orientation and Heating Load for Tall Buildings')

#Violin Plot
ggplot(Tall, aes(x = factor(Glazing.Area), y = Heating.Load)) + 
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75)) + xlab('Glazing Area')  + 
  ggtitle('Heating Load by Glazing Area')

Tall.Dark<-subset(Tall,Glazing.Area==0)
summary(Tall.Dark)

options(repr.plot.width=8, repr.plot.height=8)
scatterplotMatrix(~ Relative.Compactness + Surface.Area + Wall.Area + Roof.Area + 
                    Orientation  +  Heating.Load, data = Tall.Dark)

# Look at all buildings again

# 2D Scatterplot 
ggplot(bldg.data, aes(Surface.Area, Heating.Load)) + geom_point(aes(color = factor(Overall.Height))) + 
  xlab('Surface Area') + ylab('Heating Load') + 
  ggtitle('Relationship between Heating Load and Surface Area, \n with Overall Height Shown')

# do it with Glazing Area
ggplot(bldg.data, aes(Surface.Area, Heating.Load)) + geom_point(aes(color = factor(Glazing.Area),shape = factor(Overall.Height))) + 
       xlab('Surface Area') + ylab('Heating Load') + 
       ggtitle('Relationship between Cooling Load and Surface Area, \n with Overall Height Shown')

# Do the same for Cooling Load

ggplot(bldg.data, aes(x = factor(Orientation), y = Cooling.Load)) + geom_boxplot() + 
  xlab('Orientation') + ggtitle('Cooling Load by Orientation')

ggplot(bldg.data, aes(x = factor(Glazing.Area.Distribution), y = Cooling.Load)) + geom_boxplot() + 
  xlab('Orientation') + ggtitle('Cooling Load by Glazing Area Distribution')