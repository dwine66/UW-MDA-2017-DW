#UW Data Science - Winter 2017
#Assignment #1
#Dave Wine 8430191

# Import packages
require(ggplot2)
require(car)

# File read function
read.bldg = function(file = 'EnergyEfficiencyData.csv'){

bldg.data <- read.csv(file, header=TRUE, stringsAsFactors=FALSE)

factcols <- c('Orientation','Glazing.Area.Distribution')

bldg.data[, factcols]<-lapply(bldg.data[,factcols], as.numeric)

bldg.data[complete.cases(bldg.data),]
}

# 2D Scatterplot function
#This works OK but I haven't figured out how to pass or infer labels yet....
plots.2DS=function(x,y,c){ 
ggplot(bldg.data, aes(x, y)) + geom_point(aes(color = factor(c))) 
  #+ 
  #xlab(x) + ylab(y) + 
  #ggtitle('Relationship between'+ x +' and ' + y +' \n with '+c+' Shown')
}

# Read data in
bldg.data = read.bldg()

# View dataset and summary statistics
str(bldg.data)
summary(bldg.data)

# Create Factors from appropriate variables
factOrient <- cut(bldg.data$Orientation,breaks=4,labels=1:4)
#factOrient <- lapply(factOrient, as.character)
factDist <- cut(bldg.data$Glazing.Area.Distribution,breaks=6,labels=1:6)
#factDist <- lapply(factDist, as.character)
factHeight <- cut(bldg.data$Overall.Height,breaks=2,labels=1:2)

# Main Analysis

# (See Word Doc for analysis and conclusions)
# Try various plots for Heating Load

#First, look at the overall correlation plots
options(repr.plot.width=8, repr.plot.height=8)
scatterplotMatrix(~ Relative.Compactness + Surface.Area + Wall.Area + Roof.Area + 
Overall.Height + Orientation + Glazing.Area + Glazing.Area.Distribution + Heating.Load, data = bldg.data)

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

#Scatterplot Grid
options(repr.plot.width=8, repr.plot.height=11)
ggplot(bldg.data, aes(Surface.Area, Heating.Load)) + 
  geom_point(aes(color = Relative.Compactness,  size = Glazing.Area, shape = factor(Glazing.Area.Distribution)), alpha = 0.3) +
  facet_grid(factor(Overall.Height) ~ factor(Orientation)) +
  xlab('Surface Area') + ylab('Heating Load') + 
  ggtitle('Relationship between Surface Area and Heating Load, \n with Height, 
              \n with marker radius indicating Relative Compactness \n and shape showing Orientation')

# Look at each building height bin separately to study other effects

# Tall Buildings

Tall<-subset(bldg.data,Overall.Height==7)

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

TallGlassy<-subset(Tall,Glazing.Area==0.4)

options(repr.plot.width=8, repr.plot.height=8)
scatterplotMatrix(~ Relative.Compactness + Surface.Area + Wall.Area + Roof.Area + 
                    Orientation  + Glazing.Area.Distribution + Heating.Load, data = TallGlassy)

# Short Buildings

Short<-subset(bldg.data,Overall.Height==3.5)

options(repr.plot.width=8, repr.plot.height=8)
scatterplotMatrix(~ Relative.Compactness + Surface.Area + Wall.Area + Roof.Area + 
                    Orientation + Glazing.Area + Glazing.Area.Distribution + Heating.Load, data = Short)

#
# Do the same for Cooling Load
#

ggplot(bldg.data, aes(x = factor(Orientation), y = Cooling.Load)) + geom_boxplot() + 
  xlab('Orientation') + ggtitle('Cooling Load by Orientation')

ggplot(bldg.data, aes(x = factor(Glazing.Area.Distribution), y = Cooling.Load)) + geom_boxplot() + 
  xlab('Orientation') + ggtitle('Cooling Load by Glazing Area Distribution')

# 2D Scatterplot 
ggplot(bldg.data, aes(Surface.Area, Cooling.Load)) + geom_point(aes(color = factor(Overall.Height))) + 
  xlab('Surface Area') + ylab('Heating Load') + 
  ggtitle('Relationship between Cooling Load and Surface Area, \n with Overall Height Shown')

