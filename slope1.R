#######################
###load data
#######################
rm(list=ls())
options(max.print=999999)
library(pacman)
p_load(here)
p_load(openxlsx)
p_load(tidyverse)
library(animation)
library(bueri)
set.seed(333)
n<-25

z<-seq(-1,1,length.out = n)
x<-seq(-1,1,length.out = n)
slpe_lims<- c(-1.5,1.5)
slpe_coefs<-(solve(matrix(c(1,-1,1,1),nrow = 2,byrow = T)) %*% slpe_lims) %>% drop

intrcp<-runif(1,-1,1)

f1<-function(x,z,intrcp,slpe_coefs){
  return(intrcp+(slpe_coefs[1]+slpe_coefs[2]*z)*x)
}

y<-outer(x, z,f1,intrcp=intrcp,slpe_coefs=slpe_coefs)

my_theta<-seq(-45,45,length.out = 40)
my_theta<-c(my_theta,rev(my_theta))

nrz <- nrow(y)
ncz <- ncol(y)
# Create a function interpolating colors in the range of specified colors
jet.colors <- colorRampPalette( c("blue", "green") )
# Generate the desired number of colors from this palette
nbcol <- 100
color <- jet.colors(nbcol)
# Compute the z-value at the facet centres
zfacet <- y[-1, -1] + y[-1, -ncz] + y[-nrz, -1] + y[-nrz, -ncz]
# Recode facet z-values into color indices
facetcol <- cut(zfacet, nbcol)

saveGIF({
  ani.options(interval = 0.2, nmax = length(my_theta))
  ## use a loop to create images one by one
  for (i in 1:ani.options('nmax')) {
    persp(x, z, y, theta = my_theta[i], phi = 45, expand = 0.5,xlab="x",ylab="z",zlab="y",col = color[facetcol])
    ani.pause()   ## pause for a while ('interval')
  }
}, interval = 0.05, movie.name = 'slope1.gif', ani.width = 600, ani.height = 500)



persp(x, z, y,col = color[facetcol])
