# ISLR - PCA

library(ISLR)
USArrests

states = row.names(USArrests)
states

apply(USArrests, 2, mean) #1 for the rows and 2 (here) for the columns
apply(USArrests, 2, var)

pr.out = prcomp(USArrests, scale =TRUE)
names(pr.out)
pr.out$rotation
nrow(USArrests)

pr.out$x

#plot it
biplot(pr.out , scale =0)
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot (pr.out , scale =0)

pr.out$sdev
pr.var =pr.out$sdev^2

pve = pr.var/sum(pr.var)
pve

plot(pve, xlab="Principal Component", ylab=" Proportion of Variance Explained ", ylim=c(0,1), type="b")

plot(cumsum (pve ), xlab=" Principal Component ", ylab ="Cumulative Proportion of Variance Explained ", ylim=c(0,1) ,type="b")

