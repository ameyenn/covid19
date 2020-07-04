library(data.table)
library(factoextra)
library("GGally")
library(tidyverse)
library(tidygraph)
library(dplyr)
library(Hmisc)
#data source https://covidtracking.com/race
####AIM: explore the existence of a difference between expected and acctual death rates between
####Afro-American and White populations. Expcted is based on racial proportion, actual is the racial
####deaths

setwd("C:/Users/ameye/OneDrive/R")
df<-fread("./datSets/UScovidRaceJuly.csv") #available on github site
str(df)
df<-df %>% remove_rownames %>% column_to_rownames(var="Code") #set rownames

####Check normal distribition
#### Ho is that each is normal, hence p>0.05 at 95%, look for p<0.05 to reject and find distribution not normal
st<-apply(df[, 3:15], 2, shapiro.test)

####Check there is some variance, need some to apply PCA
var(df[,c(3:9)]) 

####Check distribution and correlations. PCA also works well with some correlation
ggpairs(df[, 3:15]) #library(GGally)

####Check correlation and p-values in more detail
####Ho is there is no correlation, ie r=0, p>0.05, if p<-0.05 consider the correlation.Some of the correlations
####seem just between similar measures. But the AA% and W% might show something of interest.

rcorr(as.matrix(df[, 3:15]), type=c("spearman")) #library(Hmisc)

#### INVESTIGATE. STEP 1 PLOTS
#### Plot1: US POverty

plot(df[,3], type="l", main="US % poverty per state")              
text(df[,3], row.names(df), cex=0.85)

#### Plot2
#### shows the difference between a state's race proportion and corvid race death proportion.
#### AL shows 18% difference for Afro-Americans, 44% of deaths as compared to 26% of the population
#### AL shows -21% difference for Whites, 48% of deaths as compared to 69% of the population.
#### note: other ethicnities data incomplete

plot(df[,15], type="l", ylim=c(-0.6, 0.5), main = "AA and W covid Deaths", ylab = "% acc-expectded ", xlab="State")   
abline(h=0)
text(df[,15], row.names(df), cex=0.85)
lines(df[,14], type="l", col="red")
text(df[,14], row.names(df), cex=0.75)

#### INVESTIGATE PCA

pca<-prcomp(df[,3:15], scale=TRUE) 
fviz_pca_biplot(pca) 

####PCA Biplot show negative relationship see the vectors forming straight line.Separates the AA/B and W.
####There is no relationship with the vectors pointing upwards left. PC1(45%) and PC2(24%) describe around 70% 
####of the variance across the variables. THe contributions are shown in the next section.

as.data.frame(pca$rotation[,1:2])

#### INVESTIAGTE Dendrogram and clusters
hc <- hclust(dist(as.data.table(df[,3:15])), "ave")
dend1 <- as.dendrogram(hc)
plot(dend1)

####locate and plot Clusters
sub_grp <- cutree(hc, k = 3)
table(sub_grp)
plot(hc, cex = 0.6)
rect.hclust(hc, k = 6, border = 2:5)
fviz_cluster(list(data = df[,3:15], cluster = sub_grp))

####heatmap
dfScale <- scale(df[, 3:15])
heatmap(dfScale, scale = "row")

#### Parametric: Consider t-tests berween white and AA deaths, check assumptions
var(df[,8])
var(df[,9])
#not equal
summary(df[,8:9])
plot(df[,8], type="l")
plot(df[,9], type="l")
#neither are normal shaped
boxplot(df[,8], df[,9]) 
t.test(df[,8],df[,9], var.equal=FALSE) #irrespective of assumptions, sample >30, suggests a difference.

#try and transform to normalish and test again

dt <- log10(df[,8:9]) #check of -Inf and reset
dt[dt == -Inf] <- 0

t.test(dt[,1],dt[,2], var.equal=FALSE) 

#again p<0.05 hence statistically significant, reject Ho of no difference

####Non-parametic tests of rank, Wilcox handels not normal, where there is group independence.
#But we are apply difference between the two groups and difference should be symetric around the median. 

wilcox.test(dfT[,8], dfT[,9], alternative = "two.sided", paired=TRUE) #p<0.05 suggests a difference

hist(df[,8]-df[,9])
median(df[,8]-df[,9]) #ignoring the outliers we have symmetry around the median of 227!

####Conclusion: based on tests with p<0.05 we are entitled to claim a statistically significant differnce. 
####PCA gives further evidence of a difference.
