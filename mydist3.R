#############################################################################
# A {mydist} function to compute common pairwise coefficients of distance/dissimilarity
# "x" and "y" are two vectors of equal length with presence/absence or abundance/count values
# Indices for both abundance and presence-absence data are included
# Missing values are not allowed
# For presence/absence (binary) data some coefficients cannot be computed
# 
# The author assumes no liability for errors in this function
#
# Written by Michal Kowalewski, University of Florida, kowalewski@ufl.edu
# Last updated: June 15, 2014
# R version 3.0.1
#############################################################################

mydist<- function(x,y) {
 if (length(x)!=length(y)){stop("vectors of unequal length")}
 if (length(x)==length(y)){
   if (length(which(c(x,y)==0 | c(x,y)==1))==length(c(x,y)))
   {warning("binary data: missing values generated for measures that require abundance data")}
   if (length(na.omit(c(x,y)))!=length(c(x,y)))
   {stop("missing values are not allowed")}
 # abundance-based indices
   if (length(which(c(x,y)==0 | c(x,y)==1))==length(c(x,y)))
    {
    BC<- NA; JC1<- NA; JC2<- NA; SC1<- NA; SC2<- NA; tn<- NA; n1<- NA; n2<- NA
    }
   if (length(which(c(x,y)==0 | c(x,y)==1))!=length(c(x,y)))
   {
   tn<- sum(c(x,y)); n1<- sum(x); n2<- sum(y)
   BC<- sum(abs(x-y))/(sum(x)+sum(y))
   S1X<- length(which(x==1 & y>0))
   S2X<- length(which(x==2 & y>0))
   S1Y<- length(which(y==1 & x>0))
   S2Y<- length(which(y==2 & x>0))
   if (S1Y==0) S1Y<-1; if (S2Y==0) S2Y<-1; if (S1X==0) S1X<-1; if (S2X==0) S2X<-1
   x2<- x[which(x*y!=0)]
   y2<- y[which(x*y!=0)]
   U1<-sum(x2/sum(x))
   V1<-sum(y2/sum(y))
   U2<- sum(x2/sum(x))+(((sum(y)-1)/sum(y))*(S1Y/(2*S2Y))*sum(as.numeric(y2==1)*(x2/sum(x))))
   V2<- sum(y2)/sum(y)+(((sum(x)-1)/sum(x))*(S1X/(2*S2X))*sum(as.numeric(x2==1)*(y2/sum(y))))
   if (U2>1) {U2=1}
   if (V2>1) {V2=1}
   JC1<- (U1*V1)/(U1+V1-U1*V1)
   JC2<- (U2*V2)/(U2+V2-U2*V2)
   SC1<- 2*U1*V1/(U1+V1)
   SC2<- 2*U2*V2/(U2+V2)
   }
 # presence-absence indices
   x[x>0]<- 1
   y[y>0]<- 1
   z<- x+y
   a<- length(which(z==2))
   b<- length(which(z==1 & x==1))
   c<- length(which(z==1 & y==1))
   d<- length(which(z==0))
   ts<- length(x)-d
   no<- sum(c(x,y))
   no1<- sum(x)
   no2<- sum(y)
   n3<- a+b+c
   FA<- (n3+sqrt(n3))*a/((n3+sqrt(n3))*a+3/2*b*c)
   SM<- (a+d)/(a+b+c+d)
   J<- a/(a+b+c)
   S<- 2*a/(2*a+b+c)
   out<- rbind(a,b,c,d,ts,tn,n1,n2,no,no1,no2,SM,J,S,1-J,1-S,FA,1-BC,BC,JC1,JC2,SC1,SC2,1-JC1,1-JC2,1-SC1,1-SC2)
   rownames(out)<- c(" 1. Shared taxa"," 2. Present in sample 1 only"," 3. Present in sample 2 only",
                  " 4. Shared absences", " 5. Total number of species present"," 6. Total number of specimens",
			" 7. Total number of specimens in sample 1"," 8. Total number of specimens in sample 2",
			" 9. Total number of occurrences","10. total number of occurrences in sample 1",
			"11. Total number of occurrences in sample 2","12. Simple Matching Coefficient",
                   "13. Jaccard Similarity","14. Sorenson Similarity","15. Jaccard Dissimilarity","16. Sorenson Dissimilarity",
			 "17. Forbes-Alroy Similarity", "18. Percentage Similarity", "19. Bray Curtis Dissimilarity",
                   "20. Jaccard-Chao Similarity","21. Jaccard-Chao Similarity Adj","22. Sorenson-Chao Similarity",
			"23. Sorenson-Chao Similarity Adj", "24. Jaccard-Chao Dissimilarity","25. Jaccard-Chao Dissimilarity Adj",
			"26. Sorenson-Chao Dissimilarity","27. Sorenson-Chao Dissimilarity Adj")
   colnames(out)<- c("parameter")
   return(out)
   }
  }
