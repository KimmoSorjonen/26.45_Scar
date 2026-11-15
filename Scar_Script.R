

############################# BIENVENUE #############################

############# I SOLEMNLY SWEAR THAT I AM UP TO NO GOOD ##############

## Loading package

library(MASS)
library(metafor)
library(lavaan)

#################
## Data
## Correlations reported by Fukuzaki and Iwata (2027)

rm <- matrix(c(
  
   1.00,  0.65, -0.34, -0.33, -0.26, -0.22,  0.30,  0.27,  0.29,  0.24,  0.24,  0.26,  0.27,  0.28,
   0.65,  1.00, -0.35, -0.39, -0.20, -0.27,  0.20,  0.26,  0.25,  0.34,  0.20,  0.31,  0.21,  0.25,
  -0.34, -0.35,  1.00,  0.71,  0.35,  0.37, -0.26, -0.30, -0.24, -0.24, -0.31, -0.33, -0.25, -0.28,
  -0.33, -0.39,  0.71,  1.00,  0.35,  0.37, -0.22, -0.33, -0.26, -0.29, -0.29, -0.42, -0.20, -0.28,
  -0.26, -0.20,  0.35,  0.35,  1.00,  0.66, -0.23, -0.24, -0.24, -0.19, -0.30, -0.32, -0.10, -0.17,
  -0.22, -0.27,  0.37,  0.37,  0.66,  1.00, -0.27, -0.28, -0.16, -0.29, -0.29, -0.33, -0.18, -0.10,
   0.30,  0.20, -0.26, -0.22, -0.23, -0.27,  1.00,  0.72, -0.01,  0.03,  0.27,  0.23,  0.48,  0.36,
   0.27,  0.26, -0.30, -0.33, -0.24, -0.28,  0.72,  1.00,  0.05,  0.08,  0.24,  0.33,  0.40,  0.54,
   0.29,  0.25, -0.24, -0.26, -0.24, -0.16, -0.01,  0.05,  1.00,  0.72,  0.36,  0.34,  0.17,  0.16,
   0.24,  0.34, -0.24, -0.29, -0.19, -0.29,  0.03,  0.08,  0.72,  1.00,  0.28,  0.37,  0.18,  0.16,
   0.24,  0.20, -0.31, -0.29, -0.30, -0.29,  0.27,  0.24,  0.36,  0.28,  1.00,  0.71,  0.32,  0.28,
   0.26,  0.31, -0.33, -0.42, -0.32, -0.33,  0.23,  0.33,  0.34,  0.37,  0.71,  1.00,  0.27,  0.31,
   0.27,  0.21, -0.25, -0.20, -0.10, -0.18,  0.48,  0.40,  0.17,  0.18,  0.32,  0.27,  1.00,  0.64,
   0.28,  0.25, -0.28, -0.28, -0.17, -0.10,  0.36,  0.54,  0.16,  0.16,  0.28,  0.31,  0.64,  1.00), nrow=14)

n <-  500 ## total sample size

dfall <- data.frame(mvrnorm(n=n, mu=rep(0,14), ## generating data frame with required size and corr.
            Sigma=rm, empirical=T))

names(dfall) <- c("WE1","WE2","BO1","BO2","NE1","NE2","EX1", ## naming variables
               "EX2","CO1","CO2","AG1","AG2","OP1","OP2")

## subsets of data for different combinations

df1 <- dfall[,c("WE1","WE2","NE1","NE2")]
df2 <- dfall[,c("WE1","WE2","EX1","EX2")]
df3 <- dfall[,c("WE1","WE2","CO1","CO2")]
df4 <- dfall[,c("WE1","WE2","AG1","AG2")]
df5 <- dfall[,c("WE1","WE2","OP1","OP2")]
df6 <- dfall[,c("BO1","BO2","NE1","NE2")]
df7 <- dfall[,c("BO1","BO2","EX1","EX2")]
df8 <- dfall[,c("BO1","BO2","CO1","CO2")]
df9 <- dfall[,c("BO1","BO2","AG1","AG2")]
df10 <- dfall[,c("BO1","BO2","OP1","OP2")]

dflist <- list(df1,df2,df3,df4,df5,df6,df7,
               df8,df9,df10) ## list with all subsets

## The three models, with dy = Y2-Y1

m1 <- "dy ~ x1 + y1"
m2 <- "dy ~ x1 + y2"
m3 <- "dy ~ x1"

modlist <- list(m1,m2,m3) ## list with all three models

#################
## Figure

panlab <- c("WE/BO → Trait", 
            "Trait → WE/BO") ## labels on top

rlab <- c("WE,NE","WE,EX","WE,CO","WE,AG","WE,OP",
          "BO,NE","BO,EX","BO,CO","BO,AG","BO,OP") ## labels to the right

slab <- c("4.RMA","","3.no.adj", ## labels for y-axis
          "2.adj.y2","1.adj.y1")

cx <- 0.8 ## sizing factor
r.low <- -0.25 ## range, lower
r.upp <- 0.25 ## range, upper
f.upp <- r.upp+0.6*(r.upp-r.low) ## room for text

if(dev.cur()==2) dev.off() ## removing earlier plots
par(mar=c(1,1,0,0), oma=c(1.5,3,1.4,1.4), mfrow=c(10,2)) ## setting margins and layout
pl <- 1 ## keeping track of panels

for(k in 1:10){ ## for each combination/subset of data
  
  df <- dflist[[k]] ## picking subset of data from the list
  
  for(i in 1:2){ ## in both directions
    
    if(i==1) names(df) <- c("x1","x2","y1","y2") ## WE/BO as X and trait as Y
    if(i==2) names(df) <- c("y1","y2","x1","x2") ## Trait as X and WE/BO as Y
    
    dy <- df$y2 - df$y1 ## difference score
    
    numeff <- 5 ## number of rows per panel (one is empty)
    
    plot(c(r.low,f.upp),c(0.5,5.5), type="n",xaxt="n",yaxt="n",xlab="",ylab="") ## empty plot
    axis(1,at=c(-0.2,0,0.2),labels=F, cex.axis=cx) ## x-tics
    if(k==10) axis(1,at=c(-0.2,0,0.2),labels=c(-0.2,0,0.2), cex.axis=cx) ## x-labels
    axis(2,at=1:5,labels=F, las=1, cex.axis=cx) ## y-tics
    if(i==1) axis(2,at=1:5,labels=slab, las=1, cex.axis=cx) ## y-labels
    
    if(k==1) mtext(panlab[i],3, line=0.2, cex=cx) ## label on top
    if(i==2) mtext(rlab[k],4, line=0.2, cex=cx) ## label to the right
    
    lines(c(0,0),c(-1,15),col="gray") ## vertical gray line at x=0
    
    ball <- vector() ## to be filled with effects below
    seall <- vector() ## to be filled with standard error below
    
    for(j in 1:3){ ## for the three models
      
      fit <- lm(modlist[[j]], data=df) ## fitting the model
      b <- fit$coefficients[2] ## reg. coefficient
      low <- confint(fit)[2,1] ## CI, low
      upp <- confint(fit)[2,2] ## CI, upp
      se <- summary(fit)$coefficients[2,2] ## standard error
      tx <- paste(round(b,2)," [", round(low,2),"; ", 
                  round(upp,2),"]", sep="") ## string with values
      
      ball <- c(ball,b) ## adding effect to object
      seall <- c(seall,se) ## adding se to object
      
      lines(c(-2,r.upp),c(numeff,numeff), col="gray") ## vertical gray line
      arrows(low,numeff,upp,numeff,angle=90,code=3,length=0.05,lwd=2) ## CI
      points(b,numeff,pch=21,col="black",bg="black") ## point for effect
      text(r.upp,numeff,tx,pos=4,cex=cx) ## adding string with values
      
      numeff <- numeff-1 ## next row, please
    }
    
    b.fish <- 0.5*log((1+ball)/(1-ball)) ## Fisher's transformation of effects
    se.fish <- 0.5*log((1+seall)/(1-seall)) ## Fisher's transformation of SE
    
    we <- 1/3 ## weighting the effects (a1, b1, and c1) equally
    
    ###### Following Bartos et al.
    
    random1 <- rma(yi=b.fish, vi=se.fish^2)
    res.ma <- rma(yi=b.fish, vi=(se.fish^2)/we, tau=random1$tau2)
    
    ######
    
    pred <- predict(res.ma, transf=transf.ztor) ## transforms back from Fisher's
    metb <- pred$pred; metlow <- pred$ci.lb; metupp <- pred$ci.ub ## estimate with lower and upper CI limit
    
    lines(c(-2,5),c(2,2),lty=2) ## dashed line
    lines(c(-2,r.upp),c(1,1),col="gray") ## gray vertical line
    
    polygon(c(metlow,metb,metupp,metb), c(1,1.5,1,0.5), ## diamond for RMA 
            col = "black", border = "black", lwd = 1)
    
    tx.ma <- paste(round(metb,2)," [", round(metlow,2),"; ", round(metupp,2),"]", sep="") ## string with values
    text(r.upp,1,tx.ma,pos=4,cex=cx) ## adding string of values
    
    text(0.16,1, LETTERS[pl], pos=4, cex=1.5) ## panel letter
    
    pl <- pl+1
  }
}


#############################
## The SPAM

spam <- "

## latent gx, gy

gx =~ b*x1 + b*x2
gy =~ c*y1 + c*y2

## second-order trait-variable

t =~ a*gx+a*gy

## state-correlations
## not required here

#x1 ~~ d*y1
#x2 ~~ d*y2

## (error) variances

t ~~ 1*t
gx ~~ 1*gx
gy ~~ 1*gy
x1 ~~ x1
x2 ~~ x2
y1 ~~ y1
y2 ~~ y2

"

dfspam <- df6 ## burnout and neuroticism

names(dfspam) <- c("x1","x2","y1","y2")

fit <- lavaan(spam, meanstructure=F, data=dfspam) ## fitting the SPAM

summary(fit, fit.measures=T, ## let's have a look 
        standardized=T, ci=T, rsq=T)


########################## MISCHIEF MANAGED #########################

############################# AU REVOIR #############################



