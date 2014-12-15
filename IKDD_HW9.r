
#Input data
data <-read.table(url("http://www.datagarage.io/api/5488687d9cbc60e12d300ba5"))
o<-seq(2, 4000, by=2)
d_c<-as.character(data[o, ])
p_m<-sapply(strsplit(d_c[], ""), function(d_c) which(d_c == ":"))
data<-data.frame(X=as.double(substr(d_c,p_m[2,]+1, 38)), Y=as.double(substr(d_c,p_m[1,]+1, 17)))
plot(data,pch=20)

#Set parameter
#data <- read.csv("~/curve/HW.csv", stringsAsFactors=FALSE)
x <- data$X
y <- data$Y
#plot(x,y,pch=19)
xx <- seq(-10,10, length=2000)
#plot(x,y,pch=10,ylim=c(-10,8))
View(xx)
#test curve
fit1 <- lm(y~x)
fit4 <- lm(y~poly(x,4,raw=TRUE))
fit5 <- lm(y~poly(x,5,raw=TRUE))
fit6 <- lm(y~poly(x,6,raw=TRUE))

#y = f + ex + dx^2 + cx^3 + bx^4 + ax^5
poly_five <- function(parameter, model) {
    coefs <- coef(model)
    res <- coefs[1] + (coefs[2] * parameter) + (coefs[3] * parameter^2) + (coefs[4] * parameter^3) + (coefs[5] * parameter^4) + (coefs[6] * parameter^5)
    return(res)
}

#predict result
yy <- poly_five(xx,fit5)
View(yy)
lines(xx,yy,col="Green",lwd=2)  #draw curve
#lines(xx, predict(fit5, data.frame(x=xx)), col="red")
write.csv(yy , file = "predict_y.csv")  #predict value of y


