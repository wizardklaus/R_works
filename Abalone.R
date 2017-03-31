abalone
names(abalone) <- c("Sex","Length","Diameter","Height","Whole weight"
                    ,"Shucked weight","Viscera weight","Shell weight"
                    , "Rings")
x <- c("M",0.455,0.365,0.095,0.514,0.2245,0.101,0.15,15)
abalone <- rbind(x,abalone)
? rbind
x
sessionInfo()
sample()
? lm
meanLength <- mean(abalone$Length)
model <- lm(abalone$'Whole weight' ~ Length + Sex, data=abalone)
x <- 1:3
cv <- function(x, na.rm=FALSE){
  sd(x, na.rm=na.rm)/mean(x, na.rm=na.rm)
}
abalone$`Whole weight`
names(abalone) <- c("Sex","Length","Diameter")
? subset
library(ggplot2)
qplot(x=Rings, y=Length, data=abalone)
plot(Length ~ Sex, data=abalone)
library(manipulate)
manipulate(
  plot( Length ~ Rings, data=abalone
        , axes=axes
        , cex=cex
        , pch=if(pch) 19 else 1)
        , axes=checkbox(TRUE, "Show axes")
        , cex=slider(0,5,initial=1,step=0.1,label="Point size")
        , pch=button("Fill points")
)
manipulate({
  if(is.null(manipulatorGetState("model"))){
    fit <- lm(Length~abalone$'Whole weight', data=abalone)
    manipulatorSetState("model",fit)
    print("hey, I just estimated a model!")
  }else{
    fit <- manipulatorGetState("model")
    print("Not I just retrieved the model from storage")
  }
})
manipulate({
  plot(Length~Rings, data=abalone)
  xy <- manipulatorMouseClick()
  if(!is.null(xy)) points(xy$userX, xy$userU, pch=4)
})
dataplot <- function(dat){
  name <- sys.call()[[2]]
  vars <- as.list(names(dat))
  e <- new.env()
  e$data <- name
  manipulate(
    {
      form=as.formula(paste(y,x,sep="~"))
      plot(form,data=dat,main=as.character(name),las=1)
      e$form <- form
    },
    x=do.call(picker,c(vars,initial=vars[1])),
    y=do.call(picker,c(vars,initial=vars[2]))
  )
  invisible(e)
}
f <- dataplot(abalone)
class(abalone)
library(abalone)
data(abalone)
plot(abalone)
abalone
plot(abalone$Height,abalone$Length)
plot(abalone$Height,abalone$Length,xlab="Height",ylab="Length",main="Abalone",pch="+",cex=.1,col="#343434",
     xlim=c(0,0.4),ylim=c(0,0.9))
example(points)
min(abalone$Height,na.rm=TRUE)
max(abalone$Height,na.rm=TRUE)
min(abalone$Length,na.rm=TRUE)
max(abalone$Length,na.rm=TRUE)



















