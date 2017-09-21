require(ggplot2)
require(rpart)
require(caret)
require(corrplot)
require(reshape2)
require(Amelia)
require(doParallel)
require(scholar)
ls(pos="package:corrplot")
ls(pos="package:reshape2")
ls(pos="package:Amelia")


getwd()
setwd('!!Regression/ML Process/!2 Restaurent Revenue Prediction/! 6 Understand the EDA Process/')
Rest_training=read.csv('train.csv',header=T,sep=",",na.strings=c("NA"," ",""))
dim(Rest_training)
str(Rest_training)
levels(Rest_training$Type)=c(levels(Rest_training$Type),"MB")

## Using bar Plot
## Bar plot Supported to Single Categorical variable and double Categorical variable
?geom_bar()
x11()

ggplot(Rest_training,aes(x=City.Group))+geom_bar()
ggplot(Rest_training,aes(x=Type))+geom_bar()
ggplot(Rest_training,aes(x=City))+geom_bar()

ggplot(Rest_training,aes(x=City.Group))+geom_bar(fill="red",colour="black")
ggplot(Rest_training,aes(x=Type))+geom_bar(fill="red",colour="black")
ggplot(Rest_training,aes(x=City))+geom_bar(fill="red",colour="black")

ggplot(Rest_training,aes(x=Type,fill=City.Group))+geom_bar()
ggplot(Rest_training,aes(x=City.Group,fill=Type))+geom_bar(position="dodge")
ggplot(Rest_training,aes(x=City.Group,fill=City))+geom_bar(position="fill")


### Using Dot Plot
###

?geom_dotplot()
x11()
ggplot(Rest_training,aes(x=Type,fill=P17))+geom_dotplot()


ggplot(Rest_training,aes(x=P17))+geom_dotplot(method="histodot",
  binwidth=0.4,
  fill="red",colour="black",
  binpositions="dotdensity",dotsize=0.8,
  stackdir="down")

ggplot(Rest_training,aes(x=revenue))+geom_dotplot()
ggplot(Rest_training,aes(x=P17,fill=Type))+geom_dotplot(fill="green",colour="black",binwidth=0.1)

#### Using Histogram for single variable
#### Histgrom is supports with Factor and Numerical

?geom_histogram
x11()
ggplot(Rest_training,aes(x=P10))+geom_histogram(fill="red",colour="black")
ggplot(Rest_training,aes(x=revenue,fill=City.Group))+geom_histogram(position="dodge",bins=10,aes(y=..density..))
ggplot(Rest_training,aes(x=revenue,fill=Type))+geom_histogram(position="dodge",bins=10,aes(y=..density..))
ggplot(Rest_training,aes(x=P17,fill=Type))+geom_histogram(position="dodge",bins=10,aes(y=..density..))


### Using density Plots for single variable $ Multiple variable
?geom_density
X11()
ggplot(Rest_training,aes(x=P17))+geom_density()
ggplot(Rest_training,aes(x=P17))+geom_density(fill="green",colour="black")
ggplot(Rest_training,aes(x=P17,fill=City.Group))+geom_density()
ggplot(Rest_training,aes(x=P17,fill=City.Group))+geom_density(position="stack")
ggplot(Rest_training,aes(x=P17,fill=City.Group))+geom_density(position="fill")
ggplot(Rest_training,aes(x=revenue,fill=City.Group))+geom_density(position="stack")
ggplot(Rest_training,aes(x=revenue,fill=City.Group))+geom_density(position="stack",adjust=1)
ggplot(Rest_training,aes(x=revenue,fill=City.Group))+geom_density(position="stack",adjust=2)
ggplot(Rest_training,aes(x=revenue))+geom_density(position="stack",fill="darkgreen",colour="black")+coord_flip()
ggplot(Rest_training,aes(x=revenue))+geom_density(position="stack",fill="darkgreen",colour="black",adjust=2)

# Using boxplot single variable and multple variable
?geom_boxplot
x11()

ggplot(Rest_training,aes(x=factor(0),y=P17))+geom_boxplot()
ggplot(Rest_training,aes(x=City.Group,y=P17))+geom_boxplot()
ggplot(Rest_training,aes(x=factor(0),y=P20))+geom_boxplot()
ggplot(Rest_training,aes(x=City,y=P17))+geom_boxplot()



ggplot(Rest_training,aes(x=City.Group,y=P17))+geom_boxplot(fill="green",colour="red")
ggplot(Rest_training,aes(x=City.Group,y=P17,P18))+geom_boxplot(position="identity",fill="blue",colour="red")
ggplot(Rest_training,aes(x=City.Group,y=P17))+geom_boxplot(notch=FALSE,outlier.colour="red",outlier.size=5)
ggplot(Rest_training,aes(x=City.Group,y=P17))+geom_boxplot(varwidth=TRUE,outlier.colour="red",outlier.size=5)
ggplot(Rest_training,aes(x=City.Group,y=P17))+geom_boxplot(outlier.colour="red",outlier.size=5)


ggplot(Rest_training,aes(x=City.Group,y=P17))+geom_boxplot(outlier.colour="red",outlier.size=5,fill="red",colour="black")+coord_flip()
ggplot(Rest_training,aes(x=City.Group,y=P17))+geom_boxplot(outlier.colour="red",outlier.size=5)+coord_equal()
ggplot(Rest_training,aes(x=City.Group,y=P17))+geom_boxplot(outlier.colour="red",outlier.size=5)+coord_cartesian()
ggplot(Rest_training,aes(x=City.Group,y=P17))+geom_boxplot(outlier.colour="red",outlier.size=5)+coord_polar()
ggplot(Rest_training,aes(x=City.Group,y=P17))+geom_boxplot(outlier.colour="red",outlier.size=5)+coord_fixed()
ggplot(Rest_training,aes(x=City.Group,y=P17))+geom_boxplot(outlier.colour="red",outlier.size=5)+coord_trans()
ggplot(Rest_training,aes(x=City.Group,y=P17))+geom_boxplot(outlier.colour="red",outlier.size=5)+coord_quickmap()
ggplot(Rest_training,aes(x=City.Group,y=P17))+geom_boxplot(outlier.colour="red",outlier.size=5)+coord_
ggplot(Rest_training,aes(x=City.Group,y=P17))+geom_boxplot(outlier.colour="red",outlier.size=5)+benchplot()
ggplot(Rest_training,aes(x=City.Group,y=P17))+geom_boxplot(outlier.colour="red",outlier.size=5)+coord_polar()

ls(pos="package:ggplot2")





