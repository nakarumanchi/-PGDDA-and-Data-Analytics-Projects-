require(Hmisc) #### To identify the Missing Values
library(ggplot2) #### For EDA Purpose
install.packages("VIM")
require(VIM) #### To identify the Missing Values
require(gmodels) ### for Cross tableation
library(gridExtra) ### Creating the Grid

options(max.print = .Machine$integer.max)

### Set Path
getwd()


### Loading the file

Aging_report= read.csv('Aging_report.csv',header=T,sep=",",na.strings=c(" ","NA",""))
summary(Aging_report)
str(Aging_report)
dim(Aging_report)
describe(Aging_report)

### Change the class as per requirement

Aging_report$Enc.Dt = as.character(Aging_report$Enc.Dt)
Aging_report$First.Bill.Dt = as.character(Aging_report$First.Bill.Dt)
Aging_report$Lst.Bill.Dt = as.character(Aging_report$Lst.Bill.Dt)
Aging_report$Payer.Name = as.character(Aging_report$Payer.Name)
Aging_report$Sec.Payer = as.character(Aging_report$Sec.Payer)

contents(Aging_report)

### Identify the missing Values in Data, But it's Valid Cases
NA_values = which(colSums(sapply(Aging_report,is.na))>0)
sort(colSums(sapply(Aging_report[NA_values],is.na)), decreasing = TRUE)

### EDA For Billing Status Vs 30days Buckets
describe(Aging_report$Status)
ggplot(Aging_report, aes(x=reorder(Status, -table(Status)[Status]),fill=Status)) + geom_bar()

describe(Aging_report$X0.30)
aggregate(Aging_report$X0.30,by=list(Aging_report$Status),FUN = sum)
P1 = qplot(Status, data = Aging_report, geom = "bar", weight = X0.30 , main="Status Vs 30days bucket" ,xlab="Billing Status",fill=Status,width=0.1) + scale_y_continuous("0.30 days")

describe(Aging_report$X31.60)
aggregate(Aging_report$X31.60,by=list(Aging_report$Status),FUN = sum)
P2 = qplot(Status, data = Aging_report, geom = "bar", weight = X31.60 ,main="Status Vs 60days bucket" ,xlab="Billing Status",fill=Status,width=0.1) + scale_y_continuous("31.60 days")

describe(Aging_report$X61.90)
aggregate(Aging_report$X61.90,by=list(Aging_report$Status),FUN = sum)
P3 = qplot(Status, data = Aging_report, geom = "bar", weight = X61.90 ,main="Status Vs 90days bucket" ,xlab="Billing Status",fill=Status,width=0.1) + scale_y_continuous("31.60 days")

describe(Aging_report$X91.120)
aggregate(Aging_report$X91.120,by=list(Aging_report$Status),FUN = sum)
P4 = qplot(Status, data = Aging_report, geom = "bar", weight = X91.120 , main="Status Vs 120days bucket",xlab="Billing Status",fill=Status,width=0.1) + scale_y_continuous("91.120 days")

describe(Aging_report$X121.150)
aggregate(Aging_report$X121.150,by=list(Aging_report$Status),FUN = sum)
P5 = qplot(Status, data = Aging_report, geom = "bar", weight = X121.150, main="Status Vs 150days bucket",xlab="Billing Status",fill=Status,width=0.1) + scale_y_continuous("121.150 days")

describe(Aging_report$X151.180)
aggregate(Aging_report$X151.180,by=list(Aging_report$Status),FUN = sum)
P6 = qplot(Status, data = Aging_report, geom = "bar", weight = X151.180, main="Status Vs 180days bucket",xlab="Billing Status",fill=Status,width=0.1) + scale_y_continuous("151.180 days")

describe(Aging_report$X181.up)
aggregate(Aging_report$X181.up,by=list(Aging_report$Status),FUN = sum)
P7 = qplot(Status, data = Aging_report, geom = "bar", weight = X181.up , main="Status Vs above 181 days bucket",xlab="Billing Status",fill=Status,width=0.1) + scale_y_continuous("181.up days")

describe(Aging_report$Bal.Amt)
aggregate(Aging_report$Bal.Amt,by=list(Aging_report$Status),FUN = sum)
P8 = qplot(Status, data = Aging_report, geom = "bar", weight = Bal.Amt , main="Total Balance amount ",xlab="Billing Status",fill=Status,width=0.1) + scale_y_continuous("Balance amount ")

describe(Aging_report$Ins1.Amt)
aggregate(Aging_report$Ins1.Amt,by=list(Aging_report$Status),FUN = sum)
P9 = qplot(Status, data = Aging_report, geom = "bar", weight = Ins1.Amt , main="Total Balance amount - for Primary Insurance",xlab="Billing Status",fill=Status,width=0.1) + scale_y_continuous("Primary Insurance")

describe(Aging_report$Ins2.Amt)
aggregate(Aging_report$Ins2.Amt,by=list(Aging_report$Status),FUN = sum)
P10 = qplot(Status, data = Aging_report, geom = "bar", weight = Ins2.Amt , main="Total Balance amount - for Secondary Insurance",xlab="Billing Status",fill=Status,width=0.1) + scale_y_continuous("Secondary Insurance")

describe(Aging_report$Pat.Amt)
aggregate(Aging_report$Pat.Amt,by=list(Aging_report$Status),FUN = sum)
P11 = qplot(Status, data = Aging_report, geom = "bar", weight = Pat.Amt , main="Balance amount - Patient",xlab="Billing Status",fill=Status,width=0.1) + scale_y_continuous("Balance amount - Patient")

x11()
grid.arrange(P1,P2, P3, P4,P5,P6,P7,P8)
x11()
grid.arrange(P9,P10,P11) 

#### Buckets wise Vs Enc.Rendering plots
X11()
  ggplot(Aging_report, aes(x=reorder(Enc.Rendering, -table(Enc.Rendering)[Enc.Rendering]),fill=Status)) + 
  geom_bar()+coord_flip()
x11()
ggplot(data=Aging_report, aes(x=Enc.Rendering, y=X0.30, fill=Status)) +
  geom_bar(stat="identity")+  coord_flip()
X11()
ggplot(data=Aging_report, aes(x=Enc.Rendering, y=X31.60, fill=Status)) +
  geom_bar(stat="identity")+  coord_flip()
x11()
ggplot(data=Aging_report, aes(x=Enc.Rendering, y=X61.90, fill=Status)) +
  geom_bar(stat="identity")+  coord_flip()
x11()
ggplot(data=Aging_report, aes(x=Enc.Rendering, y=X91.120, fill=Status)) +
  geom_bar(stat="identity")+  coord_flip()
x11()
ggplot(data=Aging_report, aes(x=Enc.Rendering, y=X121.150, fill=Status)) +
  geom_bar(stat="identity")+  coord_flip()
x11()
ggplot(data=Aging_report, aes(x=Enc.Rendering, y=X151.180, fill=Status)) +
  geom_bar(stat="identity")+  coord_flip()
x11()
ggplot(data=Aging_report, aes(x=Enc.Rendering, y=X181.up,fill=Status)) +
  geom_bar(stat="identity")+  coord_flip()
x11()
ggplot(data=Aging_report, aes(x=Enc.Rendering, y=Bal.Amt ,fill=Status)) +
  geom_bar(stat="identity")+  coord_flip()
x11()
ggplot(data=Aging_report, aes(x=Enc.Rendering, y=Ins1.Amt,fill=Status)) +
  geom_bar(stat="identity")+  coord_flip()
x11()
ggplot(data=Aging_report, aes(x=Enc.Rendering, y=Ins2.Amt,fill=Status)) +
  geom_bar(stat="identity")+  coord_flip()
x11()
ggplot(data=Aging_report, aes(x=Enc.Rendering, y=Pat.Amt,fill=Status)) +
  geom_bar(stat="identity")+  coord_flip()

### Crosstabulation on Buckets and Enc.Rendering

sort(xtabs(X0.30~Enc.Rendering,Aging_report),decreasing=TRUE)
sort(xtabs(X31.60~Enc.Rendering ,Aging_report),decreasing=TRUE)
sort(xtabs(X61.90~Enc.Rendering ,Aging_report),decreasing=TRUE)
sort(xtabs(X91.120~Enc.Rendering ,Aging_report),decreasing=TRUE)
sort(xtabs(X121.150~Enc.Rendering ,Aging_report),decreasing=TRUE)
sort(xtabs(X151.180~Enc.Rendering ,Aging_report),decreasing=TRUE)
sort(xtabs(X181.up~Enc.Rendering ,Aging_report),decreasing=TRUE)

str(Aging_report)

### EDA process on (Billing Status, Balance amount - Patient and Enc.Rendering)
### Billing Status Vs Provider Id for Balance amount - Patient  
x11()
ggplot(data=Aging_report, aes(x=Enc.Rendering, y=Pat.Amt, fill=Status)) +
  geom_bar(stat="identity",position = "dodge")+  facet_grid(. ~ Status)+ theme_bw()+
  theme(axis.text.x = element_text(angle = 0))+coord_flip()
##geom_text(aes(label=Pat.Amt), vjust=0, colour="black",position=position_dodge(.9), size=2)

### Billing Status Vs Catogory of Primary Insurance Name for Balance amount - Patient
x11()
ggplot(data=Aging_report, aes(x=Fin.Class, y=Pat.Amt, fill=Status)) +
  geom_bar(stat="identity",position = "dodge")+  facet_grid(. ~ Status)+ theme_bw()+
  theme(axis.text.x = element_text(angle = 0))+coord_flip()

### Billing Status Vs Catogory of Secondary Payer Name for Balance amount - Patient
x11()
ggplot(data=Aging_report, aes(x=Sec.Fin.Class, y=Pat.Amt, fill=Status)) +
  geom_bar(stat="identity",position = "dodge")+  facet_grid(. ~ Status)+ theme_bw()+
  theme(axis.text.x = element_text(angle = 0))+coord_flip()


### Creating Cube to Retrive the Slice and Diece
#### Cube_30Days  ########
str(Aging_report)
Cube_30Days <- tapply(Aging_report$X0.30,
  Aging_report[,c("Enc.Rendering","Status","Fin.Class","Sec.Fin.Class")], 
  FUN=function(x){return(sum(x))})

apply(Cube_30Days, c("Enc.Rendering", "Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(Cube_30Days, c("Fin.Class", "Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(Cube_30Days, c("Sec.Fin.Class", "Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(Cube_30Days, c("Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

#### Cube_60days  ########
Cube_60days <- tapply(Aging_report$X31.60,
  Aging_report[,c("Enc.Rendering","Status","Fin.Class","Sec.Fin.Class")], 
  FUN=function(x){return(sum(x))})

apply(Cube_60days, c("Enc.Rendering", "Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(Cube_60days, c("Fin.Class", "Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(Cube_60days, c("Sec.Fin.Class", "Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(Cube_60days, c("Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

#### Cube_90days  ########
Cube_90days <- tapply(Aging_report$X61.90,
  Aging_report[,c("Enc.Rendering","Status","Fin.Class","Sec.Fin.Class")], 
  FUN=function(x){return(sum(x))})

apply(Cube_90days, c("Enc.Rendering", "Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(Cube_90days, c("Fin.Class", "Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(Cube_90days, c("Sec.Fin.Class", "Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(Cube_90days, c("Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})


#### Cube_120days  ########
Cube_120days <- tapply(Aging_report$X91.120,
  Aging_report[,c("Enc.Rendering","Status","Fin.Class","Sec.Fin.Class")], 
  FUN=function(x){return(sum(x))})

apply(Cube_120days, c("Enc.Rendering", "Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(Cube_120days, c("Fin.Class", "Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(Cube_120days, c("Sec.Fin.Class", "Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(Cube_120days, c("Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

#### Cube_150days  ########

Cube_150days <- tapply(Aging_report$X121.150,
  Aging_report[,c("Enc.Rendering","Status","Fin.Class","Sec.Fin.Class")], 
  FUN=function(x){return(sum(x))})

apply(Cube_150days, c("Enc.Rendering", "Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(Cube_150days, c("Fin.Class", "Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(Cube_150days, c("Sec.Fin.Class", "Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(Cube_150days, c("Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

#### Cube_180days  ########

Cube_180days <- tapply(Aging_report$X151.180,
  Aging_report[,c("Enc.Rendering","Status","Fin.Class","Sec.Fin.Class")], 
  FUN=function(x){return(sum(x))})

apply(Cube_180days, c("Enc.Rendering", "Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(Cube_180days, c("Fin.Class", "Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(Cube_180days, c("Sec.Fin.Class", "Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(Cube_180days, c("Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

#### Cube_Above180days  ########

Cube_Above180days <- tapply(Aging_report$X181.up,
  Aging_report[,c("Enc.Rendering","Status","Fin.Class","Sec.Fin.Class")], 
  FUN=function(x){return(sum(x))})

apply(Cube_Above180days, c("Enc.Rendering", "Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(Cube_Above180days, c("Fin.Class", "Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(Cube_Above180days, c("Sec.Fin.Class", "Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(Cube_Above180days, c("Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

#### Cube_Pat_Amt Cube ########

Cube_Pat_Amt <- tapply(Aging_report$Pat.Amt,
  Aging_report[,c("Enc.Rendering","Status","Fin.Class","Sec.Fin.Class")], 
  FUN=function(x){return(sum(x))})

apply(Cube_Pat_Amt, c("Enc.Rendering", "Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(Cube_Pat_Amt, c("Enc.Rendering"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

#### Cube_Ins1_Amt Cube ########

Cube_Ins1_Amt <- tapply(Aging_report$Ins1.Amt,
  Aging_report[,c("Enc.Rendering","Status","Fin.Class","Sec.Fin.Class")], 
  FUN=function(x){return(sum(x))})

apply(Cube_Ins1_Amt, c("Enc.Rendering", "Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(Cube_Ins1_Amt, c("Status"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})

####### Cube_Ins2_Amt Cube #######

Cube_Ins2_Amt <- tapply(Aging_report$Ins2.Amt,
  Aging_report[,c("Enc.Rendering","Status","Fin.Class","Sec.Fin.Class")], 
  FUN=function(x){return(sum(x))})

apply(Cube_Ins2_Amt, c("Enc.Rendering", "Status"),
FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(Cube_Ins2_Amt, c("Status"),
FUN=function(x) {return(sum(x, na.rm=TRUE))})

##### End of Code #########



