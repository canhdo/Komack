########## Multinomial Logit Model ##########
#### Individual Unit ##########
##### Access mode choices ##### Sample size: 1042 Observations #####
# Code: 1 (Motorbike); 2 (Private car); 3 (Taxi); 4 (Airport bus); 5 (VCAT)#

#<<<First step>>>
##### Data Reading from csv file #####

library(readr)
data <- read_csv("DataClean.csv")


 #attach(data)
 #names(data)
 #summary(data)




### Correlation matrix ####
#data1 <- data[,c("HJob","Hinc1","Hinc2","Hinc3","Hinc4","Hinc5","Hinc6","Hinc7","Hinc8")]
#cor(data1)

#data2 <- data[,c("HEdu","Job1","Job2","Job3","Job4","Job5","Job6","Job7","Job8","Job9")]
#cor(data2)

#data3 <- data[,c("Male","Lao","Nat3")]
#cor(data3)

#<<<Second step>>>
##### Defining Log-likehood function #####
LL <- function (Cm,
                Ct,
                Cb,
                Cv,
                Cost,
               Vehtime,
                #Waiting,
                #Access,
                #Delay,
                Transfer,
                Parking #,
                #Safety
)
  
  
{ #Begin function
  
  # Utility function
  
 
  
  Vm   <- Cm   + data$Cost_MT*Cost +
                 data$Park_MT*Parking +
                 data$Vehtime_MT*Vehtime# + 
                 #data$Safe_MT*Safety
  
  Vc   <- 0    + data$Cost_Car*Cost +
                 data$Park_Car*Parking +
                 data$Vehtime_Car*Vehtime # + 
                 #data$Delay_Car*Delay +
                 #data$Safe_Car*Safety    
    
  Vt   <- Ct   + data$Cost_Taxi*Cost +
                data$Vehtime_Taxi*Vehtime# + 
                 #data$Safe_Taxi*Safety + 
                 #data$Delay_Taxi*Delay
  
  Vb   <- Cb   + data$Cost_Bus*Cost  +
                 data$Vehtime_Taxi*Vehtime + 
                 data$Trans_Bus*Transfer #+ 
                 #data$Wtime_Bus*Waiting + 
                 #data$Acctime_Bus*Access + 
                 #data$Delay_Bus*Delay +
                 #data$Safe_Bus*Safety     
  
  Vv   <- Cv   + data$Cost_VCAT*Cost  +
                 data$Vehtime_VCAT*Vehtime + 
                 data$Trans_VCAT*Transfer    #+ 
                 #data$Wtime_VCAT*Waiting  + 
                 #data$Acctime_VCAT*Access  +                                                    
                 #data$Safe_VCAT*Safety     
    
  
  
  
  
  # Denominator
  Sum <- exp(Vm) + exp(Vc) + exp(Vt) + exp(Vb) + exp(Vv)
  
  
  # Probabilities
  Pm <- exp(Vm)/Sum
  Pc <- exp(Vc)/Sum
  Pt <- exp(Vt)/Sum
  Pb <- exp(Vb)/Sum
  Pv <- exp(Vv)/Sum
  
  
  
  # Dummies (or indicator) for alternatives
  Dm <- ifelse(data$MODE == "Motorbike",1,0)
  Dc <- ifelse(data$MODE == "Private Car",1,0)
  Dt <- ifelse(data$MODE == "Taxi",1,0)
  Db <- ifelse(data$MODE == "Airport Bus",1,0)
  Dv <- ifelse(data$MODE == "Airport Limousine Bus",1,0)
  
  
  
  
  
  # Log-likelihood function
  L <- -(sum(Dm*log(Pm) + Dc*log(Pc) + Dt*log(Pt) + Db*log(Pb) + Dv*log(Pv)))
  return(L)
} #End function

#<<<Third step>>>
##### Model Estimation #####
initValue <- list(Cm=0,
                  Ct=0,
                  Cb=0,
                  Cv=0,
                  Cost=0,
                  Vehtime=0,
                  #Waiting=0,
                  #Access=0,
                  #Delay=0,
                  Transfer=0,
                  Parking=0#,
                  #Safety=0
)


#install packages: NoOTICE: Packages is needed to install for the first time using the packages
# install.packages("bbmle")
library(bbmle)
MNL <- mle2(LL, start = initValue, method = "BFGS", control = list(trace = 1, maxit = 1e6))
summary(MNL)

#<<<Fourth step>>>
LL0 <- NROW(data)*log(1/5)     ## Initial log-likelihood
LL1 <- logLik(MNL)             ## Converged log-likelihood
## McFadden's Rho square
Rho     <- 1-(LL1/LL0)
Rho.adj <- 1-((LL1-length(coef(MNL)))/LL0)
GoF <- cbind(LL0,LL1, Rho, Rho.adj)  ## Goodness of fit
print(GoF)

