#This code tries to replicate the results obtained with XSMLE package in Stata. I will use the data for Spain,
#Portugal and France. 

#------------------------------------------------------------------------------------------------------#
###                                         LIBRARIES                                      #############
#------------------------------------------------------------------------------------------------------#

library(plm) #For paneldata objects 
library(spdep) #For w Matrix
library(splm) #For spatial models
library(tidyverse)

#------------------------------------------------------------------------------------------------------#
###                                             DATA                                       #############
#------------------------------------------------------------------------------------------------------#

W_matrix<-matrix(c(0, 1, 0,
                   0.437654567, 0, 0.562345433,
                   0, 1, 0), nrow = 3, byrow = T)
W_SPML<-mat2listw(W_matrix)

FEP_Stata_2018 <- read.csv("C:/Users/Eve/Desktop/FEP_Stata_2018.txt", row.names=1)

FEP_Stata_2018<-pdata.frame(FEP_Stata_2018%>%
                              group_by(Date, Country)%>%
                              relocate(Country),#%>%
                              #group_by(Country)%>%
                              #mutate(Lag= lag(T.mean))%>%
                              #filter(Lag!=is.na(T)),
                            index = c("Country", "Date") )


#------------------------------------------------------------------------------------------------------#
###                                           THE MODEL                                       #############
#------------------------------------------------------------------------------------------------------#

#Testing spatial autocorrelation with Pesaran Test
pcdtest(FEP_Stata_2018$T.mean, w = W_matrix)

#Estimate the SAR MODEL:

Model<-T.mean ~ T.G_Fossil +T.G_Hydro +T.G_Wind+ T.G_Nuclear+ T.G_Solar +T.G_Others +T.Forecast_load # Step 1: Define the eq. to estimate

summary(
  spml(Model, data = FEP_Stata_2018, listw = W_SPML, #Step 2: Estimate the SAR model
       model = "random", # Define RE or FE
       spatial.error = "b",  #none=SAR model (Same coeffficients estimated with STATA) , 
       lag = T))

pmg(T.mean ~ T.G_Nuclear, data = FEP_Stata_2018)
pmg(T.mean ~ T.G_Fossil +T.G_Hydro +T.G_Wind+ T.G_Nuclear+ T.G_Solar +T.G_Others +T.Forecast_load, data = FEP_Stata_2018, model = "cmg")

#SDM Dynamic e

w_Variables<- function(W_matrix, Variable){
  kronecker(W_matrix, diag(1, 1096)) %*% Variable #diag is the dimension of T in my panel Data
}

W_data<-lapply(4:11, function(x)
  w_Variables(W_matrix, FEP_Stata_2018[[x]])
  )


W_data<-do.call(cbind, W_data)
colnames(W_data) <-c("W_T.mean", "W_T.G_Fossil", "W_T.G_Hydro", "W_T.G_Wind", "W_T.G_Nuclear",     
                   "W_T.G_Solar", "W_T.G_Others", "W_T.Forecast_load")

W_data<-cbind(FEP_Stata_2018, W_data)
W_data<-pdata.frame(W_data, index = c("Country", "Date"))

SDM_Model<-T.mean ~ T.G_Fossil +T.G_Hydro +T.G_Wind+ T.G_Nuclear+ T.G_Solar +T.G_Others +T.Forecast_load +
 W_T.G_Fossil + W_T.G_Hydro+ W_T.G_Wind + W_T.G_Nuclear + W_T.G_Solar + W_T.G_Others + W_T.Forecast_load

SDM<-summary(spml(SDM_Model, 
             data = W_data, 
             listw = W_matrix, lag = TRUE, 
             model = "within",  spatial.error = "none")
)

Dynamic<-summary(spml(T.mean ~ Lag+W_lag +T.G_Fossil +T.G_Hydro +T.G_Wind+ T.G_Nuclear+ T.G_Solar +T.G_Others +T.Forecast_load +
                        W_T.G_Fossil + W_T.G_Hydro+ W_T.G_Wind + W_T.G_Nuclear + W_T.G_Solar + W_T.G_Others + W_T.Forecast_load, 
                      data = W_data, 
                      listw = W_matrix, lag = TRUE, 
                      model = "within",  spatial.error = "none")
)

spatialreg::lagsarlm(Model, W_data, listw=W_SPML, Durbin=TRUE)

sdm_obj2 <- spatialreg::lagsarlm(av55_59 ~ transp + salesTax, used.cars, listw=lw, Durbin=TRUE)

impac1 <- impacts(SDM, listw = mat2listw(W_matrix, style = "W"), time = 1096, R=5)

impacts.
