library(plumber)
library(foreign)

#* @apiTitle Diabetic Retinopathy Detector 
#* @apiDescription This API can predict the instance of DR among patients using features extracted from the retinal fundus image.The model is trained on Diabetic Retinopathy Debrecen dataset. 

#* @apiname Diabetic retinopathy

#* Execute to Return the data used in model building
library(rsconnect)
#* @get /sample
Showdata<-function(){
  sampledata=read.arff("messidor_features.arff")
  colnames(sampledata) <- c(
    "q",      #  0 The binary result of quality assessment. 0 = bad quality 1 = sufficient quality
    "ps",     #  1 The binary result of pre-screening, 1 indicates severe retinal abnormality and 0 its lack
    "nma.a",  #  2 Number of MAs found at the confidence levels alpha = 0.5
    "nma.b",  #  3 Number of MAs found at the confidence levels alpha = 0.6
    "nma.c",  #  4 Number of MAs found at the confidence levels alpha = 0.7
    "nma.d",  #  5 Number of MAs found at the confidence levels alpha = 0.8
    "nma.e",  #  6 Number of MAs found at the confidence levels alpha = 0.9
    "nma.f",  #  7 Number of MAs found at the confidence levels alpha = 1.0
    "nex.a",  #  8 Number of Exudates found at the confidence levels alpha = 0.5
    "nex.b",  #  9 Number of Exudates found at the confidence levels alpha = 0.6
    "nex.c",  # 10 Number of Exudates found at the confidence levels alpha = 0.7
    "nex.d",  # 11 Number of Exudates found at the confidence levels alpha = 0.8
    "nex.e",  # 12 Number of Exudates found at the confidence levels alpha = 0.9
    "nex.f",  # 13 Number of Exudates found at the confidence levels alpha = 1.0
    "nex.g",  # 14 Number of Exudates found at the confidence levels alpha = 1.0
    "nex.h",  # 15 Number of Exudates found at the confidence levels alpha = 1.0
    "dd",     # 16 The euclidean distance of the center of the macula and the center of the optic disc
    "dm",     # 17 The diameter of the optic disc
    "amfm",   # 18 The binary result of the AM/FM-based classification
    "class"   # 19 Class label. 1 = contains signs of DR, 0 = no signs of DR
  )
  headdata<-head(sampledata)
  return(head(headdata))
  
}



#* Return the Predicted class of Diabetic Retinopathy
#* @param q The quality assessment. 0 = bad quality 1 = sufficient quality
#* @param ps The Retinal abnrmality pre-screening. 0 = Lack 1 = sevier 
#* @param nma.a The number of Microaneurysm(MAs) at 0.5 CI 
#* @param nma.b The number of Microaneurysm(MAs) at 0.6 CI 
#* @param nma.c The number of Microaneurysm(MAs) at 0.7 CI 
#* @param nma.d number of Microaneurysm(MAs) at 0.8 CI 
#* @param nma.e number of Microaneurysm(MAs) at 0.9 CI 
#* @param nma.f number of Microaneurysm(MAs) at 1.0 CI 
#* @param nex.a The number of Exudates at 0.5 CI
#* @param nex.b The number of Exudates at 0.6 CI
#* @param nex.c The number of Exudates at 0.7 CI
#* @param nex.d The number of Exudates at 0.8 CI
#* @param nex.e The number of Exudates at 0.9 CI
#* @param nex.f The number of Exudates at 1.0 CI
#* @param nex.g The number of Exudates at 1.0 CI
#* @param nex.h The number of Exudates at 1.0 CI
#* @param dd The euclidean distance(center of the macula and the center of the optic disc)
#* @param dm The diameter of the optic disc. 
#* @param amfm The AM/FM-based classification 0 = no sign of DR 1 = sign of DR

#* @post /Predict
predictDR<-function(q ,ps ,nma.a ,nma.b , nma.c,nma.d, nma.e , nma.f,nex.a ,nex.b
                    , nex.c, nex.d, nex.e, nex.f, nex.g, nex.h ,dd,dm  ,amfm ) {
  
  super_model <- readRDS("SVMdiabeticretinopathy.rds")
  
  
  data <- list(
    q= q
    ,ps=ps
    ,nma.a=nma.a
    ,nma.b=nma.b
    ,nma.c=nma.c
    ,nma.d=nma.d
    ,nma.e=nma.e
    ,nma.f=nma.f
    ,nex.a=nex.a
    ,nex.b=nex.b
    ,nex.c=nex.c
    ,nex.d=nex.d
    ,nex.e=nex.e
    ,nex.f=nex.f
    ,nex.g=nex.g
    ,nex.h=nex.h
    ,dd =dd
    ,dm=dm
    ,amfm=amfm
  )
  data<-as.data.frame(lapply(data, as.numeric))
  data[,c("q","ps","amfm")]<-lapply(data[,c("q","ps","amfm")], as.factor)
  
  prediction <- predict(super_model, data)
  return(Diabetic_retinopathy=ifelse(prediction==1,"You might have signs of DR","You may not have signs of DR"))
}


