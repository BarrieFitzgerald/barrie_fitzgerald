## Data taken from VSU's FactBook
## NOTE: THIS IS A WORK IN PROGRESS
## need to center align the IVs in the linear regression models
##       this will prudce a significant y-intercept

## 10 year trend projection
Projected_NewFreshmanClass_10yrtrend<-function(NumberApplied){
        AdmitNewFr<-data.frame()
        Applied<-c(5224,5782,6281,5978,7025,6703,8298,7950,6268,5701)
        Accepted<-c(3440,3643,3839,3796,4360,4744,5182,4648,3713,3148)
        Enrolled<-c(1697,1757,1994,2077,2137,2467,2557,2249,1971,1734)
        ActualEnrolled<-c(1696,1757,2059,2070,2143,2470,2522,2204,1918,1683)
        ModelApplyAccept<-lm(Accepted ~ Applied)
        ModelAcceptEnrolled<-lm(Enrolled ~ Accepted)
        ModelEnrollActual<-lm(ActualEnrolled ~ Enrolled)
        Projected<-round(ModelApplyAccept$coef[1]+(ModelApplyAccept$coef[2]*NumberApplied),0)
        Projected2<-round(ModelAcceptEnrolled$coef[1]+(ModelAcceptEnrolled$coef[2]*Projected),0)
        Projected3<-round(ModelEnrollActual$coef[1]+(ModelEnrollActual$coef[2]*Projected2),0) 
        print("Est. Accept")
        print(Projected)
        print("Est. Enroll")
        print(Projected2)
        print("Est. Actual Enroll")
        print(Projected3)
}

## 5 year trend projection
Projected_NewFreshmanClass_5yrtrend<-function(NumberApplied){
        AdmitNewFr<-data.frame()
        Applied<-c(6703,8298,7950,6268,5701)
        Accepted<-c(4744,5182,4648,3713,3148)
        Enrolled<-c(2467,2557,2249,1971,1734)
        ActualEnrolled<-c(2470,2522,2204,1918,1683)
        ModelApplyAccept<-lm(Accepted ~ Applied)
        ModelAcceptEnrolled<-lm(Enrolled ~ Accepted)
        ModelEnrollActual<-lm(ActualEnrolled ~ Enrolled)
        Projected<-round(ModelApplyAccept$coef[1]+(ModelApplyAccept$coef[2]*NumberApplied),0)
        Projected2<-round(ModelAcceptEnrolled$coef[1]+(ModelAcceptEnrolled$coef[2]*Projected),0)
        Projected3<-round(ModelEnrollActual$coef[1]+(ModelEnrollActual$coef[2]*Projected2),0) 
        print("Est. Accept")
        print(Projected)
        print("Est. Enroll")
        print(Projected2)
        print("Est. Actual Enroll")
        print(Projected3)
}

## 3 year trend projection
Projected_NewFreshmanClass_3yrtrend<-function(NumberApplied){
        AdmitNewFr<-data.frame()
        Applied<-c(7950,6268,5701)
        Accepted<-c(4648,3713,3148)
        Enrolled<-c(2249,1971,1734)
        ActualEnrolled<-c(2204,1918,1683)
        ModelApplyAccept<-lm(Accepted ~ Applied)
        ModelAcceptEnrolled<-lm(Enrolled ~ Accepted)
        ModelEnrollActual<-lm(ActualEnrolled ~ Enrolled)
        Projected<-round(ModelApplyAccept$coef[1]+(ModelApplyAccept$coef[2]*NumberApplied),0)
        Projected2<-round(ModelAcceptEnrolled$coef[1]+(ModelAcceptEnrolled$coef[2]*Projected),0)
        Projected3<-round(ModelEnrollActual$coef[1]+(ModelEnrollActual$coef[2]*Projected2),0) 
        print("Est. Accept")
        print(Projected)
        print("Est. Enroll")
        print(Projected2)
        print("Est. Actual Enroll")
        print(Projected3)
}
