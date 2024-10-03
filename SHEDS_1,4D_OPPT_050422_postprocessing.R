#Collect and process outputs for use in water contamination model 
setwd("output")

#Create template to build on
DTD=read.csv(paste0("SHT_Run_14D_HM.1000.010/DTXSID4020533_all.csv"))
DTD=DTD[,c("person", "gender", "age", "weight")]
#Add each product ID
for(i in prodids){
  datafile=read.csv(paste0("SHT_Run_14D_",i,"/DTXSID4020533_all.csv"))
  sub=datafile[,c("person", "gender", "age", "weight", "exp.drain")]
  names(sub)[which(names(sub)=="exp.drain")]=i
  DTD=merge(DTD, sub[,c("person",i)], by="person")}



#Convert DTD to long form
library(reshape2)
DTDlong=reshape2::melt(DTD, id.vars=c("person", "gender", "age","weight"), variable.name="pucid") 


#Create a reference table to link COUs and PUCs in the DTD table
reftab=chem14dformat1[,c("pucid", "form", "PUC_level3")]
reftab$COU=NA
#Link reftab puc levels and COU's
COUlist=c("paint", "stain", "laundry", "cleaner", "dye", "antifreeze", "soap", "foam","dishwashing")
for(i in COUlist){
  positions=grep(i, reftab$PUC_level3)
  if(i=="paint" | i=="stain"){reftab$COU[positions]<-"Paint"}else
    if(i=="laundry"){reftab$COU[positions]<-"Laundry_Detergent"} else
      if(i=="cleaner"){reftab$COU[positions]<-"Surface_Cleaner"} else
        if(i=="dye"){reftab$COU[positions]<-"Dye"} else
          if(i=="antifreeze"){reftab$COU[positions]<-"Antifreeze"} else
            if(i=="soap"){reftab$COU[positions]<-"Dish_Soap"} else
              if(i=="foam"){reftab$COU[positions]<-"SPF"} else
              {reftab$COU[positions]<-"Dishwashing_Detergent"}
}

###Combine with reftable
DTDmerge=merge(DTDlong, reftab[,c("pucid", "PUC_level3", "COU")], by="pucid")

#All COUS
#data.frame(unique(DTDmerge$PUC_level3), stringsAsFactors = FALSE)
#Template
                       c("body paint",
                       "fabric dye",
                 "bathroom cleaner",
  "automatic dishwashing detergent",
                        "dish soap",
                  "surface cleaner",
                "laundry detergent",
                       "spray foam",
                            "paint",
               "water-based paint",
    "water-based paint - exterior",
                           "stain",
                "stain - exterior",
                      "auto paint",
                      "antifreeze")

COUScreen=c("fabric dye",                     
            "bathroom cleaner", 
            "automatic dishwashing detergent",
            "dish soap",
            "surface cleaner",                
            "laundry detergent",               
            "spray foam",                     
            "paint",                           
            "water-based paint",              
            "water-based paint - exterior",                          
            "antifreeze")
            

DTDmergesub=DTDmerge[which(DTDmerge$PUC_level3%in%COUScreen==TRUE),]



DTDmergesub1=aggregate(value~COU+person, data=DTDmergesub, FUN="sum")

options(scipen = 0)
#Note: Fraction down the drain for things like Antifreeze and Spray Foam are assumed to be zero in 
#SHEDS-HT
#Dye has 100% DTD, but but it has a low use frequency(like 4 times a year) and a low use.prev(like 0.01), so the chances of it contributing are low

#Aggregate to determine median cpDTD by COU; note, including all subtypes here

sumresults=aggregate(value~COU, data=DTDmergesub1, FUN=function(x){
  qs=quantile(x, probs=c(0.01,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9, 0.99))
  mean=mean(x)
  sd=sd(x)
  return(c(qs,mean,sd))
})

sumresults=do.call("data.frame", sumresults)
names(sumresults)[2:14]=c("Q1%","Q10%","Q20%","Q30%","Q40%", "Q50%","Q60%","Q70%","Q80%","Q90%","Q99%","Mean", "SD")
sumresults




