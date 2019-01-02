

#Add stringAsFactors = false argument to sqlquery??
#Or you have to relabel the factor level
#The easiest way is to use revalue() or mapvalues() from the plyr package:
  


revalue(hh, c("Los Penasquitos Canyon Preserve"="Los Penas. Can. Pres."))
        #or 
hh$cpa <- revalue(hh$cpa, c("Los Penasquitos Canyon Preserve"="Los Penas. Can. Pres." ))
 



#try 
  #hh_jur$cityname==i below ?

#remove labels because they are the same as the original factor labels 
+scale_fill_manual(values=c("blue", "red"), name=NULL, breaks=c(jur_list[i],"Region")) 
# or try 
scale_fill_discrete


scale_fill_discrete(values= c("red","blue"))
scale_fill_manual(values= c("red","blue"))

guides(fill = guide_legend(order = 1))

