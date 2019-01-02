

pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dep = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("data.table", "ggplot2", "scales", "sqldf", "rstudioapi", "RODBC", "plyr", "dplyr","reshape2",
              "stringr","gridExtra","grid","lattice","gtable", "openxlsx", "readstata13", "writexl")
pkgTest(packages)


##Birthrate Check
birthrate <- read.dta13('C:/Users/kmc/Documents/birthrate.dta')
totalpop <- read.dta13('C:/Users/kmc/Documents/DTA Files/totalpop.dta')
birthingage <- read.dta13('C:/Users/kmc/Documents/DTA Files/birthingage.dta')
#Get column names from dta
names <- as.vector(names(birthrate)[2:27], mode = "character")

birthrates <- round(((totalpop[1, names(totalpop)[2:27]])/(birthingage[1, names(birthingage)[1:26]]))*100, digits = 2)
birthrates <- as.vector(birthrates, mode = "numeric")
birthratediff <- as.vector(round(birthrates - birthrate[1, names(birthrate)[2:27]]), mode = "numeric")
#Convert birthrate dataframe to vector
dtavector <- as.vector(round(birthrate[2:27], digits=2), mode = "numeric")

birthratecheck <- data.frame(year = names,formula_check = birthrates, beacon_data = dtavector, diff = birthratediff) 
View(birthratecheck)





##Deathrate Check
deathforecast <- read.dta13('C:/Users/kmc/Documents/DTA Files/deathforecast.dta')
deathrateforecast <- read.dta13('C:/Users/kmc/Documents/DTA Files/deathrateforecast.dta')
deaths <- read.dta13('C:/Users/kmc/Documents/DTA Files/deaths.dta')

dr <- deathforecast[1:86,58:83]
year <- 1990:2015
names(dr) <- sprintf("dr%d",year)

deathrates <-deaths[1:86,6:31]/totalpop[1:86,2:27]
names(deathrates) <- sprintf("dr_check%d",year)


deathratediff <- round(deathrates-dr, digits = 3)
names(deathratediff) <- sprintf("diff%d",year)

age <- c(0:84, "85+")

deathratecheck <- data.frame(agegroup = age, deathrates, dr, deathratediff)
deathratecheck <- deathratecheck[,c(1,2,28,54,3,29,55,4,30,56,5,31,57,6,32,58,7,33,59,8,34,60,9,35,61,10,36,62,11,37,63,12,38,64,13,39,65,14,40,66,15,41,67,16,42,68,17,43,69,18,44,70,19,45,71,20,46,72,21,47,
                                        73,22,48,74,23,49,75,24,50,76,25,51,77,26,52,78,27,53,79)]







##Net Migration Check
netmigration <- read.dta13('C:/Users/kmc/Documents/DTA Files/netmigration.dta')
names(netmigration)[2:26] <- sprintf("nm%d",years)

nm <-totalpop[2:85,3:27]-(totalpop[1:84,2:26]*(1-dr[1:84,1:25]))
years <- 1991:2015
names(nm) <- sprintf("nm_check%d",years)

ages <- 1:84
nmdiff <- round(netmigration[1:84,2:26]-nm)
names(nmdiff) <- sprintf("diff%d",years)

nmgroupcheck <- data.frame(agegroup = ages, nm, netmigration[1:84,2:26],nmdiff)
nmgrouptest <- nmgroupcheck[,c(1,2,27,52,3,28,53,4,29,54,5,30,55,6,31,56,7,32,57,8,33,58,9,34,59,10,35,60,11,36,61,12,37,62,13,38,63,14,39,64,15,40,65,16,41,66,17,42,67,18,43,68,19,44,69,20,45,70,21,46,
                                    71,22,47,72,23,48,73,24,49,74,25,50,75,26,51,76)]

#85+ calculation
beacon85plus <- netmigration[85, 2:26]
names(beacon85plus) <- sprintf("nm%d", years)

nm85plus <- totalpop[86,3:27]-((totalpop[85,2:26])*(1-dr[85,1:25])+ totalpop[86,2:26]*(1-dr[86,1:25]))
names(nm85plus) <- sprintf("nm_check%d",years)

diff85plus <- round(nm85plus-beacon85plus)
agegroup <- c("85+")
names(diff85plus) <- sprintf("diff%d", years)

nm85group <- data.frame(agegroup = agegroup,nm85plus, beacon85plus, diff85plus)
nm85group <- nm85group[,c(1,2,27,52,3,28,53,4,29,54,5,30,55,6,31,56,7,32,57,8,33,58,9,34,59,10,35,60,11,36,61,12,37,62,13,38,63,14,39,64,15,40,65,16,41,66,17,42,67,18,43,68,19,44,69,20,45,70,21,46,
                          71,22,47,72,23,48,73,24,49,74,25,50,75,26,51,76)]

#Append 85+ row 
nmgrouptest <- rbind(nmgrouptest,nm85group)
View(nmgrouptest)



##Net migration forecast check
NMforecast <- read.dta13('C:/Users/kmc/Documents/DTA Files/NMforecast.dta')


#NMforecast$nm_check2016 <- rowMeans(NMforecast[,2:26])
#NMforecast$nm_check2017 <- rowMeans(NMforecast[,3:27])
#NMforecast$nm_check2018 <- rowMeans(NMforecast[,4:28])
#NMforecast$nm_check2019 <- rowMeans(NMforecast[,5:29])
#NMforecast$nm_check2020 <- rowMeans(NMforecast[,6:30])



year <- 2
endyear <- 26
years <- 2016

#Equivalent to increment operator +=
for (i in 1:35)
{
  print(endyear)
test <- sprintf("nm_check%d", years)
NMforecast[,test] <- rowMeans(NMforecast[,year:endyear])
years <- years + 1
year <- year + 1
endyear <- endyear + 1
}
View(NMforecast)

NMforecast <- NMforecast[, c(1,27:96)]
############NMforecast <- NMforecast[, c(1,2,37)]


sheets <- list("birthratecheck" = birthratecheck, "deathratecheck" = deathratecheck, "netmigrationcheck" =nmgrouptest) 
write_xlsx(sheets, 'C:\\Users\\kmc\\Documents\\beaconformulachecks.xlsx', col_names = TRUE)

