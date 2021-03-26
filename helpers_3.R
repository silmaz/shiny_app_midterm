
#url_province <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province-latest.csv"
#data_province<-read_csv(url(url_province))

#appo<-preparation_data_prov(data_province)
#View(appo)


preparation_data_prov <- function(dataset){
  
  dataset <- as.data.frame(dataset)
  dataset[is.na(dataset)] = 0
  DATE_provincie <- substring(as.character(dataset$data[1]), 1, 10)   
  
  dataset <- dataset[,-c(1,2,3,5,7,11,12,13,14)]
  
  dataset[is.na(dataset)] = 0
  dataset[dataset == 0.0 ] = NA
  dataset=na.omit(dataset)
  
  rownames(dataset) = c(1:length(dataset$denominazione_provincia))
  
  
  # Barletta-Andria-Trani in Bari
  dataset$totale_casi[69] <- dataset$totale_casi[69] + dataset$totale_casi[73]
  # Crotone in Catanzaro
  dataset$totale_casi[8] <- dataset$totale_casi[8] + dataset$totale_casi[10]
  # Fermo in Ascoli Piceno
  dataset$totale_casi[54] <- dataset$totale_casi[54] + dataset$totale_casi[55]
  # Lecco in Beragamo
  dataset$totale_casi[43] <- dataset$totale_casi[43] + dataset$totale_casi[48]
  # Lodo in Milano
  dataset$totale_casi[42] <- dataset$totale_casi[42] + dataset$totale_casi[49]
  # monza brianza in milano
  dataset$totale_casi[42] <- dataset$totale_casi[42] + dataset$totale_casi[50]
  # prato in Firenze
  dataset$totale_casi[91] <- dataset$totale_casi[91] + dataset$totale_casi[97]
  # rimini in forli
  dataset$totale_casi[24] <- dataset$totale_casi[24] + dataset$totale_casi[25]
  # sud sardegli in cagliari
  dataset$totale_casi[76] <- dataset$totale_casi[76] + dataset$totale_casi[78]
  # Verbano-Cusio-Ossola in novara
  dataset$totale_casi[62] <- dataset$totale_casi[62] + dataset$totale_casi[67]
  # Vibo Valentia in catanzaro
  dataset$totale_casi[8] <- dataset$totale_casi[8] + dataset$totale_casi[11]
  # biella in vercelli
  dataset$totale_casi[61] <- dataset$totale_casi[61] + dataset$totale_casi[66]
  
  dataset <- dataset[-c(73,67,10,11,55,48,49,50,97,25,78,66),]
  
  # da Forlì-Cesena a Forlì
  library(stringr)
  dataset$denominazione_provincia <- str_replace_all(dataset$denominazione_provincia,"Forlì-Cesena","Forli'")
  dataset$denominazione_provincia <- str_replace_all(dataset$denominazione_provincia,"Reggio di Calabria","Reggio Calabria")
  dataset$denominazione_provincia <- str_replace_all(dataset$denominazione_provincia,"Reggio nell'Emilia","Reggio Emilia")
  dataset$denominazione_provincia <- str_replace_all(dataset$denominazione_provincia,"Bolzano","Bolzano-Bozen")
  dataset$denominazione_provincia <- str_replace_all(dataset$denominazione_provincia,"Massa Carrara","Massa-Carrara")
  
  
  rownames(dataset) = c(1:length(dataset$denominazione_provincia))
  
  
  new_order <- rep(NA, length(dataset$totale_casi))
  region_order <- unique(italy_map$region)
  for(j in 1:length(region_order)){
    for(i in 1:length(dataset$totale_casi)) {
      if(dataset$denominazione_provincia[i] == region_order[j]){
        new_order[j] <- i
      }
    }
  }
  
  new_data_porovincie <- data.frame(dataset[new_order,])
  rownames(new_data_porovincie) = c(1:length(new_data_porovincie$denominazione_provincia))
 
  return(new_data_porovincie)
}
