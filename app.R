# https://github.com/pcm-dpc/COVID-19

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(shinycssloaders)
library(shinyjs)
library(shinythemes) # caricatore
library(plyr)
library(stringr)
#library(tidyverse)

library(reshape2)
library(data.table)

library(tidyr)
library (readr)
# to upload csv data from github

# for the maps
library(maps)
library(mapproj)
library(cartography)
library(devtools)
#install_github("nicolasturaro/mapIT")
library(mapIT)
#source("helpers.R")
#source("helpers_new.R")
source("helpers_2.R")
source("helpers_3.R")

################# province (NO) ----
# province
# https://github.com/pcm-dpc/COVID-19/blob/master/dati-province/dpc-covid19-ita-province-latest.csv
# dati:
# https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province-latest.csv
url_province <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province-latest.csv"
data_province<-read_csv(url(url_province))

DATE_province <- substring(as.character(data_province$data[1]), 1, 10)   
new_data_province <- preparation_data_prov(data_province)

#data_province<-as.data.frame(data_province)
#data_province[is.na(data_province)] = 0
#DATE_province <- substring(as.character(data_province$data[1]), 1, 10)   

#data_province <- data_province[,-c(1,2,3,5,7,11,12,13,14)]

#data_province[is.na(data_province)] = 0
#data_province[data_province == 0.0 ] = NA
#data_province=na.omit(data_province)

#rownames(data_province) = c(1:length(data_province$denominazione_provincia))


# Barletta-Andria-Trani in Bari
#data_province$totale_casi[69] <- data_province$totale_casi[69] + data_province$totale_casi[73]
# Crotone in Catanzaro
#data_province$totale_casi[8] <- data_province$totale_casi[8] + data_province$totale_casi[10]
# Fermo in Ascoli Piceno
#data_province$totale_casi[54] <- data_province$totale_casi[54] + data_province$totale_casi[55]
# Lecco in Beragamo
#data_province$totale_casi[43] <- data_province$totale_casi[43] + data_province$totale_casi[48]
# Lodo in Milano
#data_province$totale_casi[42] <- data_province$totale_casi[42] + data_province$totale_casi[49]
# monza brianza in milano
#data_province$totale_casi[42] <- data_province$totale_casi[42] + data_province$totale_casi[50]
# prato in Firenze
#data_province$totale_casi[91] <- data_province$totale_casi[91] + data_province$totale_casi[97]
# rimini in forli
#data_province$totale_casi[24] <- data_province$totale_casi[24] + data_province$totale_casi[25]
# sud sardegli in cagliari
#data_province$totale_casi[76] <- data_province$totale_casi[76] + data_province$totale_casi[78]
# Verbano-Cusio-Ossola in novara
#data_province$totale_casi[62] <- data_province$totale_casi[62] + data_province$totale_casi[67]
# Vibo Valentia in catanzaro
#data_province$totale_casi[8] <- data_province$totale_casi[8] + data_province$totale_casi[11]
# biella in vercelli
#data_province$totale_casi[61] <- data_province$totale_casi[61] + data_province$totale_casi[66]

#data_province <- data_province[-c(73,67,10,11,55,48,49,50,97,25,78,66),]

# da Forlì-Cesena a Forlì

#data_province$denominazione_provincia <- str_replace_all(data_province$denominazione_provincia,"Forlì-Cesena","Forli'")
#data_province$denominazione_provincia <- str_replace_all(data_province$denominazione_provincia,"Reggio di Calabria","Reggio Calabria")
#data_province$denominazione_provincia <- str_replace_all(data_province$denominazione_provincia,"Reggio nell'Emilia","Reggio Emilia")
#data_province$denominazione_provincia <- str_replace_all(data_province$denominazione_provincia,"Bolzano","Bolzano-Bozen")

#data_province$denominazione_provincia <- str_replace_all(data_province$denominazione_provincia,"Massa Carrara","Massa-Carrara")


#rownames(data_province) = c(1:length(data_province$denominazione_provincia))


#new_order <- rep(NA, length(data_province$totale_casi))
#region_order <- unique(italy_map$region)
#for(j in 1:length(region_order)){
#    for(i in 1:length(data_province$totale_casi)) {
#        if(data_province$denominazione_provincia[i] == region_order[j]){
#            new_order[j] <- i
#        }
#    }
#}

#new_data_porovincie <- data.frame(data_province[new_order,])
#rownames(new_data_porovincie) = c(1:length(new_data_porovincie$denominazione_provincia))

rem <- c("Barletta", "Crotone", "Fermo", "Lecco", "Lodi", "Monza e Brianza",
         "Prato", "Rimini", "Sud Sardegna", "Verbano", "Vibo Valentina")
fin <- c("Bari", "Catanzaro", "Ascoli-Piceno", "Bergamo", "Milano", "Milano",
         "Firenze", "Forli'", "Cagliari", "Novara", "Catanzaro")
prov_removed <- data.frame(originali = rem, prov_mappa = fin)



############### REGIONI (SI) ----


# regioni
# https://github.com/pcm-dpc/COVID-19/blob/master/dati-regioni/dpc-covid19-ita-regioni-latest.csv
# dati:
# https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni-latest.csv
url_regioni <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni-latest.csv"
data_regioni<-read_csv(url(url_regioni))
data_regioni<-as.data.frame(data_regioni)
data_regioni <- data_regioni[,-c(2,3,16,21,23,24,29,30)]
data_regioni[is.na(data_regioni)] = 0

# riga 12 = bolzano
# riga 13 = trento -> Trentino-Alto Adige

for (i in 5:22){
    data_regioni[13,i] <- data_regioni[12,i]+ data_regioni[13,i]
}
data_regioni <- data_regioni[-c(12),]
rownames(data_regioni) = c(1:20)


popolazione <- c(1293941, 553254, 1894110, 5712143, 4464119,
                 1206216, 5755700, 1524826, 10027602, 1512672,
                 300516, 4311217, 3953305, 1611621, 4875290, 3692555,
                 1078069, 870165, 125034, 4879133)
data_regioni <- data.frame(data_regioni, popolazione)

DATE_regioni <- substring(as.character(data_regioni$data[1]), 1, 10)   
data_regioni <- data_regioni[,-c(1)]
#data_regioni$denominazione_regione<-as.factor(data_regioni$denominazione_regione)



# se voglio i dati di un dato giorno allora metto l'url che si scrive
# https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni-20200312.csv
# e compongo la data



################ NAZIONE (SI) ----

# andamento nazionale
# https://github.com/pcm-dpc/COVID-19/blob/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv
# dati:
# https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv
url_nazione <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv"
data_nazione <- read_csv(url(url_nazione))
data_nazione<-as.data.frame(data_nazione)
head(data_nazione)

day <- c(NA, length(data_nazione$data))
for (i in 1:length(data_nazione$data)){
    day[i] <- substring(as.character(data_nazione$data[i]), 1, 10)   
}
data_nazione<-data.frame(day,data_nazione)
data_nazione<-data_nazione[,-c(2)]

data_nazione[is.na(data_nazione)] = 0

deceduti_oggi <- rep(NA, length(day))
deceduti_oggi[1]<- data_nazione$deceduti[1]
guariti_oggi <- rep(NA,length(day))
guariti_oggi[1] <- data_nazione$dimessi_guariti[1]
isolamento_oggi <- rep(NA,length(day))
isolamento_oggi[1] <- data_nazione$isolamento_domiciliare[1]
ospedalizzati_non_gravi <- rep(NA, length(day))
for (i in 1:(length(day)-1)){
    deceduti_oggi[i+1] <- data_nazione$deceduti[i+1]-data_nazione$deceduti[i]
    guariti_oggi[i+1] <- data_nazione$dimessi_guariti[i+1]-data_nazione$dimessi_guariti[i]
    isolamento_oggi[i+1] <- data_nazione$isolamento_domiciliare[i+1]-data_nazione$isolamento_domiciliare[i]
    ospedalizzati_non_gravi[i] <- data_nazione$totale_ospedalizzati[i]-data_nazione$terapia_intensiva[i]
}

data_nazione<-data.frame(data_nazione, deceduti_oggi,guariti_oggi,isolamento_oggi,ospedalizzati_non_gravi)

#################### APP ----


# non uso fluidPage
ui <- fluidPage(theme = shinytheme("superhero"),    # https://rstudio.github.io/shinythemes/
                useShinyjs(),
 
            navbarPage("COVID-19 Data Visualizer",
                
                tabPanel("Introduction", # navbar 1
                         mainPanel(
                             h4("Table"),
                             h4("Verbatim text output"),
                             h1("Header 1"),
                             h2("Header 2"),
                             h3("Header 3"),
                             h4("Header 4"),
                             h5("Header 5")
                         )  , 
                        # IO COMUNQUE QUETSO TEXT NON LO METTEREI 
                    textOutput("text")
                 ),
                
                tabPanel("Contagi", # navbar 2
                    sidebarPanel(#"side_1",
                        dateRangeInput("dates", label = h3("Date range"),min = data_nazione$day[1],
                                       max = data_nazione$day[length(data_nazione$day)],
                                       start = data_nazione$day[250],
                                       end = data_nazione$day[length(data_nazione$day)],
                                       format = "yyyy-mm-dd"),
                        checkboxInput("casi", "Plot casi al giorno",value = TRUE),
                        checkboxInput("guariti", "Plot guariti al giorno",value = FALSE),
                        selectInput("select", label = h3("Select box"), 
                                    choices = list("Lines" = 1, "Bars" = 2), 
                                    selected = 2)
                    ),
                    mainPanel(
                        tabsetPanel(
                        tabPanel("tab 1",
                           plotOutput("plot_1") %>% withSpinner(color="#0dc5c1")
                       ),
                       tabPanel("tab_2",
                           plotOutput("plot_2") %>% withSpinner(color="#0dc5c1")
                        )
                       )
                    )
                   ),  # fine tab contagi
                
                
                tabPanel("Ospedali", # navbar 3
                         sidebarPanel(#"side_2",
                             dateRangeInput("dates", label = h3("Date range"),min = data_nazione$day[1],
                                            max = data_nazione$day[length(data_nazione$day)],
                                            start = data_nazione$day[250],
                                            end = data_nazione$day[length(data_nazione$day)],
                                            format = "yyyy-mm-dd")
                             
                         ),
                         mainPanel(
                             tabsetPanel(
                                    tabPanel("osp 1",plotOutput("plot_osp_1") %>% withSpinner(color="#0dc5c1")),
                                    tabPanel("osp 2",plotOutput("plot_osp_2") %>% withSpinner(color="#0dc5c1")),
                                    tabPanel("osp 3",plotOutput("plot_osp_3") %>% withSpinner(color="#0dc5c1"))
                            )
                            #id="main_3"
                          )
                        ), # fine ospedavi
            
                tabPanel("Province", # navbar 4 
                                 fluidPage(
                                     column(6,
                                            plotOutput("plot_prov") %>% withSpinner(color="#0dc5c1"),
                                            textOutput("text_prov_1"),
                                            textOutput("text_prov_2"),
                                            textOutput("text_prov_3"),
                                            tableOutput("table_prov_remove") %>% withSpinner(color="#0dc5c1")
                                     ),
                                     column(6,
                                            dateInput("date_prov",
                                                      label = paste('Note: i dati sia aggiornano alle',
                                                                    '18:00 di ogni giorno, quindi per vedere',
                                                                    'i dati di oggi devi attendere quell ora.'),
                                                      min = "2020-03-18",
                                                      max = format(Sys.Date(),"%Y-%m-%d"),
                                                      value = DATE_province),
                                                      #datesdisabled = c("20-03-01", "2012-03-02")),
                                            dataTableOutput("table_prov") %>% withSpinner(color="#0dc5c1")
                                     )
                                ),
                            
                ) # fine province

# https://stackoverflow.com/questions/61120731/how-to-hide-sidebarpanel-of-a-shiny-app-for-a-particular-tab
                
        ) # fine novbarPage
)




######## SERVER ----

server <- function(input, output) {

    
    output$text <- renderText({
        paste("You have chosen a range that goes from", input$dates[1], "to", input$dates[2], "...  ")
    })
    values <- reactiveValues()
    
    output$plot_1 = renderPlot({
        values$DT <- data.frame(giorno = data_nazione$day[match(format(input$dates[1]),data_nazione$day) : match(format(input$dates[2]),data_nazione$day)],
                                positivi = data_nazione$nuovi_positivi[match(format(input$dates[1]),data_nazione$day) : match(format(input$dates[2]),data_nazione$day)],
                                guariti = data_nazione$guariti_oggi[match(format(input$dates[1]),data_nazione$day) : match(format(input$dates[2]),data_nazione$day)])
        
        if(input$select == 2){         
            if(input$casi==TRUE & input$guariti==FALSE){
                ggplot(values$DT,aes(x = as.Date(giorno), y = positivi)) + geom_bar(color = "#FC4E07", alpha=0.4, stat = "identity") +
                    scale_x_date(date_breaks = "1 month",
                                 date_labels = "%b-%y") +
                    labs(title = "Contagi") +
                    labs(x = "days") +
                    labs(y = "contagi")
            }
            else if(input$casi==FALSE & input$guariti==TRUE){
                ggplot(values$DT,aes(x = as.Date(giorno), y = guariti)) + geom_bar(color="#00AFBB", alpha=0.4, stat = "identity") +
                    scale_x_date(date_breaks = "1 month",
                                 date_labels = "%b-%y") + labs(x = "days")
            }
            else if(input$casi==TRUE & input$guariti==TRUE){
                ggplot(values$DT, aes(as.Date(giorno))) + 
                    geom_col(aes(y = positivi, colour = "positivi"), colour = "#FC4E07", alpha = 0.4) + 
                    geom_col(aes(y = guariti, colour = "guariti"), color = "#00AFBB", alpha = 0.4) 
            }
            else if(input$casi==FALSE & input$guariti==FALSE){
                ggplot(values$DT) + labs(x = "days") 
            }
        }
        else if(input$select ==1){
            if(input$casi==TRUE & input$guariti==FALSE){
                ggplot(values$DT,aes(x = as.Date(giorno), y = positivi, color="#FC4E07")) + geom_point() + geom_line() +
                    scale_x_date(date_breaks = "1 month",
                                 date_labels = "%b-%y") +
                    labs(title = "Contagi") +
                    labs(x = "days") +
                    labs(y = "contagi")
            }
            else if(input$casi==FALSE & input$guariti==TRUE){
                ggplot(values$DT,aes(x = as.Date(giorno), y = guariti, color="#00AFBB")) + geom_point() + geom_line() +
                    scale_x_date(date_breaks = "1 month",
                                 date_labels = "%b-%y") + labs(x = "days")
            }
            else if(input$casi==TRUE & input$guariti==TRUE){
                ggplot(values$DT, aes(as.Date(giorno))) + 
                    geom_area(aes(y = positivi, colour = "positivi", fill = "positivi"), fill="#FC4E07", color="#FC4E07",  alpha = 0.4, position = 'identity') + 
                    geom_area(aes(y = guariti, colour = "guariti", fill= "guariti"),fill="#00AFBB", color="#00AFBB", alpha=0.4, position = 'identity')
            }
            else if(input$casi==FALSE & input$guariti==FALSE){
                ggplot(values$DT) + labs(x = "days") 
            }
        }
    })
    
    
    
    output$plot_osp_1 = renderPlot({
        
        positivi = data_nazione$totale_positivi[length(data_nazione$day)-1:length(data_nazione$day)]
        ospedale = data_nazione$totale_ospedalizzati[length(data_nazione$day)-1:length(data_nazione$day)]
        
        positivi_non_osp <- rep(NA, length(data_regioni$totale_positivi))
        for(i in 1:length(positivi)){
            positivi_non_osp[i] <- positivi[i]-ospedale[i]
        }
        
        values$DT <- data.frame( giorno = data_nazione$day[length(data_nazione$day)-1:length(data_nazione$day)],
                                 tot_positivi = data_nazione$totale_positivi[length(data_nazione$day)-1:length(data_nazione$day)],
                                 terapia_intensiva = data_nazione$terapia_intensiva[length(data_nazione$day)-1:length(data_nazione$day)],
                                 ospedalizzati_non_gravi = data_nazione$ospedalizzati_non_gravi[length(data_nazione$day)-1:length(data_nazione$day)],
                                 positivi_non_ospedalizzati = positivi_non_osp)
        
        ggplot() +
            geom_line(data=values$DT, aes(x=as.Date(giorno), y=tot_positivi, color = "positivi totali")) +
            geom_line(data=values$DT, aes(x=as.Date(giorno), y=terapia_intensiva, color = "terapia intensiva")) +
            geom_line(data=values$DT, aes(x=as.Date(giorno), y=ospedalizzati_non_gravi, color = "ospedalizzati non gravi")) +
            geom_line(data=values$DT, aes(x=as.Date(giorno), y=positivi_non_ospedalizzati, color = "positivi in isolamento domiciliare")) #+
            #geom_point()
    })
    
    output$plot_osp_2 = renderPlot({
        
        ospedale = data_nazione$totale_ospedalizzati[length(data_nazione$day)-1:length(data_nazione$day)]
        terapia_intensiva = data_nazione$terapia_intensiva[length(data_nazione$day)-1:length(data_nazione$day)]
        ospedalizzati_non_gravi = data_nazione$ospedalizzati_non_gravi[length(data_nazione$day)-1:length(data_nazione$day)]
        
        osp_non_gr <- rep(NA,length(ospedale))
        osp_ter_int <- rep(NA,length(ospedale))
        
        for(i in 1:length(ospedale)){
            osp_non_gr[i] <- ospedalizzati_non_gravi[i]/ospedale[i]
            osp_ter_int[i] <- terapia_intensiva[i]/ospedale[i]
        }
        
        values$DT <- data.frame( giorno = data_nazione$day[length(data_nazione$day)-1:length(data_nazione$day)],
                                 osp_non_gravi = osp_non_gr,
                                 osp_terapia_intensiva = osp_ter_int)
        ggplot() +
            geom_line(data=values$DT, aes(x=as.Date(giorno), y=osp_non_gravi, color = "ospedalizzati non gravi")) +
            geom_line(data=values$DT, aes(x=as.Date(giorno), y=osp_terapia_intensiva, color = "terapia intensiva")) 

    })

    
    output$plot_osp_3 = renderPlot({
        
        ospedale = data_nazione$totale_ospedalizzati[length(data_nazione$day)-1:length(data_nazione$day)]
        positivi = data_nazione$totale_positivi[length(data_nazione$day)-1:length(data_nazione$day)]
        positivi_non_osp <- rep(NA, length(data_regioni$totale_positivi))
        for(i in 1:length(positivi)){
            positivi_non_osp[i] <- positivi[i]-ospedale[i]
        }
        
        pos_non_osp <- rep(NA,length(ospedale))
        pos_osp <- rep(NA,length(ospedale))
        
        for(i in 1:length(ospedale)){
            pos_non_osp[i] <- positivi_non_osp[i]/positivi[i]
            pos_osp[i] <- ospedale[i]/positivi[i]
        }
        
        values$DT <- data.frame( giorno = data_nazione$day[length(data_nazione$day)-1:length(data_nazione$day)],
                                 positivi_in_ospedale = pos_osp,
                                 positivi_a_casa = pos_non_osp)
        ggplot() +
            geom_line(data=values$DT, aes(x=as.Date(giorno), y=positivi_in_ospedale, color = "ospedalizzati")) +
            geom_line(data=values$DT, aes(x=as.Date(giorno), y=positivi_a_casa, color = "isolamento domiciliare")) 
        
    })
    
    
    
    
    
    # map from function
    output$plot_prov <- renderPlot({
        
        full_date <- as.character(input$date_prov)
        yeas_date <- substring(as.character(full_date), 1, 4)
        month_date <- substring(as.character(full_date), 6, 7)
        day_date <- substring(as.character(full_date), 9, 10)
        
        url_now <- paste('https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province-',
                     yeas_date,month_date,day_date,'.csv', sep = "")
        data_now<-read_csv(url(url_now))
        
        data_now <- as.data.frame(data_now)
        data_now_new <- preparation_data_prov(data_now)
        data_now_new <- as.data.frame(data_now_new)
        
        map_reg(data_now_new$totale_casi)
        #map_reg_dat(generate_tab(data_now_new$totale_casi))
        
    })
    
    
    # tab
    output$table_prov <- renderDataTable({
        full_date <- as.character(input$date_prov)
        yeas_date <- substring(as.character(full_date), 1, 4)
        month_date <- substring(as.character(full_date), 6, 7)
        day_date <- substring(as.character(full_date), 9, 10)
        
        url_now <- paste('https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province-',
                     yeas_date,month_date,day_date,'.csv', sep = "")
        data_now<-read_csv(url(url_now))
        
        data_now <- as.data.frame(data_now)
        data_now_new <- preparation_data_prov(data_now)
        data_now_new <- as.data.frame(data_now_new)
        
        generate_tab(data_now_new$totale_casi)
        
    })
    
    # text
    output$text_prov_1 <- renderText({
        paste("I merged some regioni to fit the data with the names of regioni of the map data set.")
    })
    output$text_prov_2 <- renderText({
        paste("I considered data of the first column as of the second one and delate them.")
    })
    output$text_prov_3 <- renderText({
        paste("I dati contrassegnati nel file originale come 'Fuori Regione / 
                Provincia Autonoma' o 'In fase di definizione/aggiornamento' sono stati 
                inseriti all'interno del conteggio dei capoluoghi di regione.")
    })
    # table
    output$table_prov_remove = renderTable(prov_removed)
    
   
    
    
    
}





shinyApp(ui = ui, server = server)


