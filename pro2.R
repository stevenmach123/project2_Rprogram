library(leaflet)
library(ggplot2)
library(lubridate)
library(reshape2)
library(dplyr)
library(shiny)
library(shinydashboard)
library(rgeos)
library(shinyBS)

#KHANG MACH
# CS424 PROJECT 2 



lote1<- read.table(file = "2018_datav2.csv", sep = ",",skipNul="TRUE",header = TRUE)

# Data process for 2018 
names(lote1)[names(lote1) =="Plant.latitude" ] <- "Lat"
lote1<- subset(lote1, Coal!="");
lote1 <- lote1[names(lote1) != "Year" ]
lote1$Coal<-gsub(",","",lote1$Coal,fixed=TRUE)
lote1$Oil <-gsub(",","",lote1$Oil,fixed=TRUE)
lote1$Gas <- gsub(",","",lote1$Gas,fixed=TRUE)
lote1$Nuclear <- gsub(",","",lote1$Nuclear,fixed=TRUE)
lote1$Hydro <- gsub(",","",lote1$Hydro,fixed=TRUE)
lote1$Biomass<- gsub(",","",lote1$Biomass,fixed=TRUE)
lote1$Wind <- gsub(",","",lote1$Wind,fixed=TRUE)
lote1$Solar <- gsub(",","",lote1$Solar,fixed=TRUE)
lote1$Geo <- gsub(",","",lote1$Geo,fixed=TRUE)
lote1$Other1 <- gsub(",","",lote1$Other1,fixed=TRUE)
lote1$Other2 <- gsub(",","",lote1$Other2,fixed=TRUE)


lote11<-lote1  
lote11 <-melt(data = lote1,id.vars=c("State","Plant","Long","Lat"))
lote11$value<-as.numeric(lote11$value)
lote11$value<- ifelse(lote11$value<0,0,lote11$value)
lote12 <-dcast(lote11,State+Plant+Long+Lat~variable)  # mealt and convert column to numeric
lote13 <-lote12 
lote13$Other <-ifelse(lote12$Other1==0,lote12$Other2,lote12$Other1)  # merge Other
lote13<-lote13[names(lote12) !="Other1" & names(lote12) !="Other2"] 


#View(lote13)
 

lote2018 <- lote13# will be used in part3
  lote13il <- subset(lote13,State=="IL")  # in part2

lote14d <- melt(data = lote13,id.vars=c("State","Plant","Long","Lat"))
vac<-as.factor(lote14d$variable)
#View(subset(lote13il,Other >0))


######

lote2 <- read.table(file = "2010_plnt_data.csv", sep = ",",skipNul="TRUE",header = TRUE)
lote4 <- read.table(file = "2000_plnt_data.csv", sep = ",",skipNul="TRUE",header = TRUE)

# Data process 2010 
lote2 <-subset(lote2, !is.na(Lat) & !is.na(Long))

names(lote2)[names(lote2) == "STATE"] <- "State"
lote21 <-melt(data =lote2,id.vars=c("State","Plant","Lat","Long"))
lote21$value <- gsub(",","",lote21$value,fixed=TRUE)
lote21$value <- as.numeric(lote21$value)  #melt and convert to numeric

lote21$value<-ifelse(lote21$value<0,0,lote21$value)  


lote22 <- dcast(data=lote21,State+Plant+Long+Lat~variable)# no melt 
lote22$Other <-ifelse(lote22$Other1==0,lote22$Other2,lote22$Other1) 
lote22<-lote22[names(lote22) !="Other1" & names(lote22) !="Other2"]  

# delete no data
lote22 <-subset(lote22,Coal>0 |Gas>0 |Oil>0 | Nuclear>0| Hydro>0 | Solar>0| Biomass>0| Wind>0| Geo>0 |Other>0 )


lote2010<-lote22

######
#Process data 2000 
names(lote4)[names(lote4)=="Wood"]  <- "Biomass"
lote43 <- subset(lote4,Lat != "N/A" & Long != "N/A" & Coal != "N/A" & Gas != "N/A" & Oil!="N/A" & Nuclear != "N/A" & Hydro != "N/A" & Solar !="N/A" & Biomass != "N/A" & Wind!= "N/A" & Geo != "N/A" & Other != "N/A")

lote43 <-subset(lote43,State !="")

lote4w <- lote43

lote43 <- melt(data =lote43,id.vars=c("State","Plant","Lat","Long"))

lote44 <-lote43
lote44$Lat <- as.numeric(lote43$Lat)
lote44$Long  <- as.numeric(lote43$Long)
lote44$value <- as.numeric(lote43$value)
lote44$Long <- lote44$Long*-1 
lote44 <-subset(lote44,State !="")

lote432 <-group_by(lote44,State,Plant,Lat,Long,variable) %>%  summarise_at(vars(value),list(sum_val=sum))

lote433 <-dcast(lote432,State+Plant+Lat+Long~variable,value.var ="sum_val") # check similarity,since some plant repeat with name, lat, long. 
lote2000 <-lote433


state_name <-c(state.name,"Washington DC") # I remember one year have DC  
state_key  <- c(state.abb,"DC")
con_key <- setNames(state_key,state_name)


ui  <- dashboardPage(
 
  dashboardHeader(title ="Khang Mach project2"),
  dashboardSidebar(
    checkboxGroupInput("variables","Source",c("All"="All","Renewable"="renew","Non-renewable"="non-renew","Coal"="Coal","Biomass"="Biomass","Gas"="Gas","Geo"="Geo","Hydro"="Hydro","Nuclear"="Nuclear","Oil"="Oil","Other"="Other","Solar"="Solar","Wind"="Wind"),selected =c("All"))
  ),
  
  
  dashboardBody( 
    fluidRow(
      textOutput("txt"),
      tabsetPanel(type="tabs",
                  tabPanel("Task 1", h1("hello"),
                           fluidRow( 
                             
                             column(width=12,leafletOutput("my_map"))
                           )
                           
                  )
                  ,tabPanel("Task 2",
                              fluidRow(
                                  column(width =6,h3("Map 1 checkboxs"),checkboxGroupInput("sel_map1","Source",c("All"="All","Renewable"="renew","Non-renewable"="non-renew","Coal"="Coal","Biomass"="Biomass","Gas"="Gas","Geo"="Geo","Hydro"="Hydro","Nuclear"="Nuclear","Oil"="Oil","Other"="Other","Solar"="Solar","Wind"="Wind"),selected =c("renew"),inline =TRUE  ),column(width=4,selectInput("year1","Year 1",c(2000,2010,2018),selected=2000 )), column(width=4,selectInput("state1","State 1",state_name,selected  ="Illinois")), column(width=3,selectInput("s1","Style 1",c(0,1,2,3),selected=0 )   ) )    
                                  
                                  ,column(width =6,h3("Map 2 checkboxs"),checkboxGroupInput("sel_map2","Source",c("All"="All","Renewable"="renew","Non-renewable"="non-renew","Coal"="Coal","Biomass"="Biomass","Gas"="Gas","Geo"="Geo","Hydro"="Hydro","Nuclear"="Nuclear","Oil"="Oil","Other"="Other","Solar"="Solar","Wind"="Wind"),selected =c("renew"),inline =TRUE  ),column(width=4,selectInput("year2","Year 2",c(2000,2010,2018),selected=2018)),column(width=4,selectInput("state2","State 2",state_name,selected  ="Illinois")),column(width=3,selectInput("s2","Style 2",c(0,1,2,3),selected=0 ) ) ) 
                                  ,column(width =12,style ="text-align:center",
                                          bsButton("unbone",label ="back to normal"),bsButton("bone",label="same source handle") )
                                  ,column(width =12, column(width =6,bsButton("reset1",label="reset view 1")), column(width =6,bsButton("reset2",label="reset view 2") ))
                                  ,column(width=12,column(width=6,leafletOutput("map1"),style="padding-top:60px",),column(width=6,leafletOutput("map2"),style="padding-top:60px")    )
                                  
                                  
                                         
                                          
              
                              )
                            
                            )
                  ,tabPanel("Task 3",
                       fluidRow(
                         column(width =4,selectInput("state3",label = "State",choices =c("All State",label ="State",state_name),selected="Illinois"))
                         ,column(width=4,selectInput("year3",label = "Year",choices=c(2000,2010,2018),selected =2018) )
                         ,column(width=4,checkboxGroupInput("sel_map3","Source",c("All"="All","Renewable"="renew","Non-renewable"="non-renew","Coal"="Coal","Biomass"="Biomass","Gas"="Gas","Geo"="Geo","Hydro"="Hydro","Nuclear"="Nuclear","Oil"="Oil","Other"="Other","Solar"="Solar","Wind"="Wind"),selected =c(""),inline =TRUE), bsButton("reset3",label ="reset view")     )
                         ,column(width =7,sliderInput("slid1", "slide 1",
                                                      min = 0, max = 200,
                                                      value = 0, step = 1,
                                                       sep = ",",
                                                      animate = TRUE))
                         ,column(width =7,sliderInput("slid2", "slide 2",
                                                      min = 0, max = 200,
                                                      value = 200, step = 1,
                                                       sep = ",",
                                                      animate = TRUE))
                       ),
                       fluidRow(
                         column(width=12,leafletOutput("map3"))
                       )
                    )
                  ,tabPanel("About",
                    column(width =4, h3("Khang Mach"),h4("Uploaded 3/11/2021"),h4("Project 2 -Raw Power"),h4("\n The original data is available from https://www.epa.gov/egrid/download-data \n
in particular this file: eGRID2018v2.xlsx  & eGRID2010_Data.xls & eGRID2000_plant.xls where we will be looking at data from the PLNT18, PLNTs. "))
                    
                  )
                  ,tabPanel("y", h2("heelo"),
                             fluidRow(
                                column(width=4,box(width=12)),
                                column(width=4,box(width=12))
                                
                             ),
                             fluidRow(
                               column(width=12,box(width=6),box(width=5))
                               
                             )
                            
                  )
                  
                             
                  )
                  
      )
      
    )
    
    
  )

# check how many matched from selection of individual source of a plant.
a<-function(e,matched){# match is linear list of source selected
  li <-c()
  
  for(r in 1:nrow(e)){
    t=0
    for(c in 5:ncol(e)){
      val <-e[r,c] 
      if(r ==1){
        #print(sapply(e[r,c],class))
      }
      if(names(e)[c] %in% matched){ 
        if(r==1){
          #print(names(e)[c])
        }
        if(val > 0){
          t =t +1
        }
      }
    }
    li <-c(li,t)
  }
  li # return list of match 
}

# similar to a(), but now specify what source is match.
group_select <-function(e,matched){
  lu <- c()
  
  for(i in 1:nrow(e) ){
    combine = ""
    comma = FALSE
    
    for(c in 5:ncol(e)){
      if(e[i,c]> 0 & comma ==FALSE ){
        if( names(e)[c] %in% matched ){
          combine = paste(sep="",combine, names(e)[c]) #no , if only 1 source matched
          comma = TRUE
        }
      }   
      else if(e[i,c]> 0 & comma ==TRUE){
        if( names(e)[c] %in% matched ){
          combine = paste(sep="",combine,",",names(e)[c]) #combine sources matched into single string 
        } 
      }
    }
    lu <- c(lu,combine)   # list of string
    
  }
  lu
}

# identify percent of renewable and non
renew <-function(e){
  per_frame <- data.frame(renew =c(), non=c() )
  for(r in 1:nrow(e)){
    renew =0 
    non  =0
    for(c in 1:ncol(e) ){
      if(names(e)[c] == "Coal" | names(e)[c] =="Oil"| names(e)[c]=="Gas" | names(e)[c]=="Nuclear" | names(e)[c] =="Other" | names(e)[c]=="Hydro" | names(e)[c]=="Biomass" | names(e)[c]=="Solar"|names(e)[c]=="Wind" | names(e)[c] =="Geo" ){
        if(names(e)[c] == "Coal" | names(e)[c] =="Oil"| names(e)[c]=="Gas" | names(e)[c]=="Nuclear" | names(e)[c] =="Other"){
          non = non +  e[r,c] # accumulate numeric for non 
        }
        if(names(e)[c]=="Hydro" | names(e)[c]=="Biomass" | names(e)[c]=="Solar"|names(e)[c]=="Wind" | names(e)[c] =="Geo"){
          renew =  renew + e[r,c] # accumulate numeric for renew
          
        }
        
      }
   
    }  
    
    p_renew = (renew/(non +renew))*100 
    p_non = (non/(non +renew))*100 #check percent
    p_renew = round(p_renew,digits=2)
    p_non = round(p_non,digits=2)
    sub <-data.frame(p_renew,p_non)
    names(sub) <- c("renew","non") 
    per_frame <-rbind(per_frame,sub) # create data frame 
    
  }
  #View(per_frame) 
  return(per_frame)#return frame with renew and non %
  
}


# used with unik, give unique value by position of all possible mix 
getIndex <- function(cha,index_l){
  inde = 1
  for(i in index_l ){
    if(cha == index_l[inde]){
      cat_v  =length(strsplit(cha,",")[[1]]) 
      return(inde)
    }
    inde = inde +1
  }
  return(-1)
  
}



# gernrate unique key for each combination of mixed. 
unik <- function(e){
  uni  <-c()
  two <-c()
  three <-c() 
  
  for(r in 1:nrow(e)){   
    ei <- e[r,]
    
    if(ei$match >1) { # only give key for mixed
      if( !( ei$select %in% two) & !(ei$select %in% three) ){
        cat_v =length(strsplit(ei$select,",")[[1]]) # parse string source matched
        
        if(cat_v == 2){   
          two <- c(two,ei$select) # concat 
        }
        if(cat_v >=3){
          three <- c(three,ei$select) 
          
        }
      }    
    }
    
    
  }
  
  ord <-c(two,three)
  #print(ord)
  
  for(r in 1:nrow(e)){ 
    ei <- e[r,]
    if(ei$match > 1){ 
      uni <- c(uni,getIndex(ei$select,ord)) #give key by position 
    } 
    else {
      uni <- c(uni,0) # 0 for single match 
    }          
    
    
  }
  return(uni)
  
}


# replace for subset(lotem, Oil >0 | Coal>0..) this will replace write down all possbile of selected source,where match at least a source will be stored.
sf <-function(e,choices){
  
  fr <- data.frame(State=c(),Plant= c(),Long =c(),Coal =c(),Gas =c(),Nuclear =c(),Hydro =c(), Biomass=c(),Wind=c(),Solar=c(),Geo=c(), Other=c() )
  for(r in 1:nrow(e)){
   
    check = FALSE
    for(choice in choices){
      if(e[r,choice] > 0){ #match a source selection, put in !
        check= TRUE
      }  
    }
    if(check==TRUE){
      fr <- rbind(fr,e[r,]) #valid sources 
    }
    
  }
  return(fr)
  
}

# find total kwh for 10 source for each plant
tgenerate <- function(e){
  list_total  <- c()
  for(r in 1:nrow(e) ){
    total = e[r,]$Coal + e[r,]$Gas + e[r,]$Oil + e[r,]$Nuclear + e[r,]$Other + e[r,]$Solar + e[r,]$Biomass + e[r,]$Wind + e[r,]$Geo + e[r,]$Hydro
    list_total <- c(list_total,total)
    total = 0 
  }
  list_total
}

seesource <- function(e){
  list_source <- c()
  for(r in 1:nrow(e)){
    name =""
    name = paste(name,e[r,]$Plant,"<br/>")
    
    for(c in 1:ncol(e) ){
      
      if(names(e)[c] == "Coal" | names(e)[c] =="Oil"| names(e)[c]=="Gas" | names(e)[c]=="Nuclear" | names(e)[c] =="Other" | names(e)[c]=="Hydro" | names(e)[c]=="Biomass" | names(e)[c]=="Solar"|names(e)[c]=="Wind" | names(e)[c] =="Geo" ){
        if(e[r,c] != 0){
          name = paste(name,names(e)[c],e[r,c]," ")
        }
        
      }
      
    }
    list_source <- c(list_source,name)
    
  }
  list_source 
}

reset = 0
# stimulation of task2 
task2_sti <- function(state_data,year_data,sel_type,ol,mtask=0,v1=0,v2 =0){
  
  sel <-c()
  lotem <- data.frame(State=c(),Plant= c(),Long =c(),Coal =c(),Gas =c(),Nuclear =c(),Hydro =c(), Biomass=c(),Wind=c(),Solar=c(),Geo=c(), Other=c() )
  if(year_data ==2018){
    lotem<-lote2018
  }
  if(year_data ==2010 ){
    lotem <-lote2010
  }
  
  if(year_data == 2000 ){
    lotem <- lote2000
  }
  sta <- con_key[state_data] #conver state to key 
  
  if( state_data !="All State"){
    lotem <- subset(lotem,State == sta)
  }
  #print(sel_type)
  if( !("All" %in% sel_type)  ){ # All is in selection
        if( (length(sel_type) ==1 | (length(sel_type) ==2 & "None" %in% sel_type ) )  & !( "renew" %in% sel_type) & !( "non-renew" %in% sel_type )  ){
          #CASE FOR ONE SELECTION (not involve renew and non renew)
          if(sel_type[1] ==  "Coal"){
            lotem3 <- subset(lotem, Coal >0)
          } 
          else if(sel_type[1] == "Gas"){
            lotem3 <- subset(lotem, Gas >0)
          }
          else if(sel_type[1] == "Oil"){
            lotem3 <- subset(lotem, Oil >0)
          }
          else if(sel_type[1] == "Other"){
            lotem3 <- subset(lotem, Other >0)
          }
          else if(sel_type[1] == "Nuclear" ){
            lotem3 <- subset(lotem, Nuclear >0)
          }
          else if(sel_type[1] == "Biomass"){
            lotem3 <- subset(lotem, Biomass >0)
          }
          else if (sel_type[1] == "Hydro"){
            lotem3 <- subset(lotem, Hydro >0) 
          }
          else if(sel_type[1] == "Solar"){
            lotem3 <- subset(lotem, Solar >0) 
          }
          else if(sel_type[1] == "Wind"){
            lotem3 <- subset(lotem, Wind >0)  
          }
          else if(sel_type[1] == "Geo"){
            lotem3 <- subset(lotem, Geo >0)  
          }
          else{
            lotem3 <- data.frame(State=c(),Plant= c(),Long =c(),Coal =c(),Gas =c(),Nuclear =c(),Hydro =c(), Biomass=c(),Wind=c(),Solar=c(),Geo=c(), Other=c() )
            
          }
          
          percent_e <- renew(lotem3) # percent
          lotem3$energy  <- sel_type[1]   # type energy
          #print(sel_type[1])
          lotem3$total_e <- tgenerate(lotem3) # total generation
          lotem3$list_source <- seesource(lotem3) # list of ALL source identity and its generation
          bmax =0 
          bmin = 0
          
          if(mtask ==3){ # provoked for task3
            maxi <<-max(lotem3$total_e)
            mini <<- min(lotem3$total_e) # find min,max of subset
            
            if((maxi*v1)/prev_max  > (maxi*v2)/prev_max ) { # TWO SLIDER CAN BE EITHER one min and other max
              bmax = (maxi*v1)/prev_max
              bmin = (maxi*v2)/prev_max 
            }
            else{
              bmax  = (maxi*v2)/prev_max 
              bmin = (maxi*v1)/prev_max
            }
            print(paste("bmax",bmax))
           print(paste("bmin",bmin))
            lotem3 <-subset(lotem3, total_e >= bmin & total_e <= bmax) # subset again with boundary
           # print(nrow(lotem3))
          }
          
          
          
     
          #color for single energy
         icons <-  awesomeIcons(icon = 'ios-close',iconColor = ~pa(as.factor(energy)), markerColor ="white",library = 'ion')
                        
         if(mtask !=3){
             mapp <- leaflet(lotem3) %>%addTiles %>% addAwesomeMarkers(lng=~Long,lat =~Lat,icon = icons,popup = ~paste("Source matched <br/>",~energy,"<br/> renewable:",percent_e$renew ,"%","<br/> non-renewable:",percent_e$non,"%")) %>%addLegend(position ="bottomleft", pal=pa ,value =lotem3$energy ,title ="1 energy type")
         }
         else{ # for task 3
           
           mapp <- leaflet(lotem3) %>%setView(lng = mean(lotem3$Long), lat = mean(lotem3$Lat),zoom=6) %>% addTiles() %>% addCircleMarkers(lng =~Long ,lat = ~Lat,radius = 3,color =~pa(as.factor(energy)),popup = ~paste("Source matched<br/>",energy,"<br/> Renewable:",percent_e$renew,"%","<br/>Non-Renewable:",percent_e$non,"%","<br/>Plant identity:<br/>",list_source)) %>% addLegend(position ="bottomleft", pal=pa ,value=lotem3$energy ,title ="1 energy type")
           
          
         }
         
         nhd_wms_url <- "https://basemap.nationalmap.gov/arcgis/services/USGSTopo/MapServer/WmsServer"
         if(ol != 0){# three diff layer map
           if(ol ==1){
             mapp <- mapp %>%  addWMSTiles(nhd_wms_url, layers = "0")
           }
           else if(ol==2){
             mapp <- mapp %>% addProviderTiles(providers$Esri.NatGeoWorldMap)
           }
           else if(ol==3){
             mapp <- mapp %>%  addProviderTiles(providers$Stamen.Toner)
           }
         }
         
         
         
         
         
         
         return(mapp)
      }
      else{
         sel <- c()
         if("renew" %in% sel_type ){ # when select renew, then no need other indiv sources of renew
            sel <- c(sel, c("Hydro","Biomass","Wind","Solar","Geo"))
            
          }
          else{
              for(type in sel_type){ # if renew not selected, then need see what source of renew is picked
              if(type == "Hydro" | type=="Biomass" | type== "Wind" | type=="Solar" | type=="Geo" ){
                if(type == "Hydro"){
             
                  sel <- c(sel,"Hydro")
                }
                else if(type=="Biomass"){
                 
                  sel <- c(sel,"Biomass")
                }
                else if(type=="Wind"){
                  
                  sel <- c(sel,"Wind")
                }
                else if(type=="Solar" ){
                 
                  sel <- c(sel,"Solar")
                }
                else if(type =="Geo"){
                  
                  sel <- c(sel,"Geo")
                }
                
              }
            }
          }

         if("non-renew" %in% sel_type){ # when select non, then no need other indiv sources of non
            sel <-c(sel,c("Oil","Gas","Other","Nuclear","Coal"))  
          }
         else{
            for(type in sel_type){ #if non not selected, then need see what source of non is picked
              if(type == "Coal" | type=="Gas" | type== "Oil" | type=="Other" | type=="Nuclear" ){
                if(type == "Coal"){
   
                  sel  <- c(sel,"Coal")
                }
                else if(type=="Gas"){
                  
                  sel <- c(sel, "Gas")
                }
                else if(type=="Oil"){
                 
                  sel <- c(sel,"Oil")
                }
                else if(type=="Other" ){
                  
                  sel <- c(sel,"Other")
                }
                else if(type =="Nuclear"){
                 
                  sel <- c(sel,"Nuclear")
                }
                
              }
            }
            
         }
          #print(sel)
         
        
          
         if(mtask!=3){
           valid_frame <- sf(lotem,sel) # frame of OR subset. talked above
           
           lotem6 <- valid_frame
           
          lotem6$match <- a(lotem6,sel)  # number of match selection
          lotem6$select  <- group_select(lotem6,sel) #what source are in matched
          lotem6$selectk <- unik(lotem6)   #unique key for all possible mix
          lotem6.percent_e <- renew(lotem6) # percent renew and non
          lotem6$list_source <- seesource(lotem6) # list of all source identity and its generation 
          lotem6$total_e <- tgenerate(lotem6)
         }
          bmax =0 
          bmin = 0
        
          if(mtask ==3){ # task3 ,desibed above
            print("above3")
            lotem6 <- lotem  
            lotem6$match <- a(lotem6,sel)
            lotem6 <-subset(lotem6,match>0)
          
            lotem6$select  <- group_select(lotem6,sel)
            View(lotem6)
            #lotem6$selectk <- unik(lotem6)
           #lotem6$total_e <- tgenerate(lotem6)
          #  lotem6.percent_e <- renew(lotem6)
           # lotem6$list_source <- seesource(lotem6)
           # maxi <<-max(lotem6$total_e)
           # mini <<- min(lotem6$total_e)
            
        #  if((maxi*v1)/prev_max  > (maxi*v2)/prev_max ) {
        #     bmax = (maxi*v1)/prev_max
        #     bmin = (maxi*v2)/prev_max 
         #  }
       #    else{
        #     bmax  = (maxi*v2)/prev_max 
      #       bmin = (maxi*v1)/prev_max
      #     }
       #    print(paste("bmax",bmax))
     #      print(paste("bmin",bmin))
         #  lotem6 <-subset(lotem6, total_e >= bmin & total_e <= bmax)
           print("below 3")
          }
       
          print(unique(names(lotem6)))
         
          
          
          
          # color for mixed, generate dynamically based on how many mix and what mixed on each plant
        b_color  <-colorNumeric(c("#feb3ff","pink","purple","#cc0099","#ff6680"), domain= lotem6$selectk)
        icons <- awesomeIcons(icon = 'ios-close',iconColor = ~ifelse(match >1,b_color(selectk),pa(select)),  markerColor = "white" ,library = 'ion') 
        #View(lotem6)
        if(mtask !=3){
          #selectk is unique key for b_color , and pa is for one only source
          out_lotem6 <-leaflet(lotem6)  %>%  setView(lng = mean(lotem6$Long), lat = mean(lotem6$Lat),zoom=6) %>%addTiles  %>% addAwesomeMarkers(lng=~Long,lat =~Lat,icon = icons,popup = ~paste("Source matched<br/>",select,"<br/>Mix-key ",selectk,"<br/> Renewable:",lotem6.percent_e$renew,"%","<br/>Non-Renewable:",lotem6.percent_e$non,"%","<br/>Plant identity:<br/>",list_source)) %>% addLegend(position ="bottomleft", pal=pa ,value=~ifelse(match==1,select,"") ,title ="1 energy type") %>% addLegend(position ="bottomright", pal=b_color, value = ~selectk ,title ="Mix type ") %>%addLegend(position ="topright",pal=pa,value=~ifelse(match>1,paste(select,":",selectk),""),title="Mix type name-key")
        }      
        else{
         #out_lotem6 <- leaflet(lotem6) %>%setView(lng = mean(lotem6$Long), lat = mean(lotem6$Lat),zoom=6) %>% addTiles %>% addCircleMarkers(lng =~Long ,lat = ~Lat,radius = 3,color =~ifelse(match >1,b_color(selectk) ,pa(select)),popup = ~paste("Source matched<br/>",select,"<br/>Mix-key ",selectk,"<br/> Renewable:",lotem6.percent_e$renew,"%","<br/>Non-Renewable:",lotem6.percent_e$non,"%","<br/>Plant identity:<br/>",list_source)) %>% addLegend(position ="bottomleft", pal=pa ,value=~ifelse(match==1,select,"") ,title ="1 energy type") %>% addLegend(position ="bottomright", pal=b_color, value = ~selectk ,title ="Mix type ") 
        # out_lotem6 <- leaflet(lotem6) %>%setView(lng = mean(lotem6$Long), lat = mean(lotem6$Lat),zoom=6) %>% addTiles %>% addCircleMarkers(lng =~Long ,lat = ~Lat,radius = 3,color =~ifelse(match >1,"#ff6680",pa(select)),popup = ~paste("Source matched<br/>","<br/>Mix-key ","<br/> Renewable:","%","<br/>Non-Renewable:","%","<br/>Plant identity:<br/>",Plant)) %>% addLegend(position ="bottomleft", pal=pa ,value=~ifelse(match==1,select,"Mix") ,title ="1 energy type")
          #out_lotem6 <- leaflet(lotem6) %>%setView(lng = mean(lotem6$Long), lat = mean(lotem6$Lat),zoom=6) %>% addTiles %>% addCircleMarkers(lng =~Long ,lat = ~Lat,radius = 3,color =~ifelse(pa(select)))
         out_lotem6 <- leaflet(lotem6) %>%setView(lng = mean(lotem6$Long), lat = mean(lotem6$Lat),zoom=6) %>% addTiles %>% addCircleMarkers(lng =~Long ,lat = ~Lat,radius = 3,color= ~ifelse(match >1,"#ff6680",pa(select) ))
        }
        
        nhd_wms_url <- "https://basemap.nationalmap.gov/arcgis/services/USGSTopo/MapServer/WmsServer"
        if(ol != 0){
          if(ol ==1){
             out_lotem6 <- out_lotem6 %>%  addWMSTiles(nhd_wms_url, layers = "0")
          }
          else if(ol==2){
            out_lotem6 <- out_lotem6 %>% addProviderTiles(providers$Esri.NatGeoWorldMap)
          }
          else if(ol==3){
            out_lotem6 <- out_lotem6 %>%  addProviderTiles(providers$Stamen.Toner)
          }
        }
       
        
        
        
        return(out_lotem6)
    }  
        
  }
  else{ # when ALL selected
    sel <-c("Oil","Gas","Other","Nuclear","Coal","Hydro","Biomass","Wind","Solar","Geo")
    lotem6 <- subset(lotem, Oil> 0 | Gas>0 | Other>0 | Nuclear >0 | Coal >0 | Hydro>0 | Biomass>0 | Wind>0 | Solar >0 | Geo>0 ) # at least a match
    lotem6$match <- a(lotem6,sel)
    lotem6$select  <- group_select(lotem6,sel)
    lotem6$selectk <- unik(lotem6)
    lotem6.percent_e <- renew(lotem6)
    
    b_color  <-colorNumeric(c("#feb3ff","pink","purple","#cc0099","#ff6680"), domain= lotem6$selectk)
    icons <- awesomeIcons(icon = 'ios-close',iconColor = ~ifelse(match >1,b_color(selectk),pa(select)),  markerColor = "white" ,library = 'ion') 
    
    
    
    lotem6$total_e <- tgenerate(lotem6)
    bmax =0 
    bmin = 0
    
    if(mtask ==3){ # for task3
      maxi <<-max(lotem6$total_e)
      mini <<- min(lotem6$total_e)
      
      if((maxi*v1)/prev_max  > (maxi*v2)/prev_max ) {
        bmax = (maxi*v1)/prev_max
        bmin = (maxi*v2)/prev_max 
      }
      else{
        bmax  = (maxi*v2)/prev_max 
        bmin = (maxi*v1)/prev_max
      }
     #print(paste("bmax",bmax))
     #print(paste("bmin",bmin))
      lotem6 <-subset(lotem6, total_e >= bmin & total_e <= bmax)
     # print(nrow(lotem6))
    }
    lotem6$list_source <-seesource(lotem6)
    
    
    
    
    if(mtask !=3){
    out_lotem6 <-leaflet(lotem6) %>%  setView(lng = mean(lotem6$Long), lat = mean(lotem6$Lat),zoom=6)  %>%  addTiles %>% addAwesomeMarkers(lng=~Long,lat =~Lat,icon = icons,popup = ~paste("Source matched<br/>",select,"<br/>Mix-key ",selectk,"<br/> Renewable:",lotem6.percent_e$renew,"%","<br/>Non-Renewable:",lotem6.percent_e$non,"%")) %>% addLegend(position ="bottomleft", pal=pa ,value=~ifelse(match==1,select,"") ,title ="1 energy type") %>% addLegend(position ="bottomright", pal=b_color, value = ~selectk ,title ="Mix type ") %>%addLegend(position ="topright",pal=pa,value=~ifelse(match>1,paste(select,":",selectk),""),title="Mix type name-key")  
    }
    else {
      out_lotem6 <- leaflet(lotem6) %>%setView(lng = mean(lotem6$Long), lat = mean(lotem6$Lat),zoom=6) %>% addTiles %>% addCircleMarkers(lng =~Long ,lat = ~Lat,radius = 3,color =~ifelse(match >1,b_color(selectk) ,pa(select)),popup = ~paste("Source matched<br/>",select,"<br/>Mix-key ",selectk,"<br/> Renewable:",lotem6.percent_e$renew,"%","<br/>Non-Renewable:",lotem6.percent_e$non,"%","<br/>Plant identity:<br/>",list_source)) %>% addLegend(position ="bottomleft", pal=pa ,value=~ifelse(match==1,select,"") ,title ="1 energy type") %>% addLegend(position ="bottomright", pal=b_color, value = ~selectk ,title ="Mix type ") 
     
    }
    
    nhd_wms_url <- "https://basemap.nationalmap.gov/arcgis/services/USGSTopo/MapServer/WmsServer"
    if(ol != 0){ # map layer
      if(ol ==1){
        out_lotem6 <- out_lotem6 %>%  addWMSTiles(nhd_wms_url, layers = "0")
      }
      else if(ol==2){
        out_lotem6 <- out_lotem6 %>% addProviderTiles(providers$Esri.NatGeoWorldMap)
      }
      else if(ol==3){
        out_lotem6 <- out_lotem6 %>%  addProviderTiles(providers$Stamen.Toner)
      }
    }
    
    return(out_lotem6)
  }



}




pa<-colorFactor(c("green","red","#99e2ff","brown","blue","orange","#e68a00","#cccc00","#99ccff","#66ffe3"),domain= vac)
 
deci2 = 0

### This is variable set of task3

pre_year = 0
pre_state = "" 
pre_sel =""
slid_val1  =0 # slider1 value
slid_val2 = 0  #slider2 value
maxi = 0 # who represent max from 2 slider
mini= 0  # who represent min from 2 slider

prev_max= 200
tag =1 


server <- function(input,output,session){
   
    checkin <- reactive({ # paste all list of selection source
    icons <- paste(input$variables, collapse = ",")
    icons
    
    }) 
    
    observeEvent(input$bone,({ #part 2 for checkbox if needed to update simutanously
      deci2 <<-1
    
     # print("bone")
      
    }))
    observeEvent(input$unbone,({ # back to normal
      deci2 <<-0
     # print("unbone")
    }))
    
    observeEvent(input$reset1, ({
      reset <<- 1
      
    }))
    # for task 1, but in simplify version of task 2,3
    output$my_map <-renderLeaflet({
      com <- checkin()
      co <-strsplit(com,",")[[1]] 
      lote13dd <- lote13il 
      if( !("All" %in% co) ){  
        
        if(length(co)==1 &&  !( "renew" %in% co) && !( "non-renew" %in% co ) ){
          lote13dd <- melt(data = lote13il,id.vars=c("State","Plant","Long","Lat") )
          lote13dd <- subset(lote13dd,variable ==co[1] & value >0 )
          
         
          # pa will check energy
          icons <-  awesomeIcons(icon = 'ios-close',iconColor = ~pa(variable), markerColor ="white",library = 'ion')
          
          mapp <- leaflet(lote13dd) %>%addTiles %>% addAwesomeMarkers(lng=~Long,lat =~Lat,icon = icons,popup = ~paste("Source matched </br>",~variable))
          return(mapp)
        }
        else{
          sel <- c()
          if("renew" %in% co){ # renew is in selection 
            
            lote13s <-lote13dd
           # View(lote13s)
            #print("renew")
            sel <- c(sel, c("Hydro","Biomass","Wind","Solar","Geo"))
            
          }
          else{ # if renew not in, then do little exercise 
            for(type in co){
              if(type == "Hydro" | type=="Biomass" | type== "Wind" | type=="Solar" | type=="Geo" ){
                if(type == "Hydro"){
             
                  sel <- c(sel,"Hydro")
                }
                else if(type=="Biomass"){
                 
                  sel <- c(sel,"Biomass")
                }
                else if(type=="Wind"){
                  
                  sel <- c(sel,"Wind")
                }
                else if(type=="Solar" ){
                 
                  sel <- c(sel,"Solar")
                }
                else if(type =="Geo"){
                  
                  sel <- c(sel,"Geo")
                }
                
              }
            }
            
           # print("renew select")
            
          
            
          }
          
          
          if("non-renew" %in% co){ # if non-renew is in 
           
            #print("non-renew")
            sel <-c(sel,c("Oil","Gas","Other","Nuclear","Coal"))  
          }
          else{ # if renew is not in, then try pick non source individually
            for(type in co){
              if(type == "Coal" | type=="Gas" | type== "Oil" | type=="Other" | type=="Nuclear" ){
                if(type == "Coal"){
   
                  sel  <- c(sel,"Coal")
                }
                else if(type=="Gas"){
                  
                  sel <- c(sel, "Gas")
                }
                else if(type=="Oil"){
                 
                  sel <- c(sel,"Oil")
                }
                else if(type=="Other" ){
                  
                  sel <- c(sel,"Other")
                }
                else if(type =="Nuclear"){
                 
                  sel <- c(sel,"Nuclear")
                }
                
              }
            }
            #print("non-renew select" )
          }
          
        }
        
        valid_frame <- sf(lote13dd,sel)
        
        lote13dd <- valid_frame # or subset for selection
       # View(lote13dd)
        lote13dd$match <- a(lote13dd,sel) # number of match
        lote13dd$select  <- group_select(lote13dd,sel) # source of selected
        lote13dd$selectk <- unik(lote13dd) # selected key for mix
        
        icons <- awesomeIcons(icon = 'ios-close',iconColor = ~ifelse(match >1,b_color(selectk),pa(select)),  markerColor = "white" ,library = 'ion')
        b_color  <-colorNumeric(c("#feb3ff","pink","purple","#cc0099","#ff6680"), domain= lote13dd$selectk)
        # legend for match =1 by pa(check 9 sources and give each with consistence color) , and match >1 by b_color(identify number of mixed)  . and give name matched of mixed key 
        out <-leaflet(lote13dd) %>%addTiles %>% addAwesomeMarkers(lng=~Long,lat =~Lat,icon = icons,popup = ~paste("Source matched<br/>",select,"<br/>Mix-key ",selectk)) %>% addLegend(position ="bottomleft", pal=pa ,value=~ifelse(match==1,select,"") ,title ="1 energy type") %>% addLegend(position ="bottomright", pal=b_color, value = ~selectk ,title ="Mix type ") %>%addLegend(position ="topright",pal=pa,value=~ifelse(match>1,paste(select,":",selectk),""),title="Mix type name-key")  
        
        #print("---")
       
        return(out)
        
      }
      else{ # when All is slected
        sel <-c("Oil","Gas","Other","Nuclear","Coal","Hydro","Biomass","Wind","Solar","Geo")
        lote13dd <- subset(lote13dd, Oil> 0 | Gas>0 | Other>0 | Nuclear >0 | Coal >0 | Hydro>0 | Biomass>0 | Wind>0 | Solar >0 | Geo>0 )
        lote13dd$match <- a(lote13dd,sel)
        lote13dd$select  <- group_select(lote13dd,sel)
        lote13dd$selectk <- unik(lote13dd)
       icons <- awesomeIcons(icon = 'ios-close',iconColor = ~ifelse(match >1,b_color(selectk),pa(select) ), markerColor = "white",library = 'ion')
     # icons <- awesomeIcons(icon :L.divIcon({html: '<ion-icon name="arrow-redo-outline"></ion-icon>',iconSize :[20,20], className :'div1'}),iconColor = ~ifelse(match >1,b_color(selectk),pa(select)), markerColor = "white",library = 'ion')
         
     
        
        b_color  <-colorNumeric(c("#feb3ff","pink","purple","#cc0099","#ff6680"), domain= lote13dd$selectk)
        #View(lote13dd)
        
      
        
      
        leaflet(lote13dd) %>% addTiles() %>%addAwesomeMarkers(lng=~Long,lat =~Lat,icon = icons ,popup = ~paste("Source matched <br/>",select,"<br/>Mix-key ",selectk)) %>% addLegend(position ="bottomleft", pal=pa ,value=~ifelse(match==1,select,"") ,title ="1 energy type") %>% addLegend(position ="bottomright", pal=b_color, value = ~selectk ,title ="Mix type ") %>%addLegend(position ="topright",pal=pa,value=~ifelse(match>1,paste(select,":",selectk),""),title="Mix type name-key") 
      }
      
    
    })
    
    output$map1 <-renderLeaflet({
        rrr<-input$reset1
        if(deci2 != 0){ # if check map1, also copy check appearance to check2
          #print("1in bone")
          x <- input$sel_map1
          updateCheckboxGroupInput(session,"sel_map2",selected =x)
        }
        year <- input$year1
        state  <- input$state1
       
        seel <-paste(input$sel_map1, collapse = ",")
        
        seelo   <-strsplit(seel,",")[[1]]  # parse string of selection map1 into list
        
        re <- task2_sti(state,year,seelo,input$s1) # stimulate task2
       
       # print("ee")
        re
     
    }) 
    
    output$map2 <-renderLeaflet({
      rrr<-input$reset2
      if(deci2 != 0){ # if check map2, also copy check appearance to check1
       # print("2in sbone")
        x <- input$sel_map2
        updateCheckboxGroupInput(session,"sel_map1",selected =x)
      }
      seel <-paste(input$sel_map2, collapse = ",") #parse string of selection map2 into list
      seelo  <-strsplit(seel,",")[[1]] 
      re <-task2_sti(input$state2,input$year2,seelo, input$s2) # stimulate task2
      re
      
      
    })
    
   
    
    re_map <- c()
    #task3
    output$map3 <- renderLeaflet({
      print("--")
      seel <-paste(input$sel_map3, collapse = ",")
      seelo  <-strsplit(seel,",")[[1]]  # prase selection source 
      j <- input$reset3
      print(paste("-",slid_val1,input$slid1))
      print(paste("-",slid_val2,input$slid2))
      
      #print("ne")
      if(pre_year  != input$year3 | pre_state != input$state3 | pre_sel != paste(input$sel_map3, collapse = ",") |  slid_val1 != input$slid1 | slid_val2 != input$slid2 ){
       
        tag <<-0 
       print("ss_0")
      }
      
    
        
      if(tag ==0){
       print("ss")
        mapp <-task2_sti(input$state3,input$year3,seelo,0,mtask =3, v1 = input$slid1, v2= input$slid2) # activate task3 mood by mtask =3, v1 is value of slider1, v2 for slider2
        
        updateSliderInput(inputId="slid1",session,max = maxi,min =mini,value = (maxi*input$slid1)/prev_max ) # update slider by new max  and min, as well as selection no change in position, but in value 
        updateSliderInput(inputId="slid2",session,max = maxi,min =mini,value = (maxi* input$slid2)/prev_max )
      
         # record previous max
        
       print(paste(slid_val1,input$slid1))
       print(paste(slid_val2,input$slid2))
     
        slid_val1 <<- (maxi*input$slid1)/prev_max
        slid_val2  <<-(maxi* input$slid2)/prev_max
   
       if(slid_val1 <= mini ){
          slid_val1  <<- round(mini,digits=-1)
        }
      
       
      if(slid_val2 <= mini ){
          slid_val2  <<- round(mini,digits=-1)
      }
       
        
        prev_max  <<- maxi
        pre_year  <<- input$year3
        pre_state  <<- input$state3 
        pre_sel <<- paste(input$sel_map3, collapse = ",") # resord previous selection
        tag<<-1
       
        re_map <<- mapp
        return(mapp)
      }
      else{
        
        return(re_map)
      }
    
      
      
     
    })
    
   
    
    
    
    
    
    
    
}
shinyApp(ui,server)
