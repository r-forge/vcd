if (FALSE) {
## collection of R functions to query http://www.dfb.de/
## for Bundesliga results


## convenience functions

## transliteration
translit <- function(x) {
  x <- gsub("&auml;", "ae", x, extended = FALSE, fixed = TRUE)
  x <- gsub("&ouml;", "oe", x, extended = FALSE, fixed = TRUE)
  x <- gsub("&uuml;", "ue", x, extended = FALSE, fixed = TRUE)
  x <- gsub("&Auml;", "Ae", x, extended = FALSE, fixed = TRUE)
  x <- gsub("&Ouml;", "Oe", x, extended = FALSE, fixed = TRUE)
  x <- gsub("&Uuml;", "Ue", x, extended = FALSE, fixed = TRUE)
  x <- gsub("&szlig;", "ss", x, extended = FALSE, fixed = TRUE)
  x
}

home_team <- function(x) strsplit(x[3], "  ")[[1]][1]
away_team <- function(x) strsplit(x[4], "  ")[[1]][1]
home_goals <- function(x) {
  x <- gsub(" ", "", strsplit(x[5], "  ")[[1]][1])
  strsplit(x, ":")[[1]][1]
}
away_goals <- function(x) {
  x <- gsub(" ", "", strsplit(x[5], "  ")[[1]][1])
  strsplit(x, ":")[[1]][2]
}

      ## extract time                                                                      
play_time <- function(x){                                                                                                                                                         
          strsplit(x[2], "<\\TD>")                                                                                                                                     
          ptime <- strptime(strsplit(x[2], "</TD>")[[1]][1], format = "%d.%m.%Y, %H:%M")
           }

game_info <- function(x) {
  c(translit(home_team(x)), translit(away_team(x)), home_goals(x), away_goals(x),as.character(play_time(x)))
}
                
## years and rounds
Year <- as.character(1963:2005)                     # Label Jahr des Saisonstarts
Round <- gsub(" ", "0", format(1:38))               # Label Rundenzahl (zw. 30 und 38)
w<-as.list(1:43)                                     # Jahresliste
Ses <- c(as.character(63:99),gsub(" ", "0", format(0:10))) #Saisoninfo relevant fuer die neuesten Jahre


######################################################################################################
          ## ERSTEN 2 JAHRE      (nur 30 Runden)
######################################################################################################
                                                                
y<-as.list(1:30)        # 30 Runden (Liste)                                                                      
                                                                                                                                                                                            
for(j in 1:2){                                                                               
   year<-Year[j]                                                                              
   for(i in 1:30) {                                                                                                                                                       

      ## read raw HTML                                                                        
      x <- readLines(url(paste("http://www.dfb.de/bliga/bundes/archiv/", year,                
                                "/spieltage/bs1m", Round[i], ".html", sep = "")))             
                                                                                              
      ## select relevant lines                                                                
      x <- x[substr(x, 1, 13) == "<tr><TD><font"]                                             
      ## split on font information                                                            
      x <- strsplit(x, "face=\"Arial,helvetica,sans-serif\">")                                
      ## extract relevant information                                                         
      x <- t(sapply(x, game_info))                                                            
      ## collect results                                                                      
      y[[i]] <- cbind(x, as.numeric(Round[i]), as.vector(year))                               
      closeAllConnections()                                                                   
                                                                                              
        ## merge suitably with previous rounds...                                             
        z<-y[[1]]                                  #hänge an die erste Runde                                           
            for(i in 1:29)                         #die restlichen 33 Runden                                           
            {                                      #per RBIND dran                                           
            z<-rbind(z,y[[i+1]])                                                              
            z                                                                                 
            }                                                                                 
                                                                                              
      }                                                                                       
    w[[j]]<-z                                      #schreibe das Jahr in die Jahresliste                                                                                                                                          
    }                                                                                                                                                                                           

######################################################################################################
##                                    1965-1990
######################################################################################################

y<-as.list(1:34)                   #34 Runden (Liste)

for(j in 3:28){
   year<-Year[j]
   for(i in 1:34) { 


      ## read raw HTML
      x <- readLines(url(paste("http://www.dfb.de/bliga/bundes/archiv/", year,  
                                "/spieltage/bs1m", Round[i], ".html", sep = "")))    
  
      ## select relevant lines
      x <- x[substr(x, 1, 13) == "<tr><TD><font"]
      ## split on font information
      x <- strsplit(x, "face=\"Arial,helvetica,sans-serif\">")                                                                                            
      ## extract relevant information
      x <- t(sapply(x, game_info))
      ## collect results
      y[[i]] <- cbind(x, as.numeric(Round[i]), as.vector(year))
      closeAllConnections()
      
        ## merge suitably with previous rounds...
              z<-y[[1]]                              #hänge an die erste Runde
                    for(i in 1:33)                   #die restlichen 33 Runden
                          {                          #per RBIND dran
                            z<-rbind(z,y[[i+1]])
                            z
                          }
      }    
  w[[j]]<-z                  #schreibe das Jahr in die Jahresliste
    }


######################################################################################################
##                                    1991 20 Teams und 38 Runden
######################################################################################################

y<-as.list(1:38)               #38 Runden (Liste)

   for(i in 1:38) { 

      ## read raw HTML
      x <- readLines(url(paste("http://www.dfb.de/bliga/bundes/archiv/1991/spieltage/bs1m", 
      Round[i], ".html", sep = "")))    
  
      ## select relevant lines
      x <- x[substr(x, 1, 13) == "<tr><TD><font"]
      ## split on font information
      x <- strsplit(x, "face=\"Arial,helvetica,sans-serif\">")                                                                                            
      ## extract relevant information
      x <- t(sapply(x, game_info))
      ## collect results
      y[[i]] <- cbind(x, as.numeric(Round[i]), as.vector("1991"))
      closeAllConnections()
      
        ## merge suitably with previous rounds...
              z<-y[[1]]                              #hänge an die erste Runde
                    for(i in 1:37)                   #die restlichen 37 Runden
                          {                          #per RBIND dran
                            z<-rbind(z,y[[i+1]])
                            z
                          }
      }    
  w[[29]]<-z                  #schreibe das Jahr in die Jahresliste
    
    
    
    
######################################################################################################
##                                    1992-1996
######################################################################################################

y<-as.list(1:34)                #34 Runden (Liste) 
 
for(j in 30:34){
   year<-Year[j]
   for(i in 1:34) { 

      ## read raw HTML
      x <- readLines(url(paste("http://www.dfb.de/bliga/bundes/archiv/", year,  
                                "/spieltage/bs1m", Round[i], ".html", sep = "")))    
  
      ## select relevant lines
      x <- x[substr(x, 1, 13) == "<tr><TD><font"]
      ## split on font information
      x <- strsplit(x, "face=\"Arial,helvetica,sans-serif\">")                                                                                            
      ## extract relevant information
      x <- t(sapply(x, game_info))
      ## collect results
      y[[i]] <- cbind(x, as.numeric(Round[i]), as.vector(year))
      closeAllConnections()
      
        ## merge suitably with previous rounds...
              z<-y[[1]]                              #hänge an die erste Runde
                    for(i in 1:33)                   #die restlichen 33 Runden
                          {                          #per RBIND dran
                            z<-rbind(z,y[[i+1]])
                            z
                          }
      }    
  w[[j]]<-z                  #schreibe das Jahr in die Jahresliste
    }    

######################################################################################################
##                                 1997 2003
######################################################################################################   
              
## neues Zeitformat in der Datenbank.. ansonsten gleich wie der grosse block
                
play_time <- function(x){                                                                                                                                                         
          strsplit(x[2], "<\\TD>")                                                                                                                                     
          ptime <- strptime(strsplit(x[2], "</TD>")[[1]][1], format = "%d.%m.%Y, %H.%M")
          #ptime <- as.POSIXct(ptime)
          #ptime
           }                                 

y<-as.list(1:34)        # 34 Runden (Liste)
 
for(j in 35:41){
   year<-Year[j]
   for(i in 1:34) { 

      ## read raw HTML
      x <- readLines(url(paste("http://www.dfb.de/bliga/bundes/archiv/", year,  
                                "/spieltage/bs1m", Round[i], ".html", sep = "")))    
  
      ## select relevant lines
      x <- x[substr(x, 1, 13) == "<tr><TD><font"]
      ## split on font information
      x <- strsplit(x, "face=\"Arial,helvetica,sans-serif\">")                                                                                            
      ## extract relevant information
      x <- t(sapply(x, game_info))
      ## collect results
      y[[i]] <- cbind(x, as.numeric(Round[i]), as.vector(year))
      closeAllConnections()
      
        ## merge suitably with previous rounds...
        z<-y[[1]]                                          #hänge an die erste Runde  
            for(i in 1:33)                                 #die restlichen 33 Runden  
            {                                              #per RBIND dran            
            z<-rbind(z,y[[i+1]])
            z
            }

      }                                                      #schreibe das Jahr in die Jahresliste
    w[[j]]<-z
    }

######################################################################################################
##                                 2004 bis 2005
######################################################################################################   
              
## neues Datenbank-/Url- format

home_team <- function(x) strsplit(x[3], " - ")[[1]][1]
away_team <- function(x) strsplit(x[4], "</td>")[[1]][1]
home_goals <- function(x) {
                      strsplit(x[5], ":")[[1]][1]
                          }

away_goals <- function(x) {
            x<- strsplit(x[5], ":")[[1]][2]
            strsplit(x, " ")[[1]][1]
                          }

      ## extract time                                                                      
play_time <- function(x){     
     strptime(strsplit(x[2], "</td>")[[1]][1], format = "%d.%m.%Y, %H.%M")
                         }              

for(j in 42:43){
      year<-Year[j]
      ses<-Ses[j]
   for(i in 1:34) { 

      ## read raw HTML
      x <- readLines(url(paste("http://www.dfb.de/bliga/spiel.php?liga=bl1m&spieltag=", Round[i],  
                                "&art=e&saison=", ses ,"&saisonl=",year, "&lang=D", sep = "")))    
  
      ## select relevant lines    
      x1 <- x[substr(x, 1, 14) == "<table BORDER="]
      x <- x[substr(x, 1, 12) == "<tr bgcolor="]
      ## split on font information
      x <- strsplit(x, "face='Arial,helvetica,sans-serif'>")
        x1 <- gsub("Spieltag</b></font></td></tr><tr bgcolor=''><td width='25%'><font size='2'", "", x1)
        x1 <- gsub("face='Arial,helvetica,sans-serif'><b>", "", x1)
      x1<- strsplit(x1, "face='Arial,helvetica,sans-serif'>")                                                   
      ## extract relevant information
      x <- t(sapply(x, game_info))
      x1 <- t(sapply(x1, game_info))
      x <- rbind(x1,x)
      ## collect results
      y[[i]] <- cbind(x, as.numeric(Round[i]), as.vector(year))
      closeAllConnections()
      
        ## merge suitably with previous rounds...
        z<-y[[1]]                                   #hänge an die erste Runde 
            for(i in 1:33)                          #die restlichen 33 Runden 
            {                                       #per RBIND dran           
            z<-rbind(z,y[[i+1]])
            z
            }

      }    
    w[[j]]<-z
    }
   

    ## merge years

  v<-w[[1]]
   for(i in 1:42)                               #hänge an das erste Jahr 
   {                                            #die restlichen 42 Jahre 
   v<-rbind(v,w[[i+1]])                         #per RBIND dran           
   v
   }
  
  ## Schoenheitsfehler bereinigen

v[v == "Borussia Moenchengladbach </TD><TD><font size=\"2\" "] <- "Borussia Moenchengladbach" 
v[v == "Hannover 1896"] <- "Hannover 96" 
v[v == "TSV Muenchen 1860"] <- "TSV 1860 Muenchen" 
v[v == "1860 Muenchen"] <- "TSV 1860 Muenchen"   
v[v == "1860 Muenchen"] <- "TSV 1860 Muenchen"
v[v == "1. FC Köln"] <- "1. FC Koeln"  
v[v == "Bayern München"] <- "Bayern Muenchen"
v[v == "1. FC Nürnberg"] <- "1. FC Nuernberg"
v[v == "Borussia Mönchengladbach"] <- "Borussia Moenchengladbach"

v[v == "2*</TD><td><fontsize=\"2\""] <- "2"
v[v == "0*</TD><td><fontsize=\"2\""] <- "0"

 ## Finishing

BL<- as.data.frame(v)
colnames(BL)<-c("HomeTeam","AwayTeam","HomeGoals","AwayGoals","PlayTime","Round","Year")

  ## Finishing additions

Bundesliga <- BL
Bundesliga$PlayTime <- NULL
Bundesliga$Date <- as.POSIXct(BL$PlayTime)
Bundesliga$Year <- as.integer(as.character(Bundesliga$Year))
Bundesliga$Round <- as.integer(as.character(Bundesliga$Round))
Bundesliga$AwayGoals <- as.integer(as.character(Bundesliga$AwayGoals))
Bundesliga$HomeGoals <- as.integer(as.character(Bundesliga$HomeGoals))
}
