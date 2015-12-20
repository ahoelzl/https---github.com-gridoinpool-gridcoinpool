if(!require(RCurl)){install.packages("RCurl", dependencies=T,repos='http://cran.us.r-project.org')}
if(!require(RSelenium)){install.packages("RSelenium", dependencies=T,repos='http://cran.us.r-project.org')}
if(!require(XML)){install.packages("XML", dependencies=T,repos='http://cran.us.r-project.org')}
if(!require(RMySQL)){install.packages("RMySQL", dependencies=T,repos='http://cran.us.r-project.org')}

library(RCurl)
library(RSelenium)
library(XML)
library(RMySQL)
RSelenium::checkForServer()

bamname <- "bamemail"
bampwd <- "bampwd" 
user <- "user"
dbname <- "dbname"
host <- "host"
password <- "password"
gridcoinpwd <- "gridcoinpwd"


remDr <- remoteDriver(browserName = "phantomjs")
#remDr <- remoteDriver(browserName = "firefox")

remDr$open()
remDr$navigate("http://boincstats.com/de")
remDr$setAsyncScriptTimeout(milliseconds = 12000) 
remDr$setImplicitWaitTimeout(milliseconds = 12000) 
remDr$setTimeout(type = "page load", milliseconds = 12000)
webElem <- remDr$findElement(using = 'id', value = "user_name")
webElem$sendKeysToElement(list(bamname))


webElem <- remDr$findElement(using = 'id', value = "password")

webElem$clearElement()

webElem$sendKeysToElement(list(bampwd))

webElem <- remDr$findElement(using = 'name', value="login")

webElem$sendKeysToElement(list("R Cran", key = "enter"))

remDr$navigate("http://boincstats.com/de/bam/hosts/")

webElem <- remDr$findElement(using = 'id', value = "toggleRetiredT")

webElem$sendKeysToElement(list("R Cran", key = "enter"))

doc <- htmlParse(remDr$getPageSource()[[1]])

table.crawled <- readHTMLTable(doc)

print(table.crawled)


links <- xpathSApply(doc, "//a/@href")
gotos <- paste0("boincstats.com",links[grep("host/detail", links)])


remDr$navigate("http://boincstats.com/de/stats/-1/team/detail/118094994/projectList")
doc <- htmlParse(remDr$getPageSource()[[1]])
table <- readHTMLTable(doc, header=F)

mydb = dbConnect(MySQL(), user=user, password=password, dbname=dbname, host=host, overwrite=T)
dbListTables(mydb)

rs = dbSendQuery(mydb, "select * from teamstats")
data.team = fetch(rs, n=-1)

projectlistReference <- list("123Numbers"=0,"Asteroids@home"=0,"BOINC kombiniert" = 0, "ATLAS@Home"=0, "Bitcoin Utopia"=0,
                             "BURP"=0, "CAS@HOME"=0,"Citizen Science Grid"=0,
                             "Climate Prediction"=0, "Collatz Conjecture"=0,
                             "Cosmology@Home"=0,"DENIS@Home"=0, "DistributedDataMining"=0,
                             "Einstein@Home"=0,"Enigma@Home"=0,"FiND@Home"=0, "Gridcoin Finance"=0,
                             "GPUGRID"=0, "Leiden Classical"=0, "LHC@Home Classic"=0,
                             "Malaria Control"=0,"MilkyWay@home"=0, "MindModeling@Home"=0,
                             "Moo! Wrapper"=0, "NFS@Home"=0, "NumberFields@home"=0,
                             "POEM@HOME"=0, "PrimeGrid"=0, "Rosetta@Home"=0,
                             "SAT@home" = 0, "SETI@Home"=0, "SZTAKI Desktop Grid"=0,
                             "theSkyNet POGS"=0, "Universe@Home"=0,"vLHCathome"=0,
                             "World Community Grid"=0,"WUProp@Home"=0, "YAFU"=0,
                             "yoyo@home"=0)

projectlist <- list("123Numbers"=0,"Asteroids@home"=0,"BOINC kombiniert" = 0, "ATLAS@Home"=0, "Bitcoin Utopia"=0,
                    "BURP"=0, "CAS@HOME"=0,"Citizen Science Grid"=0,
                    "Climate Prediction"=0, "Collatz Conjecture"=0,
                    "Cosmology@Home"=0,"DENIS@Home"=0, "DistributedDataMining"=0,
                    "Einstein@Home"=0,"Enigma@Home"=0,"FiND@Home"=0, "Gridcoin Finance"=0,
                    "GPUGRID"=0, "Leiden Classical"=0, "LHC@Home Classic"=0,
                    "Malaria Control"=0,"MilkyWay@home"=0, "MindModeling@Home"=0,
                    "Moo! Wrapper"=0, "NFS@Home"=0, "NumberFields@home"=0,
                    "POEM@HOME"=0, "PrimeGrid"=0, "Rosetta@Home"=0,
                    "SAT@home" = 0, "SETI@Home"=0, "SZTAKI Desktop Grid"=0,
                    "theSkyNet POGS"=0, "Universe@Home"=0,"vLHCathome"=0,
                    "World Community Grid"=0,"WUProp@Home"=0, "YAFU"=0,
                    "yoyo@home"=0)

totalsum <- 0
for(i in 1:dim(table$tblStats)[1]) {
  if(table$tblStats[i,1] %in% names(projectlistReference )) {
    indices <- table$tblStats[i,1] == names(projectlist)
    number <-gsub(",", "",  as.character(table$tblStats[i,4]))
    data.team[which(indices)] <- max(as.numeric(number),1)
    value <- as.numeric(number)
    
    if(is.na(value)) {
      value <- 1
    }
    #absurdly small team rac numbers are probably wrong, replace them with very large number
    #to give very little influence to these numbers
    if(value < 100) {
      value <- 10000000000
    }
    totalsum <- totalsum + value
    projectlistReference[ as.character(table$tblStats[i,1])] <- value
  }
}

if(dim(table$tblStats)[1] < 2) {
  projectlistReference["BOINC kombiniert"] <- totalsum
  dbWriteTable(mydb, name='teamstats', value=data.team, overwrite=TRUE, row.names=FALSE)
} else {
  rs = dbSendQuery(mydb, "select * from teamstats")
  data.team = fetch(rs, n=-1)
  
  for(z in 1:dim(data.team)[1]) {
    projectlistReference[names(data.team)[z]] <- data.team[z]
  }
  
  projectlistReference["BOINC kombiniert"] <- sum(data.team)
  
}


getUserWorktable <- function(g) {
  
  remDr$navigate(paste0("http://",g))
  
  doc <- htmlParse(remDr$getPageSource()[[1]])
  links2 <-  xpathSApply(doc, "//a/@href")
  
  link <- paste0("boincstats.com",links2[grep("projectList", links2)])[1]
  
  
  projectlist <- list("123Numbers"=0,"Asteroids@home"=0,"BOINC kombiniert" = 0, "ATLAS@Home"=0, "Bitcoin Utopia"=0,
                      "BURP"=0, "CAS@HOME"=0,"Citizen Science Grid"=0,
                      "Climate Prediction"=0, "Collatz Conjecture"=0,
                      "Cosmology@Home"=0,"DENIS@Home"=0, "DistributedDataMining"=0,
                      "Einstein@Home"=0,"Enigma@Home"=0,"FiND@Home"=0, "Gridcoin Finance"=0,
                      "GPUGRID"=0, "Leiden Classical"=0, "LHC@Home Classic"=0,
                      "Malaria Control"=0,"MilkyWay@home"=0, "MindModeling@Home"=0,
                      "Moo! Wrapper"=0, "NFS@Home"=0, "NumberFields@home"=0,
                      "POEM@HOME"=0, "PrimeGrid"=0, "Rosetta@Home"=0,
                      "SAT@home" = 0, "SETI@Home"=0, "SZTAKI Desktop Grid"=0,
                      "theSkyNet POGS"=0, "Universe@Home"=0,"vLHCathome"=0,
                      "World Community Grid"=0,"WUProp@Home"=0, "YAFU"=0,
                      "yoyo@home"=0)
  
  if(length(links2[grep("projectList", links2)]) == 0) {
    return(list("sum"=0, "link"=link, "projectlist"=unlist(projectlist)))
  }
  remDr$navigate(paste0("http://",link))
  doc <- htmlParse(remDr$getPageSource()[[1]])
  table <- readHTMLTable(doc)
  
  # Sys.sleep( runif(1) * 10)
  
  if(is.null(table[[1]])) {
    return(0)
  }
  
  sum <- 0
  for(j in 1:dim(table[[1]])[1]) {
    if(table[[1]][j,1] %in% names(projectlist)) {
      cat(j, "-", number, "  ")
      indices <- table[[1]][j,1] == names(projectlist)
      
      number <-gsub(",", "",  as.character(table[[1]][j,4]))
      print(paste0("name :", table[[1]][j,1], "  ", as.numeric(number), "  divided by ", projectlistReference[ as.character(table[[1]][j,1])], "\n"))
      add <-  as.numeric(number)/  as.numeric(projectlistReference[ as.character(table[[1]][j,1])])
      add <- min(add, 0.05)
      projectlist[which(indices)] <- add
      sum <- sum + add 
      
      
    } 
  }
  
  rlist <- list("sum"=sum, "link"=link, "projectlist"=unlist(projectlist))
  rlist
  
} 


mydb = dbConnect(MySQL(), user=user, password=password, dbname=dbname, host=host, overwrite=T)
dbListTables(mydb)

rs = dbSendQuery(mydb, "select * from hosts")
data = fetch(rs, n=-1)

print(data)
data[is.na(data[,"bamid"]), "bamid"] <- 0
data[is.na(data[,"name"]), "name"]<- 0
table.crawled
write( as.character(Sys.Date()),file="errs.txt",append=F)
for(i in 1:dim(table.crawled$tblHosts)[1]) {
  print(i)
  
  same <- (table.crawled$tblHosts[i,1] == data[,"bamid"])
  if(sum(same) == 1) {
    tryCatch( {
      workDone <- getUserWorktable(gotos[i])
      data[which(same),which(colnames(data) == "123Numbers")
           :which(colnames(data) == "yoyo@home")] <- workDone$projectlist
      data[which(same),"research"] <- workDone$sum
      data[which(same),"link"] <- workDone$link
    } , error= function(e){
      data[which(same),which(colnames(data) == "123Numbers")
           :which(colnames(data) == "yoyo@home")] <- rep(0, length(projectlist))
      data[which(same),"research"] <-0
      data[which(same),"link"] <- "timeout"
    })
    
    
  } else if(sum(same)==0) {
    names <- c()
    for(r in 1:length(data[,"name"])) {
      names[r] <- gsub(" ", "", data[r,"name"])
    }
    
    same2 <- (table.crawled$tblHosts[i,2] == names & data[,"bamid"] == 0)
    if(sum(same2) == 0) {
      
      
      write(paste0(" found ", sum(same2) , " times"),file="errs.txt", append=TRUE)
      write(as.character(table.crawled$tblHosts[i,2]),file="errs.txt",append=TRUE)
      write(i,file="errs.txt",append=TRUE)
      write("--------------------------------",file="errs.txt",append=TRUE)
    } else {
      print(paste0("user: ",  data[which(same),"name"], " "))
      workDone <- getUserWorktable(gotos[i])
      data[which(same),which(colnames(data) == "123Numbers")
           :which(colnames(data) == "yoyo@home")] <- workDone$projectlist
      data[which(same2),"bamid"] <- as.numeric(as.character(table.crawled$tblHosts[i,1]))
      data[which(same2),"research"] <- workDone$sum
      data[which(same2),"link"] <- workDone$link
      
    }
  }
}



data[is.na(data[,"research"]), "research"] <- 0
sum <- sum(data[,"research"])
data[,"research"]  <- (data[,"research"]/sum)*0.96
write("--------------------------------",file="log-research-amount.txt",append=TRUE)
write( as.character(Sys.Date()),file="log-research-amount.txt",append=TRUE)
write.table(data,file="log-research-amount.txt",append=TRUE)
write("--------------------------------",file="log-research-amount.txt",append=TRUE)

remDr$close()

print("write db")

dbWriteTable(mydb, name='hosts', value=data, overwrite=TRUE, row.names=FALSE)


system("gridcoinresearchd walletlock" )
system(paste0("gridcoinresearchd walletpassphrase 
              ", gridcoinpwd, " 1000000000 true" )


print("between")

mydb = dbConnect(MySQL(), user=user, password=password, dbname=dbname, host=host, overwrite=T)
rs = dbSendQuery(mydb, "select * from hosts")
data = fetch(rs, n=-1)

system("gridcoinresearchd walletlock" )









print("test0")
mydb = dbConnect(MySQL(), user=user, password=password, dbname=dbname, host=host, overwrite=T)
rs = dbSendQuery(mydb, "select * from hosts")
data = fetch(rs, n=-1)

print("test1")
ret <- system2("gridcoinresearchd","getinfo", stdout=TRUE, stderr=TRUE)
stake <- as.numeric(substring(ret[8], 15, 20))
CurrentValue <- as.numeric(system("gridcoinresearchd getbalance", intern=T)) + stake
print(paste0("current value: ", CurrentValue))
#SHGFAeQsffEmz7LPx99DoZuQziokwQaUZX

sendout <- data[,"research"]

ConstantValue <- 100000


send <- CurrentValue - ConstantValue
print(paste0("current value: ", send))
data[is.na(data[,"research"]),"research"] <- 0
sum <- sum(data[,"research"])
data[,"research"]  <- (data[,"research"]/sum)*0.96
print(data[,"research"])
sent <- 0
for(i in 1:dim(data)[1]) {
  tosend <- send * data[i,"research"]
  tosend <- min(tosend, 100)
  order(tosend, decreasing=T)
  
  print(paste0("send ", tosend , " to ",  data[i,"grc_address"]))
  sent <- sent + send * data[i,"research"]
}



write("--------------------------------",file="log-payout.txt",append=TRUE)
write( as.character(Sys.Date()),file="log-payout.txt",append=TRUE)
sent <- 0
print("start")
system(paste0("gridcoinresearchd walletpassphrase 
              ", gridcoinpwd, " 100" ))
for(i in 1:dim(data)[1]) {
  
  tosend <- send * data[i,"research"]
  if(tosend > 0) {
    tosend <- min(tosend, 100)
    write(paste0("send ", tosend , " to ",  data[i,"grc_address"]), ,file="log-payout.txt",append=TRUE)
    print(paste0("send ", tosend , " to ",  data[i,"grc_address"]), ,file="log-payout.txt",append=TRUE)
    
    sent <- sent + tosend
    
    system(paste0("gridcoinresearchd sendtoaddress ",data[i,"grc_address"]," ",tosend ))
    
    data[i,"lastPaymentDate"] <- as.character(format(Sys.time(), "%a %b %d %X %Y"))
    data[i,"lastPaymentAmount"] <- tosend
  }
}
write("--------------------------------",file="log-payout.txt",append=TRUE)

print("end")

bonus <- send - sent
system("gridcoinresearchd walletlock" )
system(paste0("gridcoinresearchd walletpassphrase 
              ", gridcoinpwd, " 10" ))
system(paste0("gridcoinresearchd sendtoaddress SAQgxoeYS1Vmo6aHdYEsJaQKqYUehcvwwF ",bonus/4))
system(paste0("gridcoinresearchd sendtoaddress SCv4tCRAz9QrLkmSe4ss26pS8p2LvBUWpK ",bonus/4))
system(paste0("gridcoinresearchd sendtoaddress S8Qdsfbtk4UydgdKWuZidy5HMEqUBMzx7p ",bonus/4))
system(paste0("gridcoinresearchd sendtoaddress SLwrMpdmuZAA8ttdD4AbBez8aMycWJeLzX ",bonus/4))
print("bonus sent")
system("gridcoinresearchd walletlock" )
system(paste0("gridcoinresearchd walletpassphrase 
              ", gridcoinpwd, " 1000000000 true" )
dbWriteTable(mydb, name='hosts', value=data, overwrite=TRUE, row.names=FALSE)

