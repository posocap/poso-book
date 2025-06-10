
#' ID list for merging PSID
#'
#' @description this list is taken from http://ideas.repec.org/c/boc/bocode/s457040.html
#' @details this function hardcodes the PSID variable names of "interview number" from both family and individual file for each wave, as well as "sequence number", "relation to head" and numeric value x of that variable such that "relation to head" == x means the individual is the head. Varies over time.
makeids <- function(){
  
  id.list <- data.table(year = c(1968:1997, seq(1999,2021,by = 2)))
  id.list$ind.interview <- c("ER30001","ER30020","ER30043","ER30067",
                             "ER30091","ER30117","ER30138","ER30160", 
                             "ER30188","ER30217","ER30246","ER30283",
                             "ER30313","ER30343","ER30373","ER30399",
                             "ER30429","ER30463","ER30498","ER30535",
                             "ER30570","ER30606","ER30642","ER30689",
                             "ER30733","ER30806","ER33101","ER33201",
                             "ER33301","ER33401","ER33501","ER33601",
                             "ER33701","ER33801","ER33901","ER34001",
                             "ER34101","ER34201","ER34301","ER34501",
                             "ER34701", "ER34901")
  
  id.list$ind.seq <- c(NA,"ER30021","ER30044","ER30068","ER30092","ER30118","ER30139",
                       "ER30161","ER30189","ER30218","ER30247","ER30284","ER30314", 
                       "ER30344","ER30374","ER30400","ER30430","ER30464","ER30499", 
                       "ER30536","ER30571","ER30607","ER30643","ER30690","ER30734", 
                       "ER30807","ER33102","ER33202","ER33302","ER33402","ER33502", 
                       "ER33602","ER33702","ER33802","ER33902","ER34002","ER34102",
                       "ER34202","ER34302","ER34502","ER34702", "ER34902")
  
  # name of variable "relationship to head"
  id.list$ind.head <- c("ER30003",
                        "ER30022",
                        "ER30045",
                        "ER30069",
                        "ER30093",
                        "ER30119",
                        "ER30140",
                        "ER30162",
                        "ER30190",
                        "ER30219",
                        "ER30248",
                        "ER30285",
                        "ER30315",
                        "ER30345",
                        "ER30375",
                        "ER30401",
                        "ER30431",
                        "ER30465",
                        "ER30500",
                        "ER30537",
                        "ER30572",
                        "ER30608",
                        "ER30644",
                        "ER30691",
                        "ER30735",
                        "ER30808",
                        "ER33103",
                        "ER33203",
                        "ER33303",
                        "ER33403",
                        "ER33503",
                        "ER33603",
                        "ER33703",
                        "ER33803",
                        "ER33903",
                        "ER34003",
                        "ER34103",
                        "ER34203",
                        "ER34303",
                        "ER34503",
                        "ER34703",
                        "ER34903")
  
  # numeric code for "i am the head"
  id.list$ind.head.num <- c(rep(1,15),rep(10,27))
  
  id.list$fam.interview <- c("V3"      , "V442"    , "V1102"   , "V1802"   , "V2402"    , "V3002" ,
                             "V3402"   , "V3802"   , "V4302"   , "V5202"   , "V5702"    ,
                             "V6302"   , "V6902"   , "V7502"   , "V8202"   , "V8802"    ,
                             "V10002"  , "V11102"  , "V12502"  , "V13702"  , "V14802"   ,
                             "V16302"  , "V17702"  , "V19002"  , "V20302"  , "V21602"   ,
                             "ER2002"  , "ER5002"  , "ER7002"  , "ER10002" , "ER13002"  ,
                             "ER17002" , "ER21002" , "ER25002" , "ER36002" , "ER42002"  , 
                             "ER47302" , "ER53002" , "ER60002" , "ER66002" , "ER72002"  ,
                             "ER78002")
  id.list$stratum <- rep("ER31996",nrow(id.list))
  setkey(id.list,year)
  return(id.list)
}

makeids.wealth <- function(){
  
  id = data.table(year=c(1984, 1989, 1994, 1999, 2001, 2003, 2005, 2007),interview = c("S101","S201","S301","S401","S501","S601","S701","S801"))
  setkey(id,year)
  return(id)
  
}




#' get.psid connects to PSID database and downloads into Rda
#'
#' see \url{http://asdfree.com/} for other usage and \url{https://stackoverflow.com/questions/15853204/how-to-login-and-then-download-a-file-from-aspx-web-pages-with-r}
#' @author Anthony Damico <ajdamico@@gmail.com>
#' @param file string psid file number
#' @param name string of filename on disc
#' @param params postForm{RCurl} parameters
#' @param curl postForm{RCurl} curl handle
get.psid <- function( file , name , params , curl ){
  
  html = postForm('http://simba.isr.umich.edu/u/Login.aspx', .params = params, curl = curl)
  
  if ( !grepl('Logout', html) ) stop( 'no longer logged in' )
  
  
  tf <- tempfile() ; td <- tempdir()
  
  flog.info('downloading file %s',name)
  file <- getBinaryURL( paste0( "http://simba.isr.umich.edu/Zips/GetFile.aspx?file=" , file ) , curl = curl )
  writeBin( file , tf )
  z <- unzip( tf , exdir = td )
  fn <- z[ grepl( ".txt" , tolower( z ) , fixed = TRUE ) & ! grepl( "_vdm|readme|doc|errata" , tolower( z ) ) ]
  sas_ri <- z[ grepl( '.sas' , z , fixed = TRUE ) ]
  
  flog.info('now reading and processing SAS file %s into R',name)
  
  #SAScii version check SAScii_fork
  # if (!exists("SAScii_fork",mode="function")){
  # 	warning("you may run into trouble now. There was a change of file format on some PSID family files. \n If you get the an error \n toupper(SASinput) \n
  # 		then you need to re-install the SAScii package from my github fork at \n
  # 		https://github.com/floswald/SAScii \n
  # 		an easy way to do this is to use the devtools package. then:
  # 		install_github('floswald/SAScii')")
  # }
  
  
  x <- read.SAScii( fn , sas_ri )
  
  save( x , file = paste0( name , '.rda' ) )
  
  file.remove( tf , z )
  
  rm( x )
  
  gc()
  
  TRUE
}



#' Convert factor to character
#'
#' @param x a \code{factor}
#' @return a character
#' @description helper function to convert factor to character in a data.table
make.char <- function(x){
  if (is.factor(x)){
    return(as.character(x))
  } else {
    return(x)
  }
}



#' Create a test PSID dataset
#'
#' makes artifical PSID data with variables \code{age} and \code{income}
#' for two consecutive years 1985 and 1986.
#' @param N number of people in each wave
#' @param N.attr number of people lost to attrition
#' @return list with (fake) individual index file IND2009ER and
#' family files for 1985 and 1986
#' @export
testPSID <-function(N=100,N.attr = 0){
  
  # need to bind some vars for R CHECK:
  ER30001 <- ER30002 <-intnum86 <- intnum85 <- Money85 <- Money86 <- age85 <- age86 <- smpls <- NULL
  
  stopifnot(N %% 4 == 0)
  # for sake of illustration, suppose the PSID has a total
  # of 2N people (i.e. N are neither in year1 nor year2, 
  # but in some other years)
  # draw interview numbers from realistic range: [1,9308]
  
  # in each wave, a quarter of observations is from
  # 1) core sample: interview number < 3000
  # 2) immigrant sample: interview number in [3000,5000)
  # 3) poor sample: interview number in [5000,7000)
  # 4) latino sample: interview number in [7000,9308)
  
  smpl <- ceiling(1:N / (N/4))
  IND2009ER <- data.table(smpls=c(smpl,smpl))  # get 2*N inds
  IND2009ER[, ER30001 := 0L]
  IND2009ER[smpls==1, ER30001 := sample(1:2999,size=sum(smpls==1))]
  IND2009ER[smpls==2, ER30001 := sample(3001:4999,size=sum(smpls==2))]
  IND2009ER[smpls==3, ER30001 := sample(5001:6999,size=sum(smpls==3))]
  IND2009ER[smpls==4, ER30001 := sample(7001:9308,size=sum(smpls==4))]
  
  IND2009ER[,c("intnum85","intnum86") := lapply(1:2,function(x) sample(1:(2*N)))]
  # IND2009ER[smpls==1,c("intnum85","intnum86") := lapply(1:2,function(x) sample(1:2999,size=sum(smpls==1)))]
  # IND2009ER[smpls==2,c("intnum85","intnum86") := lapply(1:2,function(x) sample(3001:4999,sum(smpls==2)))]
  # IND2009ER[smpls==3,c("intnum85","intnum86") := lapply(1:2,function(x) sample(5001:6999,sum(smpls==3)))]
  # IND2009ER[smpls==4,c("intnum85","intnum86") := lapply(1:2,function(x) sample(7001:9308,sum(smpls==4)))]
  
  # only N invidividuals show up in 1985 though.
  IND2009ER[sample(1:(2*N),size=N),c("intnum85") := 0]
  
  # and there is potential attrition from 1985 to 1986
  if (N.attr>0){
    out = sample(which(IND2009ER[["intnum85"]] != 0),size=N.attr,replace=FALSE)
    IND2009ER[out,"intnum86" := 0]
  }
  
  # add 1968 person id
  IND2009ER[,ER30002 := sample(1:(2*N))]
  
  # also need relationship to head in each year in the index
  # 50% prob of being head in year1
  IND2009ER$ER30465 <- sample(rep(c(10,20),c(N,N)),size=2*N,replace=TRUE)	
  IND2009ER$ER30500 <- sample(rep(c(10,20),c(N,N)),size=2*N,replace=TRUE)
  
  # as well as the sequence number: 1 for current heads, > 50 for movers
  # 75% prob of being current head
  IND2009ER$ER30464 <- sample(rep(c(1,20),c(ceiling(0.75*2*N),ceiling(0.5*N))),size=2*N,replace=TRUE)	
  IND2009ER$ER30499 <- sample(rep(c(1,20),c(ceiling(0.75*2*N),ceiling(0.5*N))), size=2*N,replace=TRUE)
  # and a survey weight
  IND2009ER$ER30497 <- runif(2*N)
  IND2009ER$ER30534 <- runif(2*N)
  
  # you would download files like those two data.frames:
  fam85 <- data.table(intnum85 = IND2009ER[intnum85>0,sample(intnum85,size=N)],Money85=rlnorm(n=N,10,1),age85=sample(20:80,size=N,replace=TRUE))
  fam86 <- data.table(intnum86 = IND2009ER[intnum86>0,sample(intnum86,size=sum(intnum86>0))],Money86 = rlnorm(n=IND2009ER[,sum(intnum86>0)],10,1),age86 = sample(20L:80L,size=IND2009ER[,sum(intnum86>0)],replace=TRUE))
  
  continuers85 = IND2009ER[intnum85>0 & intnum86>0][["intnum85"]]
  continuers86 = IND2009ER[intnum85>0 & intnum86>0][["intnum86"]]
  
  for (i in 1:nrow(fam86)){
    if (fam86[i,intnum86] %in% continuers86){
      int86 = fam86[i,intnum86]
      fam86[i,Money86 := fam85[intnum85==IND2009ER[intnum86==int86,intnum85], Money85 + rnorm(1,500,30)]]
      fam86[i,age86 := fam85[intnum85==IND2009ER[intnum86==int86,intnum85], age85 + 1]]
    }
  }
  
  # assign correct PSID varname of "family interview 1985/86"
  setnames(fam85,"intnum85", "V11102")
  setnames(fam86,"intnum86","V12502")
  
  # same on index file
  setnames(IND2009ER,c("intnum85","intnum86"), c("ER30463","ER30498"))
  
  return(list(famvars1985=fam85,famvars1986=fam86,IND2019ER=IND2009ER))
}

get_data <- function(datadir=NULL,
                     fam.vars,
                     ind.vars=NULL,
                     heads.only=FALSE,
                     current.heads.only=FALSE,
                     sample=NULL,
                     design="balanced",
                     loglevel=INFO,
                     user = NULL,
                     pass = NULL){
  
  flog.threshold(loglevel)
  
  # locally bind all variables to be used in a data.table
  # or R CMD CHECK complains.
  
  interview <- headyes <- .SD <- fam.interview <- ind.interview <- ind.head <- ER30001 <- ind.head.num <- pid <- ID1968 <- pernum <- isna <- present <- always <- enough <- ind.seq <- name <- variable <- NULL
  
  stopifnot(is.numeric(fam.vars$year))
  years <- fam.vars$year
  flog.debug("years:",years,capture=TRUE)
  
  # figure out platform
  s <- .Platform$file.sep
  if ( .Platform$OS.type != 'windows' ) {
    # warning("I'm setting your encoding to windows now")
    options( encoding = "windows-1252" )		# # only macintosh and *nix users need this line
  }
  
  
  # sort out data storage: either to tmp or datadir
  if (is.null(datadir)){
    datadir <- tempdir()	
    datadir <- paste0(datadir,s)
  } else {
    if (substr(datadir,nchar(datadir),nchar(datadir))!=s) datadir <- paste0(datadir,s)
  }
  flog.debug("datadir: %s",datadir)
  
  # are we processing wealth supplements? No.
  # any.wealth = is.data.frame(wealth1984.vars)
  any.wealth = FALSE
  flog.debug("any.wealth? %d",any.wealth)
  
  
  # data acquisition
  # ----------------
  lf = list.files(datadir)
  flog.debug("datadir contains: ",lf,capture=TRUE)
  
  wlth.down <- TRUE  # initiate to something
  
  # all psid family files
  family    <- data.frame(year = c( 1968:1997 , seq( 1999 , 2021 , 2 ) ),file = c( 1056 , 1058:1082 , 1047:1051 , 1040 , 1052 , 1132 , 1139 , 1152  , 1156, 1164 , 1183 , 1187 , 1194, 1205))
  # family    <- data.frame(year = c( 1968:1997 , seq( 1999 , 2013 , 2 ) ),file = c( 1056 , 1058:1082 , 1047:1051 , 1040 , 1052 , 1132 , 1139 , 1152  , 1156, 1164  ))
  
  #subset to the years we want
  family <- family[family$year %in% years, ]
  flog.debug("family df:",family,capture=TRUE)
  
  families.down <- rep(FALSE,nrow(family))
  for ( i in 1:nrow( family )) {
    if ((paste0("FAM" , family[ i , 'year' ], "ER.rda") %in% lf) | (paste0("FAM" , family[ i , 'year' ], "ER.RData") %in% lf)) {
      families.down[i] <- TRUE
      flog.info('found %s already downloaded',paste0("FAM" , family[ i , 'year' ], "ER.rda"))
    }
  }
  
  if (any.wealth){
    # if any of 1984, 1989, 1994, 1999, 2001, 2003, 2005, 2007 in years, also download the associated wealth supplement
    wlth = data.frame(year=c(1984),file=c(1175))
    flog.debug("working on wealth")
    flog.debug("wlth: ",wlth, capture=TRUE)
    
    wlth.down <- rep(FALSE,nrow(wlth))
    for ( i in 1:nrow( wlth )) {
      if ((paste0("WEALTH" , wlth[ i , 'year' ], "ER.rda") %in% lf) | (paste0("WEALTH" , wlth[ i , 'year' ], "ER.RData") %in% lf)) {
        wlth.down[i] <- TRUE
        flog.info('found %s already downloaded',paste0("WEALTH" , wlth[ i , 'year' ]))
      } else {
        flog.info('will download 1984 supplement as %s',paste0("WEALTH" , wlth[ i , 'year' ], "ER.rda"))
      }
    }
  }
  
  ind.down <- FALSE
  # check if datadir contains individual index already
  if (("IND2021ER.rda" %in% lf) | ("IND2021ER.RData" %in% lf)) {
    #download latest individual index
    ind.down = TRUE
  }
  if (all(all(families.down),ind.down,all(wlth.down))) {
    flog.info("everything already downloaded. Build dataset now")
  } else {
    flog.info("Will download missing datasets now")
    if (!all(families.down)) {
      flog.info("will download family files: %s",paste(family[!families.down,"year"],collapse=", "))
    } 
    if (!ind.down) {
      flog.info("will download latest individual index: IND2021ER")
    } 
    if (!wlth.down) {
      flog.info("will download missing wealth files.")
    }
    
    confirm <- "yes" 
    print("If you selected a lot of data this might take a while.")
    if (confirm == "yes"){
      
      ftype <- "Rdata"

      curl = getCurlHandle()
      curlSetOpt(cookiejar = 'cookies.txt', followlocation = TRUE, autoreferer = TRUE, curl = curl)
      
      html <- getURL('http://simba.isr.umich.edu/u/Login.aspx', curl = curl)
      
      viewstate <- as.character(sub('.*id="__VIEWSTATE" value="([0-9a-zA-Z+/=]*).*', '\\1', html))
      
      # extract the `eventvalidation` string
      eventvalidation <- 
        as.character(
          sub(
            '.*id="__EVENTVALIDATION" value="([0-9a-zA-Z+/=]*).*' , 
            '\\1' , 
            html
          )
        )
      
      # construct a list full of parameters to pass to the umich website
      params <- 
        list(
          'ctl00$ContentPlaceHolder1$Login1$UserName'    = user ,
          'ctl00$ContentPlaceHolder1$Login1$Password'    = pass ,
          'ctl00$ContentPlaceHolder1$Login1$LoginButton' = 'Log In' ,
          '__VIEWSTATE'                                  = viewstate ,
          '__EVENTVALIDATION'                            = eventvalidation
        )
      
      #file number 1053 is always the individual cross year index file
      #it must always be the last file in this list.
      #you always want to download that.

      for ( i in 1:nrow( family )) {
        if (!(paste0("FAM" , family[ i , 'year' ], "ER.rda") %in% lf)) {
          get.psid( family[ i , 'file' ] ,name= paste0(datadir, "FAM" , family[ i , 'year' ], "ER") , params , curl )
        }
      }
      if (any.wealth){
        for ( i in 1:nrow(wlth)){
          if (!(wlth.down[i])){
            get.psid( wlth[ i , 'file' ] ,name= paste0(datadir, "WEALTH" , wlth[ i , 'year' ], "ER") , params , curl )
          }
        }
      }
      
      # check if datadir contains individual index already
      if (!("IND2021ER.rda" %in% lf)) {
        #download latest individual index
        get.psid( 1053 ,name= paste0(datadir, "IND2021ER") , params , curl )
      }
      
      flog.info('finished downloading files to %s', datadir)
      flog.info('continuing now to build the dataset')
      
    } else if (confirm=="no") {
      return(0)
    }
  }  # end download data
  
  l <- list.files(datadir)
  if (length(l)==0) stop('there is something wrong with the data directory. please check path')
  for (i in 1:length(l)) {
    if (tail(strsplit(l[i],"\\.")[[1]],1) == "rda") {ftype   <- "Rdata"; break}
    if (tail(strsplit(l[i],"\\.")[[1]],1) == "RData") {ftype <- "Rdata"; break}
  }
  if (!(exists("ftype"))) stop("No .rda or .RData files found in directory\n you need to download data first.")
  
  flog.info('psidR: Loading Family data from .rda files')
  # familiy data downloaded directly into a dataframe
  fam.dat  <- paste0(datadir,grep("FAM",l,value=TRUE,ignore.case=TRUE))
  fam.dat  <- grep(paste(years,collapse="|"),fam.dat,value=TRUE)
  
  # wealth data downloaded directly into a dataframe
  if (any.wealth){
    wlth.dat  <- paste0(datadir,grep("WEALTH",l,value=TRUE,ignore.case=TRUE))
    wlth.dat  <- grep(paste(years,collapse="|"),wlth.dat,value=TRUE)
  }
  
  # individual index
  tmp <- grep("IND",l,value=TRUE,ignore.case=TRUE)
  if (length(tmp)>1) {
    flog.warn("Warning: you have more than one IND file in your datadir. I take the last one:",tail(tmp,1))
    ind.file <- paste0(datadir,tail(tmp,1))	# needs to be updated with next data delivery.
  } else {
    ind.file <- paste0(datadir,grep("IND",l,value=TRUE,ignore.case=TRUE))	# needs to be updated with next data delivery.
  }
  tmp.env  <- new.env()

  
  #### Added to fix the fatal error when loading the file
  # Note: This was not necessary when the code was originally written in 2023.
  #       It's likely an issue with my current device. 
  tryCatch({
    ind <- readRDS("data/PSID/person_level_data_set.RDS") # load the individual index file
    
  }, error = function(e) {
    flog.error("Error loading file: ", e$message)
  }) 
  
  #### Original:
  #load(file=ind.file,envir=tmp.env, verbose = T) # this is the original... used to work... likely a memory issue on my machine
  #ind      <- get(ls(tmp.env),tmp.env)	# assign loaded dataset a new name
  #setnames(ind,names(ind), sapply(names(ind), toupper))	## convert all column names to uppercase
  #ind      <- data.table(ind)
  
  
  flog.info('psidR: loaded individual file: %s',ind.file)
  vvs = ceiling(object.size(ind)/1024^2)
  flog.info("psidR: total memory load in MB: %d",as.numeric(vvs))
  
  # output data.tables
  datas <- vector("list",length(years))
  
  # make an index of interview numbers for each year
  ids <- makeids()
  w_ids <- makeids.wealth()
  
  flog.debug('psidR: here is the list of hardcoded PSID variables')
  flog.debug('psidR: The merge is based on equal values in ind.interview and fam.interview')
  flog.debug("ids:",ids,capture=TRUE)
  
  # convert fam.vars to data.table
  stopifnot(is.data.frame(fam.vars))
  fam.vars <- data.table(fam.vars)
  fam.vars <- copy(fam.vars[,lapply(.SD,make.char)])
  setkey(fam.vars,year)
  
  # convert ind.vars to data.table if not null
  if (!is.null(ind.vars)){
    stopifnot(is.data.frame(ind.vars))
    ind.vars <- data.table(ind.vars)
    ind.vars <- copy(ind.vars[,lapply(.SD,make.char)])
    setkey(ind.vars,year)
  }
  # convert ind.vars to data.table if not null
  if (any.wealth){
    stopifnot(is.data.frame(wealth1984.vars))
    wealth1984.vars <- data.table(wealth1984.vars)
    wealth1984.vars <- copy(wealth1984.vars[,lapply(.SD,make.char)])
    setkey(wealth1984.vars,year)
    wealth1984.vars[,interview := w_ids[wealth1984.vars][,interview]]
  }
  
  # which vars to keep from ind.files?
  if (!is.null(ind.vars))	stopifnot(is.list(ind.vars))
  
  # add compulsory vars to fam.vars
  fam.vars[,interview := ids[fam.vars][,fam.interview]]
  
  # loop over years
  for (iy in 1:length(years)){
    
    flog.info('psidR: currently working on data for year %d',years[iy])
    
    
    # keeping only relevant columns from individual file
    # subset only if requested.
    curr <- ids[list(years[iy])]
    if (years[iy] == 1968){
      # there is no sequence variable
      ind.subsetter <- as.character(curr[,list(ind.interview,ind.head)])	# keep from ind file
    } else {
      ind.subsetter <- as.character(curr[,list(ind.interview,ind.seq,ind.head)])	# keep from ind file
    }
    def.subsetter <- c("ER30001","ER30002")	# must keep those in all years
    
    
    # select required variables from ind 
    # ----------------------------------
    # the default column order is: 
    # "ER30001","ER30002", "current year interview var", "current year sequence number", "current year head indicator"
    # get a character vector from ind.vars with variable names for this year
    ind.nas <- NULL
    if (!is.null(ind.vars)){
      ind.vars.yr <- ind.vars[list(years[iy]),which(names(ind.vars)!="year"),with=FALSE]
      flog.debug("ind.vars.yr:",ind.vars.yr,capture=TRUE)
      
      # issue https://github.com/floswald/psidR/issues/4
      # ------------------------------------------------
      # check for NA in ind.vars: these are years when a certain variable isn not available in the individual index file.
      # adjust for first year (1968) when `sequence` was not available
      ind.notnas <- NULL
      if (any(is.na(ind.vars.yr))){
        ind.nas <- ind.vars.yr[,which(is.na(ind.vars.yr)),with=FALSE]
        flog.debug("ind.nas:",ind.nas,capture=TRUE)
      }
      if (any(!is.na(ind.vars.yr))){
        ind.notnas <- ind.vars.yr[,which(!is.na(ind.vars.yr)),with=FALSE]
        flog.debug("ind.notnas:",ind.notnas,capture=TRUE)
        # browser()
        yind    <- copy(ind[,c(def.subsetter,c(ind.subsetter,as.character(ind.notnas))),with=FALSE])	
      } else {
        yind    <- copy(ind[,c(def.subsetter,c(ind.subsetter)),with=FALSE])	
      }
      # add NA columns
      if (!is.null(ind.nas)){
        yind[,(names(ind.nas)) := NA]
      }
    } else {
      yind <- copy(ind[,c(def.subsetter,c(ind.subsetter)),with=FALSE])	
    }
    
    if (years[iy]==1968){
      yind[,sequence := NA]
      setcolorder(yind,c(1:3,ncol(yind),4:(ncol(yind)-1)))
    }
    
    
    # sample selection
    # ----------------
    
    # based on: https://psidonline.isr.umich.edu/Guide/FAQ.aspx?Type=ALL#250
    
    if (!is.null(sample)){
      
      if (sample == "SRC"){
        n    <- nrow(yind)
        yind <- copy(yind[ER30001<3000])	# individuals 1-2999 are from SRC sample
        
        flog.info('full %d sample has %d obs',years[iy],n)
        flog.info('you selected %d obs belonging to %s',nrow(yind),sample)
        
      } else if (sample == "SEO"){
        n    <- nrow(yind)
        yind <- copy(yind[ER30001<7000 & ER30001>5000])
        
        flog.info('full %d sample has %d obs',years[iy],n)
        flog.info('you selected %d obs belonging to %s',nrow(yind),sample)
        
      } else if (sample == "immigrant"){
        n    <- nrow(yind)
        yind <- copy(yind[ER30001<5000 & ER30001>3000])	# individuals 1-2999 are from SRC sample
        flog.info('full %d sample has %d obs',years[iy],n)
        flog.info('you selected %d obs belonging to %s',nrow(yind),sample)
      } else if (sample == "latino"){
        n    <- nrow(yind)
        yind <- copy(yind[ER30001<9309 & ER30001>7000])	# individuals 1-2999 are from SRC sample
        flog.info('full %d sample has %d obs',years[iy],n)
        flog.info('you selected %d obs belonging to %s',nrow(yind),sample)
      }
    }
    
    # current heads only selection
    # --------------------
    
    # https://github.com/floswald/psidR/issues/2
    # for current heads only need to subset "relationship to head" AS WELL AS "sequence number" == 1 
    # otherwise a head who died between last and this wave is still head, so there would be two heads in that family.
    # https://psidonline.isr.umich.edu/Guide/FAQ.aspx?Type=ALL#150
    if (current.heads.only) {
      n    <- nrow(yind)
      if (years[iy]==1968){
        yind <- yind[,headyes := (yind[,curr[,ind.head],with=FALSE]==curr[,ind.head.num])]
      } else {
        yind <- yind[,headyes := (yind[,curr[,ind.head],with=FALSE]==curr[,ind.head.num]) & (yind[,curr[,ind.seq],with=FALSE]== 1)]
      }
      yind <- copy(yind[headyes==TRUE])
      flog.info('dropping non-current-heads leaves %d obs',nrow(yind))
      yind[,headyes := NULL]
    } else if (heads.only){
      # https://psidonline.isr.umich.edu/Guide/FAQ.aspx?Type=ALL#250
      # To create a single year Head/Wife file: 
      # Select individuals with Relationship to Head of "Head" (a code value of 1 for 1968-1982; code 10 from 1983 onward) 
      # and with values for Sequence Number in the range 1-20. 
      n    <- nrow(yind)
      if (years[iy]==1968){
        yind <- yind[,headyes := (yind[,curr[,ind.head],with=FALSE]==curr[,ind.head.num])]
      } else {
        yind <- yind[,headyes := (yind[,curr[,ind.head],with=FALSE]==curr[,ind.head.num]) & ((yind[,curr[,ind.seq],with=FALSE]> 0) & (yind[,curr[,ind.seq],with=FALSE]< 21))]
      }
      yind <- copy(yind[headyes==TRUE])
      
      flog.info('dropping non-heads leaves %d obs',nrow(yind))
      
      yind[,headyes := NULL]
      
    }
    
    
    
    if (!is.null(ind.nas)){
      setnames(yind,c("ID1968","pernum","interview","sequence","relation.head",
                      (names(ind.vars)[-1])[-which(names(ind.vars)[-1] %in% names(ind.nas))],
                      names(ind.nas)))  
    } else {
      setnames(yind,c("ID1968","pernum","interview","sequence","relation.head",
                      (names(ind.vars)[-1])))
    }
    
    yind[,pid := ID1968*1000 + pernum]	# unique person identifier
    setkey(yind,interview)
    
    
    # bring in family files, subset them
    # ==================================
    
    # load data for current year, make data dictionary for subsets and save data as data.table
    rm(list=ls(envir=tmp.env),envir=tmp.env)
    load(file=fam.dat[iy],envir=tmp.env)
    tmp             <- get(ls(tmp.env),tmp.env)	# assign loaded dataset a new name
    tmp             <- data.table(tmp)
    
    vs = ceiling(object.size(tmp)/1024^2)
    flog.debug('loaded family file: ',fam.dat[iy])
    flog.debug('current memory load in MB: %d',vs)
    
    
    
    # convert all variable names to lower case in both fam.vars and data file
    curvars <- fam.vars[list(years[iy]),which(names(fam.vars)!="year"),with=FALSE]
    tmpnms = tolower(as.character(curvars))
    for (i in 1:length(tmpnms)){
      curvars[[i]] <- tmpnms[i]
    }
    setnames(tmp,tolower(names(tmp)))
    
    curnames <- names(curvars)
    # current set of variables
    # caution if there are specified NAs
    if (curvars[,any(is.na(.SD))]) {
      na      <- curvars[,which(is.na(.SD))]
      codes   <- as.character(curvars)
      nanames <- curnames[na]
      tmp     <- copy(tmp[,codes[-na],with=FALSE])
      tmp[,(nanames) := NA_real_,with=FALSE]
      setnames(tmp,c(curnames[-na],nanames))
      setkey(tmp,interview)
    } else {
      codes <- as.character(curvars)
      tmp   <- copy(tmp[,codes,with=FALSE])
      setnames(tmp,curnames)
      setkey(tmp,interview)
    }
    
    # merge family and yind
    m <- copy(tmp[yind])
    m[,year := years[iy] ]
    setkey(m,interview)
    
    # bring in wealth files, subset them
    # ==================================
    # merge m and wealth
    if (any.wealth){
      flog.warn("merging of wealth files currently turned off!")
      # check if there is a wealth file for this year
      # iw = grep(years[iy],wlth.dat,value=TRUE)
      # if (length(iw)>0){
      # 	rm(list=ls(envir=tmp.env),envir=tmp.env)
      #    	load(file=iw,envir=tmp.env)
      # 	tmp             <- get(ls(tmp.env),tmp.env)	# assign loaded dataset a new name
      # 	tmp             <- data.table(tmp)
      # 	flog.debug("wealth tmp: ",head(tmp),capture=TRUE)
      
      # 	# convert all variable names to lower case in both fam.vars and data file
      # 	curvars <- wealth1984.vars[list(years[iy]),which(names(wealth1984.vars)!="year"),with=FALSE]
      # 	flog.debug("wealth curvars: ",curvars,capture=TRUE)
      # 	curvars[,name := tolower(name)]
      # 	curvars[,variable := tolower(variable)]
      # 	curvars[,interview := tolower(interview)]
      
      # 	setnames(tmp,tolower(names(tmp)))
      # 	flog.debug("wealth tmp: ",head(tmp),capture=TRUE)
      
      # 	# current set of variables
      # 	codes <- c(curvars[,variable],curvars[,interview][[1]])
      # 	flog.debug("wealth codes: ",codes,capture=TRUE)
      # 	tmp   <- copy(tmp[,codes,with=FALSE])
      # 	setnames(tmp,c(curvars[,name],"interview"))
      # 	flog.debug("wealth tmp: ",head(tmp),capture=TRUE)
      # 	setkey(tmp,interview)
      
      # 	# merge m and wealthfile
      # 	m <- merge(m,tmp,all.x=TRUE)
      
      # }  # end wealth files
    }
    
    # note: a person who does not respond in wave x has an interview number in that wave, but NAs in the family file variables. remove those records.
    idx <- which(!is.na(unlist(fam.vars[list(years[iy])][,curnames,with=FALSE])))[1]	# index of first non NA variable
    m[,isna := is.na(m[,curnames[idx],with=FALSE])]
    m <- copy(m[isna == FALSE])
    m[,isna := NULL]
    # all remaining NAs are NAs which the user knows about and actually requested when specifying fam.vars
    # if (iy>1)	setcolorder(m,names(datas[[1]]))
    datas[[iy]] <- copy(m)
    
    
  }  # end year
  
  data2 <- rbindlist(datas,use.names=TRUE,fill=TRUE)	# glue together
  rm(datas)
  
  # design
  # keep all
  # keep only obs who show up in each wave: balanced
  # keep only obs who show up at least in g consecutive waves
  
  data2[,present := length(year), by=pid]
  if (design == "balanced"){
    n <- nrow(data2)
    data2[,always := max(present) == length(years),by=pid]
    data2 <- copy(data2[always==TRUE])
    data2[,always := NULL]
    flog.info("balanced design reduces sample from %d to %d",n,nrow(data2))
  } else if (is.numeric(design)){
    n <- nrow(data2)
    data2[,enough := max(present) >= design,by=pid]
    data2 <- copy(data2[enough==TRUE])
    data2[,enough := NULL]
    flog.info("design choice reduces sample from %d to %d",n,nrow(data2))
  } else if (design=="all"){
    # do nothing
  }
  data2[,present := NULL]
  
  #     setkey(datas,pid,year)
  #     rm(ind)
  flog.info("End of build.panel")
  return(data2)
}

#' one year test, no ind file
#' 
#' @param dd Data Dictionary location. If NULL, 
#' use temp dir and force download
#' @export
small.test.noind <- function(dd=NULL){
  cwf = openxlsx::read.xlsx(system.file(package="psidR","psid-lists","psid.xlsx"))
  head_age_var_name <- getNamesPSID("ER17013", cwf, years=c(2003))
  famvars = data.frame(year=c(2003),age=head_age_var_name)
  build.panel(fam.vars=famvars,datadir=dd)
}

#' one year test, ind file
#' 
#' @param dd Data Dictionary location. If NULL, 
#' use temp dir and force download
#' @export
small.test.ind <- function(dd=NULL){
  cwf = openxlsx::read.xlsx(system.file(package="psidR","psid-lists","psid.xlsx"))
  head_age_var_name <- getNamesPSID("ER17013", cwf, years=c(2003))
  educ = getNamesPSID("ER30323",cwf,years=2003)
  famvars = data.frame(year=c(2003),age=head_age_var_name)
  indvars = data.frame(year=c(2003),educ=educ)
  build.panel(fam.vars=famvars,ind.vars=indvars,datadir=dd)
}

#' three year test, ind file
#' 
#' @param dd Data Dictionary location. If NULL, 
#' use temp dir and force download
#' @export
medium.test.ind <- function(dd=NULL){
  cwf = openxlsx::read.xlsx(system.file(package="psidR","psid-lists","psid.xlsx"))
  head_age_var_name <- getNamesPSID("ER17013", cwf, years=c(2003,2005,2007))
  educ = getNamesPSID("ER30323",cwf,years=c(2003,2005,2007))
  famvars = data.frame(year=c(2003,2005,2007),age=head_age_var_name)
  indvars = data.frame(year=c(2003,2005,2007),educ=educ)
  build.panel(fam.vars=famvars,ind.vars=indvars,datadir=dd)
}

#' three year test, no ind file
#' 
#' @param dd Data Dictionary location
#' @export
medium.test.noind <- function(dd=NULL){
  cwf = openxlsx::read.xlsx(system.file(package="psidR","psid-lists","psid.xlsx"))
  head_age_var_name <- getNamesPSID("ER17013", cwf, years=c(2003,2005,2007))
  famvars = data.frame(year=c(2003,2005,2007),age=head_age_var_name)
  build.panel(fam.vars=famvars,datadir=dd)
}

#' three year test, ind file and one NA variable
#' 
#' @param dd Data Dictionary location. If NULL, 
#' use temp dir and force download
#' @export
medium.test.ind.NA <- function(dd=NULL){
  cwf = openxlsx::read.xlsx(system.file(package="psidR","psid-lists","psid.xlsx"))
  head_age_var_name <- getNamesPSID("ER17013", cwf, years=c(2003,2005,2007))
  educ = getNamesPSID("ER30323",cwf,years=c(2003,2005,2007))
  educ[2] = NA
  famvars = data.frame(year=c(2003,2005,2007),age=head_age_var_name)
  indvars = data.frame(year=c(2003,2005,2007),educ=educ)
  build.panel(fam.vars=famvars,ind.vars=indvars,datadir=dd,loglevel = DEBUG)
}

#' three year test, ind file and one NA variable and wealth
#' 
#' @param dd Data Dictionary location. If NULL, 
#' use temp dir and force download
#' @export
medium.test.ind.NA.wealth <- function(dd=NULL){
  flog.warn("this functionality is not implemented any more.")
  # 	cwf = openxlsx::read.xlsx(system.file(package="psidR","psid-lists","psid.xlsx"))
  # 	head_age_var_name <- getNamesPSID("ER17013", cwf, years=c(2005,2007))
  #  	educ = getNamesPSID("ER30323",cwf,years=c(2005,2007))
  #  	educ[2] = NA
  # 	r = system.file(package="psidR")
  #     w = fread(file.path(r,"psid-lists","wealthvars-small.txt"))
  # 	famvars = data.frame(year=c(2005,2007),age=head_age_var_name)
  # 	indvars = data.frame(year=c(2005,2007),educ=educ)
  # 	build.panel(fam.vars=famvars,ind.vars=indvars,wealth1984.vars
  # =w,datadir=dd,loglevel = DEBUG)
}


#' Build example PSID
#' 
#' @description Builds a panel from the full PSID dataset
#' @export
#' @param datadr string of the data directory
#' @param small logical TRUE if only use years 2013 and 2015.
#' @return a data.table with panel data
build.psid <- function(datadr="~/datasets/psid/",small=TRUE){
  variable <- name <- NULL
  r = system.file(package="psidR")
  if (small){
    f = fread(file.path(r,"psid-lists","famvars-small.txt"))
    i = fread(file.path(r,"psid-lists","indvars-small.txt"))
  } else {
    f = fread(file.path(r,"psid-lists","famvars.txt"))
    i = fread(file.path(r,"psid-lists","indvars.txt"))
  }
  setkey(i,"name")
  setkey(f,"name")
  
  i = dcast(i[,list(year,name,variable)],year~name, value.var = "variable")
  f = dcast(f[,list(year,name,variable)],year~name, value.var = "variable")
  d = build.panel(datadir=datadr,fam.vars=f,ind.vars=i, heads.only = TRUE,sample="SRC",design="all")
  save(d,file="~/psid_no_wealth.RData")
  
  return(d)
}



#' GetPSID variables names from various years
#'
#' The user can specify one variable name from any year. This function
#' will find that variable's correct name in any of the years
#' specified by the user. If user does not specify the \code{years}
#' variable, return will represent all years in which variable was
#' present.
#'
#' This uses the psid.xlsx crosswalk file from UMich, which is
#' available at http://psidonline.isr.umich.edu/help/xyr/psid.xlsx. In the 
#' example, the package openxlsx's read.xlsx is used to import the crosswalk
#' file.
#'
#' Ask for one variable at a time.
#' @param aname A variable name in any of the PSID years
#' @param file optional file name to write csv
#' @param cwf A data.frame representation of the cross-walk file,
#'     (the psid.xlsx file).
#' @param years A vector of years. If NULL, all years in which that
#'     variable existed are returned
#' @return A vector of names, one for each year.
#' @author Paul Johnson <pauljohn@@ku.edu> and Florian Oswald
#' @export
#' @examples
#' # read UMich crosswalk from installed file
#' r = system.file(package="psidR")
#' cwf = openxlsx::read.xlsx(file.path(r,"psid-lists","psid.xlsx"))
#' 
#' # or download directly
#' # cwf <- read.xlsx("http://psidonline.isr.umich.edu/help/xyr/psid.xlsx")
#' 
#' # then get names with
#' getNamesPSID("ER17013", cwf, years = 2001)
#' getNamesPSID("ER17013", cwf, years = 2003)
#' getNamesPSID("ER17013", cwf, years = NULL)
#' getNamesPSID("ER17013", cwf, years = c(2005, 2007, 2009))
getNamesPSID <- function(aname, cwf, years = NULL,file = NULL){
  myvar <- which(cwf == aname, arr.ind=TRUE)
  ## variables that begin with Y
  ynames.all <- grep("^Y", colnames(cwf))
  ynames.labs <- grep("^Y", colnames(cwf),value = TRUE)
  
  if (is.null(years)){
    yearkeep <- ynames.all
  } else {
    yearkeep <- paste0("Y", years)
    iyearkeep <- ynames.labs %in% yearkeep
    yearkeep <- yearkeep[yearkeep %in% colnames(cwf)]
    ynames.labs <- ynames.labs[iyearkeep]
  }
  ovalue <- transpose(cwf[myvar[1], yearkeep, drop = FALSE])
  od = data.frame(year = ynames.labs, variable = ovalue$V1)
  if (!is.null(file)){
    write.table(od,file = file,row.names = FALSE)
  }
  od
}

