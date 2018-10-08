# v1.0 
# Last Modified: 051018
# setwd("E:\\Shiny\\UploadMatch v1.0")

library(readxl); library(reshape2); library(dplyr); options(stringsAsFactors=FALSE); 
source("mysm v1.0.R")
Sys.setenv(TZ="Asia/Singapore")
# acp & ocp are samples of how data should look like
acp <- readxl::read_xlsx("AChead Proper.xlsx", col_types="text") %>% as.data.frame()  
ocp <- readxl::read_xlsx("OChead Proper.xlsx", col_types="text") %>% as.data.frame()

shinyServer(function(input, output, session) {
  # file1 is agency, file2 is officer
  Dataset1 <- reactive({
    if (is.null(input$file1)) { return(data.frame()) } # User has not uploaded a file yet
    Dataset1 <- as.data.frame(do.call(input$readFunction, c(list(input$file1$datapath) )))
    return(Dataset1)
  })
  Dataset2 <- reactive({
    if (is.null(input$file2)) { return(data.frame()) }
    Dataset2 <- as.data.frame(do.call(input$readFunction, c(list(input$file2$datapath) )))
    return(Dataset2)
  })
  output$table1 <- renderTable({
    if (is.null(input$file1)) { return(acp) } 
    return(Dataset1())
  })
  output$table2 <- renderTable({
    if (is.null(input$file2)) { return(ocp) }  
    return(Dataset2())
  })
  output$dlready <- renderText({ })  # Placeholder for "Your download is ready"
  output$dl1 <- renderUI({ })  # Placeholder for download button1
  output$dl2 <- renderUI({ })  # download button2

# For testing via R Console: 
# app <- acp; app <- readxl::read_xlsx(file.choose())
# opp <- ocp; opp <- readxl::read_xlsx(file.choose())
  
observe({
  if (!is.null(input$file1) & !is.null(input$file2) ) {
  f1n <- gsub("\\..*", "", input$file1$name)   # AgencyChoice.xlsx
  f2n <- gsub("\\..*", "", input$file2$name)
  app <- Dataset1()   
  opp <- Dataset2()  
  colnames(opp)[1:2] <- c("officer", "first")
  colnames(app)[1:2] <- c("agency", "first")
  opp$officer <- as.character(opp$officer); app$agency <- as.character(app$agency)
  ap <- app; op <- opp
  opref <- ncol(op) - 1
  apref <- ncol(ap) - 1
  keep <- apply(ap[, 2:ncol(ap)], 1, function(x) sum(is.na(x)) < apref)
  agencynochoice <- ap$agency[!keep]  # Agencies with no choices (all NA) exclude from this round
  ap <- ap[keep, ]
  t1 <- table(as.matrix(ap[, 2:ncol(ap)]))  # Count of times each officer was chosen.
  off_id <- as.numeric(gsub("[A-Za-z]", "", op$officer))  # first column must have numbers, cannot be pure english.  
  officerunloved <- off_id[! off_id %in% dimnames(t1)[[1]] ]  # Exclude form this round
  officerunloved <- op$officer[officerunloved]
  anc <- paste(agencynochoice, collapse=" "); oul <- paste(officerunloved, collapse=" ")
  if (length(anc)==0) {anc <- NA}
  if (length(oul)==0) {oul <- NA}
  officerchosenonce <- dimnames(t1)[[1]]
  keep <- off_id %in% officerchosenonce
  op <- as.data.frame(op[keep, ])  
  
  output$chk <- renderText({
    sprintf("EXCLUSION: Agencies %s are excluded from this round because they have no preferences. Officers %s are excluded from this round because no agencies chose them.", anc, oul)
  })
  output$chk2 <- renderText({
    if(nrow(ap) != nrow(op)) {paste("WARNING: After exclusion, there are not enough Agencies/Officers.", nrow(ap), "Agencies and", nrow(op), "Officers. There will be some unmatched entries.")}
  })
 
  ### CREATE DICTIONARIES FOR REMAPPING
  # Deferred Acceptance value uses 1:4 etc
  # Agency value is 1; 2.2; 2.2 and need to be remapped to 1; 2; 3
  # Officer value is 5; 6; 7 and need to be remapped to 1; 2; 3

  odict <- data.frame( officer = op$officer )
  odict$value <- as.numeric( gsub("[A-Za-z]", "", odict$officer) )
  odict$davalue <- 1:nrow(odict)
  adict <- data.frame( agency = ap$agency )
  adict$value <- gsub("[A-Za-z]", "", adict$agency)
  adict$davalue <- 1:nrow(adict)
  apm <- as.matrix(ap[, -1])
  opm <- as.matrix(op[, -1])

  opmu <- na.omit( unique(as.character(opm)) )
  apmu <- na.omit( unique(as.character(apm)) )
  opmu1 <- opmu[!(opmu %in% adict$value)]  # officers chose an agency, not in agency dictionary
  apmu1 <- apmu[!(apmu %in% odict$value)]  # agency chose an officer, not in officer dictionary
  opmu2 <- paste(opmu1, collapse=" ")
  apmu2 <- paste(apmu1, collapse=" ")
  output$chk2b <- if (length(opmu1)==0) {renderText({})} else renderText({
    sprintf( "WARNING: Agencies %s are preferred by officers. Is this a data entry mistake by the officer?", opmu2 )
  })
  output$chk2c <- if (length(apmu1)==0) {renderText({})} else renderText({
    sprintf( "WARNING: Officers %s are preferred by agencies. Is this a data entry mistake by the agency?", apmu2 )  
  })
  jobpref <- t(ap[, -1])
  candpref <- t(op[, -1])  
  jobpref[apply(jobpref, 2, duplicated)] <- NA  # Remove entries where agency selects same officer 3 times
  candpref[apply(candpref, 2, duplicated)] <- NA
  colnames(jobpref) <- ap$agency
  colnames(candpref) <- op$officer

  cp <- melt(candpref)
  cp <- merge(x=cp, y=adict, by="value", all.x=TRUE)
  cp <- rename(cp, choicelabel=Var1, officername=Var2)
  cpin <- dcast(cp, choicelabel ~ officername, value.var="davalue" )
  cpin <- as.matrix( select(cpin, -choicelabel) )
  
  jp <- melt(jobpref)
  jp <- merge(x=jp, y=odict, by.x="value", by.y="value")
  jp <- rename(jp, choicelabel=Var1, agencyname=Var2)
  jpin <- dcast(jp, choicelabel ~ agencyname, value.var="davalue" )
  jpin <- as.matrix( select(jpin, -choicelabel) )
  
  withProgress(message='Matching in progress', value=0, {
                 for (i in 1:10) {
                   incProgress(1/10)
                   Sys.sleep(0.10)
                 }
               })
  
  ## USING OFFICERS AS MEN ####
  # Remember: GaleShapeley favors the proposers (proposers are men) 
  # Produces out1
  
  # unmen, unwom: unmatched men and women
  # menmatch1: same ncols as manpref=, each column is a man. value is the assigned woman.
  # wommatch1: same ncols as wompref=, each column is a woman. value is the assigned man.
  # menronw, womronm: man's rank on the assigned woman's choices, and vice versa.
  # mp: "residuals" of the manpref=, 0 indicates cells where the algorithm processed.
  
  res <- mysm(manpref=cpin, wompref=jpin)
  tab1 <- with(res, data.frame(men=1:ncol(menmatch1), menmatch=as.vector(menmatch1), menchoice=as.vector(womronm)) )
  ym <- data.frame(menmatch=1:ncol(res$menronw), womchoice=as.vector(res$menronw))
  tab2 <- merge(x=tab1, y=ym, by="menmatch", all.x=TRUE)
  tab2 <- select(tab2, men, menmatch, menchoice, womchoice) %>% arrange(men)
  ym <- select(adict, davalue, agency)
  tab3 <- merge(x=tab2, y=ym, all.x=TRUE, by.x="menmatch", by.y="davalue")
  tab3 <- arrange(tab3, men)
  tab4 <- merge(x=tab3, y=odict, all.x=TRUE, by.x="men", by.y="davalue")
  outC <- select(tab4, officer, agency, menchoice, womchoice)
  outC$drop <- (is.na(outC$menchoice)|is.infinite(outC$menchoice)) | (is.na(outC$womchoice)|is.infinite(outC$womchoice))
  out1 <- outC[outC$drop==FALSE, ] %>% select(-drop)
  out1$menchoice <- as.integer(out1$menchoice); out1$womchoice <- as.integer(out1$womchoice)
  colnames(out1) <- c("Officer", "Agency allocated", "Officer's ranked choice", "Agency's ranked choice")

  ofgd <- as.character(outC$officer[outC$drop==FALSE])
  ofun <- opp$officer[!opp$officer %in% ofgd]
  aggd <- as.character(outC$agency[outC$drop==FALSE])
  agun <- app$agency[!app$agency %in% aggd]
  
  ofundfC <- data.frame(`Unmatched officers`=ofun)
  colnames(ofundfC) <- "Unmatched officers"
  agundfC <- data.frame(`Unmatched agencies`=agun)
  colnames(agundfC) <- "Unmatched agencies"
  
  output$table3 <- renderTable({
    out1
  })
  output$tab3_atext <- renderText({
    paste("These are the officers that were excluded/were matched to agencies which did not prefer them.")
  })
  output$table3a <- renderTable({
    ofundfC
  })
  output$tab3_btext <- renderText({
    paste("These are the agencies that were excluded/were matched to officers which did not prefer them.")
  })
  output$table3b <- renderTable({
    agundfC
  })
  
  ## USING AGENICES AS MEN ####
  # Remember: GaleShapeley favors the proposers (proposers are men) 
  # Produces out2
  res2 <- mysm(manpref=jpin, wompref=cpin)
  tab1 <- with(res2, data.frame(men=1:ncol(menmatch1), menmatch=as.vector(menmatch1), menchoice=as.vector(womronm)) )
  ym <- data.frame(menmatch=1:ncol(res2$menronw), womchoice=as.vector(res2$menronw))
  tab2 <- merge(x=tab1, y=ym, by="menmatch", all.x=TRUE)
  tab2 <- select(tab2, men, menmatch, menchoice, womchoice) %>% arrange(men)
  ym <- select(odict, davalue, officer)  
  tab3 <- merge(x=tab2, y=ym, all.x=TRUE, by.x="menmatch", by.y="davalue")  # menmatch is which Officer was chosen
  tab3 <- arrange(tab3, men)
  tab4 <- merge(x=tab3, y=adict, all.x=TRUE, by.x="men", by.y="davalue")  # men are the Job
  # tab4 <- arrange(tab4, onode)
  outA <- select(tab4, officer, agency, womchoice, menchoice)
  outA$drop <- (is.na(outA$menchoice)|is.infinite(outA$menchoice)) | (is.na(outA$womchoice)|is.infinite(outA$womchoice))
  out2 <- outA[outA$drop==FALSE, ] %>% select(-drop)
  out2$menchoice <- as.integer(out2$menchoice); out2$womchoice <- as.integer(out2$womchoice)
  colnames(out2) <- c("Officer", "Agency allocated", "Officer's ranked choice", "Agency's ranked choice")

  ofgd <- as.character(outA$officer[outA$drop==FALSE])
  ofun <- opp$officer[!opp$officer %in% ofgd]
  aggd <- as.character(outA$agency[outA$drop==FALSE])
  agun <- app$agency[!app$agency %in% aggd]
 
  ofundfA <- data.frame(`Unmatched officers`=ofun)
  colnames(ofundfA) <- "Unmatched officers"
  agundfA <- data.frame(`Unmatched agencies`=agun)
  colnames(agundfA) <- "Unmatched agencies"
  
  output$table4 <- renderTable({
    out2  
  })
  output$tab4_atext <- renderText({
    paste("These are the officers that were excluded/were matched to agencies which did not prefer them.")
  })
  output$table4a <- renderTable({
    ofundfA
  })
  output$tab4_btext <- renderText({
    paste("These are the agencies that were excluded/were matched to officers which did not prefer them.")
  })
  output$table4b <- renderTable({
    agundfA
  })
  # REMEMBER: Must view in browser, not Rstudio
  output$dlready <- renderText({
    paste("Matching complete. Please click on the tabs to view the results.")
  })
  output$dh1 <- downloadHandler(
    filename = function() { paste(f1n, f2n, " favOfficer ", Sys.time(), ".csv", sep="") },
    content = function(file) { write.csv(out1, file) }
  )
  output$dl1 <- renderUI({
    downloadButton('dh1', 'Download FavourOfficer')
  })
  output$dh2 <- downloadHandler(
    filename = function() { paste(f1n, f2n, " favAgency ", Sys.time(), ".csv", sep="") },
    content = function(file) { write.csv(out2, file) }
  )  
  output$dl2 <- renderUI({
    downloadButton('dh2', 'Download FavourAgency')
  })
  updateTabsetPanel(session, inputId="mytabset", selected="warntab")
  }  # if both uploads are not null
})  # observe
})  # ShinyServer

########################### 


# setwd("C:\\Users\\PSD HRI\\Desktop")
# library(shiny); setwd("F:\\Shiny\\UploadMatch v1.0"); shiny::runApp()
# shiny::runApp("UploadMatch", launch.browser=TRUE)
# shiny::runApp(, launch.browser=TRUE)
# rsconnect::deployApp(appName="matchingV01")
# don't put version control there
# WARNING: drop_upload works via rsconnect::deployApp() BUT not runApp()

# USER LOG-IN
# https://gist.github.com/withr/9001831

# SELECT LAST FILE IN DROP BOX
# x1 <- drop_search(input$userName, dtoken=mytoken) %>% dplyr::select(path) %>% tail(n=1) %>% unlist()
# x2 <- drop_read_csv(x1, dtoken=mytoken)
