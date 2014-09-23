######################################
#### baby names display            ###
######################################

library(shiny)
library(ggplot2)
library(babynames)
library(grid)
library(data.table)
library(mFilter)

source("~/Documents/R/names/textInput.R")

simpleCap <- function(x) {  # make names Capital first only
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
      sep="", collapse=" ")
}

# function to pad profiles by inserting zeros for missing years
# this is needed for differencing of two names
# input data frame must first be subsetted as one name and sex
fullProfile <- function(nameDF) { 
  full <- data.frame(year = c(1880:2013),
              sex = nameDF$sex[1],
              name = nameDF$name[1])
  fullPro <- merge(full, nameDF, by = "year", all.x = TRUE)
  names(fullPro)[2] <- "sex"
  names(fullPro)[3] <- "name"
  fullPro$sex.y <- NULL
  fullPro$name.y <- NULL
  fullPro$n[is.na(fullPro$n)] <- 0
  fullPro$prop[is.na(fullPro$prop)] <- 0.0
  return(fullPro)
 }

options(warn = -1)
shinyServer(function(input, output, session) {

# extract data for first selected name
  names1 <- reactive({
    baby <- babynames[babynames$name == simpleCap(input$nameID1) &
                      babynames$sex == input$gender1,]
    if (dim(baby)[1] == 0) baby2 <- baby
    else if (dim(baby)[1] < 134) baby2 <- fullProfile(baby)
    else baby2 <- baby
    return(baby2)
  })
  
# extract data for second selected name
  names2 <- reactive({
    baby <- babynames[babynames$name == simpleCap(input$nameID2) &
                      babynames$sex == input$gender2,]
    if (dim(baby)[1] < 134) {baby2 <- fullProfile(baby) }
    else baby2 <- baby
    return(baby2)
  })

# function to prepare selected name data for plotting
  smoothed <- reactive({
    n1 <- names1()
    n2 <- names2()
    n1$perc <- n1$prop * 100
    n2$perc <- n2$prop * 100
    switch(input$smooth,
           N = {n1$plot <- n1$perc
                n2$plot <- n2$perc},  # no smoothing applied
           L = {n1$plot <- unlist(loess(perc ~ year, data = n1,
                                        span = input$lospan)[2])
                n2$plot <- unlist(loess(perc ~ year, data = n2,
                                        span = input$lospan)[2])},  #LOESS
           B = {bwf1 <- bwfilter(n1$perc, freq = input$filtfreq, nfix = 2)
                n1$plot <- bwf1$trend
                bwf2 <- bwfilter(n2$perc, freq = input$filtfreq, nfix = 2)
                n2$plot <- bwf2$trend}  # high pass Butterworth filter
           )
    n1color <- "#0072B2"; n1$color <- n1color
    n2color <- "#E69F00"; n2$color <- n2color
# insert synthetic data point where time series cross
    pcompare <- n2$plot - n1$plot
    pcompsign <- sign(pcompare)
    pdelta <- diff(pcompare, lag = 1)
    prunavg <- filter(n1$plot, filter=c(1,1), sides=1)[2:length(n1$plot)] / 2
    pavgyear <- filter(n1$year, filter=c(1,1), sides=1)[2:length(n1$year)] / 2
    pcross <- ifelse(abs(diff(pcompsign, lag = 1)) == 2, 1, 0)
  if (sum(pcross) > 0) {
    pn1synth <- prunavg[which(pcross==1)]
    pyrsynth <- pavgyear[which(pcross==1)]
    pn1synthrec <- data.frame(year = pyrsynth,
                              sex = n1$sex[1],
                              name = n1$name[1],
                              n = n1$prop[1],
                              prop = pn1synth,
                              perc = pn1synth,
                              plot = pn1synth,
                              color = n1color)
    pn2synthrec <- data.frame(year = pyrsynth,
                              sex = n2$sex[1],
                              name = n2$name[1],
                              n = n2$prop[1],
                              prop = pn1synth,
                              perc = pn1synth,
                              plot = pn1synth,
                              color = n2color)
    n1 <- rbind(n1, pn1synthrec)
    n1 <- n1[order(n1$year),]
    n2 <- rbind(n2, pn2synthrec)
    n2 <- n2[order(n2$year),]
  }
    smoothed <- rbind(n1, n2)
    return(smoothed)
  })

# render the profile plot for the two selected names
  output$namePlot <- renderPlot({
    .e <- environment()
    rp <- smoothed()
    maxp <<- max(rp[rp$year >= input$yearRange[1] &
                  rp$year <= input$yearRange[2], 7])
    rp$Profile <- paste0(rp$name, " (", rp$sex, ")")
    n1color <- "#0072B2"
    n2color <- "#E69F00"
    p <- ggplot(data= rp, aes(x=year, y=plot, group = Profile,
                        color = Profile), environment = .e)
    p <- p + ggtitle("Year of Birth") +
      scale_x_continuous(limits = c(input$yearRange[1], input$yearRange[2])) +
      scale_y_continuous(limits = c(0.0, maxp)) +
      xlab("") + ylab("Percent within Gender") +
# colors taken from colorblind palette cb_pallette
      scale_color_manual(values = c("#0072B2", "#E69F00")) +
      theme(title = element_text(face = "bold", size = 15, vjust = 1.0),
        axis.text = element_text(face = "bold", size=15),
        axis.title.y = element_text(face = "bold", size = 20, vjust = 2.0),
        legend.position = "bottom",
        legend.key.size = unit(0.7, "cm"),
        legend.key.width = unit(4.5, "cm"),
        legend.title = element_text(size = 0),
        legend.text = element_text(face = "bold", size =15)) +

        geom_line(data = rp[rp$name == simpleCap(input$nameID1),], size = 1.5, 
          aes(color = Profile)) +
        geom_line(data = rp[rp$name == simpleCap(input$nameID2),], size = 1.5, 
          aes(color = Profile))
    print(p)
  }) # end namePlot renderPlot

# render the difference plot for the two selected names
# 'smoothed()' object will have rows for all years of these two names
  output$diffPlot <- renderPlot({
    rpd <- smoothed()
    rpd1 <- rpd[rpd$name == simpleCap(input$nameID1),]
    rpd2 <- rpd[rpd$name == simpleCap(input$nameID2),]
    rpdif <- merge(rpd1, rpd2, by = "year")
    rpdif$diff <- rpdif$plot.x - rpdif$plot.y
    rpdif$sign <- sign(rpdif$diff)

    rpdif$pos <- with(rpdif, pmax(0, diff))
    rpdif$neg <- with(rpdif, pmin(0, diff))
    posCol <- ifelse(rpdif$name.x[1] < rpdif$name.y[1], "#0072B2", "#E69F00")
    negCol <- ifelse(rpdif$name.x[1] < rpdif$name.y[1], "#E69F00", "#0072B2")
    p <- ggplot(data= rpdif, aes(x=year), ) + 
      ggtitle("") +
      scale_x_continuous(limits = c(input$yearRange[1], input$yearRange[2])) +

      xlab("") + ylab("Difference") +
# colors taken from colorblind palette cb_pallette
       scale_color_manual(values = c("#E69F00", "#0072B2"))

      p <- p + 
        theme(
          axis.text = element_text(face = "bold", size=15),
          axis.title.x = element_text(face = "bold", size=15, vjust = -1.0),
          axis.title.y = element_text(face = "bold", size = 20, vjust = 2.0),
          legend.position = "none") +
        geom_ribbon(aes(ymin = 0, ymax = pos), fill = posCol) +
        geom_ribbon(aes(ymin = neg, ymax = 0), fill = negCol)
      p <- p + geom_hline(yintercept=0)
      p <- p +  geom_line(aes(y = pos), color = "black")
      p <- p +  geom_line(aes(y = neg), color = "black")
  print(p)
  }) # end diffPlot renderPlot
    
# in renderDataTable, can also use bPaginate = FALSE to disable pagination
#   but don't do that here, as number of records would take very long to display
  output$tabl <- renderDataTable({
    nameDT <- data.table(babynames)
    setnames(nameDT, "year", "Year")
    setnames(nameDT, "name", "Name")
    setnames(nameDT, "n", "Count")
    nameDT[, Percent := round(prop*100,3)]
    nameDT[, prop := NULL]
    if (input$sortL == "Y") {
      setkey(nameDT, Year, Percent)
      nameDT <- nameDT[,.SD[order(-Year, -Percent)]] 
    } else {  # sortL = "P"
      setkey(nameDT, Percent)
      nameDT <- nameDT[,.SD[order(-Percent)]]       
    }

# aggregate over years for each name if requested
# either way, restrict to selected gender and year range
# aggregation currently assumes equal sample sizes per year
    switch(input$aggrL,
           Y = {  # aggregate by Year
             nameDT[sex==input$genderL & Percent >= input$minPercent &
             Year >= input$yearRangeL[1] & Year <= input$yearRangeL[2], 
             j = list(Sum_Pct = round(sum(Percent),4)), 
             by = Year]
           },
           M = {  # aggregate by Name
             nameDT[sex==input$genderL & Percent >= input$minPercent &
             Year >= input$yearRangeL[1] & Year <= input$yearRangeL[2], 
             j = list(Avg_Pct = round(mean(Percent),4),
                      Min_Pct = round(min(Percent),4),
                      Max_Pct = round(max(Percent),4)), 
             by = Name]
           },
           N = { # no aggregation, that is, each Year/Name combination
             nameDT[sex==input$genderL & Percent >= input$minPercent &
             Year >= input$yearRangeL[1] & Year <= input$yearRangeL[2]]
           }
    ) # end switch

    },

    options = list(
      aLengthMenu = c(12, 40, 100),
      iDisplayLength = 12,
      bFilter = FALSE
    )
  ) # end renderDataTable

# using renderUI to display stats text because it is more compact on screen
  output$stat1 <- renderUI({
    n <- names1()$year[which.max(names1()$prop)]
    p1 <- paste0(simpleCap(input$nameID1), "'s peak: ", n)
    s <- sum(names1()$n)
    p2 <- paste0("Total namings: ", s)
    HTML(paste(p1, p2, sep = '<br/>'))
  })

  output$stat2 <- renderUI({
    n <- names2()$year[which.max(names2()$prop)]
    p1 <- paste0(simpleCap(input$nameID2), "'s peak: ", n)
    s <- sum(names2()$n)
    p2 <- paste0("Total namings: ", s)
    HTML(paste(p1, p2, sep = '<br/>'))
  })

# acknowledgments, on separate tab
# text is defined on textInput.R file that is sourced at start of this file
  output$acks <- renderUI({
    HTML(paste(blk1, blk2, blk3, sep = '<br/><br/>'))
  })

# check to see if too many records returned from input data frame
  output$error1 <- renderUI({
#    if (dim(names1())[1] == 0) txt <- {"First name not found"}
    if (dim(names1())[1] > 134) {txt <- "too many of name 1"}
    else if
    (dim(names2())[1] > 134) {txt <- "too many of name 2"}
    else {txt <- ""}
    return(txt)
  })

})  # end shinyServer function
options(warn = 0)