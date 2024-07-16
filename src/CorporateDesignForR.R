#############################################################################
################# Corporate-Design Casra f?r R ##############################
#############################################################################

#### Anleitung:
# Am Anfang des Scripts diese Zeile einf?gen:
# if(!exists("theme_casra", mode="function")) source("X:\\1_Public\\Templates\\R\\CorporateDesignForR.R")

#### Ben?tigte packages:
#install.packages("ggplot2)
#install.packages("extrafont") #F?r Arial-Font

require(ggplot2)

if(!require(extrafont)){
  print("Ich installiere extrafont")
  debug(utils:::unpackPkgZip)
  install.packages("extrafont")
  library(extrafont)

}

#?berpr?fen ob Arial schon existiert. Falls nicht wird es importiert (y in Konsole eingeben)
if(is.null(fonts())){
  font_import(pattern = "[A/a]rial")
}
if (fonts()[[1]] != "Arial"){
  font_import(pattern = "[A/a]rial")
}

casraBlack <- "#000000"
casraGrayDark <- "#BCBCBC"
casraGrayLight <- "#EAEAEA"

casraBlueLight <- "#1e8bce"
casraBlueMed <- "#0066b0"
casraBlueDark <- "#004bb0"

casraOrange <- "#ee7d00"

casraGreenLight <- "#52a74c"
casraGreenDark <- "#255627"

fhnwIndigoBlau <- "#2D2D8A"

####################################################################################
############ ggplot2 ################################################################

# Gute Tutorials.
# http://www.sthda.com/english/wiki/be-awesome-in-ggplot2-a-practical-guide-to-be-highly-effective-r-software-and-data-visualization


### Theme f?r ggplot
theme_casra <- function(){
  
  titlePlot <- element_text(size = 18, face ="bold", vjust = 2)
  titleAxis <- element_text(size = 18, face ="bold")
  textAxis <- element_text(size=14)
  titleGrid <- element_text(size = 16)
  txt_legend <- element_text(size = 15, face = "plain", colour = casraBlack)
  txt <- element_text(size = 20, face = "plain", colour = casraBlack)
  gridline_y <- element_line(colour = casraGrayDark, size = 0.5)
  gridline_x <- element_blank()
  
  theme_light(base_family = "Arial") +
    theme(
      
      text = txt,
      strip.text = txt,
      axis.title = titleAxis,
      axis.title.x = element_blank(), #element_text(vjust = 2),
      axis.title.y = titleAxis, #element_text(vjust = 2),
      axis.text=textAxis,
      axis.line = element_line(colour = casraGrayDark),
      legend.text = txt_legend,
      legend.title = element_blank(),
      plot.title = titlePlot,
      strip.text.x = titleGrid,
      strip.text.y = titleGrid,
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = gridline_y,
      panel.border = element_blank(),
      
      strip.background = element_rect(fill = casraGrayDark)
      
    )
}

theme_casra_full <- function(sizeTextAxis = 14){
  
  titlePlot <- element_text(size = sizeTextAxis + 4, face ="bold", vjust = 2)
  titleAxis <- element_text(size = sizeTextAxis + 4, face ="bold")
  textAxis <- element_text(size=sizeTextAxis)
  titleGrid <- element_text(size = sizeTextAxis + 2)
  txt_legend <- element_text(size = sizeTextAxis + 1, face = "plain", colour = casraBlack)
  txt <- element_text(size = sizeTextAxis + 2, face = "plain", colour = casraBlack)
  gridline_y <- element_line(colour = casraGrayDark, size = 0.5)
  gridline_x <- element_blank()
  
  theme_light(base_family = "Arial") +
    theme(
      
      text = txt,
      strip.text = txt,
      axis.title = titleAxis, #element_text(vjust = 2),
      axis.text=textAxis,
      axis.line = element_line(colour = casraGrayDark),
      legend.text = txt_legend,
      legend.title = element_blank(),
      plot.title = titlePlot,
      strip.text.x = titleGrid,
      strip.text.y = titleGrid,
      plot.background = element_blank(),
      panel.grid.major = gridline_y,
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      
      strip.background = element_rect(fill = casraGrayDark)
      
    )
}

theme_casra_report <- function(){
  
  titlePlot <- element_text(size = 14, face ="bold", vjust = 2)
  titleAxis <- element_text(size = 11, face ="bold")
  textAxis <- element_text(size=10)
  titleGrid <- element_text(size = 11)
  txt_legend <- element_text(size = 8, face = "plain", colour = casraBlack)
  txt <- element_text(size = 8, face = "plain", colour = casraBlack)
  gridline_y <- element_line(colour = casraGrayDark, size = 0.5)
  gridline_x <- element_blank()
  
  theme_light(base_family = "Arial") +
    theme(
      
      text = txt,
      strip.text = txt,
      axis.title = titleAxis,
      axis.title.x = element_blank(), #element_text(vjust = 2),
      axis.title.y = titleAxis, #element_text(vjust = 2),
      axis.text=textAxis,
      axis.line = element_line(colour = casraGrayDark),
      legend.text = txt_legend,
      legend.title = element_blank(),
      plot.title = titlePlot,
      strip.text.x = titleGrid,
      strip.text.y = titleGrid,
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = gridline_y,
      panel.border = element_blank(),
      
      strip.background = element_rect(fill = casraGrayDark)
      
    )
}

theme_casra_report_full <- function(){
  
  titlePlot <- element_text(size = 14, face ="bold", vjust = 2)
  titleAxis <- element_text(size = 11, face ="bold")
  textAxis <- element_text(size=10)
  titleGrid <- element_text(size = 11)
  txt_legend <- element_text(size = 8, face = "plain", colour = casraBlack)
  txt <- element_text(size = 8, face = "plain", colour = casraBlack)
  gridline_y <- element_line(colour = casraGrayDark, size = 0.5)
  gridline_x <- element_blank()
  
  theme_light(base_family = "Arial") +
    theme(
      
      text = txt,
      strip.text = txt,
      axis.title = titleAxis,
      #axis.title.x = element_blank(), #element_text(vjust = 2),
      #axis.title.y = titleAxis, #element_text(vjust = 2),
      axis.text=textAxis,
      axis.line = element_line(colour = casraGrayDark),
      legend.text = txt_legend,
      legend.title = element_blank(),
      plot.title = titlePlot,
      strip.text.x = titleGrid,
      strip.text.y = titleGrid,
      plot.background = element_blank(),
      panel.grid.major = gridline_y,
      panel.grid.minor = element_blank(),
      #panel.grid.major.y = gridline_y,
      panel.border = element_blank(),
      
      strip.background = element_rect(fill = casraGrayDark)
      
    )
}



# Einstellungen R-Markdown für Plots

# knitr::opts_chunk$set(echo = FALSE, # Code nicht anzeigen
#                       fig.path = "Bilder_New/", # Ordner indem die Grafiken gespeichert werden
#                       dpi = 300, # Auflösung der Bilder
#                       fig.height = 4.78, # Höhe der Bilder (inches): Höhe von Feld für Grafik etc.Corp.Des
#                       fig.width = 9.1, # Breite der Bilder (inches): Breite von Feld für Grafik etc.Corp.Des
#                       fig.align = "center", 
#                       warning = FALSE,
#                       message = FALSE,
#                       error = FALSE)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

####################################################################################
############ Installieren Pakete bei CASRA ##############################################
# Weiter klicken bis Installation beendet

install.packages.casra <- function(x) {
  debug(utils:::unpackPkgZip)
  install.packages(x)
}


####################################################################################
############ Sammlung von n?tzlichen Plot ##############################################

## Bereich Fragebogen
# Die Wertungen f?r jede Frage
# ggplot(tempTable, aes(x = Frage, fill = Wert, y = n)) +
#   geom_bar(position = "fill", stat = "identity") + 
#   scale_fill_brewer(palette = "RdYlGn", direction = -1, name = "Wertung") +
#   theme_casra() + theme(legend.position = "bottom") +
#   geom_text(aes(label =n),size = 3, position = position_fill(vjust = 0.5))  + 
#   theme(axis.text.x=element_text(angle=45, size=8, vjust=0.5))

## Sch?ner Korrelationsplot mit corrplot
# corrplot(
#   corMaLs[[i]], method="color", #col=col(200),  
#   #type="lower",
#   order="hclust", 
#   addCoef.col = "black", # Add coefficient of correlation
#   tl.col="black", tl.srt=45, tl.cex = 1.5,#Text label color and rotation
#   number.cex = 1.2,
#   # Combine with significance
#   #p.mat = p.mat, sig.level = 0.01, insig = "blank", 
#   # hide correlation coefficient on the principal diagonal
#   diag=FALSE
#   #, title = i
#   )



####################################################################################
############ Grösse von Files anzeigen #############################################

# improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.dim)
  names(out) <- c("Type", "Size", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

# shorthand
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}
