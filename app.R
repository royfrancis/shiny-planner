## CALENDAR PLANNER
## R shinyapp to generate ggplot2 calendar
## 2018 Roy Mathew Francis
## 04-Aug-2018

## load libraries
library(ggplot2)
library(shiny)
library(colourpicker)
#library(extrafont)
#loadfonts()

## load colours
cols <- toupper(c(
        "#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5","#FDBF6F","#A6CEE3",
        "#56B4E9","#B2DF8A","#FB9A99","#CAB2D6","#A9C4E2","#79C360","#FDB762","#9471B4",
        "#A4A4A4","#fbb4ae","#b3cde3","#ccebc5","#decbe4","#fed9a6","#ffffcc","#e5d8bd",
        "#fddaec","#f2f2f2","#8dd3c7","#d9d9d9"))

# UI ---------------------------------------------------------------------------

ui <- fluidPage(
  pageWithSidebar(
    headerPanel(title="Calendar Planner",windowTitle="Calendar Planner"),
    sidebarPanel(
      helpText("Change settings in a top-down manner."),
      h3("Duration"),
      div(class="row",
          div(class="col-md-6",
              dateInput("in_duration_date_start","From",value=format(as.Date(Sys.time(),"%Y-%m-%d",tz="Europe/Stockholm"),"%Y-%m-%d"))
          ),
          div(class="col-md-6",
              dateInput("in_duration_date_end","To",value=format(as.Date(Sys.time(),"%Y-%m-%d",tz="Europe/Stockholm")+30,"%Y-%m-%d"))
          )
      ),
      h3("Tracks"),
      helpText("If number of tracks is changed, all track variables are reset. If track date ranges overlap, the lower track overwrites the upper track."),
      sliderInput("in_num_tracks","Number of Tracks",value=1,min=1,max=20,step=1),
      uiOutput("tracks"),
      div(class="row",
          div(class="col-xs-6",style=list("padding-right: 5px;"),
              colourpicker::colourInput("in_track_colour_available",label="Track colour (Available)",
                                        palette="limited",allowedCols=cols,value=cols[length(cols)-1])
          ),
          div(class="col-xs-6",style=list("padding-left: 5px;"),
              colourpicker::colourInput("in_track_colour_weekend",label="Track colour (Weekend)",
                                        palette="limited",allowedCols=cols,value=cols[length(cols)])
          )
      ),
      tags$br(),
      h3("Settings"),
      selectInput("in_legend_position",label="Legend position",
                  choices=c("top","right","left","bottom"),selected="right",multiple=F),
      div(class="row",
          div(class="col-xs-6",style=list("padding-right: 5px;"),
              selectInput("in_legend_justification",label="Legend justification",
                          choices=c("left","right"),selected="right",multiple=F)
          ),
          div(class="col-xs-6",style=list("padding-left: 5px;"),
              selectInput("in_legend_direction",label="Legend direction",
                          choices=c("vertical","horizontal"),selected="vertical",multiple=F)
          )
      ),
      div(class="row",
          div(class="col-xs-6",style=list("padding-right: 5px;"),
              numericInput("in_themefontsize",label="Theme font size",value=8,step=0.5)
          ),
          div(class="col-xs-6",style=list("padding-left: 5px;"),
              numericInput("in_datefontsize",label="Date font size",value=2.5,step=0.1)
          )
      ),
      div(class="row",
          div(class="col-xs-6",style=list("padding-right: 5px;"),
              numericInput("in_monthfontsize",label="Month font size",value=8,step=0.5)
          ),
          div(class="col-xs-6",style=list("padding-left: 5px;"),
              numericInput("in_legendfontsize",label="Legend font size",value=5,step=0.5)
          )
      ),
      tags$br(),
      h3("Download"),
      helpText("Width is automatically calculated based on the number of weeks. File type is only applicable to download and does not change preview."),
      div(class="row",
          div(class="col-xs-6",style=list("padding-right: 5px;"),
              numericInput("in_height","Height (cm)",step=0.5,value=5.5)
          ),
          div(class="col-xs-6",style=list("padding-left: 5px;"),
              numericInput("in_width","Width (cm)",step=0.5,value=NA)
          )
      ),
      div(class="row",
          div(class="col-xs-6",style=list("padding-right: 5px;"),
              selectInput("in_res","Res/DPI",choices=c("200","300","400","500"),selected="200")
          ),
          div(class="col-xs-6",style=list("padding-left: 5px;"),
              selectInput("in_format","File type",choices=c("png","tiff","jpeg","pdf"),selected="png",multiple=FALSE,selectize=TRUE)
          )
      ),
      downloadButton("btn_downloadplot","Download Plot"),
      tags$hr(),
      helpText("2018 | Roy Francis")
    ),
    mainPanel(
      sliderInput("in_scale","Image preview scale",min=0,max=3,step=0.10,value=1),
      helpText("Scale only controls preview here and does not affect download."),
      tags$br(),
      imageOutput("out_plot")
      #verbatimTextOutput("out_display"),
      #verbatimTextOutput("out_display2")
    )
  )
)

# SERVER -----------------------------------------------------------------------

server <- function(input, output, session) {
  
  store <- reactiveValues(week=NULL)
  
  ## UI: tracks ----------------------------------------------------------------
  ## conditional ui for tracks
  
  output$tracks <- renderUI({
    num_tracks <- as.integer(input$in_num_tracks)
    
    # create date intervals
    dseq <- seq(as.Date(input$in_duration_date_start),as.Date(input$in_duration_date_end),by=1)
    r1 <- unique(as.character(cut(dseq,breaks=num_tracks+1)))
    
    lapply(1:num_tracks, function(i) {
      
      div(class="row",
          div(class="col-xs-3",style=list("padding-right: 3px;"),
              textInput(paste0("in_track_name_",i),label="Name",value=paste0("Text",i),placeholder="Available")
          ),
          div(class="col-xs-3",style=list("padding-right: 3px; padding-left: 3px;"),
              dateInput(paste0("in_track_date_start_",i),label="From",value=as.Date(r1[i],"%Y-%m-%d"))
          ),
          div(class="col-xs-3",style=list("padding-right: 3px; padding-left: 3px;"),
              dateInput(paste0("in_track_date_end_",i),label="To",value=as.Date(r1[i+1],"%Y-%m-%d")-1)
          ),
          div(class="col-xs-3",style=list("padding-left: 3px;"),
              colourpicker::colourInput(paste0("in_track_colour_",i),label="Colour",
                                        palette="limited",allowedCols=cols,value=cols[i])
          )
      )
    })
  })
  
  ## OUT: out_display -------------------------------------------------------
  ## prints general variables for debugging
  
  output$out_display <- renderPrint({
    
    cat(paste0(
      "Duration Start: ",input$in_duration_date_start,"\n",
      "Duration End: ",input$in_duration_date_end,"\n",
      "Num Tracks: ",input$in_num_tracks,"\n",
      "Available colour: ",input$in_track_colour_available,"\n",
      "Weekend colour: ",input$in_track_colour_weekend,"\n",
      "Legend position: ",input$in_legend_position,"\n",
      "Legend justification: ",input$in_legend_justification,"\n",
      "Legend direction: ",input$in_legend_direction,"\n",
      "Theme font size: ",input$in_themefontsize,"\n",
      "Date font size: ",input$in_datefontsize,"\n",
      "Month font size: ",input$in_monthfontsize,"\n",
      "Legend font size: ",input$in_legendfontsize))
    
  })
  
  ## OUT: out_display2 --------------------------------------------------------
  ## prints track variables
  
  output$out_display2 <- renderPrint({
    
    num_tracks <- input$in_num_tracks
    for(i in 1:num_tracks)
    {
      cat(paste0("track name: ",eval(parse(text=paste0("input$in_track_name_",i))),"\n",
                 "start: ",eval(parse(text=paste0("input$in_track_date_start_",i))),"\n",
                 "end: ",eval(parse(text=paste0("input$in_track_date_end_",i))),"\n",
                 "colour: ",eval(parse(text=paste0("input$in_track_colour_",i))),"\n\n"
      ))
    }
    
  })
  
  ## RFN: fn_plot -----------------------------------------------------------
  ## core plotting function, returns a ggplot object
  
  fn_plot <- reactive({
    
    shiny::req(input$in_num_tracks)
    shiny::req(input$in_duration_date_start)
    shiny::req(input$in_duration_date_end)
    shiny::req(input$in_legend_position)
    shiny::req(input$in_legend_justification)
    shiny::req(input$in_legend_direction)
    #shiny::req(input$in_themefontsize)
    #shiny::req(input$in_datefontsize)
    #shiny::req(input$in_monthfontsize)
    #shiny::req(input$in_legendfontsize)
    
    if(is.na(input$in_themefontsize)) {themefontsize <- 10}else{themefontsize <- input$in_themefontsize}
    
    if(as.Date(input$in_duration_date_start)>as.Date(input$in_duration_date_end)) stop("End duration date must be later than start duration date.")
    
    # prepare dates
    dfr <- data.frame(date=seq(as.Date(input$in_duration_date_start),as.Date(input$in_duration_date_end),by=1))
    dfr$day <- factor(strftime(dfr$date,format="%a"),levels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
    dfr$week <- factor(strftime(dfr$date,format="%V"))
    dfr$month <- strftime(dfr$date,format="%B")
    dfr$month <- factor(dfr$month,levels=unique(dfr$month))
    dfr$ddate <- factor(strftime(dfr$date,format="%d"))
    
    #add tracks
    dfr$comment <- "Available"
    
    # validate tracks
    num_tracks <- input$in_num_tracks
    for(i in 1:num_tracks){
      shiny::req(eval(parse(text=paste0("input$in_track_date_start_",i))))
      shiny::req(eval(parse(text=paste0("input$in_track_date_end_",i))))
      shiny::req(eval(parse(text=paste0("input$in_track_name_",i))))
      shiny::req(eval(parse(text=paste0("input$in_track_colour_",i))))
    }
    
    num_tracks <- input$in_num_tracks
    track_cols <- vector(length=num_tracks)
    tracks <- vector(length=num_tracks)
    for(i in 1:num_tracks)
    {
      temp_start_date <- as.Date(as.character(eval(parse(text=paste0("input$in_track_date_start_",i)))))
      temp_end_date <- as.Date(as.character(eval(parse(text=paste0("input$in_track_date_end_",i)))))
      temp_track_name <- as.character(eval(parse(text=paste0("input$in_track_name_",i))))
      temp_track_col <- as.character(eval(parse(text=paste0("input$in_track_colour_",i))))
      
      if(!is.null(temp_start_date) && !is.null(temp_end_date))
      {
        if(temp_start_date > temp_end_date) stop("End track duration date must be later than start track duration date.")
      }

      dfr$comment[dfr$date>=temp_start_date & dfr$date<=temp_end_date] <- temp_track_name
      tracks[i] <- temp_track_name
      track_cols[i] <- temp_track_col
    }
    
    dfr$comment[dfr$day=="Sat" | dfr$day=="Sun"] <- "Weekend"
    
    # create order factor
    fc <- vector(mode="character")
    if("Available" %in% unique(dfr$comment)) fc <- c(fc,"Available")
    fc <- c(fc,tracks)
    if("Weekend" %in% unique(dfr$comment)) fc <- c(fc,"Weekend")
    dfr$comment <- factor(dfr$comment,levels=fc)
    
    # prepare colours
    all_cols <- c(input$in_track_colour_available,track_cols,input$in_track_colour_weekend)
    
    # plot
    p <- ggplot(dfr,aes(x=week,y=day))+
      geom_tile(aes(fill=comment))+
      geom_text(aes(label=ddate),size=input$in_datefontsize)+
      scale_fill_manual(values=all_cols)+
      facet_grid(~month,scales="free",space="free")+
      labs(x="Week",y="")+
      theme_bw(base_size=themefontsize)+
      theme(legend.title=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank(),
            axis.ticks=element_blank(),
            axis.title=element_text(colour="grey30"),
            strip.background=element_blank(),
            strip.text=element_text(size=input$in_monthfontsize),
            legend.position=input$in_legend_position,
            legend.justification=input$in_legend_justification,
            legend.direction=input$in_legend_direction,
            legend.text=element_text(size=input$in_legendfontsize),
            legend.key.size=unit(0.3,"cm"),
            legend.spacing.x=unit(0.2,"cm"))
    
    # add number of weeks to reactive value
    store$week <- length(levels(dfr$week))
    
    return(p)
  })
  
  
  ## OUT: out_plot ------------------------------------------------------------
  ## plots figure
  
  output$out_plot <- renderImage({
    
    shiny::req(fn_plot())
    shiny::req(input$in_height)
    #shiny::req(input$in_width)
    shiny::req(input$in_res)
    shiny::req(input$in_scale)
    
    height <- as.numeric(input$in_height)
    width <- as.numeric(input$in_width)
    res <- as.numeric(input$in_res)
    
    if(is.na(width)) width <- (store$week*1)+1
    
    p <- fn_plot()
    ggsave("calendar_plot.png",p,height=height,width=width,units="cm",dpi=res,type="cairo")
    
    return(list(src="calendar_plot.png",
                contentType="image/png",
                width=round(((width*res)/2.54)*input$in_scale,0),
                height=round(((height*res)/2.54)*input$in_scale,0),
                alt="calendar_plot"))
  })
  
  # FN: fn_downloadplotname ----------------------------------------------------
  # creates filename for download plot
  
  fn_downloadplotname <- function()
  {
    return(paste0("calendar_plot.",input$in_format))
  }
  
  ## FN: fn_downloadplot -------------------------------------------------
  ## function to download plot  
  
  fn_downloadplot <- function(){
    shiny::req(fn_plot())
    shiny::req(input$in_height)
    shiny::req(input$in_res)
    shiny::req(input$in_scale)
    
    height <- as.numeric(input$in_height)
    width <- as.numeric(input$in_width)
    res <- as.numeric(input$in_res)
    format <- input$in_format
    
    if(is.na(width)) width <- (store$week*1)+1
    
    p <- fn_plot()
    if(format=="pdf" | format=="svg"){
      ggsave(fn_downloadplotname(),p,height=height,width=width,units="cm",dpi=res)
      #embed_fonts(fn_downloadplotname())
    }else{
      ggsave(fn_downloadplotname(),p,height=height,width=width,units="cm",dpi=res,type="cairo")
      
    }
  }
  
  ## DHL: btn_downloadplot ----------------------------------------------------
  ## download handler for downloading plot
  
  output$btn_downloadplot <- downloadHandler(
    filename=fn_downloadplotname,
    content=function(file) {
      fn_downloadplot()
      file.copy(fn_downloadplotname(),file,overwrite=T)
    }
  )
  
}

shinyApp(ui=ui, server=server)
