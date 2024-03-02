

# This code produces the Scholar Googler page
# This is a simple Shiny app that generates a word cloud based on the titles
# of articles in a Google Scholar profile
library(shiny)
library(shinyWidgets)
library(shinyDarkmode)
library(htmlwidgets)
library(d3wordcloud)
library(webshot2)
library(moroncolours)
library(scholar)
library(wordcloud2)
library(tm)
library(shinyjs)
library(MetBrewer)
library(wesanderson)

#some global variables and settings
terms = "EZH2,MYC,T-cell,B-cell,HIV-1,UVA,UVB,COVID-19,BCL2,microRNA,miRNA,mRNA,DLBCL,siRNA"
deps = "alterations,methods,genes,variants,lymphomas,mutations,sample,tumours,tumors"
default_id = "IDau0mkAAAAJ"
# load all the colour palettes. If you want to add new ones, incorporate them into colours.R



adjustcolor_v = Vectorize( adjustcolor )


#' Launches the Shiny app
#'
#' @return nothing
#' @export
#' @import dplyr stringr shiny wordcloud2 scholar tm htmlwidgets webshot2
#'
#' @examples
scholarGoggler2 <- function(...){
 ui <- fluidPage(
  # Application title
  titlePanel(title=div(img(src="https://github.com/rdmorin/scholargoggler/raw/main/img/goggler2.png",
                           "Scholar Goggler")),
             
             windowTitle="Where scholars go to google themselves"),
  tags$head(
    tags$style(HTML('div#wcLabel {display: none;}'))
  ),
  use_darkmode(),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("id",
                "Enter Scholar ID or full URL:",
                value=default_id),
      actionButton("button","Cloud Me"),
      checkboxInput("fix", "Redraw wordcloud each time",value=FALSE),
      prettySwitch("togglemode", "Enable Dark Mode", value = FALSE,
                   fill = TRUE, status = "info"),
      sliderInput("range",
                  "Date range to use",
                  min = 1969,
                  max = 2025,
                  value = c(1999,2024),sep=""),
      selectInput("fancycolour","Use fancy colour scheme",
                   choices=get_colour_names(),
                   selected="Hokusai1"),
      selectInput("font_family","Font",choices=c("Amatic SC",
                                                 "Antonio",
                                                 "Arial",
                                                 "Architects Daughter",
                                                 "Archivo Black",
                                                 "AppleGothic",
                                                 "Bai Jamjuree",
                                                 "Bebas Neue",
                                                 "Bebas Neue",
                                                 #"Bungee",
                                                 "Comic Neue",
                                                 "Courier",
                                                 "Creepster",
                                                 "Dosis",
                                                 #"Exo 2",
                                                 "Fjalla One",
                                                 "Gloria Hallelujah",
                                                 "Gruppo",
                                                 "Helvetica",
                                                 "Indie Flower",
                                                 "Josefin Sans",
                                                 "Kode Mono",
                                                 "Luminari",
                                                 "Lilita One",
                                                 "Luckiest Guy",
                                                 "Macondo",
                                                 "Montserrat",
                                                 "Madimi One",
                                                 "Open Sans",
                                                 "Oswald",
                                                 "Optima",
                                                 "Outfit",
                                                 "Protest Riot",
                                                 "Permanent Marker",
                                                 "Rubik Bubbles",
                                                 "Righteous",
                                                 "Single Day",
                                                 "Source Code Pro",
                                                 "Staatliches",
                                                 "Tahoma",
                                                 "Teko",
                                                 "Times","Ubuntu Condensed"
                                                 ),selected="Indie Flower"),
      selectInput("spiral","Layout Method",
                   choices=c("rectangular",
                             "archimedean"),selected="archimedean"),
      radioButtons("rotation","Limit Rotation",choices=c("No Rotation",
                                                         "A bit of rotation",
                                                         "Ridiculous Rotation",
                                                         "45 degrees","90 degrees"),inline=T,
                   selected="A bit of rotation"),
      sliderInput("padding","Padding (higher means more whitespace)",value=1,min=0,max=10),
      radioButtons("scaling","Size scaling method",inline=T,choices=c("linear","sqrt","log"),selected="linear"),
      #sliderInput("minfreq","Minimum word frequency",min=1,max=8,value=1),
      sliderInput("maxfreq","Maximum word frequency",min=10,max=300,value=40),
      textInput("dropwords","Remove these words",value="however,also"),
      textInput("keep_uppercase","Repair uppercase",value="DNA,RNA,cDNA"),
      textInput("depluralize","Depluralize and collapse these words",
               	value="tumors,patients,cells"),
      #radioButtons("cloud_type","Visualization type",choices=c("Titles","Journals"),inline=T),

    ),

    # Show a plot of the generated distribution
    mainPanel(
      tags$style(
        type = 'text/css',
        '.darkmode--activated button:not(.darkmode-toggle) {background: #bd9e68; border-color: #bd9e68; color: #fff; font-weight : 700;}'
      ),
      tabsetPanel(id="main",selected = "Word Cloud",
        tabPanel("Word Cloud", d3wordcloudOutput("cloud",width = "900px", height = "900px")),
        tabPanel("Alt Text", textOutput("alt")),
        tabPanel("Tabular result",tableOutput("tabular")),
        tabPanel("About",
        tags$div(HTML('
        
        <h4>
        Enjoying the app? Consider a small donation.</h3>
        <img src="https://github.com/rdmorin/scholargoggler/raw/main/img/goggler2.png">
        <form action="https://www.paypal.com/donate" method="post" target="_top">
<input type="hidden" name="hosted_button_id" value="WRRPVG5NDUXXN" />
<input type="image" src="https://raw.githubusercontent.com/rdmorin/scholargoggler/main/img/donate.png" border="0" name="submit" title="PayPal - The safer, easier way to pay online!" alt="Donate with PayPal button" />

</form><br>
<h2>News:</h2>
<ul>
<li>New gallery and FAQ <a href=https://scholargoggler.com/faq>here</a></li>
<li>Other flavours of the app for PubMed and Semantic Scholar available <a href=https://scholargoggler.com>here</a></li>
<li>Specify any plural words you want grouped with their singular equivalent</li>
<li>More responsive interface</li><li>Broaden date ranges to allow older papers to be included</li><li>Restrict the maximum size of high frequency words to avoid them dominating</li>
<li>New fancy colour schemes</li><li>Specify certain words to remain uppercase (or mixed case) using the Repair uppercase option</li> <li>Allows user to provide either the entire URL or scholar ID</li><li>Use the transparency option to fade the colour for lower frequency words</li>
</ul>')))
      )
    )
  ),
  hr(),
  h5("About this page:"),
  print("Created and maintained by Ryan Morin (rdmorin@sfu.ca); @morinryan morinryan.bsky.social")

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  darkmode(label = "⏳",
           mixColor = "#000")
  darkmode_toggle(inputid = 'togglemode')
  check_id = reactive({
    this_id = input$id
  })
  
  count_title_words = reactive({
    input$padding
    input$font_family
    input$scaling
    print("running count_title_words()")
    depluralize_words = c()
    if(!input$depluralize == ""){
      to_split = paste(input$depluralize,deps,sep=",")
      depluralize_words = unlist(strsplit(to_split,","))
    }
    family_words = c()
    for(dep in depluralize_words){
      trunk = str_remove(dep,"s$")
      family_words[trunk] = trunk
      family_words[dep] = trunk
    }
    clean_id = input$id
    clean_id= str_remove(input$id,".+user=")
    pubz=scholar::get_publications(clean_id) %>% dplyr::filter(year > input$range[1], year < input$range[2])
    message(paste("SCHOLAR:",clean_id,Sys.time()))
    cat(paste("SCHOLAR",clean_id,Sys.time(),input$keep_uppercase,"\n",sep="\t"),file="./log.tsv",append=T)
    message(paste("NAME:",get_scholar()$name))
    cat(paste("NAME",get_scholar()$name,Sys.time(),input$keep_uppercase,"\n",sep="\t"),file="./log.tsv",append=T)
    titles=pubz$title
    docs <- Corpus(VectorSource(titles))
    docs <- docs %>%
      #tm_map(removeNumbers) %>%
      tm_map(removePunctuation,preserve_intra_word_dashes=TRUE) %>%
      tm_map(stripWhitespace)
    docs <- suppressWarnings(tm_map(docs, content_transformer(tolower)))
    docs <- suppressWarnings(tm_map(docs, removeWords, stopwords("english")))
    dtm <- TermDocumentMatrix(docs)
    matrix <- as.matrix(dtm)
    words <- sort(rowSums(matrix),decreasing=TRUE)
    df <- data.frame(word = names(words),freq=words)
    
    df = dplyr::filter(df,grepl("...",word))
    df = dplyr::filter(df,word != "the")
        #deal with redundancy
    df = mutate(df,word_family=ifelse(word %in% names(family_words),family_words[word],word))
    df = group_by(df,word_family) %>% summarise(freq=sum(freq)) %>%
      mutate(word = word_family) %>%
      dplyr::select(word,freq)
    
    df = arrange(df,desc(freq))
    df = mutate(df,freq =ifelse( freq> input$maxfreq,input$maxfreq,freq))
    #df = filter(df,freq >= input$minfreq)
    #df = mutate(df,freq =ifelse( freq> input$maxfreq,input$maxfreq,freq))

    keep_uppercase = ""
    if(!input$keep_uppercase==""){
      all_keep = paste(input$keep_uppercase,terms,sep=",")
      keep_uppercase = unlist(strsplit(all_keep,","))
      names(keep_uppercase) = tolower(keep_uppercase)
      df = mutate(df,word=ifelse(word %in% names(keep_uppercase),keep_uppercase[word],word))
    }
    return(df)
  })

  get_scholar = reactive({
    clean_id = input$id
    clean_id= str_remove(input$id,".+user=")
    scholar::get_profile(clean_id)
  })
  observeEvent(input$button, {
    #updateTabsetPanel(session,"main",selected="About")
    output$scholar_name = renderText({
      get_scholar()$name
    })
    output$scholar_h = renderText({

      get_scholar()$h_index
    })
  })

  make_cloud = reactive({
    
      ai = count_title_words()
      print("ROWS:")
      print(nrow(ai))
      if(!input$dropwords==""){
        removeme = unlist(strsplit(input$dropwords,","))
        ai = dplyr::filter(ai,!word %in% removeme)
      }
      colour = ""
        cols = scholargoggler::get_all_colours()
        if(input$fancycolour %in% names(cols)){
          colour = sample(cols[[input$fancycolour]],nrow(ai),replace=T)
        }else{
          colour = sample(get_moron_pal(input$fancycolour),nrow(ai),replace=T)
        }
        ai$colour = colour

        colour = unname(ai$colour)
        output$tabular = renderTable(ai,rownames = F)
      print("running: $('#wordcloud svg g').empty()")
      runjs("$('#wordcloud svg g').empty()")   # <- clear the canvas before drawing new cloud
      if(input$rotation=="Ridiculous Rotation"){
        
        d3wordcloud(ai$word,ai$freq,
                    font=input$font_family,
                    colors=sample(colour,nrow(ai),replace=T),
                    spiral=input$spiral,padding=input$padding,size.scale=input$scaling)

      }else if(input$rotation=="A bit of rotation"){
        d3wordcloud(ai$word,ai$freq,
                   font = input$font_family,
                   colors=sample(colour,nrow(ai),replace=T),
                   rotate.min=-30,
                   rotate.max=0,
                   spiral=input$spiral,padding=input$padding,size.scale=input$scaling)
      }else if(input$rotation=="No Rotation"){
        d3wordcloud(ai$word,ai$freq,
                    font = input$font_family,
                    colors=sample(colour,nrow(ai),replace=T),
                    rotate.min=0,
                    rotate.max=0,
                    spiral=input$spiral,padding=input$padding,size.scale=input$scaling)
      }else if(input$rotation=="45 degrees"){
        d3wordcloud(ai$word,ai$freq,
                    font = input$font_family,
                    colors=sample(colour,nrow(ai),replace=T),
                    rotate.min=-45,
                    rotate.max=0,
                    spiral=input$spiral,padding=input$padding,size.scale=input$scaling)
      }else if(input$rotation=="90 degrees"){
        d3wordcloud(ai$word,ai$freq,
                    font = input$font_family,
                    #colors = c("#000000", "#0000FF", "#FF0000"),
                    colors=sample(colour,nrow(ai),replace=T),
                    rotate.min=-90,
                    rotate.max=0,
                    spiral=input$spiral,padding=input$padding,size.scale=input$scaling)
      }
    #}
  })
  observeEvent(input$button,{
    #updateTabsetPanel(session,"main",selected="About")
    output$cloud = renderD3wordcloud({
      
      #if(input$fix) 
      
      make_cloud()
    })
    output$alt <- renderText({

    ai = count_title_words()
    
    if(!input$dropwords==""){
      removeme = unlist(strsplit(input$dropwords,","))
      ai = dplyr::filter(ai,!word %in% removeme)
    }
    alttext = paste0("Cloud of words in various colors and sizes based on the Google Scholar profile for ",
                    get_scholar()$name ,". Some of the largest words include ")
    n_words=30
    if(n_words> nrow(ai)){
      n_words=nrow(ai)
    }
    for(i in c(1:n_words)){
      alttext = paste(alttext,ai[i,"word"])
    }
    alttext
    })
  })

 }
 # Run the application
 shinyApp(ui = ui, server = server)
}
