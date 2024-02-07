

# This code produces the Scholar Googler page
# This is a simple Shiny app that generates a word cloud based on the titles
# of articles in a Google Scholar profile
library(shiny)
library(htmlwidgets)
library(webshot2)
library(moroncolours)
library(scholar)
library(wordcloud2)
library(tm)
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
#' @import dplyr stringr
#'
#' @examples
scholarGoggler <- function(...){
 ui <- fluidPage(
  # Application title
  titlePanel(HTML("<a href=http://scholargoggler.com>Scholar Goggler</a>"),windowTitle="Where scholars go to google themselves"),
  tags$head(
    tags$style(HTML('div#wcLabel {display: none;}'))
  ),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("id",
                "Enter Scholar ID or full URL:",
                value=default_id),
      actionButton("button","Cloud Me"),
      h5("Example:"),
      h5("https://scholar.google.com/citations?user=THIS_PART_IS_THE_ID&hl=en"),
      radioButtons("cloud_type","Visualization type",choices=c("Titles","Journals"),inline=T),
      sliderInput("range",
                  "Start and end date of articles to use",
                  min = 1969,
                  max = 2025,
                  value = c(1999,2024),sep=""),
      #textInput("bgcolour","Background",value="black"),
      radioButtons("bgcolour","Background",
                choices = c("white","greyish","twilight","black","transparent"),
                selected="black",inline=TRUE),
      selectInput("fancycolour","Use fancy colour scheme",
                   choices=c("white-on-black",get_colour_names()),
                   selected="Hokusai1"),
      checkboxInput("dillute","Transparency based on frequency?",value=TRUE),
      selectInput("font_family","Font",choices=c("Helvetica","AppleGothic","Optima","Luminari",
                                                  "Courier","Klee","Arial","Tahoma",
                                                  "Times","Trebuchet MS","Verdana"),selected="Optima"),
      selectInput("shape","Cloud Shape",
                   choices=c("circle",
                             "cardioid",
                             "diamond",
                             "triangle-forward",
                             "triangle",
                             "pentagon",
                             "star"),selected="pentagon"),
      radioButtons("rotation","Limit Rotation",choices=c("No Rotation",
                                                         "Any Rotation",
                                                         "45 degrees","90 degrees"),inline=T,selected="45 degrees"),
      sliderInput("ellipticity","Ellipticity (higher is rounder)",value=1,min=0.2,max=1),
      sliderInput("zoomout","Zoom",value=0.92,min=0.1,max=1),
      sliderInput("minfreq","Minimum word frequency",min=1,max=8,value=1),
      sliderInput("maxfreq","Maximum word frequency",min=10,max=300,value=40),
      textInput("dropwords","Remove these words",value="however,also"),
      textInput("keep_uppercase","Repair uppercase",value="DNA,RNA,cDNA"),
      textInput("depluralize","Depluralize and collapse these words",
               	value="tumors,patients,cells")
      #submitButton(text = "Cloud me")

    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id="main",selected = "About",
        tabPanel("Word Cloud", wordcloud2Output("cloud",width = "1000px", height = "1000px"),downloadButton("Download PNG",outputId= "savecloud")),
        tabPanel("Alt Text", textOutput("alt")),
        tabPanel("Tabular result",tableOutput("tabular")),
        tabPanel("About",
        tags$div(HTML('<h2>Recent improvements:</></h2><h4><ul>
<li>New gallery and FAQ <a href=https://scholargoggler.com/faq>here</a></li>
<li>Other flavours of the app for PubMed and Semantic Scholar available <a href=https://scholargoggler.com>here</a></li>
<li>Specify any plural words you want grouped with their singular equivalent</li>
<li>More responsive interface</li><li>Broaden date ranges to allow older papers to be included</li><li>Restrict the maximum size of high frequency words to avoid them dominating</li>
<li>New fancy colour schemes</li><li>Specify certain words to remain uppercase (or mixed case) using the Repair uppercase option</li> <li>Allows user to provide either the entire URL or scholar ID</li><li>Use the transparency option to fade the colour for lower frequency words</li>
</ul>Small donations to help defer costs and ensure the sustainability of these apps are appreciated!</h3><form action="https://www.paypal.com/donate" method="post" target="_top">
<input type="hidden" name="hosted_button_id" value="WRRPVG5NDUXXN" />
<input type="image" src="https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif" border="0" name="submit" title="PayPal - The safer, easier way to pay online!" alt="Donate with PayPal button" />
<img alt="" border="0" src="https://www.paypal.com/en_CA/i/scr/pixel.gif" width="5" height="5" />
</form><br>')))
      )
    )
  ),
  hr(),
  h5("About this page:"),
  print("Created and maintained by Ryan Morin (rdmorin@sfu.ca); @morinryan morinryan.bsky.social")

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  #observeEvent(input$id,{
  #  updateTabsetPanel(session,inputId="main",selected="About")
  #})
  check_id = reactive({
    this_id = input$id
  #  updateTabsetPanel(session,inputId="main",selected="About")
  })
  count_journals = reactive({
    #pubz=get_publications(input$id)
    clean_id = input$id
    clean_id= stringr::str_remove(input$id,".+user=")
    pubz=scholar::get_publications(clean_id) %>% dplyr::filter(year > input$range[1], year < input$range[2])
    df = data.frame(journal=pubz$journal)
    df = mutate(df,journal = str_remove(journal,",.+")) %>%
      mutate(journal=str_to_lower(journal))
    df_count = df %>% dplyr::group_by(journal) %>% tally()
    colnames(df_count)=c("word","freq")
    df_count = dplyr::filter(df_count,freq >= input$minfreq) %>% arrange()
    df_count = dplyr::mutate(df_count,freq =ifelse( freq> input$maxfreq,input$maxfreq,freq))
    return(df_count)
  })
  count_title_words = reactive({
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
    pubz=get_publications(clean_id) %>% dplyr::filter(year > input$range[1], year < input$range[2])
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
    df = dplyr::filter(df,freq >= input$minfreq)
    df = dplyr::filter(df,grepl("...",word))
    df = dplyr::filter(df,word != "the")
        #deal with redundancy
    df = mutate(df,word_family=ifelse(word %in% names(family_words),family_words[word],word))
    df = group_by(df,word_family) %>% summarise(freq=sum(freq)) %>%
      mutate(word = word_family) %>%
      dplyr::select(word,freq)
    df = arrange(df,desc(freq))
    df = mutate(df,freq =ifelse( freq> input$maxfreq,input$maxfreq,freq))

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
    updateTabsetPanel(session,"main",selected="About")
    output$scholar_name = renderText({
      get_scholar()$name
    })
    output$scholar_h = renderText({

      get_scholar()$h_index
    })
  })



  make_cloud = reactive({

    if(input$cloud_type == "Titles"){
      minsize = input$zoomout
      ai = count_title_words()
      if(!input$dropwords==""){
        removeme = unlist(strsplit(input$dropwords,","))
        ai = dplyr::filter(ai,!word %in% removeme)
      }
      colour = ""
      bgcolour = input$bgcolour
      if(bgcolour == "greyish"){
        bgcolour = "#ebe8ed"
      }
      if(bgcolour == "twilight"){
        bgcolour = "#302f2e"
      }
      if(bgcolour == "transparent"){
        bgcolour = "#66000000"
      }

      if(input$fancycolour=="white-on-black"){
            colour = "white"
            bgcolour = "black"
            ai$colour = colour
            if(input$dillute){
                highest = max(ai$freq)
                nr = nrow(ai)
                ai$row = c(nr:1)
                ai = mutate(ai,percentile = row/nr) %>%
                mutate(colour=adjustcolor_v( colour, alpha.f = percentile))
            }
            colour = unname(ai$colour)
      }else{
        cols = scholargoggler::get_all_colours()
        if(input$fancycolour %in% names(cols)){
          colour = sample(cols[[input$fancycolour]],nrow(ai),replace=T)
        }else{
          colour = sample(get_moron_pal(input$fancycolour),nrow(ai),replace=T)
        }
        ai$colour = colour
        if(input$dillute){
          highest = max(ai$freq)
          nr = nrow(ai)
          ai$row = c(nr:1)
          ai = mutate(ai,percentile = row/nr) %>%
            mutate(colour=adjustcolor_v( colour, alpha.f = percentile))
        }
        colour = unname(ai$colour)
        output$tabular = renderTable(ai,rownames = F)
      }

      if(input$rotation=="Any Rotation"){
        wordcloud2(ai,
                   fontFamily = input$font_family,
                   backgroundColor=bgcolour,
                   color=colour,
                   shape=input$shape,size=minsize,
                   ellipticity=input$ellipticity)
      }else if(input$rotation=="No Rotation"){
        wordcloud2(ai,
                   fontFamily = input$font_family,
                   backgroundColor=bgcolour,
                   color=colour,
                   shape=input$shape,size=minsize,maxRotation = 0,
                   minRotation=0,ellipticity=input$ellipticity)
      }else if(input$rotation=="45 degrees"){
        wordcloud2(ai,
                   fontFamily = input$font_family,
                   backgroundColor=bgcolour,
                   color=colour,
                   shape=input$shape,size=minsize,minRotation=pi/4.01,maxRotation=pi/3.99,ellipticity=input$ellipticity)
      }else if(input$rotation=="90 degrees"){
        wordcloud2(ai,
                   fontFamily = input$font_family,
                   backgroundColor=bgcolour,
                   color=colour,
                   shape=input$shape,size=minsize,minRotation=pi/2.01,maxRotation=pi/1.99,ellipticity=input$ellipticity)
      }

    }else if(input$cloud_type=="Journals"){

      ai = count_journals()
      minsize = input$zoomout
      print(head(ai))
      if(!input$dropwords==""){
        removeme = unlist(strsplit(input$dropwords,","))
        ai = dplyr::filter(ai,!word %in% removeme)
      }
      colour = ""
      bgcolour = input$bgcolour
      if(bgcolour == "greyish"){
        bgcolour = "#ebe8ed"
      }
      if(bgcolour == "twilight"){
        bgcolour = "#302f2e"
      }
      if(bgcolour == "transparent"){
        bgcolour = "#66000000"
      }
      if(input$fancycolour=="black-on-white"){
        colour = "white"
        bgcolour = "black"
        ai$colour = colour
        if(input$dillute){
          highest = max(ai$freq)
          nr = nrow(ai)
          ai$row = c(nr:1)
          ai = mutate(ai,percentile = row/nr) %>%
            mutate(colour=adjustcolor_v( colour, alpha.f = percentile))
        }
        colour = unname(ai$colour)
        ai$colour = colour
      }else{
        cols = scholargoggler::get_all_colours()
        if(input$fancycolour %in% names(cols)){
          colour = sample(cols[[input$fancycolour]],nrow(ai),replace=T)
        }else{
          colour = sample(get_moron_pal(input$fancycolour),nrow(ai),replace=T)
        }
        ai$colour = colour
        if(input$dillute){
          highest = max(ai$freq)
          nr = nrow(ai)
          ai$row = c(nr:1)
          ai = mutate(ai,percentile = row/nr) %>%
            mutate(colour=adjustcolor_v( colour, alpha.f = percentile))
        }
        colour = unname(ai$colour)
        output$tabular = renderTable(ai,rownames = F)
      }

      if(input$rotation=="Any Rotation"){
        wordcloud2(ai,
                   fontFamily = input$font_family,
                   backgroundColor=bgcolour,
                   color=colour,
                   shape=input$shape,size=minsize,
                   ellipticity=input$ellipticity)
      }else if(input$rotation=="No Rotation"){
        wordcloud2(ai,
                   fontFamily = input$font_family,
                   backgroundColor=bgcolour,
                   color=colour,
                   shape=input$shape,size=minsize,maxRotation = 0,
                   minRotation=0,ellipticity=input$ellipticity)
      }else if(input$rotation=="45 degrees"){
        wordcloud2(ai,
                   fontFamily = input$font_family,
                   backgroundColor=bgcolour,
                   color=colour,
                   shape=input$shape,size=minsize,minRotation=pi/4.01,maxRotation=pi/3.99,ellipticity=input$ellipticity)
      }else if(input$rotation=="90 degrees"){
        wordcloud2(ai,
                   fontFamily = input$font_family,
                   backgroundColor=bgcolour,
                   color=colour,
                   shape=input$shape,size=minsize,minRotation=pi/2.01,maxRotation=pi/1.99,ellipticity=input$ellipticity)
      }
    }
  })
#  observeEvent({
  observeEvent(input$button,{
    updateTabsetPanel(session,"main",selected="About")
    output$cloud = renderWordcloud2({
      make_cloud()
    })
   output$savecloud <- downloadHandler(
               filename = paste("wordcloud", '.png', sep=''),
               content = function(file) {
                 owd <- setwd(tempdir())
                 on.exit(setwd(owd))
                 saveWidget(make_cloud(), "temp.html", selfcontained = FALSE)
                 webshot("temp.html", delay =5, file = file, cliprect = "viewport",vwidth=1000,vheight=1000)
    })
    output$alt <- renderText({
    if(input$cloud_type == "Titles"){
      ai = count_title_words()
    }else if(input$cloud_type=="Journals"){
      ai = count_journals()
    }
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
