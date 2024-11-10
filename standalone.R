

# This code produces the Scholar Goggler page
# This is a simple Shiny app that generates a word cloud based on the titles
# of articles in a Google Scholar profile
suppressPackageStartupMessages(require(readr))
suppressPackageStartupMessages(require(tidyverse))
suppressPackageStartupMessages(require(htmlwidgets))
suppressPackageStartupMessages(require(webshot2))
suppressPackageStartupMessages(require(moroncolours))
suppressPackageStartupMessages(require(scholar))
suppressPackageStartupMessages(require(wordcloud2))
suppressPackageStartupMessages(require(tm))
suppressPackageStartupMessages(require(MetBrewer))
require(wesanderson)
suppressPackageStartupMessages(require(optparse))
suppressPackageStartupMessages(require(scholargoggler))

#some global variables and settings
repair_words = c("EZH2","MYC","T-cell",
                 "B-cell","HIV-1","UVA",
                 "UVB",
                 "COVID-19","BCL2",
                 "microRNA","miRNA",
                 "mRNA","DLBCL","FL","siRNA")
deps = c("alterations","methods")
default_id = "IDau0mkAAAAJ"
adjustcolor_v = Vectorize( adjustcolor )
year_min = 1969
year_max = 2025
colour_options=c("white-on-black",get_colour_names())

font_choices=c("Helvetica","AppleGothic","Optima","Luminari",
          "Courier","Klee","Arial","Tahoma",
          "Times","Trebuchet MS","Verdana")

rotation_choices=c("No Rotation",
          "A bit of rotation",
          "Ridiculous Rotation",
          "45 degrees",
          "90 degrees")

shape_choices=c("circle",
          "diamond","sicklecell","hyperbolic",
          "triangle-forward",
          "triangle",
          "pentagon",
          "star","brain","droplet")




option_list = list(
  make_option(c("-s", "--scholar_id"), action="store", type="character", default=NULL, help="Google Scholar ID, e.g IDau0mkAAAAJ"),
  make_option(c("-f", "--first_name"), action="store", type="character", default=NULL, help="Scholar first name, e.g. Ryan"),
  make_option(c("-l", "--last_name"), action="store", type="character", default=NULL,  help="Scholar last name, e.g. Morin"),
  make_option(c("-c", "--colour_scheme"), action="store", type="character", 
              default=NULL, help=paste(colour_options,collapse=",")),
  make_option(c("--bg_colour"), action="store", type="character", default=NULL,  help="Background (transparent,black or white)"),
  make_option(c("--shape"), action="store", type="character", default=NULL,  
              help="Cloud shape, e.g. circle, diamond, triangle, pentagon"),
  make_option(c("--rotation"), action="store", type="character", default=NULL,  
              help="Rotation constraints, e.g. none, some, 45, 90, ridiculous"),
  make_option(c("--font_family"), action="store", type="character", default=NULL,  
              help=paste(font_choices,collapse=",")),
  make_option("--year_min", action="store", type="integer", default=1980, help="Cutoff for oldest articles"),
  make_option("--year_max", action="store", type="integer", default=2025, help="Cutoff for newest articles"),
  make_option("--zoomout", action="store", type="numeric", default=0.9, help="Reduce to zoom out, increase to zoom in")
)

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);
if(is.null(opt$bg_colour)){
  opt$bg_colour = sample(c("black","white","transparent"),1)
}
if(is.null(opt$shape)){
  opt$shape = sample(shape_choices,1)
}
if(is.null(opt$font_family)){
  opt$font_family = sample(font_choices,1)
}
if(is.null(opt$colour_scheme)){
  opt$colour_scheme = sample(colour_options,1)
}
if(is.null(opt$rotation)){
  opt$rotation = sample(c("some","none","ridiculous","45","90"),1)
}
print(opt)
scholar_cloud <- function(scholar_id=NULL,
                       last_name=NULL,
                       first_name=NULL,
                       year_min,
                       year_max,
                       zoomout = 0.9,
                       bgcolour="black",
                       colour_scheme,
                       dillute=TRUE,
                       font_family="Optima",
                       shape="circle",
                       ellipticity = 1,
                       min_freq=1,
                       max_freq=35,
                       repair_case=repair_words,
                       depluralize=deps,
                       dropwords = "some",
                       rotation
                       ){
  print(paste("LAST:",last_name,"FIRST:",first_name,"ID:",scholar_id))
    if(is.null(scholar_id)){
      if(is.null(last_name) & is.null(first_name)){
        stop("Must provide first/last name if ID is not provided")
      }
      scholar_id = get_scholar_id(last_name = last_name,first_name=first_name)
      scholar_name = paste(first_name,last_name)
      print(paste("NAME:",scholar_name))
    }else{
      print("GOT HERE, ID provided")
      scholar_obj = scholar::get_profile(scholar_id)
      scholar_name = str_replace_all(scholar_obj$name,"\\s+","_")
    }

    ai = count_title_words(scholar_id=scholar_id,
                           depluralize,
                           year_min,
                           year_max,
                           min_freq,
                           max_freq)
    #remove any dropwords specified explicitly  
    ai = dplyr::filter(ai,!word %in% dropwords)
    message("making cloud")
    widget = make_cloud(ai,bgcolour,colour_scheme,dillute,font_family,shape,ellipticity,zoomout,rotation)
    print(widget)
    
    save_cloud(widget,scholar_name,shape,colour_scheme,font_family,zoomout,bgcolour,rotation,ai)
}


get_scholar = function(first_name,last_name){
    clean_id = get_scholar_id(last_name = last_name,first_name=first_name)
  scholar_obj = scholar::get_profile(clean_id)
  message(paste("SCHOLAR:",scholar_obj$id,Sys.time()))
  message(paste("NAME:",scholar_obj$name))
  return(scholar_obj)
}



count_title_words = function(scholar_id, depluralize, year_min, year_max,min_freq, max_freq){
  if(!missing(depluralize)){
    
    family_words = c()
    for(dep in depluralize){
      trunk = str_remove(dep,"s$")
      family_words[trunk] = trunk
      family_words[dep] = trunk
    }
  }
  
  
  clean_id = scholar_id
  clean_id= str_remove(scholar_id,".+user=")
  
  pubz=scholar::get_publications(clean_id) %>% 
    filter(!is.na(year))

  pubz = pubz %>% 
    dplyr::filter(year > year_min, year <= year_max)
  
  
  titles=pubz$title
  
  docs <- Corpus(VectorSource(titles))
  docs <- docs %>%
    tm_map(removePunctuation,preserve_intra_word_dashes=TRUE) %>%
    tm_map(stripWhitespace)
  docs <- suppressWarnings(tm_map(docs, content_transformer(tolower)))
  docs <- suppressWarnings(tm_map(docs, removeWords, stopwords("english")))
  dtm <- TermDocumentMatrix(docs)
  matrix <- as.matrix(dtm)
  words <- sort(rowSums(matrix),decreasing=TRUE)
  df <- data.frame(word = names(words),freq=words)
  df = dplyr::filter(df,freq >= min_freq)
  df = dplyr::filter(df,grepl("...",word))
  df = dplyr::filter(df,word != "the")
  if(!missing(depluralize)){
     #deal with redundancy
      df = mutate(df,word_family=ifelse(word %in% names(family_words),family_words[word],word))
      df = group_by(df,word_family) %>% summarise(freq=sum(freq)) %>%
          mutate(word = word_family) %>%
          dplyr::select(word,freq)
  }
  df = arrange(df,desc(freq))
  df = mutate(df,freq =ifelse( freq> max_freq,max_freq,freq))
  
  #keep_uppercase = ""
  #if(!input$keep_uppercase==""){
  #  all_keep = paste(input$keep_uppercase,terms,sep=",")
  #  keep_uppercase = unlist(strsplit(all_keep,","))
  #  names(keep_uppercase) = tolower(keep_uppercase)
  #  df = mutate(df,word=ifelse(word %in% names(keep_uppercase),keep_uppercase[word],word))
  #}
  return(df)
}


make_cloud = function(ai,bgcolour,colour_scheme,dillute,font_family,shape,ellipticity,zoomout,rotation){
  
  
  
  #set up background colour
  if(bgcolour == "greyish"){
    bgcolour = "#ebe8ed"
  }else if(bgcolour == "twilight"){
    bgcolour = "#302f2e"
  }else if(bgcolour == "transparent"){
    bgcolour = "#66000000"
  }else if(bgcolour=="black"){
    #leave it
  }else{
    bgcolour = "white"
  }
  #set up text colour
  if(colour_scheme=="white-on-black"){
    colour = "white"
    bgcolour = "black"
    ai$colour = colour
  }else{
    cols = scholargoggler::get_all_colours()
    if(colour_scheme %in% names(cols)){
      print(colour_scheme)
      colour = sample(cols[[colour_scheme]],nrow(ai),replace=T)
    }else{
      print(colour_scheme)
      colour = sample(get_moron_pal(colour_scheme),nrow(ai),replace=T)
    }
    ai$colour = colour
  }
  #dillute if necessary
  if(dillute){
    highest = max(ai$freq)
    nr = nrow(ai)
    ai$row = c(nr:1)
    ai = mutate(ai,percentile = row/nr) %>%
      mutate(colour=adjustcolor_v( colour, alpha.f = percentile))
    colour = unname(ai$colour)
  }
  print(paste("WORDS:",nrow(ai)))
  print(head(ai))
  print(tail(ai))
  
  if(rotation=="ridiculous"){
    rotateRatio = 1
    wordcloud2(ai,
             fontFamily = font_family,
             backgroundColor=bgcolour,
             color=colour,
             size = zoomout,
             rotateRatio = rotateRatio,
             shape=shape,
             ellipticity=ellipticity)
  }else if(rotation=="some"){
    wordcloud2(ai,
               fontFamily = font_family,
               backgroundColor=bgcolour,
               color=colour,
               size = zoomout,
               minRotation=pi/5,
               maxRotation=pi/7,
               shape=shape,
               ellipticity=ellipticity)
  }else if(rotation=="none"){
    wordcloud2(ai,
               fontFamily = font_family,
               backgroundColor=bgcolour,
               color=colour,
               size = zoomout,
               minRotation=0,
               maxRotation=0,
               shape=shape,
               ellipticity=ellipticity)
  }else if(rotation=="45"){
    wordcloud2(ai,
               fontFamily = font_family,
               backgroundColor=bgcolour,
               color=colour,
               size = zoomout,
               minRotation=pi/4.01,
               maxRotation=pi/3.99,
               shape=shape,
               ellipticity=ellipticity)
  }else if(rotation=="90"){
    wordcloud2(ai,
               fontFamily = font_family,
               backgroundColor=bgcolour,
               color=colour,
               size = zoomout,
               minRotation=pi/2.01,
               maxRotation=pi/1.99,
               shape=shape,
               ellipticity=ellipticity)
  }
  
}  

save_alt <- function(ai,shape,bgcolour,file_name,n_words=30){
  if(grepl("triangle",shape)){
    shape_description = "triangle"
  }else if(shape == "sicklecell"){
    shape_description = "crescent moon"
  }else if (shape == "hyperbolic"){
    shape_description = "hyperbola"
  }else{
    shape_description = shape
  }
  alttext = paste0("Cloud of words in various colors and sizes on a ",bgcolour, 
                   " background, arranged in the shape of a ",
                   shape_description,
                   ". Some of the largest words include ")
  
  if(n_words> nrow(ai)){
    n_words=nrow(ai)
  }
  for(i in c(1:n_words)){
    
    alttext = paste(alttext,ai[i,"word"])
  }
  alttext = paste0(alttext,"\n")
  cat(alttext,file=file_name,append = F)
}

save_cloud <- function(cloud_widget,prefix,shape,colour_scheme,font_family,zoomout,bgcolour,rotation,ai){
  message("saving cloud")
  settings = paste(prefix,shape,colour_scheme,font_family,zoomout,bgcolour,rotation,sep="_")
  filename = paste0("/Users/rmorin/git/scholargoggler/clouds/",settings,"_wordcloud", '.png')
  saveWidget(cloud_widget, "temp.html", selfcontained = FALSE)
  #webshot("temp.html", delay =3, file = filename, cliprect = c(100,100,800,800),
  #        vwidth=1000,vheight=1000)
  webshot("temp.html", delay =3, file = filename, cliprect = "viewport",vwidth=1000,vheight=1000)
  save_alt(ai,shape,bgcolour,paste0(filename,"_alt.txt"))
}

scholar_cloud(scholar_id=opt$scholar_id,
              last_name=opt$last_name,
              first_name=opt$first_name,
              year_min=opt$year_min,
              year_max=opt$year_max,
              colour_scheme = opt$colour_scheme,
              shape=opt$shape,
              font_family=opt$font_family,
              zoomout = opt$zoomout,
              bgcolour=opt$bg_colour,
              rotation=opt$rotation)

    
