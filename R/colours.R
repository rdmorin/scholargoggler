library(moroncolours)
library(MetBrewer)
library(wesanderson)

tron = c("#A95253", "#FC5531", "#7E1609", "#F08D4E", "#778F56", "#376484", "#BA7653", "#DB7974", "#5D6490", "#406D8D", "#FDB401", "#FC9245", "#BF5D17", "#8DDCEA", "#C37062")
autumn = c("#c7ba08", "#C19303", "#B8AD24", "#EB8E4F", "#F10406", "#F29461", "#A9BB38", "#AB0315", "#E33D52", "#DFCF3F", "#BE0302", "#EC4720", "#EB813E", "#F08705", "#DB2158", "#c7bd38")
barbie = c("#D9757E", "#E58F8A", "#D49391", "#809C67", "#E3A5B3", "#AA7BA1", "#6A98B7", "#64629A","#B85D4A", "#F2C1B9", "#A25039", "#5E73A9", "#D45975", "#A1E5FD", "#E1A76A", "#689C9B")
dune = c("#754F3C", "#442936", "#2D4247", "#516347", "#513F31", "#2C353F" ,"#68533B", "#39384B","#845C34", "#B06637", "#8EA0AC" ,"#544C42", "#3B1601", "#EACD81", "#6A5558", "#3D3F2B")
austinpowers = c("#45568A", "#57617E", "#624648", "#936461", "#5AAEB2", "#776546", "#534575", "#974D56","#743438", "#894F4E", "#9E688E", "#685164")
vangogh = c("#429AB3", "#4C8494", "#BB963B", "#923350", "#F9DD28", "#E5D986", "#D66541", "#91AB37", "#3660A2", "#DFC35D", "#DF4823", "#508823")
bobmarley = c("#F53442", "#8E2E39", "#B3BE66", "#EF2B3B", "#B7CF91", "#D65653", "#FA325B", "#E03851", "#902018", "#71B172", "#8B3946", "#76AC5E", "#944906", "#9F6763", "#7CBF8C", "#FEE578")
superape = c("#5C7D5E", "#FED534", "#6A7486", "#60AF3F", "#FEE81D", "#C35C62", "#E2EDC5", "#6E6D83","#7A677A", "#6D9499", "#C57202", "#E1DAB6", "#F6D615", "#AD5949", "#C44F45" ,"#BB6106")
ponyo = c("#B47169", "#A17697", "#98776A", "#9CA562", "#F6CEAA", "#87A377", "#83CFD3", "#D6D582", "#8A903D", "#AC5357", "#3F9E5B", "#E98575")
picasso = c("#6A4C60", "#73A893", "#4D7350", "#4E5970", "#3D6229", "#5D8547", "#83404D", "#954E3C", "#4C7C66", "#D44D23", "#CDAA61", "#B43D1B", "#D95A34", "#5D7577", "#97620D", "#623027")
Park_Guell = c("#C691C0", "#EAC5A8", "#6F485A", "#83923A", "#963E12", "#4761D1", "#67458C", "#E55436", "#699BAB","#D97A5F", "#62815C", "#D7B15C", "#816B37", "#A97F42", "#F58C88", "#734346")
Colorado = c("#C67C59", "#7D692E", "#C4C433", "#686E38", "#968B3D", "#828210", "#3C536A", "#C8B537", "#0F437F","#DD6F3D" ,"#285684", "#AB661E", "#7E4725", "#741915", "#DAA03D", "#8E5336")
Pongal = c("#9D0F01","#CC1700","#E5651A","#F39001","#98B412","#B6D433")
BoldDecision = c("#1a3a46", "#386857", "#738564", "#e89d56", "#CD541D", "#922637")
SoftSpectrum = c("#6EA56A" ,"#8CB35C", "#CCD069", "#F68F4C", "#C1CE84", "#61A57D", "#852413",
                 "#E6E47F" ,"#2B6287","#84C6C3", "#896A2F", "#C64232", "#F9B58F", "#65B59B", "#517C85",
                 "#ECE59F", "#75C2A5", "#E56E2B")
HotCold = c("#1984c5", "#22a7f0", "#63bff0", "#a7d5ed", "#e1a692", "#de6e56", "#e14b31", "#c23728")

BottleRocket1 = wesanderson::wes_palettes$BottleRocket1
Zissou1Continuous = wesanderson::wes_palettes$Zissou1Continuous
Cross = MetBrewer::met.brewer("Cross",return_hex = T)
VanGogh2 = MetBrewer::met.brewer("VanGogh2",return_hex = T)
Homer1 = MetBrewer::met.brewer("Homer1",return_hex = T)
Hokusai1 = MetBrewer::met.brewer("Hokusai1",return_hex = T)
Tiepolo = MetBrewer::met.brewer("Tiepolo",return_hex = T)
Redon = MetBrewer::met.brewer("Redon",return_hex = T)
Renoir = MetBrewer::met.brewer("Renoir",return_hex = T)
Morgenstern = MetBrewer::met.brewer("Morgenstern",return_hex = T)
OKeeffe2 = MetBrewer::met.brewer("OKeeffe2",return_hex = T)
Tsimshian = MetBrewer::met.brewer("Tsimshian",return_hex = T)
Tam = MetBrewer::met.brewer("Tam",return_hex = T)
Klimt = MetBrewer::met.brewer("Klimt",return_hex = T)
Moreau = MetBrewer::met.brewer("Moreau",return_hex = T)
Pillement = MetBrewer::met.brewer("Pillement",return_hex = T)
VanGogh3 = MetBrewer::met.brewer("VanGogh3",return_hex = T)
LedZep = moroncolours::get_moron_pal("ledzep")
Ponyo = moroncolours::get_moron_pal("ponyo")
Morinlab = moroncolours::get_moron_pal("clinical","all")
Reds = c("#FF331F","#CC220F","#991A0C","#FF6655","#660F08")
#Colours for DLBCL genetic subgroups (LymphGen)
DLBCL = c("EZB-MYC" = "#52000F","EZB" = "#721F0F","EZB-COMP" = "#C7371A","ST2" = "#C41230","ST2-COMP" = "#EC3251","MCD" = "#3B5FAC","MCD-COMP" = "#6787CB","BN2" =  "#7F3293","BN2-COMP" = "#A949C1","N1" = "#55B55E","N1-COMP" = "#7FC787","A53" = "#5b6d8a")

cols = list("Reds"=Reds,
            "HotCold"=HotCold,
            "BobMarley"=bobmarley,
            "SoftSpectrum"=SoftSpectrum,
            "BoldDecision" = BoldDecision,
            "Pongal" = Pongal,
            "DLBCL"=DLBCL,
            "BottleRocket1"=BottleRocket1,
            "SuperApe"=superape,
            "Ponyo"=Ponyo,
            "picasso"=picasso,
            "vangogh"=vangogh,
            "austinpowers"=austinpowers,
            "dune"=dune,
            "barbie"=barbie,
            "Zissou1Continuous"=Zissou1Continuous,
            "Park_Guell"=Park_Guell,
            "Colorado"=Colorado,
            "autumn"=autumn,
            "tron"=tron,
            VanGogh2=VanGogh2,
            Homer1=Homer1,
            Hokusai1=Hokusai1,
            Tiepolo=Tiepolo,
            Renoir=Renoir,
            LedZep=LedZep,
            Morgenstern=Morgenstern,
            OKeeffe2=OKeeffe2,
            Tsimshian=Tsimshian,
            Tam=Tam,
            Cross=Cross,
            Klimt=Klimt,
            Moreau=Moreau,
            Pillement=Pillement,
            VanGogh3=VanGogh3)

#' Get all available colour palettes
#'
#' @return a named list containing all available colour palettes
#' @export
#'
#' @examples
get_all_colours = function(){
  return(cols)
}

#' Convert a coolors.co URL into a vector of hex codes
#'
#' @param cooler_url
#'
#' @return a vector of hex codes
#' @export
#'
#' @examples
coolor_colour = function(cooler_url){
  #e.g.
  # https://coolors.co/87bcde-805e73-4e4d5c-2d4654-243b4a
  if(!grepl("^https://coolors.co/",cooler_url)){
    stop("this only works with a coolors.co palette URL")
  }
  cooler_url = str_remove(cooler_url,"https://coolors.co/")
  chunks = unlist(str_split(cooler_url,"-"))
  chunks = unlist(lapply(chunks,function(x){paste0("#",x)}))
  #sanity check that the result makes sense (i.e. it wasn't a garbled URL)
  print(paste0("COOLER:",paste0(chunks,collapse = ",")))
  stopifnot(length(chunks) == 5,all(str_length(chunks)==7))
  return(chunks)
}

#' Get just the names of all available colour palettes
#'
#' @return A vector of palette names that are available
#' @export
#'
#' @examples
get_colour_names = function(){
    available_cols =c("Reds","HotCold","SoftSpectrum","BoldDecision","SgtPepper","Pongal","Zissou1Continuous","BottleRocket1",
                      "LedZep","SuperApe","BobMarley","frozen","barbie","dune",
			                "austinpowers","Ponyo","tron","Tsimshian","Tam",
                      "warhol","vangogh","VanGogh2","Cross","Pillement","VanGogh3",
                      "picasso","Homer1","Hokusai1","Tiepolo","Renoir","Morgenstern",
                      "doctorwho","HandE","Park_Guell","Colorado","autumn","Klimt",
			                "Moreau","DLBCL")
  return(available_cols[order(available_cols)])
}



#' Get an advertisement to maybe show somewhere
#'
#' @return HTML
#' @export
#'
#' @examples
get_ad = function(){
  adv = '<H3>Do you love your cloud enough to wear it?</H3>
  <a href="https://www.jdoqocy.com/click-101118824-13080011" target="_top">
  <img src="https://www.ftjcfx.com/image-101118824-13080011" width="300" height="250" alt="Custom T-Shirts By DesignAShirt" border="0"/></a>'
  return(adv)
}
