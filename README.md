# Scholar Goggler
![Ryan Morin](img/banner.png)

This R package will run a standalone version of the [Shiny](https://shiny.posit.co/) app that generates word clouds derived from the publication list in any public Google Scholar profile using the [Scholar package](https://github.com/jkeirstead/scholar). The hosted version of this tool and its relatives that use PubMed or Semantic Scholar can all be accessed at the Scholar Goggler [homepage](https://scholargoggler.com/).

## Installation

### Option 1

First, install the package and its dependencies:
```
devtools::install_github("rdmorin/scholargoggler")
```
Then load the library and launch the Shiny app:
```
library(scholargoggler)
scholarGoggler()
```

### Option 2

Clone the repository:
```
git clone git@github.com:rdmorin/scholargoggler.git
```

Go to the newly created folder (or open the R project file) in Rstudio and run the code:

```
devtools::load_all()
scholarGoggler()
```

