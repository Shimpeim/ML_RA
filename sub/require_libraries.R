

packages <- c(
  'dplyr', # progress bar
  'plyr',  # progress bar
  'dplyr', # progress bar
  'tidyr',
#  'xlsx',
  'ggplot2',
  'gplots',
  'GMD',
  'pvclust',
  'reshape2',
  'pander',
  'matrixcalc' ,
  'kernlab' ,
  'e1071' ,
  'rpart',
  'rpart.plot',
  'Formula',
  'party',
  'partykit',
  'randomForest',
  'inTrees',
  'tree',
  'caret',
  'DMwR'
  #  'scaleboot' 
  #  'biomaRt'
) 

new.packages <-
  packages[!(packages %in% installed.packages())] 
# installed.packages() returns installed packages 

if(length(new.packages) > 0){ 
  install.packages(new.packages, repos='http://cran.us.r-project.org')
}
require('plyr')  # progress bar
require('dplyr') # progress bar
require('tidyr')
require('ggplot2')
require('gplots')
require('GMD')
require('pvclust')
require('reshape2')
require('pander')
require('matrixcalc')
require('kernlab')
require('e1071')
require('rpart')
require('rpart.plot')
require('party')
require('partykit')
require('randomForest')
#require('inTrees')
#require('tree')
require('caret')
require('DMwR')

if(Bibtex){
  write(toBibtex(citation()),file="CRAN")
  for(i in 1:length(packages)){
    write(toBibtex(citation(packages[i])),file=sprintf("../Biblio/%s%s.bib",packages[i],"_CRAN"))
  }
}

