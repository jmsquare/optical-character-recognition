library(xlsx)
library(tm)
library(qdap)
library(plyr)
library(stringr)
library(XML)


dossier <- 'D:/Users/JM/Desktop/project/' 

###########################################################################""
##### OCR (Optical character recognition) with tesseract###############
#####"READ.OCR Function converts image pdf input to text##################
#############################################################################"
read.ocr <- function(myfiles) {
	system2("pdftoppm",c("-f 1 -l 10 -r 600",shQuote(myfiles),shQuote(dossier)))
	ppm<-NULL
	ppm<-list.files(dossier,".ppm",full.names=TRUE)
	system2("convert",c(shQuote(ppm),shQuote(paste0(dossier,"/tiff.tif"))))
	tif<-NULL
	tif<-list.files(dossier,".tif",full.names=TRUE)
	system2("tesseract", c(shQuote(tif),shQuote(paste0(dossier,"/texte")),"-l fra"))
	txt<-NULL
	txt<-list.files(dossier,".txt",full.names=TRUE)
	txt<-readLines(txt,encoding = "UTF-8")
                    
	file.remove(ppm) # Remove files so that it does not take to much space memory
	file.remove(tif)
 	return(txt)
}
         

############################################################################
###Read Word document##############
#Read.docx Function converts .docx file to a list containing the content and metadata of the Word file########
####################################################################

read.docx <- function(myfiles) {
  	tmp<-NULL
  	xmlfile<-NULL
  	xmlfile1<-NULL
  	doc<-NULL
  	doc1<-NULL
  	nodeSet<-NULL
  	pvalues<-NULL
  	author<-NULL
  	list<-NULL
  	tmp <- tempfile()
  	if (!dir.create(tmp))
    	stop("Temporary directory could not be established.")
  	unzip(myfiles, exdir = tmp) # Unzip to temporary directory
  	xmlfile <- file.path(tmp, "word", "document.xml") # Path to xml document
  	xmlfile1 <- file.path(tmp, "docProps", "core.xml") # Path to xml document
  	xmlfile2<-file.path(tmp, "word", "media") # Path to xml document
  	doc <- xmlTreeParse(xmlfile, useInternalNodes=TRUE) # Import XML
  	doc1 <- xmlTreeParse(xmlfile1, useInternalNodes=TRUE) # Import XML
  	unlink(tmp, recursive = TRUE) # Delete unzipped files; no longer needed
 	nodeSet <- getNodeSet(doc, "//w:p") # Access all p-nodes in document
  	nodeSet1 <- getNodeSet(doc1,"//dc:creator") # Access all p-nodes in document
  	pvalues <- sapply(nodeSet, xmlValue) # Return their (textual) values
  	pvalues<-str_replace_all(pvalues, fixed("  "), "")
  	author<-sapply(nodeSet1, xmlValue) # IMPORTANT
  	photo<-ifelse(length(xmlfile2)==0,FALSE,TRUE)
  	list<-list(pvalues,author,photo)
  	return(list)
}