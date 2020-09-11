library(RISmed)

# Annotations for dataset files at the dataset level
meta_basic <- function(file, title = " ", desc = " ",
                       type = c("obo:IAO_0000100", "dcat:Distribution", "obiwan:nPOD_Data_Item"),
                       format = "<https://www.iana.org/assignments/media-types/text/tab-separated-values>",
                       version = 1) {
  # calculated:
  info <- file.info(file)
  features <- strsplit(readLines(file, n = 1L), split = "\t")[[1]]
  numfeatures <- sum(!grepl("^ID$|_SD$|_SEM$", features))
  dt <- fread(file)
  n <- sum(!is.na(dt[, -c("ID")])) # data points is calculated as total of non-NA and non-ID values
  parts <- paste(paste0(":", features), collapse = ", ")
  created <- shQuote(strftime(info$mtime, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "cmd")
  modified <- shQuote(strftime(info$ctime, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "cmd")

  out <- c(
    paste("dct:title", shQuote(title, "cmd")),
    paste("dct:description", shQuote(desc, "cmd")),
    paste("a", paste(type, collapse = ", ")),
    paste("dcat:mediaType", format),
    paste("dct:hasPart", parts),
    paste("obiwan:numFeatures", numfeatures),
    paste("obiwan:dataPoints", n),
    paste("obiwan:version", version),
    paste("dct:created", created),
    paste("dct:modified", modified)
  )
  out <- paste(out, ";")
}

meta_prov <- function(file, contributed = NULL, from = NULL, process = "obiwan:", provenance = " ") {
  # calculated:
  pmid <- regmatches(file, regexpr("(?<=PMID)[0-9]+", file, perl = T))
  if(length(pmid)) pinfo <- pInfo(pmid) else pinfo <- list(date = NULL, ref = NULL, authors = shQuote("nPOD Core", "cmd"))

  out <- c(
    paste("obiwan:pubSource", paste(paste0("pubmed:", pmid), paste0("doi:", pinfo$doi), sep = ", ")),
    paste("obiwan:pubmedDate", pinfo$date),
    paste("obiwan:refSource", pinfo$ref),
    paste("obiwan:namedAuthor", pinfo$authors),
    paste("pav:contributedOn", contributed),
    paste("pav:importedFrom", from),
    paste("prov:wasGeneratedBy", process),
    paste("dct:provenance", shQuote(provenance, "cmd"))
  )
  out <- paste(out, ";")
}

meta_license <- function() "dct:license <https://creativecommons.org/licenses/by/4.0/> ;"

meta_app <- function(file) {
  out <- c(
    paste("obiwan:hasTheme")
  )
  out <- paste(out, ";")
  out
}

meta_method <- function() paste("obiwan:assayType", "obo:", ";")

meta_optional <- function() {

}

meta_dataset <- function(file, dir = NULL) {
  out <- c(meta_basic(file), meta_app(file), meta_method(), meta_prov(file), meta_license())
  writeLines(out, paste0(dir, gsub(".tsv", ".txt", file)))
}

p_datasets <- grep("^PMID.*tsv$", list.files(), val = T)
sapply(datasets, function(f) meta_dataset(f, dir = "Meta/"))


# Modified --------------------------------------------------------------------------------------------#

# getwd() = Meta
d_meta <- grep(".txt$", list.files(), val = T)
tracking <- fread("Tracking.csv")
for(d in d_meta) {
  file <- paste0("../", sub("txt", "tsv", d))
  info <- file.info(file)
  features <- strsplit(readLines(file, n = 1L), split = "\t")[[1]]
  numfeatures <- sum(!grepl("^ID$|_SD$|_SEM$", features))
  dt <- fread(file)
  n <- sum(!is.na(dt[, -c("ID")])) # data points is calculated as total of non-NA and non-ID values
  parts <- paste(paste0(":", features), collapse = ", ")
  modified <- shQuote(strftime(info$ctime, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "cmd")
  trackindex <- match(sub(".txt", "", d, fixed = T), tracking$DatasetID)
  prov <- shQuote(paste("Dataset was extracted from source with", tracking$ProcessTransformDetails[trackindex]), "cmd")
  proc <- paste0("obiwan:p", tracking$ProcessSummaryCode[trackindex])
  supp <- paste0("<", tracking$Process_pURL[trackindex], ">")
  condate <- shQuote(tracking$contributedOn[trackindex], "cmd")
  importl <- paste0("<", strsplit(tracking$importedFrom[trackindex], " , ")[[1]], ">", collapse = ", ")
  download <- paste0("<", gsub("https://osf.io/", "https://osf.io/download/", tracking$pURL[trackindex]), ">")
  uri <- paste0("<", tracking$pURL[trackindex], ">")
  dname <- shQuote(gsub(".txt", "", d, fixed = T), "cmd")
  title <- shQuote(tracking$Title[trackindex], "cmd")
  desc <- shQuote(tracking$Description[trackindex], "cmd")
  theme <- tracking$Theme[trackindex]

  m <- readLines(d)
  m <- c(paste("rdfs:label", dname, ";"), m)
  m <- m[!grepl("dct:hasPart", m)]
  m[grep("dct:title", m)] <-  paste("dct:title", title, ";")
  m[grep("dct:description", m)] <-  paste("dct:description", desc, ";")
  m[grep("^a", m)] <- "a obiwan:nPOD_Dataset, dcat:Distribution ;"
  m[grep("obiwan:numFeatures", m)] <- paste("obiwan:numFeatures", numfeatures, ";")
  m[grep("obibwan:dataPoints", m)] <- paste("obiwan:dataPoints", n, ";")
  m[grep("obiwan:hasTheme", m)] <-  paste("dcat:theme", theme, ";")
  m[grep("dct:modified", m)] <- paste("dct:modified", modified, ";")
  m[grep("pav:contributedOn", m)] <- paste("pav:contributedOn", condate, ";")
  m[grep("pav:importedFrom", m)] <- paste("pav:importedFrom", importl, ";")
  m[grep("prov:wasGeneratedBy", m)] <- paste("prov:hadGeneration", supp, ";")
  m[grep("dct:provenance", m)] <- paste("dct:provenance", prov, ";")
  m <- c(m, paste(supp, "prov:activity", proc, ";"))
  m <- c(m, paste("dcat:downloadURL", download, "."))
  m <- paste0("  ", m)
  m <- c(uri, m)

  writeLines(m, paste0("update/", d))
}


for(d in d_meta) {
  m <- readLines(d)
  m <- gsub("pav:contributedOn", "obiwan:authorContributedOn", m)
  writeLines(m, paste0("update2/", d))
}


prefixes <- c("@prefix : <http://purl.org/net/obi-wan#> .",
              "@prefix obo: <http://purl.obolibrary.org/obo/> .",
              "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .",
              "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .",
              "@prefix dct: <http://purl.org/dc/terms/> .",
              "@prefix prov: <http://www.w3.org/ns/prov#> .",
              "@prefix pav: <http://purl.org/pav/> .",
              "@prefix dcat: <http://www.w3.org/ns/dcat#> .",
              "@prefix pubmed: <https://pubmed.ncbi.nlm.nih.gov/> .",
              ""
              )

for(d in d_meta) {
  m <- readLines(d)
  m <- gsub("obiwan:publication", "obiwan:pubSource", m)
  m <- c(m[1:length(m)-1], "  :fairly :accessible ;", m[length(m)])
  m <- gsub("obiwan:", ":", m)
  doi <- sub(" ;", "> ;", sub("doi:", "<https://doi.org/", grep("doi:", m, val = T)))
  m[grepl("doi:", m)] <- doi
  m <- c(prefixes, m)
  activity <- grep("prov:activity", m, value = T)
  m <- m[!grepl("prov:activity", m)]
  # m <- c(m, "", sub(";", ".", activity))
  writeLines(m, paste0("update3/", d))
}




# Test

library(rdflib)
rdf <- rdf()
for(i in list.files()) {
  print(i)
  rdf_parse(i, format = "turtle", rdf = rdf)
}

for(i in list.files()) file.copy(from = i, to = paste0("update4/", sub("txt", "ttl", i)))

rdf_free(rdf, rm = TRUE)
options(rdf_print_format = "turtle")
sparql <- 'PREFIX pav: <http://github.pav/>
SELECT ?s ?o
WHERE { ?s pav:importedFrom ?o }'

rdf_query(rdf, sparql)

# Helpers -------------------------------------------------------------------------------------------- #

pInfo <- function(pmid) {
  pub <- RISmed::EUtilsGet(pmid, type = "efetch", db = "pubmed")
  authors <- Author(pub)[[1]]
  initials <- gsub("([A-Z])", "\\1.", authors$Initials, perl = T)
  authors <- shQuote(paste(authors$LastName, initials, sep = ", "), "cmd")
  authors <- paste(authors, collapse = ", ")
  date <-  shQuote(strftime(paste0(YearPubmed(pub), "-", MonthPubmed(pub), "-", DayPubmed(pub)), "%Y-%m-%d"), "cmd")
  ref <- RefSource(pub)
  ref <- if(!is.na(ref)) shQuote(ref, "cmd") else NULL
  doi <- ELocationID(pub)
  return(list(authors = authors, date = date, ref = ref, doi = doi))
}
