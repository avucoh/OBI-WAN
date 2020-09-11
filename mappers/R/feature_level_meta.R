# Annotations for dataset files at the feature level

metadata <- fread("Metadata.tsv")
setkey(metadata, Source, Variable)

meta_feat_cols <- c("rdfs:label", "dct:description", "rdfs:type", "obiwan:unit", "dct:isPartOf",
               "obiwan:cellTissue", "obiwan:cellTissueContext", "obiwan:donor",
               "obiwan:name", "obiwan:inAppData", "obiwan:inAppName",
               "obiwan:valueLabel", "obiwan:valueEncoding", "obiwan:status", "obiwan:featureSD", "obiwan:featureSEM",
               "obiwan:completeLinkage", "obiwan:dataGrade", "obiwan:fromMethod")

# "obiwan:hasDerivative", "obiwan:derivedFrom",
# "obiwan:authorNote", "obiwan:curatorNote",

datasets <- grep(".*tsv$", list.files(), val = T)
px <- c("PMID26935967_1.tsv", "PMID28877242_1.tsv")

meta_feat_tab <- function(file, px, gx = list.files("GeneAnnotation")) {
  source <- gsub("_.*", "", file)
  tab <- fread(file)
  features <- names(tab)

  ids <- tab[, sapply(.SD, function(x) paste(ID[which(!is.na(x))], collapse = "|")), .SDcols = features ]
  x <- data.table(col = seq_along(features))
  x[, (meta_feat_cols) := ""]
  x[, `dct:isPartOf` :=  file]
  x[, c("obiwan:name", "obiwan:inAppName") := features ]
  x[, `obiwan:donor` := ids]
  x[, `obiwan:featureSD` := sapply(features, function(x) any(grepl(paste0(x, "_SD"), features)))]
  x[, `obiwan:featureSEM` := sapply(features, function(x) any(grepl(paste0(x, "_SEM"), features)))]
  x[, c("obiwan:completeLinkage",  "obiwan:inAppData") := T]
  x[, `obiwan:status` := "obiwan:available"]
  x[, `obiwan:dataGrade` := "obiwan:published"]

  # Transferred column data
  mapped <- list(desc = metadata[.(source, features), Description],
                 valtype = metadata[.(source, features), Value],
                 unit = metadata[.(source, features), LabelType],
                 celltissue = metadata[.(source, features), CellTissue],
                 celltissuecon = metadata[.(source, features), CellTissueContext],
                 dataitem = metadata[.(source, features), DataItem],
                 efo = metadata[.(source, features), EFOTerm])
  for(nm in names(mapped)) mapped[[nm]][is.na(mapped[[nm]])] <- ""
  x[, `rdfs:type` := mapped$dataitem]
  x[, `dct:description` := mapped$desc]
  x[, `obiwan:valueLabel` := mapped$valtype]
  x[, `obiwan:unit` := mapped$unit]
  x[, `obiwan:cellTissue` := mapped$celltissue]
  x[, `obiwan:cellTissueContext` := mapped$celltissuecon ]
  x[, `obiwan:xfactor` := mapped$efo ]

  # Optional annotations
  if(file %in% px) x[, `obiwan:isUP` := c("", paste0("http://purl.uniprot.org/uniprot/", features[-1]))]
  if(file %in% gx) {
    geneids <- readLines(paste0("GeneAnnotation/", file, "_GeneID.txt"))
    geneids[!is.na(geneids)] <- paste0("", geneids[!is.na(geneids)])
    geneids[is.na(geneids)] <- ""
    x[, `obiwan:isGene` := c("",geneids)]
  }
  write.table(x, paste0("Meta2/", file), sep = "\t", quote = F, row.names = F)
}


meta_feature <- function(file, dir = NULL) {
  out <- c(meta_basic(file), meta_app(), meta_method(), meta_prov(file), meta_license())
  writeLines(out, paste0(dir, gsub(".tsv", ".txt", file)))
}

for(d in datasets) x <- try(meta_feat_tab(d, px = px))
which(sapply(x, class) == "try-error")

#

ht <- grep("tsv", setdiff(list.files(), list.files("Meta2/Temp")), val = T)


# ------------------------------------------------------------------------------------------------------------------#

cl <- fread("CL.csv.gz")
obi <- fread("OBI.csv.gz")
stato <- fread("STATO.csv.gz")
efo <- fread("EFO.csv.gz")
uber <- fread("UBERON.csv.gz")
uo <- fread("UO.csv.gz")


# <https://osf.io/download/abcde#col=1>
#  :featureID  "BloodVessels.head" ;
#  rdfs:label  "blood vessels count" ;
#  dct:description  "Mean count of blood...(trunc) ;
#  a  obo:IAO_0000109, :nPOD_Feature ;
#  :unit  obo:UO_0000189 ;
#  :unitLabel  "count" ;
#  obiwan:cellTissue  obo:UBERON_0001069 ;
#  obiwan:cellTissueContext  obo:UBERON_0001264 ;
#  :hasCase  cid:6089 , cid:6126 , cid:6134 ;
#  :caseLinkage :complete_cases ;
#  :dataGrade  :published ;
#  obiwan:withSEM  <https://osf.io/download/abcde#col=2> ;
#  :xfactor <efo> ;
#  dct:isPartOf  <https://osf.io/download/abcde> .
#

fs <- list.files()
file <- fs[1]
fprefix <- c("@prefix : <http://purl.org/net/obi-wan#> .",
          "@prefix obo: <http://purl.obolibrary.org/obo/> .",
          "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .",
          "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .",
          "@prefix dct: <http://purl.org/dc/terms/> .",
          "@prefix cid: <https://npoddatashare.coh.org/labkey/study/nPOD%20DataShare/participant.view?participantId=> .",
          "@prefix efo: <http://www.ebi.ac.uk/efo/> ."
          )

#  "PMID26358584_1.tsv" pancreas weight
# check  "PMID30713110_1.tsv" SD no cases
# split up datasets for PMID27506584 -- modify dataset level files
file <- list.files()[63]
F2RDF(file)
F2RDF <- function(file) {
  f1 <- fread(file, na.strings = "")
  for(col in 1:ncol(f1)) f1[[col]][f1[[col]] == ""] <- NA
  f <- split(f1, by = "col")
  trackindex <- match(sub(".tsv", "", file, fixed = T), tracking$DatasetID)
  uri <- paste0("<", sub("https://osf.io/", "https://osf.io/download/", tracking$pURL[trackindex]), ">")
  doc <- c(fprefix, "", "")
  for(i in 1:length(f)) {
    uri_hash <- sub("/>", paste0("#col=", f[[i]]$col, ">"), uri)
    triple <- c(
      paste(":featureID", shQuote(f[[i]]$`obiwan:name`, "cmd")),
      paste("rdfs:label", shQuote(f[[i]]$`rdfs:label`, "cmd")),
      paste("dct:description", shQuote(f[[i]]$`dct:description`, "cmd")),
      if(grepl("obiwan",  f[[i]]$`rdfs:type`)) {
        paste("a", sub("obiwan", "", f[[i]]$`rdfs:type`), ",", ":nPOD_Feature")
      } else {
        id <- if(!is.na(match(f[[i]]$`rdfs:type`, obi$`Preferred Label`))) obi$`Class ID`[match(f[[i]]$`rdfs:type`, obi$`Preferred Label`)] else stato$`Class ID`[match(f[[i]]$`rdfs:type`, stato$`Preferred Label`)]
        paste("a", sub("http://purl.obolibrary.org/obo/", "obo:", id), ",", ":nPOD_Feature")
      },
      if(!is.na(f[[i]]$`obiwan:unit`)) {
        id <- if(grepl("obiwan", f[[i]]$`obiwan:unit`)) sub("obiwan", "", f[[i]]$`obiwan:unit`) else sub("http://purl.obolibrary.org/obo/", "obo:", uo$`Class ID`[match(f[[i]]$`obiwan:unit`, uo$`Preferred Label`)])
        paste(":unit", id)
        },
      if(!is.na(f[[i]]$`obiwan:valueLabel`)) paste(":unitLabel", shQuote(f[[i]]$`obiwan:valueLabel`, "cmd")),
      if(!is.na(f[[i]]$`obiwan:cellTissue`)) {
        id <- if(!is.na(match(f[[i]]$`obiwan:cellTissue`, cl$`Preferred Label`))) cl$`Class ID`[match(f[[i]]$`obiwan:cellTissue`, cl$`Preferred Label`)] else uber$`Class ID`[match(f[[i]]$`obiwan:cellTissue`, uber$`Preferred Label`)]
        paste(":cellTissue", sub("http://purl.obolibrary.org/obo/", "obo:", id))
      },
      if(!is.na(f[[i]]$`obiwan:cellTissueContext`)) paste(":cellTissueContext", sub("http://purl.obolibrary.org/obo/", "obo:", uber$`Class ID`[match(f[[i]]$`obiwan:cellTissueContext`, uber$`Preferred Label`)])),
      paste(":hasCase", paste(paste0("cid:", strsplit(f[[i]]$`obiwan:donor`, "|", fixed = T)[[1]]), collapse = " , ")),
      ":caseLinkage :complete_cases",
      ":dataGrade :published",
      if(f[[i]]$`obiwan:featureSEM`) paste(":withSEM", sub("[0-9](?=>)", which(paste0(f[[i]]$`obiwan:name`, "_SEM") == f1$`obiwan:name`), uri_hash, perl = T)),
      if(f[[i]]$`obiwan:featureSD`) paste(":withSD", sub("[0-9](?=>)", which(paste0(f[[i]]$`obiwan:name`, "_SD") == f1$`obiwan:name`), uri_hash, perl = T)),
      if(!is.na(f[[i]]$`obiwan:xfactor`)) paste(":xfactor", sub("http://www.ebi.ac.uk/efo/", "efo:", efo$`Class ID`[match(f[[i]]$`obiwan:xfactor`, efo$`Preferred Label`)]))
    )
    triple <- c(paste(triple, ";"), paste("dct:isPartOf", uri, "."))
    triple <- paste("  ", triple)
    triple <- c(uri_hash, triple, "")
    doc <- append(doc, triple)
  }
  writeLines(doc, paste0("update/", sub("tsv", "ttl", file)))
}



hf <- hfs[7]
old <- fread(hf)
gene_anno <- readLines(paste0("GeneAnnotation/", sub(".tsv", "_GeneID.txt", hf, fixed = T)))
gene_anno <- as.integer(gene_anno)
gene_anno[!is.na(gene_anno)] <- paste0("<https://www.ncbi.nlm.nih.gov/gene/", gene_anno[!is.na(gene_anno)], ">")
old[, GeneID := c(NA, gene_anno)]
up_uri <- paste0("http://purl.uniprot.org/uniprot/", old$`rdfs:label`[-1])
old[, UP := c(NA, up_uri)]
fwrite(old, paste0("HT/", hf))


# "PMID26935967_1.tsv" "PMID26983959_1.tsv" "PMID28761107_1.tsv" "PMID28761107_2.tsv" "PMID28877242_1.tsv" "PMID29671030_1.tsv" "PMID31486854_1.tsv"
f2RDF(file = "PMID26983959_1.tsv",
      uri = "8kpn6",
      atype = ":log2_normalized_signal_intensity , :nPOD_Feature , :individual_level_datum",
      desc = "log2 75th percentile gene expression value with baseline transformation to the mean of the control samples",
      ct = "obo:UBERON_0001264", ctc = "obo:UBERON_0001264",
      meth = "<https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GPL14550>")


f2RDF(file = "PMID26935967_1.tsv",
       atype = ":log2_normalized_signal_intensity , :nPOD_Feature , :individual_level_datum",
      protein = T,
       uri = "ptxre",
       desc = "log2 normalized protein expression value",
       ct = "obo:UBERON_0001069", ctc = "obo:UBERON_0001264",
       meth = "<https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4893790/#S2title>")


f2RDF(file = "PMID28761107_1.tsv",
      uri = "375wd",
      atype = ":dCt , :nPOD_Feature , :individual_level_datum",
      desc = "delta Ct microRNA expression value",
      ct = "obo:CL_0000815", ctc = "obo:UBERON_0000178", # regulatory T cell, blood
      meth = "<https://hypothes.is/a/wL551Je9EeqnRmtDLQubjg>")


f2RDF(file = "PMID28761107_2.tsv",
      uri = "9yefm",
       atype = ":dCt , :nPOD_Feature , :individual_level_datum",
       desc = "delta Ct microRNA expression value",
       ct = "obo:CL_0000815", ctc = "obo:UBERON_0002527",  # regulatory T cell, PLN
       meth = "<https://hypothes.is/a/wL551Je9EeqnRmtDLQubjg>")


f2RDF(file = "PMID28877242_1.tsv",
      protein = T,
      uri = "z4jef",
       atype = ":log2_normalized_signal_intensity , :nPOD_Feature , :individual_level_datum",
       desc = "log2 normalized protein expression value",
       ct = "obo:UBERON_0001264", ctc = "obo:UBERON_0001264",
       meth = "<https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5587329/#sec002title>")


f2RDF(file = "PMID29671030_1.tsv",
      uri = "r648s",
       atype = ":normalized_signal_intensity , :nPOD_Feature , :individual_level_datum",
       desc = "log2 normalized gene expression value",
       ct = "obo:UBERON_0001264", ctc = "obo:UBERON_0001264",
       meth = "<https://hypothes.is/a/SOHOEJh3EeqLvVs293K-Sg>")


f2RDF(file = "PMID31486854_1.tsv",
      uri = "vqyst",
       atype = ":dCt , :nPOD_Feature , :individual_level_datum",
       desc = "delta Ct gene expression value normalized to the housekeeping HPRT1 gene",
       ct = "obo:CL_0000499", ctc = "obo:UBERON_0002527", # stromal cell , PLN
       meth = "<https://hypothes.is/a/hSjm0JbgEeqsOQuby6wisg>")



f2RDF <- function(file, protein = F, uri, atype, desc, ct, ctc, meth) {
  f1 <- fread(file)
  features <- names(f1)
  uri <- paste0("<https://osf.io/download/", uri, "/>")
  GeneID <- readLines(paste0("GeneAnnotation/", sub(".tsv", "_GeneID.txt", file, fixed = T)))
  GeneID <- as.integer(GeneID)
  GeneID[!is.na(GeneID)] <- paste0("<https://www.ncbi.nlm.nih.gov/gene/", GeneID[!is.na(GeneID)], ">")
  GeneID <- c(NA, GeneID)
  CaseID <- c(sub("/>", paste0("#col=", 1, ">"), uri), # uri_frag
    paste(" ",
      c(paste(":featureID", shQuote("ID", "cmd")),
        paste("rdfs:label", shQuote("Case ID", "cmd")),
        paste("dct:description", shQuote("Case ID", "cmd")),
        "a :caseID , :nPOD_Feature , :individual_level_datum",
        paste(":hasCase", paste(paste0("cid:", f1$ID), collapse = " , ")),
        ":caseLinkage :complete_cases",
        ":dataGrade :published"),
      ";"),
    paste("  dct:isPartOf", uri, "."),
    ""
  )
  triples <- lapply(seq_along(features)[-1],
         function(i) {
          uri_frag <- sub("/>", paste0("#col=", i, ">"), uri)
          fdesc <- paste(features[i], desc)
          triple <- paste(" ",
            c(
              paste(":featureID", shQuote(features[i], "cmd")),
              paste("rdfs:label", shQuote(features[i], "cmd")),
              paste("dct:description", shQuote(fdesc, "cmd")),
              paste("a", atype),
              # paste(":unitLabel", shQuote(unitlabel, "cmd")),
              paste(":cellTissue", ct),
              paste(":cellTissueContext", ctc),
              paste(":hasCase", paste(paste0("cid:", f1$ID), collapse = " , ")),
              ":caseLinkage :complete_cases",
              ":dataGrade :published",
              paste(":usedMethod", meth),
              if(!is.na(GeneID[i])) paste(":GeneID", GeneID[i]),
              if(protein) paste(":UP", paste0("<http://purl.uniprot.org/uniprot/", features[i], ">"))
            ), ";"
          )
          triple <- c(triple, paste("  dct:isPartOf", uri, "."))
          triple <- c(uri_frag, triple, "")
        }
      )
  doc <- c(fprefix, "", "", CaseID, unlist(triples))
  writeLines(doc, paste0("update/", sub("tsv", "ttl", file)))
}


# fix to include individual_level_datum
for(f in list.files()) {
  ttl <- readLines(f)
  ttl[grep("individual_level_datum", ttl, fixed = T)] <- gsub("individual_level_datum", ":individual_level_datum", grep("individual_level_datum", ttl, value = T))
  writeLines(ttl, f)
}

for(f in list.files()) {
  ttl <- readLines(f)
  n <- grep("ordinal_measurement_datum", ttl)
  if(length(n)) {
    print(paste(f, "-", length(n)))
    ttl[n] <- gsub("ordinal_measurement_datum", "ordinal_datum", ttl[n])
  }
  writeLines(ttl, f)
}


for(f in list.files()) {
  ttl <- readLines(f)
  n <- grep("binary_measurement_datum", ttl)
  if(length(n)) {
    print(paste(f, "-", length(n)))
    ttl[n] <- gsub("binary_measurement_datum", "binary_datum", ttl[n])
  }
  writeLines(ttl, f)
}

# test
rdf <- rdf()
rdf_parse(paste0("update/", sub("tsv", "ttl", file)), format = "turtle", rdf = rdf)
prefix2 <- paste(gsub(" .", "", gsub("@prefix", "PREFIX", unique(c(prefixes, fprefix))), fixed = T), collapse = "\n")
sparql1 <- paste(prefix2,
                 "SELECT ?feature ?cl
                 WHERE {
                 ?feature dct:isPartOf <https://osf.io/download/h2e45/> .
                 ?feature :cellTissueContext ?cl
                 }")
cat(sparql1)


rdf_query(rdf, sparql1)
