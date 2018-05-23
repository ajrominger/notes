library(XML)
library(rentrez)



# function to parse dna from ASN.1
dnaFromASN1 <- function(x) {
    x <- strsplit(x, '')[[1]]
    x[x == '0'] <- 'AA'
    x[x == '1'] <- 'AC'
    x[x == '2'] <- 'AG'
    x[x == '3'] <- 'AT'
    x[x == '4'] <- 'CA'
    x[x == '5'] <- 'CC'
    x[x == '6'] <- 'CG'
    x[x == '7'] <- 'CT'
    x[x == '8'] <- 'GA'
    x[x == '9'] <- 'GC'
    x[x == 'A'] <- 'GG'
    x[x == 'B'] <- 'GT'
    x[x == 'C'] <- 'TA'
    x[x == 'D'] <- 'TC'
    x[x == 'E'] <- 'TG'
    x[x == 'F'] <- 'TT'
    
    return(paste(x, collapse = ''))
}


cleanEntrez <- function(x) {
    # starting after `Seq-entry` we now need a shorter base path
    basePath <- 'Seq-entry_seq.Bioseq'
    
    # return everything we want
    c(
        genbank = as.character(x[paste(basePath, 
                                       'Bioseq_id', 'Seq-id', 'Seq-id_genbank', 
                                       'Textseq-id', 'Textseq-id_accession', 
                                       sep = '.')]),
        ncbi = as.character(x[paste(basePath, 
                                    'Bioseq_id', 'Seq-id', 'Seq-id_gi',
                                    sep = '.')]),
        taxon = as.character(x[paste(basePath,
                                     'Bioseq_descr', 'Seq-descr', 'Seqdesc', 
                                     'Seqdesc_source', 'BioSource', 'BioSource_org', 
                                     'Org-ref', 'Org-ref_taxname', 
                                     sep = '.')]),
        seqdesc_title = as.character(x[paste(basePath,
                                             'Bioseq_descr', 'Seq-descr', 'Seqdesc', 
                                             'Seqdesc_title', 
                                             sep = '.')]),
        biomol = as.character(x[paste(basePath,
                                      'Bioseq_descr', 'Seq-descr', 'Seqdesc', 
                                      'Seqdesc_molinfo', 'MolInfo', 'MolInfo_biomol', '', 
                                      'attrs.value', 
                                      sep = '.')]),
        seqfeat_comment = as.character(x[paste(basePath,
                                               'Bioseq_annot', 'Seq-annot', 
                                               'Seq-annot_data', 'Seq-annot_data_ftable', 
                                               'Seq-feat', 'Seq-feat_comment', 
                                               sep = '.')]),
        seq_length = as.integer(x[paste(basePath,
                                        'Bioseq_annot', 'Seq-annot', 'Seq-annot_data', 
                                        'Seq-annot_data_ftable', 'Seq-feat', 
                                        'Seq-feat_location', 'Seq-loc', 'Seq-loc_int', 
                                        'Seq-interval', 'Seq-interval_to', 
                                        sep = '.')]) -
            as.integer(x[paste(basePath,
                               'Bioseq_annot', 'Seq-annot', 'Seq-annot_data', 
                               'Seq-annot_data_ftable', 'Seq-feat', 
                               'Seq-feat_location', 'Seq-loc', 'Seq-loc_int', 
                               'Seq-interval', 'Seq-interval_from', 
                               sep = '.')]),
        primer_forward = as.character(x[paste(basePath,
                                              'Bioseq_descr', 'Seq-descr', 'Seqdesc', 
                                              'Seqdesc_source', 'BioSource', 
                                              'BioSource_pcr-primers', 'PCRReactionSet', 
                                              'PCRReaction', 'PCRReaction_forward', 
                                              'PCRPrimerSet', 'PCRPrimer', 'PCRPrimer_seq', 
                                              'PCRPrimerSeq', 
                                              sep = '.')]),
        primer_reverse = as.character(x[paste(basePath,
                                              'Bioseq_descr', 'Seq-descr', 'Seqdesc', 
                                              'Seqdesc_source', 'BioSource', 
                                              'BioSource_pcr-primers', 'PCRReactionSet', 
                                              'PCRReaction', 'PCRReaction_reverse', 
                                              'PCRPrimerSet', 'PCRPrimer', 'PCRPrimer_seq', 
                                              'PCRPrimerSeq', 
                                              sep = '.')]),
        lat_lon = as.character(x[grep('lat-lon', x) + 1]),
        geo_description = as.character(x[grep('country', x) + 1]),
        coll_date = as.character(x[grep('collection-date', x) + 1]),
        museum_UID = as.character(x[paste(basePath,
                                          'Bioseq_descr', 'Seq-descr', 'Seqdesc', 
                                          'Seqdesc_source', 'BioSource', 'BioSource_org', 
                                          'Org-ref', 'Org-ref_orgname', 'OrgName', 
                                          'OrgName_mod', 'OrgMod', 'OrgMod_subname', 
                                          sep = '.')]),
        other_db = as.character(x[paste(basePath,
                                        'Bioseq_descr', 'Seq-descr', 'Seqdesc', 
                                        'Seqdesc_source', 'BioSource', 'BioSource_org', 
                                        'Org-ref', 'Org-ref_db', 'Dbtag', 'Dbtag_db', 
                                        sep = '.')]),
        other_db_id = as.character(x[paste(basePath,
                                           'Bioseq_descr', 'Seq-descr', 'Seqdesc', 
                                           'Seqdesc_source', 'BioSource', 'BioSource_org', 
                                           'Org-ref', 'Org-ref_db', 'Dbtag', 'Dbtag_tag', 
                                           'Object-id', 'Object-id_str', 
                                           sep = '.')]),
        pubmed = as.character(x[paste(basePath,
                                      'Bioseq_descr', 'Seq-descr', 'Seqdesc', 'Seqdesc_pub', 
                                      'Pubdesc', 'Pubdesc_pub', 'Pub-equiv', 'Pub', 
                                      'Pub_article', 'Cit-art', 'Cit-art_ids', 
                                      'ArticleIdSet', 'ArticleId', 'ArticleId_pubmed', 
                                      'PubMedId', 
                                      sep = '.')]),
        doi = as.character(x[paste(basePath,
                                   'Bioseq_descr', 'Seq-descr', 'Seqdesc', 'Seqdesc_pub', 
                                   'Pubdesc', 'Pubdesc_pub', 'Pub-equiv', 'Pub', 
                                   'Pub_article', 'Cit-art', 'Cit-art_ids', 'ArticleIdSet', 
                                   'ArticleId', 'ArticleId_doi', 'DOI', 
                                   sep = '.')]),
        sequence = dnaFromASN1(x[paste(basePath,
                                       'Bioseq_inst', 'Seq-inst', 'Seq-inst_seq-data', 
                                       'Seq-data', 'Seq-data_ncbi2na', 'NCBI2na', 
                                       sep = '.')])
    )
}


# function to loop over records, extracting data from each
getGenBankSeq <- function(ids) {
    allRec <- entrez_fetch(db = 'nuccore', id = ids, 
                           rettype = 'native', retmode = 'xml', 
                           parsed = TRUE)
    allRec <- xmlToList(allRec)[[1]]
    
    o <- lapply(allRec, function(x) {
        cleanEntrez(unlist(x))
    })
    
    temp <- array(unlist(o), dim = c(length(o[[1]]), length(ids)))
    seqVec <- temp[nrow(temp), ]
    seqDF <- as.data.frame(t(temp[-nrow(temp), ]))
    names(seqDF) <- names(o[[1]])[-nrow(temp)]
    
    return(list(seq = seqVec, data = seqDF))
}


# retrieve Castilleja sequence IDs
castilleja <- entrez_search(db = 'nuccore', term = 'Castilleja[ORGN]', 
                            retmax = 1000)

# get clean data
castillejaDNA <- getGenBankSeq(castilleja$ids)

