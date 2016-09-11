library(RCurl)
library(XML)
library(RJSONIO)

buildQuery =
function(parameters)
{
  query = sprintf("%s(%s)", names(parameters), parameters)
  paste0(query, collapse = " AND ")
}

getAuthorID = 
function(..., key = getOption("ScopusKey", stop("need the scopus API key")), curl = getCurlHandle())
  # Get author IDs matching the specified query parameters.
  #
  # Query parameters include authfirst, authlast, affil, and any others listed
  # in the Scopus Author Search API.
{
  query = buildQuery(list(...))

  # TODO: Fetch all relevant author IDs up to max retrievable, not just 25.

  # Query Scopus to get author ID.
  response = scopusAuthorSearch(query = query, key = key, curl = curl)
  response = fromJSON(response)

  results = response[["search-results"]]

  n_entries = results[["opensearch:totalResults"]]

  if (n_entries > 0) {
    # Extract author ID from each entry.
    entries = results$entry

    vapply(entries, function(entry) {
      sub("AUTHOR_ID:", "", entry[["dc:identifier"]])
    }, "")
  } else {
    # No authors found.
    character(0)
  }
}

scopusAuthorSearch =
function(..., url,
  key = getOption("ScopusKey", stop("need the scopus API key")),
  curl = getCurlHandle(), .opts = list())
  # Query the Scopus Author Search API.
{
  .opts$httpheader = c("X-ELS-APIKey" = key)
  url = "http://api.elsevier.com/content/search/author"
  getForm(url, .params = list(...), .opts = .opts, curl = curl)
}

getDoc =
function(doi, key = getOption("ScopusKey", stop("need the scopus API key")), curl = getCurlHandle(), ...)
{
  url = paste0("http://api.elsevier.com/content/article/DOI:%s", doi)
  getURLContent(url, httpheader = c('X-ELS-APIKey' = key), ...)
}



searchScopus =
function(q, field, key = getOption("ScopusKey", stop("need the scopus API key")), curl = getCurlHandle(), ...)
{
     #http://api.elsevier.com/content/search/scopus
    
}

getAuthorDocsIds = 
function(id, field = "dc:identifier", key = getOption("ScopusKey", stop("need the scopus API key")), curl = getCurlHandle(), ...)
{
  u = "http://api.elsevier.com/content/search/scopus"
  q = sprintf("AU-ID(%s)", as.character(id))
  ans = scopusQuery(query = q, field = field, url = u, key = key, curl = curl, .opts = list(...))
  as.data.frame(do.call(rbind, ans), stringsAsFactors = FALSE)
}


stripIdPrefix =
function(id)
{
    gsub("^[A-Z_]+:", "", id)   
}

# http://api.elsevier.com/documentation/retrieval/AbstractRetrievalViews.htm
DocInfoFields = c("authors", "title", "publicationName", "volume", "issueIdentifier", "dc:description", "subject-areas",
                  "prism:pageRange" , "coverDate", "article-number",  "doi", "citedby-count", "prism:aggregationType")

getDocInfo =
    #XXX    Do we get all the bibrecord items or  just one page
    #   
function(id, fields = DocInfoFields, view = "FULL", key = getOption("ScopusKey", stop("need the scopus API key")), curl = getCurlHandle(), 
          idType = guessIDType(id), ...)
{
  u = switch(idType, 
               DOI = "http://api.elsevier.com/content/abstract/doi/",
               PII = "http://api.elsevier.com/content/abstract/pii/",
               EID = "http://api.elsevier.com/content/abstract/eid/",
               Scopus = "http://api.elsevier.com/content/abstract/scopus_id/",
               Medline = "http://api.elsevier.com/content/abstract/pubmed_id/"
            )

  id = stripIdPrefix(id)
  
  u = sprintf("%s%s", u, id)

  .params = if(!is.na(view))
               list(view = view)
            else if(length(fields) == 1 && fields %in% c("META", "META_ABS", "FULL", "REF", "ENTITLED"))
               list(view = fields)
            else 
               list(field = paste(fields, collapse = ","))
     
  .params$httpAccept = "application/json"
  ans = scopusQuery(.params = .params,  url = u, key = key, curl = curl, .opts = list(...))
  ans[[1]]
}

getAuthorDocs =
    #
    #
    #
function(name, isID = FALSE, ..., max = 25, key = getOption("ScopusKey", stop("need the scopus API key")), curl = getCurlHandle())
{
   
   # Assumes get only one id back. If more, we ignore them. And this could be multiple people.
    if(!isID) {
        r.id = scoGetAuthor(name, idOnly = TRUE, key = key, curl = curl)
        if(length(r.id) == 1 && is.na(r.id))
            return(NULL)

        if(length(r.id) > 1)
            warning("multiple ", length(r.id), " author ids for person", paste(name, collapse = ", "))
    } else
        r.id = name 
    

    doc.ids = getAuthorDocsIds (r.id[1], curl = curl)
    ids = doc.ids[, "dc:identifier"]
#    docs = mapply(getDocInfo, gsub("^[A-Z_]+:", "", ids), ids, lapply(ids, guessIDType), MoreArgs = list(curl = curl))
    docs = lapply(ids, getDocInfo, curl = curl)    
}


scoAffiliation =
    #
    #  What is query ?  ScopusID?    af-id or  city, country or organization name.  And use boolean logic
    # http://api.elsevier.com/content/search/fields/affiliation
    #  
    #
function(query, ..., max = 25, key = getOption("ScopusKey", stop("need the scopus API key")),  url = "http://api.elsevier.com/content/search/affiliation", curl = getCurlHandle())
{
  ans = scopusQuery(query = sprintf("affil(%s)", query), ..., key = key, curl = curl, url = url, max = max)

  ids = gsub("AFFILIATION_ID:", "", sapply(ans, `[[`, "dc:identifier"))
  name = sapply(ans, `[[`, "affiliation-name")
  names(ids) = name
  ids
}


getArticleText = 
# 
#  not all articles are available.
#
function(id, ..., httpAccept = "text/xml", key = getOption("ScopusKey", stop("need the scopus API key")),  
         curl = getCurlHandle(), .opts = NULL, idType = guessIDType(id))
{
  u = switch(idType, 
               DOI = "http://api.elsevier.com/content/article/doi/",
               PII = "http://api.elsevier.com/content/article/pii/",
               EID = "http://api.elsevier.com/content/article/eid/",
               Scopus = "http://api.elsevier.com/content/article/scopus_id/",
               Medline = "http://api.elsevier.com/content/article/doi/"
            )
  
  url = sprintf("%s%s", u, id)
  scopusQuery(url = url, ..., httpAccept = httpAccept, curl = curl, key = key, .opts = .opts)
}

guessIDType = 
#
#  guessIDType( "AUTHOR_ID:36625988000")
#  guessIDType( "0000-0002-9571-2312")
#  guessIDType("9-s2.0-36625988000")
#  guessIDType("10-s2.0-113209238") 
function(id)
{

  if(length(attributes(id)) > 0 && length( k <- attr(id, "class")))
     return(k)

  if(grepl("SCOPUS_ID:", id))
    "Scopus"
  else if(grepl("/", id))
    "DOI"
  else if(grepl("^([0-9]{4}-){3}[0-9]{3}[0-9X]$", id) && checkORCID(id))  # Now do the checksum as described in http://support.orcid.org/knowledgebase/articles/116780-structure-of-the-orcid-identifier
    "ORCID"
  else if(grepl("^[0-9]{1,2}-s[0-9]\\.[0-9]-[0-9]+$", id))
    "EID"
  else if(grepl("AUTHOR_ID:", id)) 
    "AUID"
  else 
     stop("not recognized yet")
}

checkORCID =
function(str)
{
   els = strsplit(gsub("-", "", str), "")[[1]]
   nums = as.integer(els)
   total = 0
   for(i in nums)
      total = (total + 2) * 2
   ans = (12 - total %% 11) %% 11
   els[length(els)] == (if(ans == 10) "X" else ans)
}

scopusQuery =
function(..., url, max = NA, curl = getCurlHandle(), key = getOption("ScopusKey", stop("need the scopus API key")), .opts = list(), 
         .params = list(...))
{
  .opts$httpheader = c('X-ELS-APIKey' = key)
  ans = getForm(url, .params = .params, .opts = .opts, curl = curl)
  res = fromJSON(ans)
  getNextPages(res, res, max = max, url = url, curl = curl)
}


getNextPages = 
function(ans, ..., url, max = NA, curl = getCurlHandle(), key = getOption("ScopusKey", stop("need the scopus API key")), .opts = list(), 
         .varName = "scopusResults", verbose = getOption("Scopus.verbose", TRUE))
{
    info = ans[[1]]
    if(!("opensearch:totalResults" %in% names(info)))
        return(ans)

    results = info$entry

    totalNum = as.integer(info[[1]])

    if(verbose)
       cat("Number of results", totalNum, "\n")

    page = 1L
    while(is.na(max) || length(results) < max) {
        if(is.null(info$link) || is.character(info$link)) # not a list of links
             break
        u = do.call(rbind, info[["link"]])
        i = match(c("next", "last"), u[, "@ref"])
        if(any(!is.na(i))) {
              u = u[i[ !is.na(i) ][1], "@href"]
             page = page + 1L
	     if(verbose)
                 cat("querying page", page, "\n")
             tmp = fromJSON(getURLContent(I(u), curl = curl))
             info = tmp[[1]]
             results = c(results, info$entry)

 	     if(!is.na( .varName ) && nchar(.varName) > 0)
                assign(.varName, results, globalenv())

             if(is.na(i)[1])
                break
        } else {
#      	   browser()
           break
        }
    }

   if(length(results) < totalNum)
      warning("did not get all results from Scopus due to system limit")

    structure(results, totalNumResults = totalNum)
}


#  http://api.elsevier.com/documentation/AUTHORSearchAPI.wadl
scoGetAuthor =
#
# scoGetAuthor("Temple Lang", "Davis")
# scoGetAuthor("Smith", NA, "MacKenzie")
# mck = scoGetAuthor("Smith", NA, "MacKenzie")
# scoGetAuthor("Tomich", "Davis")
# sz = scoGetAuthor("Sawyer", 60014439, "Suzana")
# wp = scoGetAuthor(c("Polonik", "Wolfgang"), 60014439)
# na = scoGetAuthor(c("Anderson", "Nicholas"), 60014439)
# jq = scoGetAuthor(c("Quinn", "Jim"), 60014439)
# dh = scoGetAuthor(c("Halfmann"))
# sch = scoGetAuthor(c("Shauman"))
# dn = scoGetAuthor(c("Niemeier"))  #  2 answers - same person?
# nina = scoGetAuthor(c("Amenta"))
# kwanliu = scoGetAuthor(c("Ma", "Kwan-Liu")) # 5 answers
# joy = scoGetAuthor(c("Joy", "Ken"))
# raissa = scoGetAuthor(c("D'Souza", "Raissa")) # 2 - Davis and SFI
# prem = scoGetAuthor(c("Devanbu")) # 2
# jimc = scoGetAuthor(c("Crutchfield"))
# tony = scoGetAuthor(c("Tyson"))   # 6 and not all the same person.
# prabir = scoGetAuthor(c("Burman"))
# ethan = scoGetAuthor(c("Anderes"))
# jie = scoGetAuthor(c("Peng", "Jie"))
# debashis = scoGetAuthor(c("Paul", "Debashis"))
# hans = scoGetAuthor(c("Muller", "Hans"))
# thomas = scoGetAuthor(c("Lee", "Thomas"))  # 5
# ben = scoGetAuthor(c("Houlton"))
# vlad = scoGetAuthor(c("Filkov"))
# norm = scoGetAuthor(c("Matloff")) 
# patrice = scoGetAuthor(c("Koehl")) # 2 not the same
# prasad = scoGetAuthor(c("Naik", "Prasad"))
# jdo = scoGetAuthor(c("Owens", "John"))  # 5 but 1st is the one I know.
# bertram = scoGetAuthor(c("Ludaescher"))
# prasant = scoGetAuthor(c("Mohapatra", "Prasant"))
# louise = scoGetAuthor(c("Kellogg", "Louise")) # 3 or 5 w/o Louise
# dawn = scoGetAuthor(c("Sumner"))  # 8
# colin = scoGetAuthor(c("Cameron")) # 13
# joe = scoGetAuthor(c("Dumit"))
# jiming = scoGetAuthor(c("Jiang", "Jiming"))
# jeisen = scoGetAuthor(c("Eisen", "Jonathan"))
#

# Many results returned
# block = scoGetAuthor(c("Block"))

#
function(last, affil = 60014439, first = NA, idOnly = FALSE, curl = getCurlHandle(), key = getOption("ScopusKey", stop("need the scopus API key")), .opts = list())
{
    if(length(last) == 2 && is.na(first)) {
        first = last[2]
        last = last[1]
    }

       # get rid of any alternative name in parenthesis.  That leads to a bad request as the Scopus engine treats () as part of the query structure.
   first = gsub("\\([^)]+\\)", "", first)

    q = sprintf("authlastname(%s)", last)
    if(!is.na(affil))
       q = sprintf("%s AND af-id(%s)", q, as.character(affil))

    if(!is.na(first))
       q = sprintf("%s AND authfirst(%s)", q, as.character(first))

    ans = scopusQuery(query = q, url = "http://api.elsevier.com/content/search/author", curl = curl, key = key, .opts = .opts)

    if(attr(ans, "totalNumResults") == 0)
       return(if(idOnly) NA else NULL)

    if(idOnly) 
      gsub("^AUTHOR_ID:", "", sapply(ans, `[[`, "dc:identifier"))
    else
       ans
}


getArticlesByAffiliation =
# 
#  ...  can include terms such as dateloaded
#
# art = getArticlesByAffiliation(60014439, max = 2000)

function(affil, max = NA, ..., curl = getCurlHandle(), key = getOption("ScopusKey", stop("need the scopus API key")), .opts = list())
{
   q = sprintf("af-id(%s)", as.character(affil))
   ans = scopusQuery(query = q, ..., max = max, url = 'http://api.elsevier.com/content/search/index:SCOPUS', curl = curl, key = key, .opts = .opts) 
}



# Not used
getDocInfo.xxx =
function(u, curl = getCurlHandle(), key = getOption("ScopusKey", stop("need the scopus API key")), .opts = list())
{
  .opts$httpheader = c('X-ELS-APIKey' = key)
  txt = getURLContent(u, curl = curl, .opts = .opts)
  doc = xmlParse(txt, asText = TRUE)
}


# Not used
processDocResults =
function(doc)
{
  # num authors, title, abstract, date, issn, "journal" type &  name/identifier, citedby-count, paste author id's and separate by ';'  link to another table
  #  publisher, scopus identifier, doi,  subject-areas
  # references 

  # What is an SGR for refd-itemidlist

  r = xmlRoot(doc)

  abs = getNodeSet(doc, "//x:abstract/ce:para",  c(x = "http://www.elsevier.com/xml/svapi/abstract/dtd", ce = "http://www.elsevier.com/xml/ani/common"))
}




subjectClassifications =
function(..., 
         scopus = FALSE,
          url = if(!scopus) 
                   "http://api.elsevier.com/content/subject/scidir" 
                else 
                   "http://api.elsevier.com/content/subject/scopus")
{
  ans = scopusQuery(..., url = url)
  tmp = ans[[1]][[1]]
  as.data.frame(do.call(rbind, tmp), stringsAsFactors = FALSE)
}


authorRetrieval = 
function(id, ..., idType = guessIDType(id), url = "http://api.elsevier.com/content/author", useIDURL = length(list(...)) == 0)
{
browser()
  if(useIDURL) {
     u = switch(idType, 
                 AUID = "http://api.elsevier.com/content/author/author_id/",
                 EID = "http://api.elsevier.com/content/author/eid/",
                 ORCID = "http://api.elsevier.com/content/author/orcid/")
     url = paste0(u, id)
     params = list(...)
  } else {

    params = if(missing(id))
                list(...)
             else
                 list(author_id = paste(id, collapse = ","), ...)
                  #set the name of the first element appropriately
  }

  ans = scopusQuery(..., url = url, httpAccept = "application/json")
  ans[[1]][[1]]   # process this to make it neater.
}


coAuthors =
# z = coAuthors("35578120000")
# me.co = coAuthors(c("Temple Lang", "Duncan"))
function(person, key = getOption("ScopusKey", stop("need the scopus API key")), curl = getCurlHandle(), url = "http://api.elsevier.com/content/search/author")
{
     if(length(person) == 2)
         id = getAuthorID(AUTHFIRST = person[2], AUTHLASTNAME = person[1], curl = curl, key = key)
     else
         id = gsub("SCOPUS_ID:", "", person)

     ans = scopusQuery('co-author' = id, url = url, curl = curl)
     collectAuthorsInfo(ans)
}


collectAuthorsInfo =
function(x, ...)
{
  lapply(x, collectAuthorInfo, ...)
}

collectAuthorInfo =
function(x, ...)
{
  ans = unlist( x[ c("dc:identifier", "eid", "preferred-name", "document-count") ] )
  sa = x[["subject-area"]]
  ans["subject-area"] = if(is.character(sa))
                           sa["@abbrev"]
                        else
                           paste(sapply(sa, `[[`, "@abbrev"), collapse = ",")

  ans
}
