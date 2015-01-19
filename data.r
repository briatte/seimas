bills = "data/bills.csv"
sponsors = "data/sponsors.csv"

# scrape bills

if(!file.exists(bills)) {

  root = "http://www3.lrs.lt/pls/inter3/"

  h = htmlParse("http://www3.lrs.lt/pls/inter3/dokpaieska.rezult_l?p_nr=&p_nuo=&p_iki=&p_org=1&p_drus=2&p_kalb_id=1&p_title=&p_text=&p_pub=&p_met=&p_lnr=&p_denr=&p_es=0&p_tkid=&p_tid=&p_t=0&p_tr1=2&p_tr2=2&p_gal=&p_rus=1&p_no=1")

  p = unique(xpathSApply(h, "//a[contains(@href, 'dokpaieska.rezult') and contains(@title, 'Į galą')]/@href"))
  n = gsub("(.*)&p_no=(\\d+)", "\\2", p) # last page
  p = gsub("(.*)&p_no=(\\d+)", "\\1", p) # root with session

  b = data.frame()
  for(i in as.numeric(n):1) {

    cat(sprintf("%4.0f", i))
    file = paste0("raw/bills-", i, ".html")

    if(!file.exists(file))
      try(download.file(paste0(root, p, "&p_no=", i), file, quiet = TRUE, mode = "wb"))

    if(!file.info(file)$size) {

      cat(": failed\n")
      file.remove(file)

    } else {

      h = htmlParse(file)

      # bill URLs
      urls = xpathSApply(h, "//table[@class='basicnoborder']/tr//a[contains(@href, 'dokpaieska')][1]/@href")
      uids = gsub("(.*)p_id=(\\d+)&(.*)", "\\2", urls)
      text = xpathSApply(h, "//table[@class='basicnoborder']/tr/td[2]", xmlValue)
      date = xpathSApply(h, "//table[@class='basicnoborder']/tr/td[3]", xmlValue)

      text = lapply(text, function(x) {
        x = unlist(strsplit(x, "\\n"))
        x = x[ x != "" ]
        data.frame(text = x[1], authors = x[2], stringsAsFactors = FALSE)
      })
      text = rbind_all(text)

      # cosponsor URLs
      co  = xpathSApply(h, "//table[@class='basicnoborder']/tr//a[contains(@href, 'p_daug')]/@href")
      coid = gsub("(.*)p_id=(\\d+)&(.*)", "\\2", co)

      cosponsors = rep(NA, length(uids))
      cosponsors[ uids %in% coid ] = co

      for(j in co) {

        cat(".")
        file = paste0("raw/details-", coid[ which(co == j) ], ".html")

        if(!file.exists(file))
          try(download.file(paste0(root, j), file, quiet = TRUE, mode = "wb"))

        if(!file.info(file)$size) {

          cat(": failed (details)\n")
          file.remove(file)

        } else {

          h = htmlParse(file)
          h = xpathSApply(h, "//ul/li", xmlValue)
          h = gsub("(.*)(Seimas|frakcija), |Darbo grupė, ", "", h)
          h = h[ !grepl("(komisija|komitetas|Seimas)$", h) & h != "Darbo grupė" ]
          stopifnot(!grepl("Darbo", h))
          if(length(h))
            cosponsors[ cosponsors == j ] = paste0(h, collapse = ", ")
          else
            cosponsors[ cosponsors == j ] = NA

        }

      }

      b = rbind(b, data.frame(uids, urls, date, text, cosponsors, stringsAsFactors = FALSE))
      cat("\n")

    }

  }

  # clean up
  b$authors = gsub("(.*)Pateikė:\\s(.*)", "\\2", b$authors)
  b$authors = gsub("Lietuvos Respublikos Seimas,\\s|Seimo nar(ė|ys)\\s|,\\s>>", "", b$authors)
  b$authors = gsub("Parengė:\\s|(, )?Darbo\\sgrupė(,\\s)?", "", b$authors)

  # drop committee bills (half of total)
  b = subset(b, !grepl("\\w+ komitetas|komisija$|Lietuvos Respublikos|Projektas|PROJEKTAS", authors))
  b = subset(b, !grepl("^Seimo|Senato|STT|1|\\.pdf|^\\(|^Liberalų|pakomitetis|raštas|rakcija", authors))
  b = subset(b, authors != "")

  au = unlist(strsplit(b$authors, ", "))
  table(au)
  table(toupper(au) %in% d$id)

  co = unlist(strsplit(b$cosponsors, ", "))
  table(co)
  table(co[ !toupper(co) %in% toupper(d$name) ])

  b$legislature = NA
  b$legislature[ as.Date(b$date) > as.Date("2012-10-14") ] = "2012-2016"
  b$legislature[ is.na(b$legislature) & as.Date(b$date) > as.Date("2008-10-12") ] = "2008-2012"
  b$legislature[ is.na(b$legislature) & as.Date(b$date) > as.Date("2004-10-10") ] = "2004-2008"
  table(b$legislature, exclude = NULL)

  write.csv(b, bills, row.names = FALSE)

}

b = read.csv(bills, stringsAsFactors = FALSE)

# scrape sponsors

if(!file.exists(sponsors)) {

  # 2004-8
  h = htmlParse("http://www3.lrs.lt/docs3/kad5/w5_istorija.show5-p_r=786&p_k=1.html")

  u = xpathSApply(h, "//a[contains(@href, 'p_asm_id=')]/@href")
  n = xpathSApply(h, "//a[contains(@href, 'p_asm_id=')]", xmlValue)
  f = xpathSApply(h, "//a[contains(@href, 'p_asm_id=')]/strong", xmlValue)
  n = str_replace(n, f, "")

  d = data.frame(legislature = "2004-2008", url = u, name = paste(n, f),
                 id = paste0(substr(n, 1, 1), ".", f),
                 stringsAsFactors = FALSE)

  # 2008-12
  h = htmlParse("http://www3.lrs.lt/docs3/kad6/w6_istorija.show6-p_r=6113&p_k=1.html")

  u = xpathSApply(h, "//a[contains(@href, 'p_asm_id=')]/@href")
  n = xpathSApply(h, "//a[contains(@href, 'p_asm_id=')]", xmlValue)
  f = xpathSApply(h, "//a[contains(@href, 'p_asm_id=')]/strong", xmlValue)
  n = str_replace(n, f, "")

  d = rbind(d, data.frame(legislature = "2008-2012", url = u, name = paste(n, f),
                          id = paste0(substr(n, 1, 1), ".", f),
                          stringsAsFactors = FALSE))

  # 2012-16
  h = htmlParse("http://www3.lrs.lt/pls/inter/w5_show?p_r=8801&p_k=1")

  u = xpathSApply(h, "//a[contains(@href, 'p_asm_id=')]/@href")
  n = xpathSApply(h, "//a[contains(@href, 'p_asm_id=')]", xmlValue)
  f = sapply(n, function(x) {
    x = unlist(strsplit(x, " "))
    paste0(x[ grepl("[A-Z]{2,}", x) ], collapse = " ")
  })
  n = str_replace(n, paste0(" ", f), "")

  d = rbind(d, data.frame(legislature = "2012-2016", url = u, name = paste(n, f),
                          id = paste0(substr(n, 1, 1), ".", f),
                          stringsAsFactors = FALSE))

  rownames(d) = paste0(d$legislature, "-", d$id)

  d$url[ d$legislature == "2012-2016" ] = paste0("http://www3.lrs.lt/pls/inter/", d$url[ d$legislature == "2012-2016" ])
  d$url[ d$legislature == "2008-2012" ] = paste0("http://www3.lrs.lt/docs3/kad6/", d$url[ d$legislature == "2008-2012" ])
  d$url[ d$legislature == "2004-2008" ] = paste0("http://www3.lrs.lt/docs3/kad5/", d$url[ d$legislature == "2004-2008" ])

  u = unique(d$url) # superfluous precaution, only unique URLs

  # parse sponsors

  s = data.frame()

  for(i in rev(u)) {

    cat(sprintf("%4.0f", which(u == i)))
    file = gsub("(.*)&p_asm_id=(.*)", "raw/mp-\\2", i)
    file = gsub("&p_kade_id=7$", ".html", file) # current legislature

    if(!file.exists(file))
      try(download.file(i, file, quiet = TRUE, mode = "wb"))

    if(!file.info(file)$size) {

      cat(": failed", i, "\n")
      file.remove(file)

    } else {

      h = htmlParse(file)
      name = xpathSApply(h, "//meta[@name='description']/@content")
      details = xpathSApply(h, "//table[@class='MsoTableGrid']//td", xmlValue)

      if(length(details)) {

        details = str_clean(details)
        born = details[ which(grepl("Gimimo data", details)) + 1 ]
        born = ifelse(length(born), as.numeric(substr(born, 1, 4)), NA)

      } else {

        born = NA # many missing

      }

      details = xpathSApply(h, "//b[contains(text(), 'Seimo nar')]/..", xmlValue)
      sex = ifelse(grepl("Seimo narė", details), "F", "M")

      constituency = gsub("Išrinkta(s)?\\s+|\\s\\((.*)", "",
                          str_extract(details, "Išrinkta(s)?\\s(.*?)(,|\\d+)"))

      party = gsub("iškėlė\\s|\\sBiuro(.*)", "",
                   str_extract(details, "iškėlė\\s(.*)"))

      if(is.na(party)) {

        party = xpathSApply(h, "//b[contains(text(), 'Seimo frakcijose')]/following-sibling::ul[1]/li/a", xmlValue)
        party = unique(party)
        party = ifelse(length(party) > 1, NA, party) # four missing, fixed later

      }

      photo = xpathSApply(h, "//img[contains(@src, 'seimo_nariu')]/@src")

      mdts = xpathSApply(h, "//table[@summary='Istorija']//a/..", xmlValue)

      s = rbind(s, data.frame(file, name, sex, born, constituency, party,
                              mandates = ifelse(length(mdts), mdts, NA),
                              photo, url = i,
                              stringsAsFactors = FALSE))
      cat(":", name, "\n")

    }

  }

  s = merge(d[, c("legislature", "url", "id") ], s, by = "url")

  # one big nationwide constituency, plus many small SMDs
  # many constituencies are missing, as with birth years
  s$constituency = gsub("\\s-(.*)|,$", "", s$constituency)

  # missing values (leaves 3 missing in 2008, 2 in 2012)
  s$constituency[ s$name %in% c("Arminas LYDEKA", "Jonas PINSKUS", "Rimas Antanas RUČYS") ] = "pagal sąrašą"

  # party abbreviations
  s$party = str_trim(s$party)
  s$party[ grepl("Išsikėlė pats|Mišri Seimo narių grupė", s$party) ] = "IND" # or mixed group
  s$party[ grepl("(V|v)alstiečių", s$party) ] = "LVZS" # ex-LVLS + Naujosios demokratijos partija, NDP
  s$party[ grepl("Tvarka|Pakso", s$party) ] = "TT"
  s$party[ grepl("Liberalų ir centro", s$party) ] = "LICS"
  s$party[ grepl("Tautos", s$party) ] = "TPP"
  s$party[ grepl("(L|l)iberalų", s$party) ] = "LS"
  s$party[ grepl("^Tėvynės", s$party) ] = "TS-LKD"
  s$party[ grepl("socialdemokratų", s$party) ] = "LSDP"
  s$party[ grepl("lenkų", s$party) ] = "LLRA"
  s$party[ grepl("Darbo", s$party) ] = "DP"
  s$party[ grepl("Drąsos", s$party) ] = "DK"
  s$party[ grepl("socialliberalai|Brazausko", s$party) ] = "NS" # + Brazauskas/Paulauskas coalition

  # missing values
  s$party[ grepl("p_asm_id=47910", s$url) ] = "IND"  # Mantas VARAŠKA
  s$party[ grepl("p_asm_id=53907", s$url) ] = "IND"  # Valdemaras VALKIŪNAS
  s$party[ grepl("p_asm_id=53904", s$url) ] = "IND"  # Jonas STANEVIČIUS
  s$party[ grepl("p_asm_id=47898", s$url) ] = "LICS" # Andrius BURBA

  s$mandates = gsub("\u0097", "-", s$mandates)
  s$mandates = gsub("Buvo išrinkta(s)? į ", "", s$mandates)
  s$mandates = gsub(" Seimą.", ";", s$mandates)
  s$mandates = gsub("(\\sAukščiausiąją\\sTarybą\\s-\\sAtkuriamąjį)?;$", "", s$mandates)

  s$mandates[ is.na(s$mandates) ] = s$legislature[ is.na(s$mandates) ]
  s$mandates = sapply(s$mandates, function(x) {
    x = unlist(strsplit(x, ";")) # each element is a term
    x = lapply(x, function(y) {
      y = unlist(strsplit(y, "-")) # each element is a year
      y = as.numeric(y)
      seq(min(y), max(y)) # could also be y[1], y[2]
    })
    x = sort(unique(unlist(x)))
    paste0(x[ x <= 2014 ], collapse = ";") # right-censored
  })

  write.csv(s, sponsors, row.names = FALSE)

}

s = read.csv(sponsors, stringsAsFactors = FALSE)

# photos, all .jpg -- rerun entire script to solve network errors
for(i in rev(unique(s$photo))) {

  #cat(sprintf("%4.0f", which(unique(s$photo) == i)))
  file = gsub("http://www3.lrs.lt/home/seimo_nariu_nuotraukos/(\\d+)/", "photos/\\1_", i)

  if(!file.exists(file))
    try(download.file(i, file, mode = "wb", quiet = TRUE))

  if(!file.info(file)$size) {

    #cat(": failed\n")
    file.remove(file)
    s$photo[ s$photo == i ] = NA

  } else {

    #cat("\n")
    s$photo[ s$photo == i ] = gsub("photos/|\\.jpg", "", i) # shortened

  }

}

# all set

