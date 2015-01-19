meta = c("Lithuania", "Seimas")
mode = "fruchtermanreingold"

for(ii in unique(b$legislature)) {

  cat(ii)

  data = subset(b, legislature == ii &!is.na(authors))
  sp = subset(s, legislature == ii)

  # translate all sponsor names to uids
  for(jj in 1:nrow(data)) {

    au = unlist(strsplit(data$authors[ jj ], ", "))
    au = sp$id[ sp$id %in% toupper(au) ]
    data$authors[ jj ] = ifelse(!length(au), NA, paste0(au, collapse = ";"))

    co = unlist(strsplit(data$cosponsors[ jj ], ", "))
    co = sp$id[ toupper(sp$name) %in% toupper(co) ]
    data$cosponsors[ jj ] = ifelse(!length(co), NA, paste0(co, collapse = ";"))

  }

  # lose a few bills from previous legislature(s)
  data = subset(data, !is.na(authors))

  # bills with no "more authors..." page
  data$sponsors = data$authors

  # bills with a "more authors..." page (all sponsors in cosponsors)
  data$sponsors[ !is.na(data$cosponsors) ] = data$cosponsors[ !is.na(data$cosponsors) ]

  data$n_au = 1 + str_count(data$sponsors, ";")

  bills = subset(data, n_au > 1)

  cat(":", nrow(bills), "cosponsored documents, ")

  #
  # directed edge list
  #

  edges = rbind_all(lapply(bills$sponsors, function(d) {

    w = unlist(strsplit(d, ";"))

    d = expand.grid(i = w, j = w[1], stringsAsFactors = FALSE)

    return(data.frame(d, w = length(w) - 1)) # number of cosponsors

  }))

  #
  # edge weights
  #

  # first author self-loops, with counts of cosponsors
  self = subset(edges, i == j)

  # count number of bills per first author
  n_au = table(self$j)

  # remove self-loops from directed edge list
  edges = subset(edges, i != j)

  # count number of bills cosponsored per sponsor
  n_co = table(edges$i)

  # identify directed ties
  edges$ij = apply(edges[, 1:2 ], 1, paste0, collapse = "///")

  # raw edge counts
  raw = table(edges$ij)

  # Newman-Fowler weights (weighted quantity of bills cosponsored)
  edges = aggregate(w ~ ij, function(x) sum(1 / x), data = edges)

  # expand to edge list
  edges = data.frame(i = gsub("(.*)///(.*)", "\\1", edges$ij),
                     j = gsub("(.*)///(.*)", "\\2", edges$ij),
                     raw = as.vector(raw[ edges$ij ]), # raw edge counts
                     nfw = edges$w, stringsAsFactors = FALSE)

  # Gross-Shalizi weights (weighted propensity to cosponsor)
  edges = merge(edges, aggregate(w ~ j, function(x) sum(1 / x), data = self))
  edges$gsw = edges$nfw / edges$w

  # final edge set: cosponsor, first author, weights
  edges = edges[, c("i", "j", "raw", "nfw", "gsw") ]

  cat(nrow(edges), "edges, ")

  #
  # directed network
  #

  n = network(edges[, 1:2 ], directed = TRUE)

  n %n% "country" = meta[1]
  n %n% "title" = paste(meta[2], paste0(range(unique(substr(bills$date, 1, 4))),
                                        collapse = " to "))

  n %n% "n_bills" = nrow(bills)
  n %n% "n_sponsors" = table(data$n_au) # already subset to legislature

  n_au = as.vector(n_au[ network.vertex.names(n) ])
  n %v% "n_au" = ifelse(is.na(n_au), 0, n_au)

  n_co = as.vector(n_co[ network.vertex.names(n) ])
  n %v% "n_co" = ifelse(is.na(n_co), 0, n_co)

  n %v% "n_bills" = n %v% "n_au" + n %v% "n_co"

  cat(network.size(n), "nodes\n")

  rownames(sp) = sp$id

  n %v% "url" = as.character(sp[ network.vertex.names(n), "url" ])
  n %v% "url" = gsub("http://www3.lrs.lt/", "", gsub("&", "&amp;", n %v% "url"))
  n %v% "sex" = as.character(sp[ network.vertex.names(n), "sex" ])
  n %v% "born" = as.numeric(substr(sp[ network.vertex.names(n), "born" ], 1, 4)) # many missing
  n %v% "party" = as.character(sp[ network.vertex.names(n), "party" ])
  n %v% "partyname" = as.character(groups[ n %v% "party" ])
  n %v% "constituency" = as.character(sp[ network.vertex.names(n), "constituency" ])
  n %v% "lr" = as.numeric(scores[ n %v% "party" ])
  n %v% "photo" = as.character(gsub("/", "_", gsub("http://www3.lrs.lt/home/seimo_nariu_nuotraukos/", "",
                                                   sp[ network.vertex.names(n), "photo" ])))
  # mandate years done up to start year of legislature
  sp$nyears = sapply(sp$mandates, function(x) {
    sum(unlist(strsplit(x, ";")) <= as.numeric(substr(ii, 1, 4)))
  })
  n %v% "nyears" = as.numeric(sp[ network.vertex.names(n), "nyears" ])

  set.edge.attribute(n, "source", as.character(edges[, 1])) # cosponsor
  set.edge.attribute(n, "target", as.character(edges[, 2])) # first author

  set.edge.attribute(n, "raw", edges$raw) # raw edge counts
  set.edge.attribute(n, "nfw", edges$nfw) # Newman-Fowler weights
  set.edge.attribute(n, "gsw", edges$gsw) # Gross-Shalizi weights

  #
  # weighted measures
  #

  n = get_modularity(n, weights = "raw")
  n = get_modularity(n, weights = "nfw")
  n = get_modularity(n, weights = "gsw")

  n = get_centrality(n, weights = "raw")
  n = get_centrality(n, weights = "nfw")
  n = get_centrality(n, weights = "gsw")

  #
  # network plot
  #

  if(plot) {

    q = n %v% "degree"
    q = as.numeric(cut(q, unique(quantile(q)), include.lowest = TRUE))

    ggnet_save(n, file = paste0("plots/net_lt", ii),
               i = colors[ sp[ n %e% "source", "party" ] ],
               j = colors[ sp[ n %e% "target", "party" ] ],
               q, colors, order)

  }

  #
  # save objects
  #

  # replace uids with names
  network.vertex.names(n) = sp[ network.vertex.names(n), "name" ]

  set.edge.attribute(n, "source", sp[ n %e% "source", "name" ])
  set.edge.attribute(n, "target", sp[ n %e% "target", "name" ])

  edges$i = sp[ edges$i, "name" ]
  edges$j = sp[ edges$j, "name" ]

  assign(paste0("net_lt", substr(ii, 1, 4)), n)
  assign(paste0("edges_lt", substr(ii, 1, 4)), edges)
  assign(paste0("bills_lt", substr(ii, 1, 4)), bills)

  #
  # export gexf
  #

  if(gexf)
    get_gexf(paste0("net_lt", ii), n, meta, mode, colors, extra = "constituency")

}

#
# save
#

if(gexf)
  zip("net_lt.zip", dir(pattern = "^net_lt\\d{4}-\\d{4}\\.gexf$"))

save(list = ls(pattern = "^(net|edges|bills)_lt\\d{4}$"), file = "data/net_lt.rda")

# kthxbye
