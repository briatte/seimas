groups = c(
  "LVZS" = "Lietuvos valstiečių ir žaliųjų sąjunga", # Peasants and Greens, green
  "LSDP" = "Lietuvos socialdemokratų partija", # Soc-Dem, red
  "DP" = "Darbo partija",    # Labour Party, dark blue
  "LLRA" = "Lietuvos lenkų rinkimų akcija",    # Poles, light red
  "NS" = "Naujoji sąjunga (socialliberalai)", # New Union, blue
  "TT" = "Tvarka ir teisingumas", # Order & Justice, yellow
  "LS" = "Liberalų sąjūdis", # Liberal Movement, orange
  "LiCS" = "Liberalų ir centro sąjunga", # Liberal/Centre Union, yellow and blue -- dark green/teal
  "TS-LKD" = "Tėvynės sąjunga - Lietuvos krikščionys demokratai", # Chr-Dems, light blue
  "TPP" = "Tautos prisikėlimo partija", # National Resurrection Party, orange and black -- brown
  "DK" = "Drąsos kelias",    # Way of Courage, purple
  "IND" = "independent"
)

colors = c(
  "LVZS" = "#4DAF4A",
  "LSDP" = "#E41A1C",
  "DP" = "#053061",
  "LLRA" = "#FB8072",
  "NS" = "#377EB8",
  "TT" = "#FFFF33",
  "LS" = "#FF7F00",
  "LiCS" = "#01665E",
  "TS-LKD" = "#80B1D3",
  "TPP" = "#A65628",
  "DK" = "#984EA3",
  "IND" = "#AAAAAA"
)

scores = c(
  "LVZS" = 3.3,
  "LSDP" = 3.6,
  "DP" = 3.9,
  "LLRA" = 3.9,
  "NS" = 4.3,
  "TT" = 5.3,
  "LS" = 7.2,
  "LiCS" = 7.8,
  "TS-LKD" = 7.8,
  "TPP" = 8.7,
  "DK" = Inf, # absent of ParlGov stable, missing in ParlGov beta
  "IND" = Inf
)

stopifnot(names(colors) == names(scores))
order = names(colors)[ order(scores) ]

