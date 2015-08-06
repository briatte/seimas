# party colors

colors = c(
  "LSDP" = "#E41A1C",
  "LVZS" = "#4DAF4A",
  "DP" = "#053061",
  "LLRA" = "#FB8072",
  "NS" = "#377EB8",
  "TT" = "#FFFF33",
  "TS-LKD" = "#80B1D3",
  "LS" = "#FF7F00",
  "LICS" = "#01665E",
  "TPP" = "#A65628",
  "DK" = "#984EA3",
  "IND" = "#AAAAAA"
)

groups = c(
  "LSDP" = "Lietuvos socialdemokratų partija", # Soc-Dem, red
  "LVZS" = "Lietuvos valstiečių ir žaliųjų sąjunga", # Peasants and Greens, green
  "DP" = "Darbo partija",    # Labour Party, dark blue
  "LLRA" = "Lietuvos lenkų rinkimų akcija",    # Poles, light red
  "NS" = "Naujoji sąjunga (socialliberalai)", # New Union, blue
  "TT" = "Tvarka ir teisingumas", # Order & Justice, yellow
  "TS-LKD" = "Tėvynės sąjunga - Lietuvos krikščionys demokratai", # Chr-Dems, light blue
  "LS" = "Liberalų sąjūdis", # Liberal Movement, orange
  "LICS" = "Liberalų ir centro sąjunga", # Liberal/Centre Union, yellow and blue -- dark green/teal
  "TPP" = "Tautos prisikėlimo partija", # National Resurrection Party, orange and black -- brown
  "DK" = "Drąsos kelias",    # Way of Courage, purple
  "IND" = "independent"
)

# ParlGov Left/Right scores

scores = c(
  "LSDP" = 3.2,
  "LVZS" = 3.3,
  "DP" = 3.9,
  "LLRA" = 3.9,
  "NS" = 4.3,
  "TT" = 5.3,
  "TS-LKD" = 7.4,
  "LS" = 7.8,
  "LICS" = 7.8,
  "TPP" = 8.7,
  "DK" = Inf, # missing
  "IND" = Inf
)

stopifnot(names(colors) == names(groups))
stopifnot(names(colors) == names(scores))

