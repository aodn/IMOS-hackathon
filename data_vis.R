library(tidyverse)

hack_meta <- read_csv("2024/Projects/Fish-DAT/data/HackathonMetadata.csv")

chosen_cols <- read_csv("2024/Projects/Fish-DAT/data/monthly_colour_palette.csv")

# 1 Jan   #D33C35
# 2 Feb   #902e39
# 3 Mar   #b200b2
# 4 Apr   #df6b87
# 5 May   #ffd2da
# 6 Jun   #42deeb
# 7 Jul   #146de7
# 8 Aug   #10abb8
# 9 Sep   #40C362
# 10 Oct   #87c24b
# 11 Nov   #F2BC11
# 12 Dec   #FE9000

loc_227150 <- read_csv("2024/Projects/Fish-DAT/data/227150/227150_daily-positions.csv")

ggplot(data = loc_227150) +
  geom_path(aes(x = Longitude,
                y = Latitude)) +
  theme_bw()

mld_227150 <- read_csv("2024/Projects/Fish-DAT/data/227150/227150-MixLayer.csv")
