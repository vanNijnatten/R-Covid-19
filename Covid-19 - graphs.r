library(tidyverse)
library(utils)
library(httr)
# library(ggpubr)
# library(RColorBrewer)

img.path <- "img"
dir.create(img.path)

# Download the dataset from the ECDC website to a local temporary file
tf <- tempfile(fileext = ".csv")
GET(
	"https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
	authenticate(":", ":", type = "ntlm"),
	write_disk(tf)
)

# Helpers
g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Colors for all the interesting countries
graphColors <- c(
    "NL" = "red",
    "AU" = "blue",
    "CN" = "green",
    "US" = "black",
    "IT" = "gold",
    "CO" = "purple",
    "Other" = grDevices::adjustcolor("black", 0.2)
  )


# read the Dataset sheet into “R”. The dataset will be called "data".
data <- read.csv(tf) %>%
  dplyr::mutate(
  	date = as.Date(.[[1]], "%e/%m/%Y")
  ) %>%
  dplyr::group_by(countryterritoryCode) %>%
  dplyr::arrange(countryterritoryCode, date) %>%
  dplyr::mutate(
  	new.cases = cases,
  	total.cases = cumsum(cases),
  	group = dplyr::case_when(
  	  as.character(geoId) == "NL" ~ "NL",
  	  as.character(geoId) == "AU" ~ "AU",
  	  as.character(geoId) == "CN" ~ "CN",
  	  as.character(geoId) == "US" ~ "US",
  	  as.character(geoId) == "IT" ~ "IT",
      as.character(geoId) == "CO" ~ "CO",
  	  TRUE ~ "Other"
  	)
  ) %>%
  dplyr::filter(
  	group != "Other",
	  total.cases > 0
  ) %>%
  dplyr::mutate(
    day = dplyr::row_number()
  ) %>%
  dplyr::ungroup()

# Plots
nc.o.tc <- ggplot2::ggplot(
  	data = data,
  	mapping = ggplot2::aes(
  	  x = total.cases,
  	  y = new.cases,
  	  color = as.factor(group)
	)
  ) +
  ggplot2::geom_line(size = 1) +
  ggplot2::scale_x_continuous(trans = 'log10') +
  ggplot2::scale_y_continuous(trans = 'log10') +
  ggplot2::scale_color_manual(values = graphColors) +
  ggplot2::theme_bw() +
  ggplot2::labs(
  	x = "Total cases",
  	y = "New cases",
  	color = "Country"
  )


tc.o.dt.log <- ggplot2::ggplot(
  	data = data,
  	mapping = ggplot2::aes(
  	  x = date,
  	  y = total.cases,
  	  color = as.factor(group)
	)
  ) +
  ggplot2::geom_line(size = 1) +
  ggplot2::scale_y_continuous(trans = 'log10') +
  ggplot2::scale_color_manual(values = graphColors) +
  ggplot2::theme_bw() +
  ggplot2::labs(
  	x = "Date",
  	y = "Total cases",
  	color = "Country"
  )

tc.o.day.log <- ggplot2::ggplot(
  	data = data,
  	mapping = ggplot2::aes(
  	  x = day,
  	  y = total.cases,
  	  color = as.factor(group)
	)
  ) +
  ggplot2::geom_line(size = 1) +
  ggplot2::scale_y_continuous(trans = 'log10') +
  ggplot2::scale_color_manual(values = graphColors) +
  ggplot2::theme_bw() +
  ggplot2::labs(
  	x = "Day",
  	y = "Total cases",
  	color = "Country"
  )

tc.o.day.lin <- ggplot2::ggplot(
  	data = data,
  	mapping = ggplot2::aes(
  	  x = day,
  	  y = total.cases,
  	  color = as.factor(group)
	)
  ) +
  ggplot2::geom_line(size = 1) +
  ggplot2::scale_color_manual(values = graphColors) +
  ggplot2::theme_bw() +
  ggplot2::labs(
  	x = "Day",
  	y = "Total cases",
  	color = "Country"
  )

tc.o.dt.lin <- ggplot2::ggplot(
  	data = data,
  	mapping = ggplot2::aes(
  	  x = date,
  	  y = total.cases,
  	  color = as.factor(group)
	)
  ) +
  ggplot2::geom_line(size = 1) +
  ggplot2::scale_color_manual(values = graphColors) +
  ggplot2::theme_bw() +
  ggplot2::labs(
  	x = "Date",
  	y = "Total cases",
  	color = "Country"
  )


figure <- ggpubr::ggarrange(
  tc.o.dt.log + ggpubr::rremove("legend"),
  tc.o.dt.lin + ggpubr::rremove("legend"),
  tc.o.day.log + ggpubr::rremove("legend"),
  tc.o.day.lin + ggpubr::rremove("legend"),
  nc.o.tc + ggpubr::rremove("legend"),
  g_legend(nc.o.tc),
  labels = c("A", "B", "C", "D", "E"),
  ncol = 2,
  nrow = 3
)

ggplot2::ggsave(
  filename = file.path(img.path, "growth.png"),
  plot = figure,
  width = 150,
  height = 225,
  units = "mm"
)
