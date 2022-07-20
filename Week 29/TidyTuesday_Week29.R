# Week 29
library(fastverse)
library(ggplot2); library(grobs)
technology <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-19/technology.csv')

# energy related observations / categories
energy <- technology |>
  fsubset(variable %in% grep("^elec", variable, value = TRUE) & variable %!in% c("electric_gen_capacity","elec_cons")) 

# get country names and regions additionally to country codes
countries <- fread("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv")

# merge
energy_merged <- energy[countries |> fselect("name","alpha-3","region","sub-region"), on = c(iso3c = "alpha-3")] |>
  frename(name = country, `sub-region` = sub_region) |>
  colorder(country,region,sub_region,iso3c) |>
  na_omit() |>
  # common starting year
  fsubset(year >= fmax(fmin(year, country)))

# aggregate
energy_agg <- energy_merged |>
  fselect(region, year, variable, value) |>
  fsubset(variable != "elecprod") |>
  fgroup_by(region, year, variable) |>
  fsummarise(total_prod = fsum(value)) |>
  fgroup_by(region, year) |>
  fmutate(share = total_prod / fsum(total_prod) * 100) |>
  fungroup() |>
  fsubset(region != "")

total_prod_2000 <- energy_agg |> fsubset(region == "Europe" & year == 2000) |> fsummarise(total = fsum(total_prod)) |> with(total) |> as.numeric()
total_prod_2020 <- energy_agg |> fsubset(region == "Europe" & year == 2020) |> fsummarise(total = fsum(total_prod)) |> with(total) |> as.numeric()
change_2000_2020 <- energy_agg |> fsubset(region == "Europe" & year %in% c(2000,2020)) |> fgroup_by(variable) |> fmutate(change =  total_prod - data.table::shift(total_prod,n = 1)) |>
  fungroup() |> fsubset(year == 2020) |> fmutate(Type = fcase(stringr::str_detect(label,"coal|gas|oil"), "Fossil Energy",
                                                              stringr::str_detect(label,"hydro|renewables|solar|wind|nuclear"),"Renewables")) |>
  fgroup_by(Type) |>
  fsummarise(total_change = fsum(change))
# get back labels
energy_agg <- energy_agg[energy |> fselect(variable, label) |> funique(), on = "variable"] |> na_omit()

# color blind friendly palette
cbbPalette <- c("#000000", "#E69F00", "#975C12", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

pic <- energy_agg |>
  fsubset(
    region == "Europe"
  ) |>
  fmutate(variable = factor(variable,
                            levels = c("elec_wind","elec_solar","elec_hydro","elec_renew_other",
                                       "elec_nuc","elec_oil","elec_gas","elec_coal"),
                            labels = c("... wind","... solar","... hydro","... other renewables",
                                       "... nuclear","... oil","... gas","... coal"))) |>
  ggplot(aes(year, total_prod, fill = variable)) +
  theme_void() +
  geom_area(position = "stack") +
  labs(
    title = "Energy Production in Europe\n",
    caption = "#TidyTuesday | Data: NBER"
  ) +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_blank(),
    strip.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust=0.5),
    plot.caption = element_text(hjust = .5),
    plot.background = element_rect(fill = "#dfd9cd"),
    legend.position = "bottom",
    plot.margin = margin(t = 0, r = 25, b = 0, l = 0, unit = "pt")
  ) +
  scale_x_continuous(breaks = c(seq(2000,2020,5))) +
  scale_y_continuous(labels = scales::number_format(big.mark = ",", decimal.mark = ".",suffix = " TWH"),
                     breaks = c(1000,2000,3000,4000)) +
  scale_fill_manual(values = rev(cbbPalette),
                    name = "Electricity\nfrom ...",
                    guide = guide_legend(reverse = TRUE)) +
  # beginning
  annotate(
    geom = "text",
    label = paste(
      format(round(total_prod_2000,0),big.mark = ",", decimal.mark = "."),"TWH"
    ),
    x = 2001 , y = total_prod_2000 + 730, size = 3
  ) +
  geom_segment(
    aes(x = 2000, y = total_prod_2000 + 550, xend = 2000, yend = total_prod_2000),
    arrow = arrow(length = unit(0.2, "cm"), type = "closed")
  ) +
  # end
  annotate(
    geom = "text",
    label = paste(
      format(round(total_prod_2020,0),big.mark = ",", decimal.mark = "."),"TWH"
    ),
    x = 2019.4, y = total_prod_2020 + 700, size = 3
  ) +
  geom_segment(
    aes(x = 2020, y = total_prod_2020 + 550, xend = 2020, yend = total_prod_2020),
    arrow = arrow(length = unit(0.2, "cm"), type = "closed")
  ) +
  # change for each type between 2000 and 2020
  annotate(
    geom = "text",
    label = paste0(
      "While Renewable Energy\nProduction increased by\n",
      change_2000_2020 |> with(total_change) |> (\(x) x[2])() |> round(0),
      " TWH between\n2000 and 2020 ..."
    ),
    x = 2021, y = 3500, size = 3, hjust = 0
  ) +
  annotate(
    geom = "text",
    label = paste0(
      "... Fossil Energy Production\ndeclined by ",
      -1 * change_2000_2020 |> with(total_change) |> (\(x) x[1])() |> round(0),
      " TWH."
    ),
    x = 2021, y = 1000, size = 3, hjust = 0
  ) +
  coord_cartesian(xlim = c(2000,2025),ylim = c(0, 4700), clip = "off")


png("tidyTuesday_week29.png", width =15, height = 10, units = "cm", res = 144)
  pic
dev.off()
