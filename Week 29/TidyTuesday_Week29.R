# Week 29
library(fastverse)
library(ggplot2); library(patchwork)
technology <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-19/technology.csv')

# energy related observations / categories
energy_raw <- technology |>
  fsubset(
    category == "Energy" & variable %!in% c("electric_gen_capacity","elec_cons")) |>
  ftransform(
    variable = factor(variable,
                      levels = c("elec_wind","elec_solar","elec_hydro","elec_renew_other",
                                 "elec_nuc","elec_oil","elec_gas","elec_coal","elecprod"),
                      labels = c("... wind","... solar","... hydro","... other renewables",
                                 "... nuclear","... oil","... gas","... coal","... total")),
    label = factor(label,
                   levels = c("Electricity from wind (TWH)","Electricity from solar (TWH)","Electricity from hydro (TWH)",
                              "Electricity from other renewables (TWH)","Electricity from nuclear (TWH)",
                              "Electricity from oil (TWH)","Electricity from gas (TWH)","Electricity from coal (TWH)",
                              "Gross output of electric energy (TWH)")),
    iso3c = as.factor(iso3c)) |>
  fselect(-group, -category)

# get country names and regions additionally to country codes
countries <- fread("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv", select = c("name","alpha-3","region","sub-region"))

# merge
energy_merged <- energy_raw[countries, on = c(iso3c = "alpha-3")] |>
  frename(name = country, `sub-region` = sub_region) |>
  colorder(country,region,sub_region,iso3c) |>
  na_omit() |>
  # common starting year
  fsubset(year >= fmax(fmin(year, country)))

# aggregate
energy_agg <- energy_merged |>
  fselect(region, year, variable, value) |>
  fsubset(variable != "... total") |>
  fdroplevels() |>
  fgroup_by(region, year, variable) |>
  fsummarise(total_prod = fsum(value)) |>
  fgroup_by(region, year) |>
  fmutate(share = total_prod / fsum(total_prod) * 100) |>
  fungroup() |>
  fsubset(region != "")

total_prod_2020 <- energy_agg |> fsubset(region == "Europe" & year == 2020) |> fsummarise(variable = variable,total = fsum(total_prod,variable)) |> (\(x) x[order(-labels(variable)),])()|> fmutate(cum_tot = fcumsum(total))
change_2000_2020 <- energy_agg |> fsubset(region == "Europe" & year %in% c(2000,2020)) |> fgroup_by(variable) |> fmutate(change =  total_prod - data.table::shift(total_prod,n = 1)) |>
  fungroup() |> fsubset(year == 2020) |> fmutate(Type = fcase(stringr::str_detect(variable,"coal|gas|oil"), "Fossil Energy",
                                                              stringr::str_detect(variable,"hydro|renewables|solar|wind|nuclear"),"Renewables")) |>
  fgroup_by(Type) |>
  fsummarise(total_change = fsum(change))
total_change_2000_2020 <- energy_agg |> fsubset(region == "Europe" & year %in% c(2000,2020)) |> fgroup_by(year) |> fsummarise(total_prod = fsum(total_prod)) |> fungroup() |> fgrowth() |> na_omit() |> with(total_prod)
# share per energy production type in 2000 and 2020 and absolute difference in percentage points
pp_shr_chng <- energy_agg |> fsubset(region == "Europe" & year %in% c(2000,2020)) |> fsummarise(year = year, variable = variable, share = total_prod / fsum(total_prod, year) * 100) |> fgroup_by(variable) |> fmutate(pp_chng = share - data.table::shift(share,1)) |> na_omit()

# get back labels
energy_agg <- energy_agg[energy_raw |> fselect(variable, label) |> funique(), on = "variable"] |> na_omit()

# color blind friendly palette
cbbPalette <- colorRampPalette(c("#335c67","#fff3b0","#e09f3e","#9e2a2b","#540b0e"))(8)

main_plot <- energy_agg |>
  fsubset(
    region == "Europe" & year %in% seq(2000,2020,5)
  ) |>
  ggplot() +
  theme_void() +
  geom_bar(
    aes(year, total_prod, fill = variable),
    stat = "identity") +
  labs(
    title = "Decomposition of Total Energy Production in Europe",
    subtitle = paste("\nBetween 2000 and 2020, the total production of energy in Europe increased by",round(total_change_2000_2020,2),"%.\nMeanwhile, the composition of energy production has changed considerably in favour of renewable energy\n(including nuclear energy production as an alternative to fossile energy production).\nThe bar chart on the left shows the development of the decomposition of energy production in european countries\nbetween 2000 and 2020.\nThe chart on the right visualizes the change of energy production share\nbetween 2000 and 2020, measured in percentage points."),
    caption = "\nDustin Hennig | #TidyTuesday Week 29 | Data: NBER"
  ) +
  theme(
    # legend
    legend.position = "bottom",
    legend.margin = margin(t = .5, unit = "cm"),
    legend.text = element_text(size = 10),
    # axis
    axis.title = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_blank(),
    # title and caption
    plot.title = element_text(size = 18, face = "bold", hjust=0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 12, margin = margin(l = .5, unit = "cm")),
    plot.caption = element_text(hjust = .5),
    # colors
    plot.background = element_rect(fill = "#dfd9cd"),
    # margins
    plot.margin = margin(t = .5, r = .5, b = .5, l = .5, unit = "cm")
  ) +
  scale_x_continuous(limits = c(1995,2030),breaks = c(seq(2000,2020,5))) +
  scale_fill_manual(values = rev(cbbPalette),
                    name = "Electricity\nfrom ...",
                    guide = guide_legend(reverse = FALSE)) +
  # labels at the top of bars
  geom_label(
    data = energy_agg |> fsubset(region == "Europe" & year %in% seq(2000,2020,5)) |> fgroup_by(year) |> fsummarise(total = fsum(total_prod)),
    aes(x = year, y = total + 150, label = paste(round(total,2),"TWH")),
        label.size = NA, fill = NA
  ) +
  geom_vline(xintercept = 2024)

scd_plot <- total_prod_2020[pp_shr_chng, on = "variable"] |>
  ggplot() +
  geom_col(aes(x = rev(pp_chng), y = variable, fill = rev(variable))) +
  geom_label(x = fmin(total_prod_2020[pp_shr_chng, on = "variable"]$pp_chng - 2),
             aes(y = variable, label = ifelse(rev(pp_chng) >= 0, paste0("+",round(rev(pp_chng),1)," pp"), paste0(round(rev(pp_chng),1)," pp"))),
             label.size = NA, fill = NA, hjust = 1) +
  theme_void() +
  theme(
    legend.position = "none"
  ) +
  scale_fill_manual(values = rev(cbbPalette)) +
  scale_x_continuous(limits = c(-25,11))

pic <- main_plot +
  inset_element(
    scd_plot,
    left = 0.8,
    right = 1,
    bottom = .05,
    top = .9)

png("tidyTuesday_week29.png", width = 25, height = 20, units = "cm", res = 144)
  pic
dev.off()
