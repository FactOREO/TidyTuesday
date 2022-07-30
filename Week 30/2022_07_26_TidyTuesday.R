# TidyTuesday Week 30
# Data from Week 9 2021 (BLS - Employment and Earnings)
library(fastverse)
library(ggplot2)
library(patchwork)
employed <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/employed.csv')

### Data cleaning
# extend the data with new table from 2021
# download.file(url = 'https://www.bls.gov/cps/cpsaat17.xlsx', destfile = 'cpsaat17.xlsx')
employed_2021 <- readxl::read_xlsx('cpsaat17.xlsx', skip = 3)

# get separate data.table for major and minor occupation
major_occupation <- employed_2021 |>
  qDT() |>
  fsubset(1) |>
  fselect(3:NCOL(employed_2021)) |>
  setColnames(paste0('V',seq.default(1,NCOL(employed_2021) - 2))) |>
  melt.data.table(id.vars = NULL, measure.vars = paste0('V',seq.default(1,NCOL(employed_2021)-2))) |>
  frename(value = 'major_occupation')

minor_occupation <- employed_2021 |>
  qDT() |>
  fsubset(2) |>
  fselect(3:NCOL(employed_2021)) |>
  setColnames(paste0('V',seq.default(1,NCOL(employed_2021) - 2))) |>
  melt.data.table(id.vars = NULL, measure.vars = paste0('V',seq.default(1,NCOL(employed_2021)-2))) |>
  frename(value = 'minor_occupation')

combi_occupation <- major_occupation[minor_occupation, on = 'variable'] |>
  tidyr::fill(major_occupation) |>
  fselect(-variable) |>
  ftransform(minor_occupation = stringr::str_replace_all(stringr::str_remove_all(minor_occupation, '-\\\r\\\n'), pattern = '\\\r\\\n',' '),
             major_occupation = stringr::str_replace_all(stringr::str_remove_all(major_occupation, '-\\\r\\\n'), pattern = '\\\r\\\n',' '))

rm(list = c('minor_occupation', 'major_occupation'))

# split employed_2021 by categories and manipulate to desired output from given data
employed_2021_clean <- employed_2021 |>
  fmutate(category = fcase(`Industry, sex, and race` == 'TOTAL', 'TOTAL',
                           `Industry, sex, and race` == 'Men'  , 'Men'  ,
                           `Industry, sex, and race` == 'Women', 'Women',
                           `Industry, sex, and race` == 'White', 'White',
                           `Industry, sex, and race` == 'Black or African American', 'Black or African American',
                           `Industry, sex, and race` == 'Asian', 'Asian',
                           default = NA_character_)) |>
  tidyr::fill(category) |>
  fsubset(!(is.na(category) | category == `Industry, sex, and race`)) |>
  rsplit(~ category) |>
  rapply2d(`colnames<-`, c('industry', 'row_sum', funique(combi_occupation$minor_occupation))) |>
  rapply2d(fselect, -row_sum) |>
  unlist2d(idcols = 'category') |>
  qDT() |>
  melt.data.table(id.vars = c('category','industry'), value.name = 'employ_n', variable.name = 'minor_occupation') |>
  (\(x) x[combi_occupation, on = 'minor_occupation'])() |>
  ftransform(employ_n = as.integer(employ_n) * 1000) |>
  fmutate(year = 2021) |>
  fselect(industry, major_occupation, minor_occupation, 'race_gender' = category, employ_n, year) |>
  # get rid of the footnote rows
  na_omit()
# extract total employed as separat dt and add as new 'industry_total' columnn
total_employed <- employed_2021_clean |>
  fsubset(race_gender == 'TOTAL') |>
  fsummarise(
    industry = funique(industry, sort = TRUE),
    industry_total = fsum(employ_n, industry)
    )

employed_2021_clean <- total_employed[employed_2021_clean, on = c('industry')]

# combine data from 2015 - 2020 with data for 2021
# correct industry strings in given dataset and remove NA industry with only NA values
employed <- employed |>
  ftransform(industry = stringr::str_replace_all(industry, pattern = '\\\r\\\n', replacement = ' '),
             minor_occupation = stringr::str_remove_all(minor_occupation,'-')) |>
  na_omit()
employed_ <- rbindlist(l = list(employed_2021_clean, employed), use.names = TRUE) |>
  fsubset(race_gender != 'TOTAL') |>
  fselect(industry, major_occupation, minor_occupation, race_gender, industry_total, employ_n, year)

# check, if there are noly 19 industries now
# funique(employed_$industry)
rm(list = setdiff(ls(), 'employed_'))

viz_data <- employed_ |>
  ### Data Wrangling for visualization
  fsubset(race_gender %in% c('Men','Women') & year %in% c(2015,2021)) |>
  fgroup_by(minor_occupation, year, race_gender) |>
  fsummarise(employ_n = fsum(employ_n)) |>
  fungroup() |>
  ftransform(employ_n = fcase(race_gender == 'Men', employ_n / 1000,
                              race_gender == 'Women', -employ_n / 1000)) |>
  fgroup_by(minor_occupation, year) |> 
  fmutate(total_employees = fsum(abs(employ_n))) |>
  fungroup()
  # Differentiate by growing and shrinking occupations
growing_occupation <- viz_data |>
  fselect(minor_occupation, year, total_employees) |>
  funique() |> fgroup_by(minor_occupation) |>
  fsummarise(growing_occupation = fifelse(fgrowth(total_employees) > 0,
                                       'Growing Occupation',
                                       'Shrinking Occupation')) |>
  fungroup() |> na_omit()
  # Join data
viz_data <- viz_data[growing_occupation, on = 'minor_occupation'] |>
   fmutate(
     minor_occupation = fcase(
       minor_occupation == 'Construction and extraction occupations', 'Construction and extraction occupations',
       minor_occupation == 'Farming, fishing, and forestry occupations', 'Farming, fishing, and\nforestry occupations',
       minor_occupation == 'Installation, maintenance, and repair occupations', 'Installation, maintenance, and\nrepair occupations',
       minor_occupation == 'Management, business, and financial operations occupations','Management, business, and financial\noperations occupations',
       minor_occupation == 'Office and administrative support occupations', 'Office and administrative support\noccupations',
       minor_occupation == 'Production occupations', 'Production occupations',
       minor_occupation == 'Professional and related occupations', 'Professional and related occupations',
       minor_occupation == 'Protective service occupations', 'Protective service occupations',
       minor_occupation == 'Sales and related occupations', 'Sales and related occupations',
       minor_occupation == 'Service occupations, except protective', 'Service occupations, except\nprotective',
       minor_occupation == 'Transportation and material moving occupations', 'Transportation and material moving\noccupations'
     )
   )

### Visualization
# Growing Occupations
grow_occ_pic <- ggplot() +
  theme_minimal() +
  geom_bar(
    data = viz_data |> fsubset(growing_occupation == 'Growing Occupation' & race_gender == 'Women'),
    aes(x = employ_n, y = minor_occupation, fill = race_gender, color = as.factor(year)),
    stat = 'identity', position = position_dodge(width = -.8), size = .1) +
  geom_bar(
    data = viz_data |> fsubset(growing_occupation == 'Growing Occupation' & race_gender == 'Men'),
    aes(x = employ_n, y = minor_occupation, fill = race_gender, color = as.factor(year)),
    stat = 'identity', position = position_dodge(width = -.8), size = .1) +
  scale_fill_manual(
    values = c('blue','red')
    ) +
  scale_color_manual(
    values = c('2015' = 'black','2021' = 'black'),
    breaks = c(' ','2021')
    ) +
  labs(
    title = 'Total Employees in Growing (left) and Shrinking (right) Occupations in the United States by Gender',
    subtitle = 'Employees in 1,000; 2015 (top) and 2021 (bottom)',
    color = '', fill = 'Gender',
    caption = '\nDustin Hennig | #TidyTuesday Week 30 | Data: BLS'
  ) +
  scale_x_continuous(
    labels = \(x) fifelse(x >= 0, x, -x)) +
  scale_y_discrete(position = "right") +
  theme(
    text = element_text(family = 'Source Sans Pro'),
    axis.title = element_blank(),
    axis.text.y = element_text(size = 12),
    legend.position = 'none',
    plot.caption.position = 'plot',
    plot.title = element_text(size = 24, face = 'italic'),
    plot.subtitle = element_text(size = 18, face = 'italic') 
  ) +
  guides(color = guide_legend(override.aes = list(fill = "white")))
grow_occ_pic

shrink_occ_pic <- ggplot() +
  theme_minimal() +
  geom_bar(
    data = viz_data |> fsubset(growing_occupation == 'Shrinking Occupation' & race_gender == 'Women'),
    aes(x = employ_n, y = minor_occupation, fill = race_gender, color = as.factor(year)),
    stat = 'identity', position = position_dodge(width = -.8), size = .1) +
  geom_bar(
    data = viz_data |> fsubset(growing_occupation == 'Shrinking Occupation' & race_gender == 'Men'),
    aes(x = employ_n, y = minor_occupation, fill = race_gender, color = as.factor(year)),
    stat = 'identity', position = position_dodge(width = -.8), size = .1) +
  scale_fill_manual(
    values = c('blue','red')
  ) +
  scale_color_manual(
    values = c('2015' = 'black','2021' = 'black'),
    breaks = c(' ',' ')
  ) +
  labs(
    title = ' ',
    subtitle = ' ',
    color = '', fill = ''
  ) +
  scale_x_continuous(
    labels = \(x) fifelse(x >= 0, x, -x)) +
  scale_y_discrete(position = "right") +
  theme(
    text = element_text(family = 'Source Sans Pro'),
    axis.title = element_blank(),
    legend.position = c(-.3,0),
    legend.direction = 'horizontal',
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 24, face = 'italic'),
    plot.subtitle = element_text(size = 18, face = 'italic') 
  ) +
  guides(color = guide_legend(override.aes = list(fill = "white")))
shrink_occ_pic

final_pic <- grow_occ_pic + shrink_occ_pic

png("tidyTuesday_week30.png", width = 25, height = 20, units = "cm", res = 144)
final_pic
dev.off()