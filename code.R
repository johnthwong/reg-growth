## ----packages-------------------------------------------------------------------------------------
#| echo: false
#| include: false

# install.packages(
#   c("tidyverse", "ivreg", "stargazer", "gridExtra", "readxl", "ivmodel", 
#     "rmdwc", "texreg"
#     )
# )

# project_directory <- "/Users/john/code/reg-iv"

# Replace "path" with "" if data folder is in project folder
data_directory <- "/Users/john/Documents/"

library(tidyverse)
library(ivreg)


## ----import-------------------------------------------------------------------

data_state_founding <-
  # This code imports data on when states are admitted
  # And also whether the state belonged to the confederacy
  read.csv(
    paste0(data_directory, "data/us_states/state_founding_data.csv")
    ) %>%
  mutate(
    # Change date format
    date_statehood = as.Date(date_statehood, "%B %d, %Y"),
    # Define "year_admission" as the year of statehood for non-confederate states
    # For confederate states, "year_admission" is forced to be 1868
    year_admission = 
       case_when(
         confederate == 0 ~ format(date_statehood, "%Y") %>% as.numeric(),
         .default = 1868),
    # Retain year of statehood as "year_establishment"
    year_establishment = format(date_statehood, "%Y") %>% as.numeric()
  ) %>%
  mutate(
    # Update name of DC for matching purposes
    state = replace(state, state == "Washington, D.C.", "District of Columbia")
      )

data_state_name <- 
  # Create a dataframe with both state names and abbreviations
  cbind(state.name, state.abb) %>% data.frame() %>%
  # Add DC
  rbind(
    data.frame(
      state.name = "District of Columbia",
      state.abb = "DC"
    )
  )

data_state_area <- 
  # This code imports data on the surface area of states
  read.csv(
    paste0(data_directory, "data/us_states/us_state_area.csv")
  )  %>%
  # According to the census, latitude/longitude is measured at 
  # ...the internal point/centroid of each state. See:
  # https://www.census.gov/programs-surveys/geography/about/glossary.html#par_textimage_3
  mutate(
    # This code measures the Euclidean distance between each non-DC state to DC.
    longitude_dc = longitude[state == "District of Columbia"],
    latitude_dc = latitude[state == "District of Columbia"],
    euclid = sqrt( (latitude - latitude_dc)^2 + (longitude - longitude_dc)^2 )
  )

import_regdata <- function(){
  # The following imports RegData files by state and merging them into two
  # ...dataframes. First file is agency codes and second file is statutes.
  
  # Define a vector of state names to iterate through. Exclude Arkansas and WV
  # ...because RegData does not cover those states.
  data_regdata_states <-
    state.name %>% 
    str_subset(("Arkansas|West Virginia"), negate = TRUE) %>%
    c(., "dc")
  
  # Construct the filenames, import each file, and store them as 
  # ...a unique object (temporarily).
  for (i in data_regdata_states){
    csvname <- tolower(gsub(" ", "_", i))
    dfname_reg <- paste("data_reg_", csvname, sep = "")
    dfname_statute <- paste("data_statute_", csvname, sep = "")
    path_reg <- 
      paste(
        data_directory,
        "data/quantgov/State-RegData-5-0_Regulations/data/", 
        csvname, "/", csvname, "_restrictions.csv", 
        sep = ""
        )
    assign(dfname_reg, read.csv(paste(path_reg)))
    path_statute <- 
      paste(
        data_directory,
        "data/quantgov/State-RegData-5-0_Statutes/data/", 
        csvname, "_statutes/", csvname, "_statutes_restrictions.csv", 
        sep = ""
      )
    assign(dfname_statute, read.csv(paste(path_statute)))
    
  }
  
  # Create two dataframes
  data_reg_2023 <<- data.frame()
  data_statute_2023 <<- data.frame()
  
  # Iterate through each state. Merge that state's codes to data_reg_2023
  # ...and that state's statutes to data_statute_2023.
  for(i in data_regdata_states){
    csvname <- tolower(gsub(" ", "_", i))
    dfname_reg <- paste("data_reg_", csvname, sep = "")
    df_reg <- get(dfname_reg)
    data_reg_2023 <<- rbind(data_reg_2023, df_reg)
    dfname_statute <- paste("data_statute_", csvname, sep = "")
    df_statute <- get(dfname_statute)
    data_statute_2023 <<- rbind(data_statute_2023, df_statute)
  }
}

# Execute function
import_regdata()

data_SQGDP9 <- 
  # This code imports quarterly GDP data from the project data directory
  # It is pulled from BEA-NIPA from some point in time
  read.csv(
    file =  paste0(data_directory, "data/bea_nipa/SQGDP9.csv"),
    na.strings = c('NA', '0')
  ) %>%
  select(-CL_UNIT, -UNIT_MULT, -code) %>%
  filter(month(year) == 1) %>%
  mutate(year = year(year)) %>%
  arrange(state, year) %>%
  rename(gdp_chained = value)

data_SQINC1 <- 
  # This code imports quarterly personal income data, also pulled from BEA-NIPA
  # This dataframe includes population data
  read.csv(
    file =  paste0(data_directory, "data/bea_nipa/SQINC1.csv"),
    na.strings = c('NA', '0')
  ) %>%
  select(-CL_UNIT, -UNIT_MULT, -Description) %>%
  filter(month(year) == 1) %>%
  mutate(year = year(year)) %>%
  arrange(state, year) %>%
    pivot_wider(
      names_from = code,
      values_from = value
    )

# Rename variables
colnames(data_SQINC1) <-
  c("state", "year", "inc_capita", "pop", "inc")

data_SQGDP1 <- 
  # This code imports early GDP data, pulled from BEA-NIPA
  read.csv(
    file =  paste0(data_directory, "data/bea_nipa/SQGDP1.csv"),
    na.strings = c('NA', '0')
  ) %>%
  select(year, state, value) %>%
  arrange(state, year) %>%
  group_by(state) %>%
  slice_head() %>%
  rename(gdp_chained_earliest = value) %>%
  mutate(year = year(year))
  

import_hsus <- function(){
  # The following imports historical statistics of the US
  # The two variables of interest are population and area, both as at the
  # ...first census taken subsequent to founding.
  
  # Note that because this dataset does not cover DC, DC is dropped from our
  # study even though we have DC's growth and regulatory data.
  
  # Detect all relevant files
  str_hsus_filenames <- list.files(
    path = paste0(data_directory, "data/hsus/HSUS_TableGroupAa2244-6550/"), 
    pattern="\\.xls")
  
  # Loop through each file name, import the file, assign state name, bind to
  # ...one dataframe called data_hsus.
  data_hsus <- data.frame()
  for(i in 1:length(str_hsus_filenames) ){
    name <- paste(
      data_directory,
      "data/hsus/HSUS_TableGroupAa2244-6550/", 
      str_hsus_filenames[i], 
      sep = "")
    df <- readxl::read_excel(name) %>%
      select(1:4) %>%
      # Note that the HSUS files are ordered by column IDs
      # ...which in turn are assigned to states by alphabetical order
      # Thus, the first file in the HSUS directory is the first state
      # ...alphabetically, which is Alabama, and so forth.
      # Thus, we can match the state name of the first row of our 
      # ...data_state_name, which is also sorted alphabetically.
      mutate(
        state = data_state_name[i, "state.name"]
      )
    colnames(df) <- c("year", "area", "density", "pop", "state")
    data_hsus <- rbind(data_hsus, df)
  }
  
  # Clean data_hsus. Assign output to global environment (<<-).
  data_hsus_founding <<- data_hsus %>%
    group_by(state) %>%
    arrange(state,year) %>%
    summarize(
      pop_admission = first(pop, na_rm = TRUE)/10^6,
      area = first(area, na_rm = TRUE)/10^6
    )
}

# Execute function.
import_hsus()
  
# Clouse's data
data_clouse <- 
  # This code imports data from Julia Clouse's dataset on state constitutions.
  readxl::read_excel(
    paste0(
      data_directory,
      "data/clouse_julia/U.S.-State-Constitutions-Dataset--1776-2017--Feb-2019-Version-.xlsx"
    ),
    skip = 2
    ) %>%
  rename(
    state = "U.S. State",
    constitution_year = "Constitution Year"
  ) %>%
  arrange(state, constitution_year) %>%
  group_by(state) %>%
  filter(row_number()==1) %>%
  select(state, constitution_year) %>%
  ungroup()



## ----merge--------------------------------------------------------------------

# Panel of latest BEA-NIPA and RegData
df32 <-
  data_reg_2023 %>%
  mutate(year = year(as.Date(date_collected))) %>%
  group_by(jurisdiction_name, year) %>%
  summarize(restrictions_reg = sum(restrictions)) %>%
  ungroup() 

df32a <- 
  data_statute_2023 %>%
  mutate(year = year(as.Date(date_collected))) %>%
  group_by(jurisdiction_name, year) %>%
  summarize(restrictions_statute = sum(restrictions)) %>%
  ungroup() 
  
df33 <- 
  left_join(
    data_state_founding,
    data_state_name,
    by = c("state" = "state.name")
    ) %>%
  left_join(
    data_hsus_founding, 
    by = "state"
    ) %>%
  left_join(
    data_clouse, 
    by = "state"
    ) %>%
  left_join(
     data_state_area %>% select(state, latitude, longitude, euclid),
     by = "state"
  ) %>%
  left_join(
    data_SQGDP9,
    by = "state"
    ) %>%
  left_join(
    data_SQINC1,
    by = c("state", "year")
  ) %>%
  group_by(state) %>%
  mutate(
    gdp_chained_capita = gdp_chained/pop,
    # generate growth rates
    across(starts_with("inc") | contains("gdp") | ends_with("pop"),
            ~ log(.x/lag(.x, n = 1L)),
            .names = "{.col}_1yrchg"
      )
    ) %>%
  ungroup() %>%
  left_join(
    df32,
    by = c("state" = "jurisdiction_name", "year")
    ) %>%
  left_join(
    df32a,
    by = c("state" = "jurisdiction_name", "year")
  ) %>%
  group_by(state) %>%
  mutate(
    restrictions = restrictions_reg + restrictions_statute,
    restrictions_1L = lag(restrictions, n = 1L),
    restrictions_2L = lag(restrictions, n = 2L)
    ) %>%
  ungroup() %>%
  mutate(
    age = year - year_admission - 2,
    age_nocivilwar = year - year_establishment - 2
  )

mean(df33$gdp_chained_1yrchg, na.rm=TRUE)

# No. of variables by state
df34 <- df33 %>%
  filter(
    !is.na(gdp_chained_1yrchg) & 
      !is.na(restrictions_2L) & 
      !is.na(area) & 
      !is.na(pop_admission)
    ) 

##----fig-state-count----

df35 <- df34 %>%
  group_by(state) %>%
  summarize(n = n())

df35 %>% 
  ggplot(aes(x = fct_reorder(state, n), y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme(axis.text.x = element_text(angle = 90,  vjust = 0.5, hjust = 1)) +
  labs(
    x = "",
    y = "Observation Count"
  ) +
  scale_y_continuous(breaks = seq(0,2,1))

##----tbl-state-count----
tbl_state_count <- df34 %>%
  group_by(state) %>%
  summarize(count = n()) %>%
  group_by(count) %>%
  summarize(count = n())

## ----ols----------------------------------------------------------------------

# Check Outcome ~ Treatment
lm37 <- df33 %>%
  lm(data = ., gdp_chained_1yrchg ~ log(restrictions_2L))
summary(lm37)

lm15 <- df33 %>%
  lm(data = ., gdp_chained_1yrchg ~ log(restrictions_2L) + pop_admission + area)
summary(lm15)

# Check Treatment ~ IV

lm34 <- df34 %>%
  lm(data = ., log(restrictions_2L) ~ age)
summary(lm34)

lm16 <- df34 %>%
  lm(data = ., log(restrictions_2L) ~ age + pop_admission + area)
summary(lm16)


## ----fig-reg-density--------------------------------

df38 <- df33 %>% 
  group_by(state) %>%
  filter(!is.na(restrictions)) %>%
  slice_tail(n = 1) %>%
  ungroup()


plot3a <- df38 %>% 
  ggplot(data = ., aes(x = restrictions)) +
  geom_density(color = "Blue") +
  labs(
    x = "Restrictions",
    y = "Probability Density"
  ) +
  scale_x_continuous(labels = scales::label_number(scale = 1e-3, suffix = "K"))

plot3b <- df38 %>%
  ggplot(data = ., aes(x = restrictions)) +
  geom_density(color = "Blue") +
  labs(
    x = "Restrictions (log-scale)",
    y = ""
  ) +
  scale_x_log10(labels = scales::label_number(scale = 1e-3, suffix = "K")) 

gridExtra::grid.arrange(plot3a, plot3b, ncol = 2)


## ----tbl-summary-stat---------------------------------------------------------
df34 %>%
  select(
    restrictions_2L, 
    gdp_chained_1yrchg, age, pop_admission, area, constitution_year, 
    euclid, year
  ) %>%
  # filter(year >= 2021) %>%
  select(-year) %>%
  labelled::set_variable_labels(
    restrictions_2L = "Restriction Count, t - 2",
    gdp_chained_1yrchg = "Chained Real GDP Growth, 1-year",
    # date_statehood = "Statehood Date",
    age = "State Age",
    pop_admission = "Population at Admission (Million)",
    area = "State Area at Admission (Million km^2)",
    constitution_year = "Date of First Constitution",
    euclid = "Euclidean Distance from D.C. (Latitude-Longitude)"
  ) %>%
  # gtsummary::tbl_summary(by = year)
  # gtsummary::tbl_summary(by = year) %>%
  gtsummary::tbl_summary(
    type = gtsummary::all_continuous() ~ "continuous2",
    statistic = list(
      gtsummary::all_continuous() ~ c(
        "{mean} ({sd})",
        "({min}, {p25}, {p75}, {max})"
      )
    )
  ) %>%
  gtsummary::as_gt() %>%
  gt::as_latex()

## ----tsls---------------------------------------------------------------------

# new data
iv28 <- df34 %>%
  ivreg(data = ., gdp_chained_1yrchg ~ log(restrictions_2L) | age)
summary(iv28)

iv29 <- df33 %>%
  ivreg(
    data = ., 
    gdp_chained_1yrchg ~ log(restrictions_2L) + pop_admission | 
      age + pop_admission)
summary(iv29)

iv30 <- df34 %>%
  ivreg(
    data = ., 
    gdp_chained_1yrchg ~ log(restrictions_2L) + pop_admission + area | 
      age + pop_admission + area)
summary(iv30)

# time-invariant instrument
iv35 <- df34 %>%
  ivreg(
    data = .,
    gdp_chained_1yrchg ~ log(restrictions_2L) + pop_admission + area | year_establishment + pop_admission + area
  )
summary(iv35)

lm45 <- df34 %>%
  lm(
    data = .,
    gdp_chained_1yrchg ~ year_establishment + pop_admission + area
  )

lm46 <- df34 %>%
  lm(
    data = .,
    gdp_chained_1yrchg ~ log(restrictions_2L) + pop_admission + area + residuals(lm45)
  )
summary(lm46)

# time fixed effects
iv36 <- df34 %>%
  ivreg(
    data = .,
    gdp_chained_1yrchg ~ log(restrictions_2L) + pop_admission + area + year | age + pop_admission + area + as.factor(year)
  )
summary(iv36)

lm47 <- df34 %>%
  lm(
    data = .,
    gdp_chained_1yrchg ~ age + pop_admission + area + year
  )
summary(lm47)

lm48 <- df34 %>%
  lm(
    data = .,
    gdp_chained_1yrchg ~ log(restrictions_2L) + pop_admission + area + residuals(lm47)
  )
summary(lm48)


# no civil war

iv37 <- df33 %>%
  ivreg(
    data = ., 
    gdp_chained_1yrchg ~ log(restrictions_2L) + pop_admission + area + as.factor(year) | 
      age_nocivilwar + pop_admission + area + as.factor(year)) 
lm49 <- df33 %>%
  lm(
    data = .,
    log(restrictions_2L) ~ age_nocivilwar + pop_admission + area + as.factor(year)
  )


# second IV

iv31 <- df33 %>%
  ivreg(
    data = ., 
    gdp_chained_1yrchg ~ log(restrictions_2L) + pop_admission + area | 
      age + constitution_year + pop_admission + area)
summary(iv31)

# three IVs
iv34 <- df33 %>%
  ivreg(
    data = ., 
    gdp_chained_1yrchg ~ log(restrictions_2L) + pop_admission + area | 
      age + constitution_year + euclid + pop_admission + area)
summary(iv34)

# per-capita
iv55 <- df34 %>%
  ivreg(
    data = ., 
    gdp_chained_capita_1yrchg ~ log(restrictions_2L) + pop_admission + area | 
      age + pop_admission + area)

iv56 <- df33 %>%
  ivreg(
    data = ., 
    pop_1yrchg ~ log(restrictions_2L) + pop_admission + area | 
      age + pop_admission + area)

## ----tbl-tsls-----------------------------------------------------------------

texreg::texreg(
  custom.model.names = c("OLS", "Stage 1", "Stage 2", "Stage 1", "Stage 2"),
  custom.coef.names = c(
    "Intercept",
    "ln$R_{t-2}$",
    "Population at Admission",
    "Area Size",
    "State Age"
  ),
  reorder.coef = c(2, 5, 3, 4, 1),
  custom.gof.rows = list(
    "R$^2$" = 
      list(
        round(summary(lm15)$r.squared, 2), 
        round(summary(lm34)$r.squared, 2), 
        "",
        round(summary(lm16)$r.squared, 2), 
        ""
      ),
    "F Statistic" =
      list(
        round(as.numeric(summary(lm15)$f[1]), 2),
        round(as.numeric(summary(lm34)$f[1]), 2),
        "",
        round(as.numeric(summary(lm16)$f[1]), 2),
        ""
      )
  ),
  custom.header = list(
    "$\\%\\Delta$GDP" = 1,
    "ln$R_{t-2}$" = 2,
    "$\\%\\Delta$GDP" = 3,
    "ln$R_{t-2}$" = 4,
    "$\\%\\Delta$GDP" = 5
  ),
  digits = 4,
  include.rsquared = FALSE,
  include.adjrs = FALSE,
  float.pos = "H",
  l = list(lm15, lm34, iv28, lm16, iv30)
)

## ----tbl-tsls-2-----------------------------------------------------------------

texreg::texreg(
  custom.model.names = c("Stage 1", "Stage 2","Stage 1", "Stage 2","Stage 1", "Stage 2"),
  custom.coef.map = list(
    "log(restrictions_2L)" = "ln$R_{t-2}$",
    "age" = "State Age",
    "year_establishment" = "$T_{i}$",
    "age_nocivilwar" = "State Age 2",
    "pop_admission" = "Pop. at Admission",
    "area" = "Area Size",
    "(Intercept)" = "Intercept"
  ),
  custom.gof.rows = list(
    "R$^2$" = 
      list(
        round(summary(lm47)$r.squared, 2), 
        "",
        round(summary(lm45)$r.squared, 2), 
        "",
        round(summary(lm49)$r.squared, 2), 
        ""
      ),
    "F Statistic" =
      list(
        round(as.numeric(summary(lm47)$f[1]), 2),
        "",
        round(as.numeric(summary(lm45)$f[1]), 2),
        "",
        round(as.numeric(summary(lm49)$f[1]), 2),
        ""
      ),
    "Year Effect" = c(
      rep("YES", 2),
      rep("NO", 2),
      rep("YES", 2)
    )
  ),
  custom.header = list(
    "ln$R_{t-2}$" = 1,
    "$\\%\\Delta$GDP" = 2,
    "ln$R_{t-2}$" = 3,
    "$\\%\\Delta$GDP" = 4,
    "ln$R_{t-2}$" = 5,
    "$\\%\\Delta$GDP" = 6
  ),
  digits = 4,
  include.rsquared = FALSE,
  include.adjrs = FALSE,
  float.pos = "H",
  no.margin = TRUE,
  l = list(lm47, iv36, lm45, iv35, lm49, iv37)
)


## ----j_test----
lm17 <- lm(
  residuals(iv31) ~ age + constitution_year + pop_admission + area, 
  data = df34)
j1 <- car::linearHypothesis(
  model = lm17,
  hypothesis.matrix = c(
    "age = 0",
    "constitution_year = 0"
  ),
  test = "Chisq"
)
j1_p <- pchisq(j1[2, 5], df = 1, lower.tail = FALSE)

# j test with one variable doesn't work, because 0 degrees of freedom
car::linearHypothesis(
  model = lm(residuals(iv30) ~ age + pop_admission + area, data = df34),
  hypothesis.matrix = c(
    "age = 0"
  ),
  test = "Chisq"
)

# with three variables
lm19 <- lm(
  residuals(iv34) ~ age + constitution_year + euclid + pop_admission + area, 
  data = df34)
j2 <- car::linearHypothesis(
  model = lm19,
  hypothesis.matrix = c(
    "age = 0",
    "constitution_year = 0",
    "euclid = 0"
  ),
  test = "Chisq"
)
j2_p <- pchisq(j2[2, 5], df = 1, lower.tail = FALSE)

## ----ar_test-----
ar_df <- df33
ar_y <- ar_df %>% select(gdp_chained_1yrchg) %>% pull()
ar_x <- ar_df %>% select(restrictions_2L) %>% log() %>% pull()
ar_z <- ar_df %>% select(age)
ar_w <- ar_df %>% select(pop_admission, area)

iv09 <- ivmodel::ivmodel(ar_y, ar_x, ar_z, ar_w)
ar1 <- ivmodel::AR.test(iv09) 

# P.value and f-stat refers to the AR test result. Meanwhile, confidence interval is that of the treatment's coefficient, which is -0.0301120424451205 in the IV model.

## ----tbl-j-test----
texreg::texreg(
  digits = 4,
  custom.coef.map = list(
    "age" = "State Age",
    "constitution_year" = "Year of Initial Constitution",
    "euclid" = "Euclidean Distance from D.C.",
    "(Intercept)" = "Intercept"
  ),
  include.adjrs = FALSE,
  custom.gof.rows = list(
    "J-statistic" = c(
      j1[2, "Chisq"],
      j2[2, "Chisq"]
    ),
    "P-value" = c(
      paste(round(j1_p, 4), "(df = 1)"),
      paste(round(j2_p, 4), "(df = 2)")
    ),
    "Controls" = c(
      rep("YES", 2)
    )
  ),
  custom.header = list("2nd-Stage Residuals" = 1:2),
  custom.model.names = c(
    "Two IVs",
    "Three IVs"
  ),
  caption = "Overidentifying Restrictions Test (J-Test)",
  custom.note = 
    "P-value indicates the probability that the null (that instruments are exogenous) is true.",
  float.pos = "H",
  l = list(lm17, lm19)
) 

## ----lag----
iv42 <- df33 %>%
  ivreg(
    data = ., 
    gdp_chained_1yrchg ~ log(restrictions_1L) + pop_admission + area | 
      age + pop_admission + area)

iv43 <- df33 %>%
  ivreg(
    data = ., 
    gdp_chained_1yrchg ~ log(restrictions) + pop_admission + area | 
      age + pop_admission + area) 

iv44 <- df33 %>%
  ivreg(
    data = ., 
    gdp_chained_1yrchg ~ 
      log(restrictions_1L) + log(restrictions_2L) + pop_admission + area | 
      age + constitution_year + pop_admission + area)

iv45 <- df33 %>%
  ivreg(
    data = ., 
    gdp_chained_1yrchg ~ 
      log(restrictions) + log(restrictions_1L) + pop_admission + area | 
      age + constitution_year + pop_admission + area)

## ----tbl-lag----
texreg::texreg(
  digits = 4,
  custom.coef.map = list(
    "log(restrictions)" = "$R_{t}$",
    "log(restrictions_1L)" = "$R_{t-1}$",
    "log(restrictions_2L)" = "$R_{t-2}$",
    "(Intercept)" = "Intercept"
  ),
  include.rsquared = FALSE,
  include.adjrs = FALSE,
  custom.gof.rows = list(
    "Controls" = c(
      rep("YES", 5)
    ),
    "Second IV?" = c(
      rep("NO", 3),
      rep("YES", 2)
    )
  ),
  custom.header = list("$\\%\\Delta$GDP" = 1:5),
  custom.model.names = c("Lag-2", "Lag-1", "Lag-0", "Lag-1,2", "Lag-0,1"),
  float.pos = "H",
  l = list(iv30, iv42, iv43, iv44, iv45)
)# %>%
#   gsub(
#     " & Model 1 & Model 2 & Model 3 & Model 4 & Model 5 \\\\\n", "", ., 
#     fixed = T
#     )

## ----linearization----

# linear
summary(iv30)

iv46 <- df33 %>%
  mutate(restrictions_2L = restrictions_2L/1e6) %>% # scale x for aesthetics
  ivreg(
    data = ., 
    gdp_chained_1yrchg ~ 
      (restrictions_2L) + pop_admission + area | 
      age + pop_admission + area)

lm21 <- 
  df33 %>%
  lm(
    data = .,
    restrictions_2L ~ age + pop_admission + area
  )
summary(lm21)

# quadratic 
# for some reason it doesn't unless I use the smaller dataframe AND use the poly() function
# is it because R doesn't recognize that variables that are created in-formula can have NAs?

# df33 %>%
#   ivreg(data = ., gdp_chained_1yrchg ~ I(restrictions_2L^2) + pop_admission + area | age + constitution_year + pop_admission + area) %>%
#   summary()

df36 <- df33 %>%
  mutate(restrictions_2L_2 = restrictions_2L^2)
iv47 <- df36 %>%
  ivreg(data = ., gdp_chained_1yrchg ~ restrictions_2L_2 + pop_admission + area | age + pop_admission + area) 
# df36 %>%
#   ivreg(data = ., gdp_chained_1yrchg ~ restrictions_2L + restrictions_2L_2 + pop_admission + area | age + constitution_year + pop_admission + area) %>%
#   summary()

# df33 %>%
#   ivreg(data = ., gdp_chained_1yrchg ~ poly(restrictions_2L, 2) + pop_admission + area | age + pop_admission + area) %>%
#   summary()
df34 %>%
  ivreg(
    data = ., 
    gdp_chained_1yrchg ~ poly(restrictions_2L, 2) + pop_admission + area | 
      age + pop_admission + area) %>%
  summary()
# df34 %>%
#   ivreg(data = ., gdp_chained_1yrchg ~ poly(restrictions_2L, 2, raw = TRUE) + pop_admission + area | age + pop_admission + area) %>%
#   summary()
# # having poly() generate raw polynomials replicates the problem.
# df34 %>%
#   ivreg(data = ., gdp_chained_1yrchg ~ poly(restrictions_2L, 2) + pop_admission + area | age + constitution_year + pop_admission + area) %>%
#   summary()

# troubleshoot by running lm?
df37 <- df34 %>%
  mutate(
    restrictions_2L_2 = restrictions_2L^2
    )

iv48 <- df34 %>%
  mutate(
    restrictions_2L_2 = restrictions_2L # placeholder for texreg
    ) %>%
  ivreg(data = ., gdp_chained_1yrchg ~ restrictions_2L_2 + pop_admission + area | age + pop_admission + area)

# try without controls?
df37 %>%
  ivreg(data = ., gdp_chained_1yrchg ~ restrictions_2L_2 | age) %>%
  summary()
# wait it works!

# df37 %>%
#   ivreg(data = ., gdp_chained_1yrchg ~ restrictions_2L_2 + pop_admission | age + pop_admission) %>%
#   summary()
# df37 %>%
#   ivreg(data = ., gdp_chained_1yrchg ~ restrictions_2L_2 + area | age + area) %>%
#   summary()
# # the inclusion of either controls lead to singularity

# correlations
cor(df37$restrictions_2L_2, df37$pop_admission)

lm18 <- df37 %>%
  lm(data = ., restrictions_2L_2 ~ age + pop_admission + area) 
summary(lm18)
predict(lm18)
# but it solves.

iv33 <- df34 %>%
  lm(data = ., gdp_chained_1yrchg ~ predict(lm18) + pop_admission + area) 
summary(iv33)
# this solves too

max(df37$restrictions_2L_2)
max(df36$restrictions_2L_2, na.rm = TRUE)

# try different IV package
ar_df <- df37
ar_y <- ar_df %>% select(gdp_chained_1yrchg) %>% pull()
ar_x <- ar_df %>% select(restrictions_2L_2) %>% pull()/1e12 # scale for aesthetic reasons
# ar_d <- rnorm(77)^2
ar_z <- ar_df %>% select(age)
ar_w <- ar_df %>% select(pop_admission, area)
# iv32 <- ivmodel::ivmodel(ar_y, ar_x, ar_z, ar_w)
# summary(iv32)
# same issue

ar_Z <- cbind(rep(1, nrow(ar_df)), ar_z, ar_w) %>%
  as.matrix()
det(t(ar_Z) %*% ar_Z)
ar_X <- cbind(rep(1, nrow(ar_df)), ar_x, ar_w) %>%
  as.matrix()
det (t(ar_X) %*% ar_X)
# solve(t(ar_X) %*% ar_X)
# ar_X's inverse cannot be computed
ar_Z_projection <- ar_Z %*% solve(t(ar_Z) %*% ar_Z) %*% t(ar_Z) 

ar_X_pred <- ar_Z_projection %*% ar_X

det( t(ar_X_pred) %*% ar_X_pred )
Matrix::rankMatrix(ar_X_pred)
Matrix::rankMatrix(t(ar_X_pred))
t(ar_X_pred)
cor(ar_X_pred[,2], ar_X_pred[,1])
# solve( t(ar_X_pred) %*% ar_X_pred )

# manual matrix inversion
t(ar_X_pred) %*% ar_X_pred
ar_X_pred_adj <- RConics::adjoint(t(ar_X_pred) %*% ar_X_pred)
ar_X_pred_inverse <- ar_X_pred_adj/det( t(ar_X_pred) %*% ar_X_pred )
# ar_beta <- solve(t(ar_X_pred)%*%ar_X_pred) %*% t(ar_X_pred) %*% ar_y
ar_beta <- ar_X_pred_inverse %*% t(ar_X_pred) %*% ar_y
# these estimates match iv33.

# variance estimation
# solve( t(ar_X) %*% ar_Z_projection %*% ar_X ) 
# this leads to singularity
ar_beta_var_cov <- var(ar_y) * 
  RConics::adjoint( t(ar_X) %*% ar_Z_projection %*% ar_X)/ 
  det( t(ar_X) %*% ar_Z_projection %*% ar_X)
ar_beta_se <- ar_beta_var_cov %>% diag() %>% sqrt()

# t-stats
ar_beta/ar_beta_se
ar_pval <- 2*as.numeric( # 2x because it's a two-tailed test
  pt(
    abs(ar_beta/ar_beta_se), 
    nrow(ar_X) - ncol(ar_X) - 1, 
    lower.tail = FALSE)
) 

## ----tbl-linearization----
texreg::texreg(
  digits = 4,
  custom.coef.map = list(
    "log(restrictions_2L)" = "ln$R_{t-2}$",
    "restrictions_2L" = "$R_{t-2}$ (Millions)",
    "restrictions_2L_2" = "$R_{t-2}^2$ (Trillions)",
    "(Intercept)" = "Intercept"
  ),
  include.rsquared = FALSE,
  include.adjrs = FALSE,
  custom.gof.rows = list(
    "1st Stage F-statistic" = c(
      round(summary(lm16)$f["value"], 4),
      round(summary(lm21)$f["value"], 4),
      round(summary(lm18)$f["value"], 4)
    ),
    "Controls" = c(
      rep("YES", 3)
    )
  ),
  custom.header = list("$\\%\\Delta$GDP" = 1:3),
  override.coef = list(
    iv30$coefficients,
    iv46$coefficients,
    as.numeric(ar_beta)
  ),
  override.se = list(
    sqrt(diag(summary(iv30)$vcov)),
    sqrt(diag(summary(iv46)$vcov)),
    as.numeric(ar_beta_se)
  ),
  override.pvalues = list(
    summary(iv30)$coefficients[, "Pr(>|t|)"],
    summary(iv46)$coefficients[, "Pr(>|t|)"],
    ar_pval
  ),
  float.pos = "H",
  l = list(iv30, iv46, iv48)
) %>%
  gsub(" & Model 1 & Model 2 & Model 3 \\\\\n", "", ., fixed = T)

## ----fig-treatment-instrument-------------------------------------------------
df34 %>%
  filter(year == 2024) %>%
  ggplot(data = ., aes(x = age, y = restrictions_2L, label = state.abb))+
  # geom_point(aes(size = gdp_chained_1yrchg), color = "blue") +
  # scale_size_continuous(range = c(1, 25)) +
  geom_point(aes(color = gdp_chained_1yrchg), size = 3) +
  scale_color_gradient() +
  # geom_text(check_overlap = TRUE, vjust = -1, hjust = 1) +
  ggrepel::geom_text_repel() +
  theme(legend.position = "bottom") +
  labs(
    x = "State Age",
    y = "Restrictions (log-scale)",
    color = "GDP Growth"
  ) +
  scale_y_log10(
    limits = c(NA, max(df34$restrictions_2L)+2e05)
  )

##----fig-age-dist------
df33 %>% 
  filter(year == 2024) %>%
  ggplot(aes(x = age)) +
  geom_histogram(fill = "blue", binwidth = 10, color = "white", boundary = 1) +
  scale_x_continuous(
    breaks = seq(round(min(df33$age), -1), round(max(df33$age), -1), by = 10)) +
  labs(
    x = "State Age (Years)",
    y = "Observation Count"
  )


## ----placebo------------------------------------------------------------------

fx_ivreg_main_vary_z <- function(z){
  formula = as.formula(
    paste(
      "inc_1yrchg ~ log(restrictions_2L) + pop_admission + area |", 
      z, 
      "+ pop_admission + area")
    )
  df34 %>% ivreg(data = ., formula)
}

str_vars_for_placebo <- c("longitude", "latitude", "longitude + latitude")

for(i in str_vars_for_placebo){
  index = match(i, str_vars_for_placebo)
  model_name = paste("iv", 24 + index, sep = "")
  assign(model_name, fx_ivreg_main_vary_z(i))
}

fx_lm_main_vary_z <- function(z){
  formula = as.formula(paste("log(restrictions_2L) ~", z))
  df34 %>% lm(data = ., formula)
}

for(i in str_vars_for_placebo){
  index = match(i, str_vars_for_placebo)
  model_name = paste("lm", 12 + index, sep = "")
  assign(model_name, fx_lm_main_vary_z(i))
}


## ----tbl-placebo--------------------------------------------------------------

# stargazer::stargazer(
#   table.placement = "H",
#   iv25,
#   iv26,
#   iv27,
#   column.sep.width = "0pt",
#   no.space = TRUE,
#   digits = 4,
#   type = "latex",
#   title = "",
#   dep.var.labels.include = FALSE,
#   #dep.var.labels = ,
#   dep.var.caption = "Outcome is GDP Growth",
#   column.labels = c("Longitude", "Latitude", "Longitude + Latitude"),
#   # column.separate = c(4, 5),
#   omit.stat=c("rsq", "adj.rsq", "ser"),
#   # covariate.labels = str_reg_tbl_rownames1,
#   model.names = FALSE,
#   header = FALSE
# )

texreg::texreg(
  digits = 4,
  custom.coef.names = c(
    "Intercept",
    "ln$R_{t-2}$",
    "Population at Admission",
    "Area Size"
  ),
  reorder.coef = c(2, 3, 4, 1),
  include.rsquared = FALSE,
  include.adjrs = FALSE,
  custom.gof.rows = list(
    "Controls" = c(
      rep("YES", 3)
    )
  ),
  custom.header = list("$\\%\\Delta$GDP" = 1:3),
  custom.model.names = c("Longitude", "Latitude", "Longitude + Latitude"),
  float.pos = "H",
  l = list(iv25, iv26, iv27)
)

## ----leave-one-out---------------------------------------------------------

results_1 <- tibble(
  state = character(),
  estimate = numeric(),
  std_error = numeric(),
  t_value = numeric(),
  p_value = numeric()
)

for (i in unique(df34$state)) {
  df <- df34 %>%
    filter(state != i)

  iv_model <- df %>%
    ivreg(
      data = ., 
      gdp_chained_1yrchg ~ log(restrictions_2L) + pop_admission + area | 
        age + pop_admission + area)
  
  iv_summary <- summary(iv_model)
  
  # Extract coefficients for the second variable
  coefficients <- iv_summary$coefficients[2, ]
  
  # Create a tibble for the current iteration's results
  results_vec <- tibble(
    state = i,
    estimate = coefficients["Estimate"],
    std_error = coefficients["Std. Error"],
    t_value = coefficients["t value"],
    p_value = coefficients["Pr(>|t|)"]
  )
  
  # Bind the current iteration's results to the main results tibble
  results_1 <- bind_rows(results_1, results_vec)
}

results_1 <- results_1 %>% mutate(
  sig = ifelse(
    p_value < 0.01, 0.01,
    ifelse(
      p_value < 0.05, 0.05, 
      ifelse(
        p_value < 0.1,
        0.1,
        1
      )
    )
    ) %>% as.factor()
)

# by observation
results_2 <- tibble(
  iteration = numeric(),
  state = character(),
  estimate = numeric(),
  std_error = numeric(),
  t_value = numeric(),
  p_value = numeric()
)

for (i in 1:nrow(df34)) {
  state <- df34 %>%
    slice(i) %>%
    select(state.abb) %>%
    pull()
  
  df <- df34 %>%
    slice(-i)
  
  iv_model <- df %>%
    ivreg(
      data = ., 
      gdp_chained_1yrchg ~ log(restrictions_2L) + pop_admission + area | 
        age + pop_admission + area)
  
  iv_summary <- summary(iv_model)
  
  # Extract coefficients for the second variable
  coefficients <- iv_summary$coefficients[2, ]
  
  # Create a tibble for the current iteration's results
  results_vec <- tibble(
    iteration = i,
    state = state,
    estimate = coefficients["Estimate"],
    std_error = coefficients["Std. Error"],
    t_value = coefficients["t value"],
    p_value = coefficients["Pr(>|t|)"]
  )
  
  # Bind the current iteration's results to the main results tibble
  results_2 <- bind_rows(results_2, results_vec)
}

results_2 <- results_2 %>% mutate(
  sig = ifelse(
    p_value < 0.05, 0.05, 
    ifelse(
      p_value < 0.1,
      0.1,
      1
    )
  ) %>% as.factor()
)

ggplot(
  data = results_2,
  aes(x = iteration, y = estimate, fill = sig)
  ) + 
  geom_col() +
  scale_y_reverse() +
  geom_hline(yintercept = iv30$coefficients[2]) +
  scale_fill_brewer(palette = "Accent") +
  labs(
    x = "Omitted Observation's Number"
  )

## ----fig-leave-one-out---------------------------------------------------------
ggplot(
  data = results_1,
  aes(x = state, y = estimate, fill = sig)
) + 
  geom_col() +
  scale_y_reverse() +
  geom_hline(
    yintercept = iv30$coefficients[2], 
    linetype = "dashed", 
    color = "black" # Optional: Customize the line's appearance
  ) +
  annotate(
    "text", 
    x = Inf, 
    y = iv30$coefficients[2], 
    label = "Full sample estimate", 
    hjust = 1.1, 
    vjust = -0.5, 
    color = "black" # Match color with the line, optional
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "bottom"
  ) +
  labs(
    x = "Omitted State",
    y = "Coefficient",
    fill = "Significance Level"
  ) +
  scale_fill_brewer(palette = "Paired")

## ----tbl-per-capita----
texreg::texreg(
  custom.model.names = c("2SLS", "2SLS"),
  custom.coef.names = c(
    "Intercept",
    "ln$R_{t-2}$",
    "Population at Admission",
    "Area Size"
  ),
  reorder.coef = c(2, 3, 4, 1),
  custom.header = list(
    "$\\%\\Delta Y_{t}$" = 1,
    "$\\%\\Delta (Y_{t}/N_{t})$" = 2
  ),
  digits = 4,
  include.rsquared = FALSE,
  include.adjrs = FALSE,
  float.pos = "H",
  l = list(iv30, iv55)
)

## ----olley-pakes--------------------------------------------------------

# olley-pakes method
df34 %>%
  mutate(regulatedness = restrictions_2L/pop) %>%
  group_by(year) %>%
  mutate(
    r_t_bar = mean(regulatedness),
    N_t_bar = mean(pop),
    N_t = sum(pop),
    u = (regulatedness - r_t_bar)*(pop - N_t_bar)
  ) %>%
  summarize(
    r_t = sum(restrictions_2L)/sum(pop),
    r_t_bar = mean(r_t_bar),
    cov_r_N = sum(u/N_t)
  ) %>%
  mutate(
    r_t_replicate = r_t_bar + cov_r_N
  )

# lippi-perri method
tbl_lippi_perri <- df34 %>%
  mutate(regulatedness = restrictions_2L/pop) %>%
  group_by(year) %>%
  mutate( 
    N_t_bar = mean(pop),
    n_it = pop/N_t_bar,
    n_it_bar = mean(n_it),
    r_it_bar = mean(regulatedness),
    u = (regulatedness - r_it_bar)*(n_it - n_it_bar)
  ) %>%
  summarize(
    r_t = mean(restrictions_2L)/mean(pop),
    cov_r_n = mean(u),
    r_t_bar = mean(regulatedness),
    sd_r = (var(regulatedness))^(1/2),
    sd_n = (var(n_it))^(1/2)
  ) %>%
  mutate(
    corr_r_n = cov_r_n/sd_r/sd_n
    # r_t_replicate = r_t_bar + cov_r_n
  )

colnames(tbl_lippi_perri) <- c(
  "Year",
  "$r$",
  "$\\mathrm{Cov}(r_i, n_i)$",
  "$\\mathrm{E}(r_i)$",
  "$\\sigma_{r_i}$",
  "$\\sigma_{n_i}$",
  "$\\mathrm{Corr}(r_i, n_i)$"
)

# both methods identical. ok that's reassuring

## ----tbl-olley-pakes----
tbl_lippi_perri %>%
  knitr::kable(
    format = "latex",
    booktabs = TRUE,
    escape = FALSE
  ) %>%
  cat("\\begin{table}[H] \\centering", .) %>%
  cat("\\end{table}")

## ----reduced-form----

df40 <- df34 %>%
  left_join(
    data_SQGDP1 %>% select(-year),
    by = "state"
  ) %>%
  group_by(state) %>%
  arrange(year) %>%
  slice_tail() %>%
  ungroup() %>%
  mutate(
    gdp_chained_longrun_chg = log(gdp_chained/gdp_chained_earliest)/(year - 2005),
    age_2005 = 2005 - year_establishment - 1
    # minus 1 because t - 2 refers to end-year minus two, i.e, base year minus one.
  )

lm53 <- df40 %>%
  lm(
    data = ., 
    gdp_chained_longrun_chg ~ age_2005 + pop_admission + area)
summary(lm53)

lm54 <- df40 %>% 
  ivreg(
    data = ., 
    log(restrictions_2L) ~ age + pop_admission + area)
summary(lm54)

## ----tbl-reduced-form----

texreg::texreg(
  custom.model.names = c("OLS"),
  custom.coef.names = c(
    "Intercept",
    "State Age ($\\alpha_1 \\theta_1$)",
    "Population at Admission",
    "Area Size"
  ),
  reorder.coef = c(2, 3, 4, 1),
  custom.header = list("$\\%\\Delta \\text{GDP}_{2005, 2024}$" = 1),
  digits = 6,
  # include.rsquared = FALSE,
  include.adjrs = FALSE,
  float.pos = "H",
  l = lm53
)

