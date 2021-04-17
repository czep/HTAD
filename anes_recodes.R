###
### ANES 2020 variable recodes
###

all_recodes <- function(data) {

  data %>%
    technical_recodes() %>%
    eda_recodes() %>%
    demo_recodes()

}


technical_recodes <- function(data) {

  data %>%
    replace_na(list(V200010b = 0)) %>%
    mutate(
      sample_mode_f = case_when(
        V200003 == 2 ~ 1,
        TRUE ~ 2
      ),
      post_complete_f = case_when(
        V202001 %in% c(-6,-7) ~ 1,
        TRUE ~ 2
      ),
      sample_mode = factor(sample_mode_f,
        levels = c(1,2),
        labels = c("ANES 2016-2020 Panel", "Fresh Cross-Sectional Sample")
      ),
      post_complete = factor(post_complete_f,
        levels = c(1,2),
        labels = c("Complete", "Incomplete")
      )
    )

}


eda_recodes <- function(data) {

  data %>%
    mutate(
      preintent = case_when(
        V201075x %in% c(10,20,30) ~ "D",
        V201075x %in% c(11,21,31) ~ "R",
        TRUE ~ NA_character_
      ),
      vip_f = case_when(
        V201075x %in% c(10,11) ~ 1,
        V201075x %in% c(20,21) ~ 2,
        V201075x %in% c(30,31) ~ 3
      ),
      vip = factor(vip_f,
        levels = c(1,2,3),
        labels = c("Vote", "Intent", "Preference")
      ),
      elec_outcome_f = case_when(
        V201217 == 1 & V201218 == 2 ~ 1,
        V201217 == 1 & V201218 == 1 ~ 2,
        V201217 == 2 & V201218 == 1 ~ 3,
        V201217 == 2 & V201218 == 2 ~ 4,
        TRUE ~ NA_real_
      ),
      elec_outcome = factor(elec_outcome_f,
        levels = c(1, 2, 3, 4),
        labels = c("Biden by a lot", "Biden close", "Trump close", "Trump by a lot")
      )
    )

}


demo_recodes <- function(data) {

  data %>%
    mutate(
      agegen_f = case_when(
        V201600 == 1 & V201507x %in% 18:34 ~ 1,
        V201600 == 1 & V201507x %in% 35:54 ~ 2,
        V201600 == 1 & V201507x %in% 55:80 ~ 3,
        V201600 == 2 & V201507x %in% 18:34 ~ 4,
        V201600 == 2 & V201507x %in% 35:54 ~ 5,
        V201600 == 2 & V201507x %in% 55:80 ~ 6,
        TRUE ~ NA_real_
      ),
      agegen = factor(agegen_f,
        levels = c(1, 2, 3, 4, 5, 6),
        labels = c("Male 18 to 34", "Male 35 to 54", "Male 55+", "Female 18 to 34", "Female 35 to 54", "Female 55+")
      ),
      agecat_f = case_when(
        V201507x %in% 18:34 ~ 1,
        V201507x %in% 35:54 ~ 2,
        V201507x %in% 55:80 ~ 3,
        TRUE ~ NA_real_
      ),
      agecat = factor(agecat_f,
        levels = c(1, 2, 3),
        labels = c("18 to 34", "35 to 54", "55+")
      ),
      race_f = case_when(
        V201549x == 1 ~ 1,
        V201549x %in% c(2, 3, 4, 5, 6) ~ 2,
        TRUE ~ NA_real_
      ),
      race = factor(race_f, levels = c(1, 2), labels = c("White", "Non-white")),
      collgrad_f = case_when(
        V201511x %in% c(4, 5) ~ 1,
        V201511x %in% c(1, 2, 3) ~ 2,
        TRUE ~ NA_real_
      ),
      collgrad = factor(collgrad_f, levels = c(1, 2), labels = c("College Grad", "Not college grad"))
    )

}

### exclude from all_recodes
howgoes_recodes <- function(data) {

  data %>%
    set_na_range(V201115 = c(-9,-1)) %>%
    set_na_range(V201116 = c(-9,-1)) %>%
    set_na_range(V201117 = c(-9,-1)) %>%
    set_na_range(V201118 = c(-9,-1)) %>%
    set_na_range(V201119 = c(-9,-1)) %>%
    set_na_range(V201120 = c(-9,-1)) %>%
    set_na_range(V201121 = c(-9,-1)) %>%
    set_na_range(V201122 = c(-9,-1)) %>%
    set_na_range(V201123 = c(-9,-1)) %>%
    user_na_to_na()

}


