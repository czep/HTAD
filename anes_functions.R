###
### ANES 2020 functions
###


fontstack_serif <- "'Source Serif Pro', serif"
fontstack_mono <- "monospace"

polilinecolors <- c("D" = "#15338d", "R" = "#db001f")
polifillcolors <- c("D" = "#00ace5", "R" = "#ff767b")


repair_labels <- function(x) {
  str_replace_all(x, "\\d+\\.\\s", "")
}


quicktable <- function(tbl, colnames, title = "", align = NULL) {

  t1 <- tbl %>%
    mutate(across(where(is.labelled), to_factor)) %>%
    mutate(across(where(is.factor), repair_labels))

  kbl(t1, col.names = colnames, caption = title,
    format.args = list(big.mark = ","),
    digits = 0,
    align = align,
    table.attr='class="qtab"'
  ) %>%
  kable_styling(full_width = FALSE, html_font = fontstack_mono)
}


wt_freq <- function(data, x, wt = V200010a) {

  data %>%
    select(c({{x}}, {{wt}})) %>%
    count({{x}}, wt = {{wt}}) %>%
    mutate(
      p = n / sum(n),
      p = percent(p, accuracy = 0.02)
    )
}


freqtable <- function(tbl, title, colnames = c("", "Count", "Pct")) {

  t1 <- tbl %>%
    mutate(across(where(is.labelled), to_factor)) %>%
    mutate(across(where(is.factor), repair_labels))

  kbl(t1, col.names = colnames, caption = title,
    format.args = list(big.mark = ","), digits = 0, align = "lrrrrrrrr", escape = FALSE,
    table.attr='class="freqtab"'
  ) %>%
  kable_classic(full_width = FALSE, html_font = fontstack_mono)

}


crosstab <- function(data, x, y, wt = V200010a) {

  data %>%
    select(c({{x}}, {{y}}, {{wt}})) %>%
    filter(!is.na({{x}}) & !is.na({{y}})) %>%
    count({{x}}, {{y}}, wt = {{wt}}) %>%
    group_by({{x}}) %>%
    mutate(p = n / sum(n)) %>%
    pivot_wider(id_cols = {{y}}, names_from = {{x}}, values_from = p) %>%
    mutate(across(.cols = last_col() + (-1:0), .fns = percent, accuracy = 0.1))
}


tabtemplate1 <- function(data, var_name, title) {

  t1 <- data %>% wt_freq({{var_name}})
  k1 <- freqtable(t1, {{title}})

  tmpdata <- data %>%
    set_na_range({{var_name}} := c(-9,-1)) %>%
    user_na_to_na()

  t2 <- tmpdata %>% crosstab(preintent, {{var_name}})
  k2 <- freqtable(t2, {{title}}, c("", "D Voters", "R Voters"))
  print(twintables(k1, k2))

  tmpdata %>% demotable({{var_name}}, {{title}})

}


quicktab <- function(data, var_name, title) {

  t1 <- data %>% wt_freq({{var_name}})
  k1 <- freqtable(t1, {{title}})

  tmpdata <- data %>%
    set_na_range({{var_name}} := c(-9,-1)) %>%
    user_na_to_na()

  t2 <- tmpdata %>% crosstab(preintent, {{var_name}})
  k2 <- freqtable(t2, {{title}}, c("", "D Voters", "R Voters"))
  twintables(k1, k2)

}


twintables <- function(k1, k2) {
  kables(
    list(
      k1 %>% kable_styling(full_width = FALSE, position = "float_left"),
      k2 %>% kable_styling(full_width = FALSE, position = "left")
    )
  )
}


plot_approval <- function(data, varname, title) {

  data %>%
    filter(!is.na(preintent)) %>%
    group_by(preintent, {{varname}}) %>%
    summarize(n = sum(V200010a)) %>%
    filter(!is.na({{varname}})) %>%
    mutate(
      p = n / sum(n),
      v = to_factor({{varname}}),
      v = repair_labels(v)
    ) %>%
    ggplot(aes(x = v, y = p, group = preintent, fill = preintent)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_y_continuous(labels=percent) +
      scale_fill_manual(values=polifillcolors) +
      labs(
        title = paste0("Approval of ", {{title}}),
        subtitle = "among likely Democratic vs Republican voters",
        x = "", y = "", fill = ""
      ) +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      )

}


reportci <- function(p, lo, hi) {
  pctp <- percent(p, accuracy = 0.02)
  pctlo <- percent(lo, accuracy = 0.02)
  pcthi <- percent(hi, accuracy = 0.02)
  paste0(
    '<span class="pctp">', pctp, '</span>',
    '<br />',
    '<span class="pctci">(', pctlo,
    ', ',
    pcthi,
    ')</span>'
  )
}


demochunk <- function(data, summary_var, group_vars) {

  data %>%
    filter(
      if_all({{group_vars}}, ~ !is.na(.x)),
      !is.na({{summary_var}})
    ) %>%
    mutate(across(c({{group_vars}}, {{summary_var}}), as_factor)) %>%
    group_by(across(c({{group_vars}}, {{summary_var}}))) %>%
    summarize(triplet = survey_mean(proportion = TRUE, vartype = "ci", na.rm = TRUE)) %>%
    unite("var_label", {{group_vars}}, sep = " - ") %>%
    mutate(var_label = repair_labels(var_label))

}


demotable <- function(data, summary_var, title) {

  t0 <- data %>%
    set_na_range(V201600 = c(-9,-1)) %>%
    set_na_range({{summary_var}} := c(-9,-1)) %>%
    user_na_to_na() %>%
    arrange(preintent, V201600, agecat_f, race, collgrad)

  design <- as_survey_design(
    svydesign(id=~V200010c, strata=~V200010d, weights=~V200010a, data=t0, nest=TRUE)
  )

  t1_overall <- design %>% demochunk(summary_var = {{summary_var}}, group_vars = c(preintent))
  t1_gender <- design %>% demochunk(summary_var = {{summary_var}}, group_vars = c(preintent, V201600))
  t1_agegen <- design %>% demochunk(summary_var = {{summary_var}}, group_vars = c(preintent, V201600, agecat))
  t1_race <- design %>% demochunk(summary_var = {{summary_var}}, group_vars = c(preintent, race))
  t1_collgrad <- design %>% demochunk(summary_var = {{summary_var}}, group_vars = c(preintent, collgrad))

  t1 <- t1_overall %>%
    bind_rows(t1_gender, t1_agegen, t1_race, t1_collgrad) %>%
    pivot_longer(cols = !c(var_label, {{summary_var}}), names_to = "result", values_to = "val") %>%
    separate(result, c("triplet", "modif")) %>%
    replace_na(list(modif = "p")) %>%
    pivot_wider(names_from = "modif", values_from = "val") %>%
    mutate(
      cell = reportci(p, low, upp)
    ) %>%
    select(var_label, {{summary_var}}, cell) %>%
    mutate({{summary_var}} := repair_labels({{summary_var}})) %>%
    pivot_wider(names_from = c({{summary_var}}), values_from = "cell")

  kbl(t1, escape = FALSE,
    format.args = list(big.mark = ","), digits = 0, align = "lcccccccc",
    caption = title,
    col.names = c("", names(t1)[-1]),
    table.attr='class="demotab"'
  ) %>%
  kable_classic(full_width = FALSE, html_font = fontstack_mono, fixed_thead = TRUE) %>%
  pack_rows("Vote Intent", 1, 2, label_row_css = "background-color: #666; color: #fff;") %>%
  pack_rows("Gender", 3, 6, label_row_css = "background-color: #666; color: #fff;") %>%
  pack_rows("Age and Gender", 7, 18, label_row_css = "background-color: #666; color: #fff;") %>%
  pack_rows("Race", 19, 22, label_row_css = "background-color: #666; color: #fff;") %>%
  pack_rows("Education", 23, 26, label_row_css = "background-color: #666; color: #fff;") %>%
  row_spec(c(1,3,4,7:12,19,20,23,24), color = "#15338d") %>%
  row_spec(c(2,5,6,13:18,21,22,25,26), color = "#db001f")

}

