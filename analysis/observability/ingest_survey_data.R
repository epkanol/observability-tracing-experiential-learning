# Helper functions managing the output and caches

figsave <- function(file, p, device="pdf", width=15, height=17.8, units="cm") ggsave(paste(params$output, file, sep="/"), p, device = device, dpi = 1200, width=width, height=height, units=units)
pngsave <- function(file, p, width=15, height=17.8, units="cm") ggsave(paste(params$output, file, sep="/"), p, device = "png", dpi = 1200, width=width, height=height, units=units)
cachefile <- function(filename) paste(params$cache, filename, sep="/")

# BRMS/Stan config parameters. Adapt these to fit your machine
CHAINS <- 4
CORES <- params$cores
ITERATIONS <- 4000
THREADS <- params$threads
ADAPT_DELTA <- 0.95


# Development Practices survey
ingest_devpractices_data <- function(file) {
  read_excel(file,
             skip=1, # first row is headers, which are quite long
             col_names = c("id",
                           "start_time",
                           "end_time",
                           "email",
                           "name",
                           "lastmodified",
                           "consent",
                           "timezone",
                           "ides",
                           "joinedYear",
                           "profYear",
                           "has_pushed_java",     # L
                           "exp_java",
                           "has_pushed_vue",
                           "exp_vue",
                           "has_added_logs",
                           "logs_kept",
                           "has_debug_unit",
                           "exp_debug_unit",  # S
                           "has_debug_ft",
                           "exp_debug_ft",
                           "has_used_pm",
                           "exp_use_pm",
                           "has_traced_jaeger",
                           "exp_used_jaeger"   # Y
             )) |> select(-email, -name, -lastmodified) |>
    mutate(
      id = as.factor(id),
      timezone = as.factor(timezone),
      joinedYear = as.integer(joinedYear),
      profYear = as.integer(profYear),
      has_pushed_java = as.factor(has_pushed_java),
      exp_java = as.integer(exp_java),
      has_pushed_vue = as.factor(has_pushed_vue),
      exp_vue = as.integer(exp_vue),
      has_added_logs = as.factor(has_added_logs),
      logs_kept = as.factor(tolower(logs_kept)),
      has_debug_unit = as.factor(has_debug_unit),
      exp_debug_unit = as.integer(exp_debug_unit),
      has_debug_ft = as.factor(has_debug_ft),
      exp_debug_ft = as.integer(exp_debug_ft),
      has_used_pm = as.factor(has_used_pm),
      exp_use_pm = as.integer(exp_use_pm),
      has_traced_jaeger = as.factor(has_traced_jaeger),
      exp_used_jaeger = as.integer(exp_used_jaeger)) |>
    replace_na(list(exp_java=0,
               exp_vue=0,
               exp_debug_unit=0,
               exp_debug_ft=0,
               exp_use_pm=0,
               exp_used_jaeger=0))
}


likert7 <- function(x) {
  # some questions have different spellings
  factor(x, levels = c("Extremely Likely", "Quite Likely", "Slightly Likely", "Neither",
                       "Slightly Unlikely", "Quite Unlikely", "Extremely Unlikely", "Extremely likely", "Somewhat Likely", "Somewhat Unlikely"),
         labels=c("XL", "QL", "SL", "N", "SU", "QU", "XU", "XL", "SL", "SU"), ordered = T)
}
streams <- function(x) {
  factor(x, ordered = F)
}
indian_teams <- c("DT20", "DT21", "DT22", "DT23", "DT24", "DT25", "DT26", "DT27")
ingest_survey_data <- function(file) {
  read_excel(file,
                  skip=1,
                  col_names = c("id",
                                "start_time",
                                "end_time",
                                "email",
                                "name",
                                "lastmodified",
                                "consent",
                                "team",
                                "joinedYear",
                                "profYear",
                                "overall_rate",
                                "overall_options",
                                "chosen_stream",
                                "genai_prior_exp",     # N
                                "genai_tasks_quickly",
                                "genai_boost_perf",
                                "genai_boost_prod",
                                "genai_boost_eff",
                                "genai_ease_job",
                                "genai_useful",
                                "genai_easy_operate",  # U
                                "genai_easy_do",
                                "genai_clear_interact",
                                "genai_flexible",
                                "genai_easy_skills",
                                "genai_easy_use",
                                "genai_easy_access",
                                "genai_easy_access_do",
                                "genai_easy_access_daily",
                                "genai_intend_use",
                                "genai_spend_time_use",
                                "genai_team_benefit",  # AF
                                "genai_teach_team",
                                "genai_teach_org",
                                "genai_sme_teach",
                                "genai_best_takeaways",
                                "genai_improvements",  # AK
                                "jaeger_prior_exp",     # AL
                                "jaeger_tasks_quickly",
                                "jaeger_boost_perf",
                                "jaeger_boost_prod",
                                "jaeger_boost_eff",
                                "jaeger_ease_job",
                                "jaeger_useful",
                                "jaeger_easy_operate",  # AS
                                "jaeger_easy_do",
                                "jaeger_clear_interact",
                                "jaeger_flexible",
                                "jaeger_easy_skills",
                                "jaeger_easy_use",
                                "jaeger_easy_access",
                                "jaeger_easy_access_do",
                                "jaeger_easy_access_daily",
                                "jaeger_intend_use",
                                "jaeger_spend_time_use",
                                "jaeger_team_benefit",   # AF
                                "jaeger_teach_team",
                                "jaeger_teach_org",
                                "jaeger_sme_teach",
                                "jaeger_best_takeaways",
                                "jaeger_improvements",  # BI
                                "vue_prior_exp",     # BJ
                                "vue_tasks_quickly",
                                "vue_boost_perf",
                                "vue_boost_prod",
                                "vue_boost_eff",
                                "vue_ease_job",
                                "vue_useful",
                                "vue_learn_use",  # BQ
                                "vue_easy_do",
                                "vue_clear_interact",
                                "vue_flexible",
                                "vue_easy_skills",
                                "vue_easy_use",    # BV
                                "vue_write_learn",  # BW
                                "vue_write_do",
                                "vue_write_skills",
                                "vue_write_easy",     # BZ
                                "vue_easy_access",    # CA
                                "vue_easy_access_do",
                                "vue_easy_access_daily",
                                "vue_intend_use",
                                "vue_spend_time_use", # CE
                                "vue_team_benefit",   # CF
                                "vue_teach_team",
                                "vue_teach_org",
                                "vue_sme_teach",
                                "vue_best_takeaways",
                                "vue_improvements",  # CK
                                "pentest_prior_exp",     # CL
                                "pentest_intend_use",
                                "pentest_spend_time_use",
                                "pentest_team_benefit",   # CO
                                "pentest_teach_team",
                                "pentest_teach_org",
                                "pentest_sme_teach",
                                "pentest_best_takeaways",
                                "pentest_improvements",  # CT
                                "product_prior_exp",     # CU
                                "product_intend_use",
                                "product_spend_time_use",
                                "product_team_benefit",   # CX
                                "product_teach_team",
                                "product_teach_org",
                                "product_sme_teach",
                                "product_best_takeaways",
                                "product_improvements",  # DC
                                "mentor_stream",     # DD
                                "mentor_prepare_time",
                                "mentor_resources",
                                "mentor_tasks_adequate",   # DG
                                "mentor_participants_learned",
                                "mentor_enjoyed",
                                "mentor_best_takeaways",
                                "mentor_improvements"  # DK
                  )) |> select(-email, -name, -lastmodified) |>
  mutate(
    id = as.factor(id),
    consent = as.factor(consent),
    team = as.factor(team),
    joinedYear = as.integer(joinedYear),
    profYear = as.integer(profYear),
    chosen_stream = streams(chosen_stream),
    genai_prior_exp = as.integer(genai_prior_exp),
    genai_tasks_quickly = likert7(genai_tasks_quickly),
    genai_boost_perf = likert7(genai_boost_perf),
    genai_boost_prod = likert7(genai_boost_prod),
    genai_boost_eff = likert7(genai_boost_eff),
    genai_ease_job = likert7(genai_ease_job),
    genai_useful = likert7(genai_useful),
    genai_easy_operate = likert7(genai_easy_operate),
    genai_easy_do = likert7(genai_easy_do),
    genai_clear_interact = likert7(genai_clear_interact),
    genai_flexible = likert7(genai_flexible),
    genai_easy_skills = likert7(genai_easy_skills),
    genai_easy_use = likert7(genai_easy_use),
    genai_easy_access = likert7(genai_easy_access),
    genai_easy_access_do = likert7(genai_easy_access_do),
    genai_easy_access_daily = likert7(genai_easy_access_daily),
    genai_intend_use = likert7(genai_intend_use),
    genai_spend_time_use = likert7(genai_spend_time_use),
    genai_team_benefit = likert7(genai_team_benefit),
    genai_teach_team = likert7(genai_teach_team),
    genai_teach_org = likert7(genai_teach_org),
    genai_sme_teach = likert7(genai_sme_teach),
    jaeger_prior_exp = as.integer(jaeger_prior_exp),
    jaeger_tasks_quickly = likert7(jaeger_tasks_quickly),
    jaeger_boost_perf = likert7(jaeger_boost_perf),
    jaeger_boost_prod = likert7(jaeger_boost_prod),
    jaeger_boost_eff = likert7(jaeger_boost_eff),
    jaeger_ease_job = likert7(jaeger_ease_job),
    jaeger_useful = likert7(jaeger_useful),
    jaeger_easy_operate = likert7(jaeger_easy_operate),
    jaeger_easy_do = likert7(jaeger_easy_do),
    jaeger_clear_interact = likert7(jaeger_clear_interact),
    jaeger_flexible = likert7(jaeger_flexible),
    jaeger_easy_skills = likert7(jaeger_easy_skills),
    jaeger_easy_use = likert7(jaeger_easy_use),
    jaeger_easy_access = likert7(jaeger_easy_access),
    jaeger_easy_access_do = likert7(jaeger_easy_access_do),
    jaeger_easy_access_daily = likert7(jaeger_easy_access_daily),
    jaeger_intend_use = likert7(jaeger_intend_use),
    jaeger_spend_time_use = likert7(jaeger_spend_time_use),
    jaeger_team_benefit = likert7(jaeger_team_benefit),
    jaeger_teach_team = likert7(jaeger_teach_team),
    jaeger_teach_org = likert7(jaeger_teach_org),
    jaeger_sme_teach = likert7(jaeger_sme_teach),
    vue_prior_exp = as.integer(vue_prior_exp),
    vue_tasks_quickly = likert7(vue_tasks_quickly),
    vue_boost_perf = likert7(vue_boost_perf),
    vue_boost_prod = likert7(vue_boost_prod),
    vue_boost_eff = likert7(vue_boost_eff),
    vue_ease_job = likert7(vue_ease_job),
    vue_useful = likert7(vue_useful),
    vue_learn_use = likert7(vue_learn_use),
    vue_easy_do = likert7(vue_easy_do),
    vue_clear_interact = likert7(vue_clear_interact),
    vue_flexible = likert7(vue_flexible),
    vue_easy_skills = likert7(vue_easy_skills),
    vue_easy_use = likert7(vue_easy_use),
    vue_write_learn = likert7(vue_write_learn),
    vue_write_do = likert7(vue_write_do),
    vue_write_skills = likert7(vue_write_skills),
    vue_write_easy = likert7(vue_write_easy),
    vue_easy_access = likert7(vue_easy_access),
    vue_easy_access_do = likert7(vue_easy_access_do),
    vue_easy_access_daily = likert7(vue_easy_access_daily),
    vue_intend_use = likert7(vue_intend_use),
    vue_spend_time_use = likert7(vue_spend_time_use),
    vue_team_benefit = likert7(vue_team_benefit),
    vue_teach_team = likert7(vue_teach_team),
    vue_teach_org = likert7(vue_teach_org),
    vue_sme_teach = likert7(vue_sme_teach),
    pentest_prior_exp = as.integer(pentest_prior_exp),
    pentest_intend_use = likert7(pentest_intend_use),
    pentest_spend_time_use = likert7(pentest_spend_time_use),
    pentest_team_benefit = likert7(pentest_team_benefit),
    pentest_teach_team = likert7(pentest_teach_team),
    pentest_teach_org = likert7(pentest_teach_org),
    pentest_sme_teach = likert7(pentest_sme_teach),
    product_prior_exp = as.integer(product_prior_exp),
    product_intend_use = likert7(product_intend_use),
    product_spend_time_use = likert7(product_spend_time_use),
    product_team_benefit = likert7(product_team_benefit),
    product_teach_team = likert7(product_teach_team),
    product_teach_org = likert7(product_teach_org),
    product_sme_teach = likert7(product_sme_teach),
    mentor_stream = streams(mentor_stream),
    mentor_prepare_time = likert7(mentor_prepare_time),
    mentor_resources = likert7(mentor_resources),
    mentor_tasks_adequate = likert7(mentor_tasks_adequate),
    mentor_participants_learned = likert7(mentor_participants_learned),
    mentor_enjoyed = likert7(mentor_enjoyed),
    site = as.factor(case_when(
      team %in% indian_teams ~ "India",
      T ~ "Europe"
    )))
}

ai_data <- function(df) {df |> filter(consent == 'Yes', chosen_stream == 'GENAI') |> select(id, team, joinedYear, profYear, starts_with("genai"), site) }
jaeger_data <- function(df) {df |> filter(consent == 'Yes', chosen_stream == 'JAEGER') |> select(id, team, joinedYear, profYear, starts_with("jaeger"), site) }
vue_data <- function(df) {df |> filter(consent == 'Yes', chosen_stream == 'VUE_JS') |> select(id, team, joinedYear, profYear, starts_with("vue"), site) }
sec_data <- function(df) {df |> filter(consent == 'Yes', chosen_stream == 'SECURITY') |> select(id, team, joinedYear, profYear, starts_with("pentest"), site) }
swt_data <- function(df) {df |> filter(consent == 'Yes', chosen_stream == 'REQ_ENG') |> select(id, team, joinedYear, profYear, starts_with("product"), site) }
qa_data <- function(df) {df |> filter(consent == 'Yes', chosen_stream == 'QA') |> select(id, team, joinedYear, profYear, site) }
teachers_data <- function(df) {df |> filter(consent == 'Yes', chosen_stream == 'TEACHER') |> select(id, team, joinedYear, profYear, starts_with("mentor"), site) }

# AI
get_ai_usable <- function(df) { 
  df |> select(id, site, joinedYear, profYear, genai_prior_exp, genai_tasks_quickly, genai_boost_perf, genai_boost_prod, genai_boost_eff, genai_ease_job, genai_useful) |>
  pivot_longer(cols=c(genai_tasks_quickly, genai_boost_perf, genai_boost_prod, genai_boost_eff, genai_ease_job, genai_useful), values_to="usable") |> 
  mutate(name=as.factor(name), id=as.factor(as.character(id)))
  
}
get_ai_ease_of_use <- function(df) { 
  df |> select(id, site, joinedYear, profYear, genai_prior_exp, genai_easy_operate, genai_easy_do, genai_clear_interact, genai_flexible, genai_easy_skills, genai_easy_use) |>
  pivot_longer(cols=c(genai_easy_operate, genai_easy_do, genai_clear_interact, genai_flexible, genai_easy_skills, genai_easy_use), values_to="ease_of_use") |> 
    mutate(name=as.factor(name), id=as.factor(as.character(id)))
}
get_ai_accessible <- function(df) { 
  df |> select(id, site, joinedYear, profYear, genai_prior_exp, genai_easy_access, genai_easy_access_do, genai_easy_access_daily) |>
  pivot_longer(cols=c(genai_easy_access, genai_easy_access_do, genai_easy_access_daily), values_to="accessible") |> 
    mutate(name=as.factor(name), id=as.factor(as.character(id)))
}
get_ai_intend_use <- function(df) {
  df |> select(id, site, joinedYear, profYear, genai_prior_exp, genai_intend_use, genai_spend_time_use) |>
  pivot_longer(cols=c(genai_intend_use, genai_spend_time_use), values_to="intent") |> 
    mutate(name=as.factor(name), id=as.factor(as.character(id)))
}

# Jaeger
get_jaeger_usable <- function(df) { 
  df |> select(id, site, joinedYear, profYear, jaeger_prior_exp, jaeger_tasks_quickly, jaeger_boost_perf, jaeger_boost_prod, jaeger_boost_eff, jaeger_ease_job, jaeger_useful) |>
    pivot_longer(cols=c(jaeger_tasks_quickly, jaeger_boost_perf, jaeger_boost_prod, jaeger_boost_eff, jaeger_ease_job, jaeger_useful), values_to="usable") |> 
    mutate(name=as.factor(name), id=as.factor(as.character(id)))
}
get_jaeger_ease_of_use <- function(df) { 
  df |> select(id, site, joinedYear, profYear, jaeger_prior_exp, jaeger_easy_operate, jaeger_easy_do, jaeger_clear_interact, jaeger_flexible, jaeger_easy_skills, jaeger_easy_use) |>
    pivot_longer(cols=c(jaeger_easy_operate, jaeger_easy_do, jaeger_clear_interact, jaeger_flexible, jaeger_easy_skills, jaeger_easy_use), values_to="ease_of_use") |> 
    mutate(name=as.factor(name), id=as.factor(as.character(id)))
}
get_jaeger_accessible <- function(df) { 
  df |> select(id, site, joinedYear, profYear, jaeger_prior_exp, jaeger_easy_access, jaeger_easy_access_do, jaeger_easy_access_daily) |>
    pivot_longer(cols=c(jaeger_easy_access, jaeger_easy_access_do, jaeger_easy_access_daily), values_to="accessible") |> 
    mutate(name=as.factor(name), id=as.factor(as.character(id)))
}
get_jaeger_intend_use <- function(df) {
  df |> select(id, site, joinedYear, profYear, jaeger_prior_exp, jaeger_intend_use, jaeger_spend_time_use) |>
    pivot_longer(cols=c(jaeger_intend_use, jaeger_spend_time_use), values_to="intent") |> 
    mutate(name=as.factor(name), id=as.factor(as.character(id)))
}

# Vue
get_vue_usable <- function(df) { 
  df |> select(id, site, joinedYear, profYear, vue_prior_exp, vue_tasks_quickly, vue_boost_perf, vue_boost_prod, vue_boost_eff, vue_ease_job, vue_useful) |>
    pivot_longer(cols=c(vue_tasks_quickly, vue_boost_perf, vue_boost_prod, vue_boost_eff, vue_ease_job, vue_useful), values_to="usable") |> 
    mutate(name=as.factor(name), id=as.factor(as.character(id))) # reduce number of levels in id to those taking this stream
}
get_vue_ease_of_use <- function(df) { 
  df |> select(id, site, joinedYear, profYear, vue_prior_exp, vue_learn_use, vue_easy_do, vue_clear_interact, vue_flexible, vue_easy_skills, vue_easy_use) |>
    pivot_longer(cols=c(vue_learn_use, vue_easy_do, vue_clear_interact, vue_flexible, vue_easy_skills, vue_easy_use), values_to="ease_of_use") |> 
    mutate(name=as.factor(name), id=as.factor(as.character(id)))
}
get_vue_write_ease <- function(df) { 
  df |> select(id, site, joinedYear, profYear, vue_prior_exp, vue_write_learn, vue_write_do, vue_write_skills, vue_write_easy) |>
    pivot_longer(cols=c(vue_write_learn, vue_write_do, vue_write_skills, vue_write_easy), values_to="ease_of_use") |> 
    mutate(name=as.factor(name), id=as.factor(as.character(id)))
}
get_vue_accessible <- function(df) { 
  df |> select(id, site, joinedYear, profYear, vue_prior_exp, vue_easy_access, vue_easy_access_do, vue_easy_access_daily) |>
    pivot_longer(cols=c(vue_easy_access, vue_easy_access_do, vue_easy_access_daily), values_to="accessible") |> 
    mutate(name=as.factor(name), id=as.factor(as.character(id)))
}
get_vue_intend_use <- function(df) {
  df |> select(id, site, joinedYear, profYear, vue_prior_exp, vue_intend_use, vue_spend_time_use) |>
    pivot_longer(cols=c(vue_intend_use, vue_spend_time_use), values_to="intent") |> 
    mutate(name=as.factor(name), id=as.factor(as.character(id)))
}

# Functions used to summarize sample means

usable_mean <- function(df) { df |> summarize(mean(as.numeric(usable))) }
ease_of_use_mean <- function(df) { df |> summarize(mean(as.numeric(ease_of_use))) }
accessible_mean <- function(df) { df |> summarize(mean(as.numeric(accessible))) }
intent_mean <- function(df) { df |> summarize(mean(as.numeric(intent))) }


# M1: intercept-only model

plot_M1_latent_distribution <- function(m, title, lower=-3.5, upper=3.5) {
  tibble(x = seq(from = lower, to = upper, length.out = 400)) |> 
    mutate(d = dnorm(x = x)) |>
    ggplot(aes(x = x, y = d)) +
    geom_area(fill = "black", alpha = 1/3) +
    geom_vline(xintercept = fixef(m)[, 1], linetype = 3) +
    scale_x_continuous(expression(Phi), breaks = -6:3,
                       sec.axis = dup_axis(
                         name = NULL,
                         breaks = fixef(m)[, 1] |> as.double(),
                         labels = c("XL", "QL", "SL", "N", "SUL", "QUL"))) +
    scale_y_continuous(NULL, breaks = NULL) +
    coord_cartesian(xlim = c(lower, upper)) +
    theme_bw() +
    labs(title = paste("Latent distribution for", title))
}

draw_M1_posterior <- function(m) {
  as_draws_df(m) %>% 
    select(.draw, starts_with("b_Intercept")) %>% 
    set_names(".draw", str_c("tau[", 1:6, "]")) %>% 
    # compute the p_k distributions
    mutate(p1 = pnorm(`tau[1]`),
           p2 = pnorm(`tau[2]`) - pnorm(`tau[1]`),
           p3 = pnorm(`tau[3]`) - pnorm(`tau[2]`),
           p4 = pnorm(`tau[4]`) - pnorm(`tau[3]`),
           p5 = pnorm(`tau[5]`) - pnorm(`tau[4]`),
           p6 = pnorm(`tau[6]`) - pnorm(`tau[5]`),
           p7 = 1 - pnorm(`tau[6]`)) %>% 
    # wrangle
    pivot_longer(starts_with("p"), values_to = "p") %>% 
    mutate(response = str_extract(name, "\\d") %>% as.double()) %>% 
    # compute p_k * k
    mutate(`p * response` = p * response) %>% 
    # sum those values within each posterior draw
    group_by(.draw) %>% 
    summarise(mean_response = sum(`p * response`))
}

expected_value_M1 <- function(m, lower=0.025, upper=0.975) {
  draw_M1_posterior(m) |> summarise(expected=mean(mean_response), lower=quantile(mean_response, lower), upper=quantile(mean_response, upper))
}

plot_M1_posterior_mean <- function(m, title, sample_mean, limits=c(1,7)) {
  mu_hat <- sample_mean(m$data) |> pull()
  draw_M1_posterior(m) %>% 
    # plot!
    ggplot(aes(x = mean_response, y = 0)) +
    stat_halfeye(.width = .95) +
    theme_bw() +
    geom_vline(xintercept = mu_hat, linetype = 2) +
    scale_y_continuous(NULL, breaks = NULL) +
    labs(title = paste("The posterior for the mean of", title),
         subtitle = "The dashed vertical line marks off the sample mean.\nSimple cumulative ordinal model fit.",
         x = expression(mu[response])) +
    scale_x_continuous(limits=limits,
                       breaks = c(1,2,3,4,5,6,7),
                       labels=c("XL", "QL", "SL", "N", "SUL", "QUL", "XUL"))
}


# M2: intercept+site (only population level)
# also works for M3, uses dummies for id and name
plot_M2_latent_distribution <- function(m, title, lower=-3.5, upper=3.5) {
  tibble(siteIndia = 0:1,
         mu   = c(0, fixef(m)["siteIndia", 1])) |>
    expand(nesting(siteIndia, mu), x = seq(from = -3.5, to = 3.5, length.out = 200)) |>
    mutate(d   = dnorm(x, mean = mu, sd = 1),
           site = ifelse(siteIndia == 0, "Europe", "India")) |>
    ggplot(aes(x = x, y = d, fill = site)) +
    geom_area(alpha = 1/2, position = "identity") +
    geom_vline(xintercept = fixef(m)[1:6, 1], linetype = 3) +
    scale_fill_manual(NULL, values = c("#e66101", "#5e3c99")) +
    scale_x_continuous(expression(Phi), breaks = -3:3,
                       sec.axis = dup_axis(
                         name = NULL,
                         breaks = fixef(m)[1:6, 1] |> as.double(),
                         labels = c("XL", "QL", "SL", "N", "SUL", "QUL"))) +
    scale_y_continuous(NULL, breaks = NULL) +
    coord_cartesian(xlim = c(lower, upper)) +
    theme_bw() +
    labs(title = paste("Latent distribution for", title),
         subtitle = paste("Formula: ", m$formula))
  #subtitle = expression("The reference category is India. "*beta[1]*" is reflected in the rightward shift"))
  
}

plot_M2_posterior_mean <- function(m, title, sample_mean, limits=c(1,7)) {
  m_Europe <- m$data |> filter(site == 'Europe') |> sample_mean() |> pull()
  m_India  <- m$data |> filter(site == 'India') |> sample_mean() |> pull()
  # note, the magrittr pipe %>% is needed, because native |> pipe does not handle dot
  as_draws_df(m) %>%
    select(.draw, starts_with("b_")) %>%
    set_names(".draw", str_c("tau[", 1:6, "]"), "beta[1]") %>%
    # insert another copy of the data below
    bind_rows(., .) %>%
    # add the two values for the dummy variable siteIndia
    mutate(siteIndia = rep(0:1, each = n() / 2)) %>%
    # compute the p_k values conditional on the site dummy
    mutate(p1 = pnorm(`tau[1]`, mean = 0 + siteIndia * `beta[1]`),
           p2 = pnorm(`tau[2]`, mean = 0 + siteIndia * `beta[1]`) - pnorm(`tau[1]`, mean = 0 + siteIndia * `beta[1]`),
           p3 = pnorm(`tau[3]`, mean = 0 + siteIndia * `beta[1]`) - pnorm(`tau[2]`, mean = 0 + siteIndia * `beta[1]`),
           p4 = pnorm(`tau[4]`, mean = 0 + siteIndia * `beta[1]`) - pnorm(`tau[3]`, mean = 0 + siteIndia * `beta[1]`),
           p5 = pnorm(`tau[5]`, mean = 0 + siteIndia * `beta[1]`) - pnorm(`tau[4]`, mean = 0 + siteIndia * `beta[1]`),
           p6 = pnorm(`tau[6]`, mean = 0 + siteIndia * `beta[1]`) - pnorm(`tau[5]`, mean = 0 + siteIndia * `beta[1]`),
           p7 = 1 - pnorm(`tau[6]`, mean = 0 + siteIndia * `beta[1]`)) %>%
    # wrangle
    pivot_longer(starts_with("p"), values_to = "p") %>%
    mutate(response = str_extract(name, "\\d") %>% as.double()) %>%
    # compute p_k * k
    mutate(`p * response` = p * response) %>% 
    # sum those values within each posterior draw, by the siteIndia dummy
    group_by(.draw, siteIndia) %>% 
    summarise(mean_response = sum(`p * response`)) %>% 
    mutate(site = ifelse(siteIndia == 0, "Europe", "India")) %>% 
    
    # the trick with and without fct_rev() helps order the axes, colors, and legend labels
    ggplot(aes(x = mean_response, y = fct_rev(site), fill = site)) +
    stat_halfeye(.width = .95) +
    geom_vline(xintercept = m_Europe, linetype = 2, color = "#e66101") +
    geom_vline(xintercept = m_India, linetype = 2, color = "#5e3c99") +
    scale_fill_manual(NULL, values = c(alpha("#e66101", 0.5), alpha("#5e3c99", 0.5))) +
    labs(title = paste0("The posterior for the mean of the rating values for ", title, ", by site"),
         subtitle = paste("The dashed vertical lines mark off the sample means, by site.\nFormula:", m$formula),
         x = expression(mu[response]),
         y = NULL) +
    theme_bw() +
    theme(axis.text.y = element_text(hjust = 0)) +
    scale_x_continuous(limits=limits,
                       breaks = c(1,2,3,4,5,6,7),
                       labels=c("XL", "QL", "SL", "N", "SUL", "QUL", "XUL"))
}

# M2b, stratify by awareness (>=25 on a scale of 0-100)
plot_M2b_latent_distribution <- function(m, title, lower=-3.5, upper=3.5) {
  tibble(awareY = 0:1,
         mu   = c(0, fixef(m)["awareY", 1])) |>
    expand(nesting(awareY, mu), x = seq(from = -3.5, to = 3.5, length.out = 200)) |>
    mutate(d   = dnorm(x, mean = mu, sd = 1),
           aware = ifelse(awareY == 0, "No", "Yes")) |>
    ggplot(aes(x = x, y = d, fill = aware)) +
    geom_area(alpha = 1/2, position = "identity") +
    geom_vline(xintercept = fixef(m)[1:6, 1], linetype = 3) +
    scale_fill_manual(NULL, values = c("#e66101", "#5e3c99")) +
    scale_x_continuous(expression(Phi), breaks = -3:3,
                       sec.axis = dup_axis(
                         name = NULL,
                         breaks = fixef(m)[1:6, 1] |> as.double(),
                         labels = c("XL", "QL", "SL", "N", "SUL", "QUL"))) +
    scale_y_continuous(NULL, breaks = NULL) +
    coord_cartesian(xlim = c(lower, upper)) +
    theme_bw() +
    labs(title = paste("Latent distribution for", title),
         subtitle = paste("Formula: ", m$formula))
  #subtitle = expression("The reference category is India. "*beta[1]*" is reflected in the rightward shift"))
  
}

plot_M2b_posterior_mean <- function(m, title, sample_mean, limits=c(1,7)) {
  m_Europe <- m$data |> filter(aware == 'Y') |> sample_mean() |> pull()
  m_India  <- m$data |> filter(aware == 'N') |> sample_mean() |> pull()
  # note, the magrittr pipe %>% is needed, because native |> pipe does not handle dot
  as_draws_df(m) %>%
    select(.draw, starts_with("b_")) %>%
    set_names(".draw", str_c("tau[", 1:6, "]"), "beta[1]") %>%
    # insert another copy of the data below
    bind_rows(., .) %>%
    # add the two values for the dummy variable awareY
    mutate(awareY = rep(0:1, each = n() / 2)) %>%
    # compute the p_k values conditional on the aware dummy
    mutate(p1 = pnorm(`tau[1]`, mean = 0 + awareY * `beta[1]`),
           p2 = pnorm(`tau[2]`, mean = 0 + awareY * `beta[1]`) - pnorm(`tau[1]`, mean = 0 + awareY * `beta[1]`),
           p3 = pnorm(`tau[3]`, mean = 0 + awareY * `beta[1]`) - pnorm(`tau[2]`, mean = 0 + awareY * `beta[1]`),
           p4 = pnorm(`tau[4]`, mean = 0 + awareY * `beta[1]`) - pnorm(`tau[3]`, mean = 0 + awareY * `beta[1]`),
           p5 = pnorm(`tau[5]`, mean = 0 + awareY * `beta[1]`) - pnorm(`tau[4]`, mean = 0 + awareY * `beta[1]`),
           p6 = pnorm(`tau[6]`, mean = 0 + awareY * `beta[1]`) - pnorm(`tau[5]`, mean = 0 + awareY * `beta[1]`),
           p7 = 1 - pnorm(`tau[6]`, mean = 0 + awareY * `beta[1]`)) %>%
    # wrangle
    pivot_longer(starts_with("p"), values_to = "p") %>%
    mutate(response = str_extract(name, "\\d") %>% as.double()) %>%
    # compute p_k * k
    mutate(`p * response` = p * response) %>% 
    # sum those values within each posterior draw, by the awareY dummy
    group_by(.draw, awareY) %>% 
    summarise(mean_response = sum(`p * response`)) %>% 
    mutate(aware = ifelse(awareY == 0, "No", "Yes")) %>% 
    
    # the trick with and without fct_rev() helps order the axes, colors, and legend labels
    ggplot(aes(x = mean_response, y = fct_rev(aware), fill = aware)) +
    stat_halfeye(.width = .95) +
    geom_vline(xintercept = m_Europe, linetype = 2, color = "#5e3c99") +
    geom_vline(xintercept = m_India, linetype = 2, color = "#e66101") +
    scale_fill_manual(NULL, values = c(alpha("#e66101", 0.5), alpha("#5e3c99", 0.5))) +
    labs(title = paste0("The posterior for the mean of the rating values for ", title, ", by aware"),
         subtitle = paste("The dashed vertical lines mark off the sample means, by aware.\nFormula:", m$formula),
         x = expression(mu[response]),
         y = NULL) +
    theme_bw() +
    theme(axis.text.y = element_text(hjust = 0)) +
    scale_x_continuous(limits=limits,
                       breaks = c(1,2,3,4,5,6,7),
                       labels=c("XL", "QL", "SL", "N", "SUL", "QUL", "XUL"))
  
  
}

# M4: Intercept+site, varying disc by site
plot_M4_latent_distribution <- function(m, title, lower=-3.5, upper=3.5) {
  tibble(siteIndia = 0:1,
         mu   = c(0, fixef(m)["siteIndia", 1]),
         sigma=1/exp(c(0, fixef(m)["disc_siteIndia", 1]))) |>
    expand(nesting(siteIndia, mu, sigma),
           x = seq(from = -3.5, to = 3.5, length.out = 200)) |>
    mutate(d   = dnorm(x, mean = mu, sd = sigma),
           site = ifelse(siteIndia == 0, "Europe", "India")) |>
    ggplot(aes(x = x, y = d, fill = site)) +
    geom_area(alpha = 1/2, position = "identity") +
    geom_vline(xintercept = fixef(m)[1:6, 1], linetype = 3) +
    scale_fill_manual(NULL, values = c("#e66101", "#5e3c99")) +
    scale_x_continuous(expression(Phi), breaks = -3:3,
                       sec.axis = dup_axis(
                         name = NULL,
                         breaks = fixef(m)[1:6, 1] |> as.double(),
                         labels = c("XL", "QL", "SL", "N", "SUL", "QUL"))) +
    scale_y_continuous(NULL, breaks = NULL) +
    coord_cartesian(xlim = c(lower, upper)) +
    theme_bw() +
    labs(title = paste("Latent distribution for", title),
         subtitle = paste("Formula: ", m$formula))
}

draw_M4_posterior <- function(m) {
  as_draws_df(m) %>%
    select(.draw, starts_with("b_")) %>%
    set_names(".draw", str_c("tau[", 1:6, "]"), "beta[1]", "b_disc[1]") %>%
    # insert another copy of the data below
    bind_rows(., .) %>%
    # add the two values for the dummy variable siteIndia
    mutate(siteIndia = rep(0:1, each = n() / 2)) %>%
    # compute the p_k values conditional on the site dummy
    mutate(p1 = pnorm(`tau[1]`, mean = 0 + siteIndia * `beta[1]`, sd = 1/exp(0 + siteIndia * `b_disc[1]`)),
           p2 = pnorm(`tau[2]`, mean = 0 + siteIndia * `beta[1]`, sd = 1/exp(0 + siteIndia * `b_disc[1]`)) - pnorm(`tau[1]`, mean = 0 + siteIndia * `beta[1]`, sd = 1/exp(0 + siteIndia * `b_disc[1]`)),
           p3 = pnorm(`tau[3]`, mean = 0 + siteIndia * `beta[1]`, sd = 1/exp(0 + siteIndia * `b_disc[1]`)) - pnorm(`tau[2]`, mean = 0 + siteIndia * `beta[1]`, sd = 1/exp(0 + siteIndia * `b_disc[1]`)),
           p4 = pnorm(`tau[4]`, mean = 0 + siteIndia * `beta[1]`, sd = 1/exp(0 + siteIndia * `b_disc[1]`)) - pnorm(`tau[3]`, mean = 0 + siteIndia * `beta[1]`, sd = 1/exp(0 + siteIndia * `b_disc[1]`)),
           p5 = pnorm(`tau[5]`, mean = 0 + siteIndia * `beta[1]`, sd = 1/exp(0 + siteIndia * `b_disc[1]`)) - pnorm(`tau[4]`, mean = 0 + siteIndia * `beta[1]`, sd = 1/exp(0 + siteIndia * `b_disc[1]`)),
           p6 = pnorm(`tau[6]`, mean = 0 + siteIndia * `beta[1]`, sd = 1/exp(0 + siteIndia * `b_disc[1]`)) - pnorm(`tau[5]`, mean = 0 + siteIndia * `beta[1]`, sd = 1/exp(0 + siteIndia * `b_disc[1]`)),
           p7 = 1 - pnorm(`tau[6]`, mean = 0 + siteIndia * `beta[1]`, sd = 1/exp(0 + siteIndia * `b_disc[1]`))) %>%
    # wrangle
    pivot_longer(starts_with("p"), values_to = "p") %>%
    mutate(response = str_extract(name, "\\d") %>% as.double()) %>%
    # compute p_k * k
    mutate(`p * response` = p * response) %>% 
    # sum those values within each posterior draw, by the siteIndia dummy
    group_by(.draw, siteIndia) %>% 
    summarise(mean_response = sum(`p * response`)) %>% 
    mutate(site = ifelse(siteIndia == 0, "Europe", "India"))
}

expected_value_M4 <- function(m, lower=0.025, upper=0.975) {
  draw_M4_posterior(m) |> group_by(site) |> summarise(expected=mean(mean_response), lower=quantile(mean_response, lower), upper=quantile(mean_response, upper))
}

plot_M4_posterior_mean <- function(m, title, sample_mean, limits=c(1,7)) {
  m_Europe <- m$data |> filter(site == 'Europe') |> sample_mean() |> pull()
  m_India  <- m$data |> filter(site == 'India') |> sample_mean() |> pull()
  # note, the magrittr pipe %>% is needed, because native |> pipe does not handle dot
    draw_M4_posterior(m) %>%
    # the trick with and without fct_rev() helps order the axes, colors, and legend labels
    ggplot(aes(x = mean_response, y = fct_rev(site), fill = site)) +
    stat_halfeye(.width = .95) +
    geom_vline(xintercept = m_Europe, linetype = 2, color = "#e66101") +
    geom_vline(xintercept = m_India, linetype = 2, color = "#5e3c99") +
    scale_fill_manual(NULL, values = c(alpha("#e66101", 0.5), alpha("#5e3c99", 0.5))) +
    labs(title = paste0("The posterior for the mean of the rating values for ", title, ", by site"),
         subtitle = paste("The dashed vertical lines mark off the sample means, by site.\nFormula:", m$formula),
         x = expression(mu[response]),
         y = NULL) +
    theme_bw() +
    theme(axis.text.y = element_text(hjust = 0)) +
    scale_x_continuous(limits=limits,
                       breaks = c(1,2,3,4,5,6,7),
                       labels=c("XL", "QL", "SL", "N", "SUL", "QUL", "XUL"))
}

# T3: Total mode, incorporating tutorial as well as a fixed effect
plot_T3_latent_distribution <- function(m, title, lower=-4.5, upper=3.5) {
  # jaeger and vue never occur simultaneously. But they do occur independent of siteIndia
  tibble(siteIndia=c(rep(0, 4), rep(1,4)),
         tutorialjaeger=rep(c(0,0,1,1), 2),
         tutorialvue=rep(c(0,1), 4)) |> 
    # remove the cases where jaeger and vue co-occur before sending the rest down to the pipeline
    filter(!(tutorialjaeger == 1 & tutorialvue == 1)) |>
    mutate(mu = siteIndia*fixef(m)["siteIndia", 1] + tutorialjaeger * fixef(m)["tutorialjaeger", 1] + tutorialvue * fixef(m)["tutorialvue", 1]) |>
    expand(nesting(siteIndia, tutorialjaeger, tutorialvue, mu), x = seq(from = lower, to = upper, length.out = 200)) |>
    mutate(d   = dnorm(x, mean = mu, sd = 1),
           site = ifelse(siteIndia == 0, "Europe", "India"),
           tutorial = as.factor(ifelse(tutorialjaeger == 1, "Jaeger", ifelse(tutorialvue == 1, "Vue", "GenAI")))) |> 
    group_by(tutorial) |>
    ggplot(aes(x = x, y = d, fill = site)) +
    geom_area(alpha = 1/2, position = "identity") +
    geom_vline(xintercept = fixef(m)[1:6, 1], linetype = 3) +
    scale_fill_manual(NULL, values = c("#e66101", "#5e3c99")) +
    scale_x_continuous(expression(phi), breaks = -4:3,
                       sec.axis = dup_axis(name = NULL,
                                           breaks = fixef(m)[1:6, 1] |> as.double(),
                                           labels = c("XL", "QL", "SL", "N", "SUL", "QUL"))) +
    scale_y_continuous(NULL, breaks = NULL) +
    coord_cartesian(xlim = c(lower, upper)) +
    theme_bw() +
    facet_wrap(~ tutorial) +
    labs(title = paste("Latent distribution for", title),
         subtitle = paste("Formula: ", m$formula))
}

# total model, incorporating all tutorials (as a fixed effect)
draw_T3_posterior <- function(m) {
  as_draws_df(m) %>%
    select(.draw, starts_with("b_")) %>%
    set_names(".draw", str_c("tau[", 1:6, "]"), "b_site_India", "b_jaeger", "b_vue") %>%
    # replicate the data for each combination of predictors
    bind_rows(., ., ., ., ., ., ., .) %>%
    # we make it easy for us, and have 8 combinations, but we filter away the ones where Jaeger and Vue co-occur, as they are mutually exclusive
    mutate(siteIndia = rep(0:1, each = n() / 2),
           tutorialjaeger=rep(rep(0:1, each= n()/4), times=2),
           tutorialvue=rep(rep(0:1, each=n()/8), times=4)) %>%
    filter(!(tutorialjaeger == 1 & tutorialvue == 1)) %>%
    # compute the p_k values conditional on the dummies
    mutate(p1 = pnorm(`tau[1]`, mean = 0 + siteIndia * b_site_India + tutorialjaeger * b_jaeger + tutorialvue * b_vue),
           p2 = pnorm(`tau[2]`, mean = 0 + siteIndia * b_site_India + tutorialjaeger * b_jaeger + tutorialvue * b_vue) - pnorm(`tau[1]`, mean = 0 + siteIndia * b_site_India + tutorialjaeger * b_jaeger + tutorialvue * b_vue),
           p3 = pnorm(`tau[3]`, mean = 0 + siteIndia * b_site_India + tutorialjaeger * b_jaeger + tutorialvue * b_vue) - pnorm(`tau[2]`, mean = 0 + siteIndia * b_site_India + tutorialjaeger * b_jaeger + tutorialvue * b_vue),
           p4 = pnorm(`tau[4]`, mean = 0 + siteIndia * b_site_India + tutorialjaeger * b_jaeger + tutorialvue * b_vue) - pnorm(`tau[3]`, mean = 0 + siteIndia * b_site_India + tutorialjaeger * b_jaeger + tutorialvue * b_vue),
           p5 = pnorm(`tau[5]`, mean = 0 + siteIndia * b_site_India + tutorialjaeger * b_jaeger + tutorialvue * b_vue) - pnorm(`tau[4]`, mean = 0 + siteIndia * b_site_India + tutorialjaeger * b_jaeger + tutorialvue * b_vue),
           p6 = pnorm(`tau[6]`, mean = 0 + siteIndia * b_site_India + tutorialjaeger * b_jaeger + tutorialvue * b_vue) - pnorm(`tau[5]`, mean = 0 + siteIndia * b_site_India + tutorialjaeger * b_jaeger + tutorialvue * b_vue),
           p7 = 1 - pnorm(`tau[6]`, mean = 0 + siteIndia * b_site_India + tutorialjaeger * b_jaeger + tutorialvue * b_vue)) %>%
    # wrangle
    pivot_longer(starts_with("p"), values_to = "p") %>%
    mutate(response = str_extract(name, "\\d") %>% as.double()) %>%
    # compute p_k * k
    mutate(`p * response` = p * response) %>% 
    # sum those values within each posterior draw, by the dummies
    group_by(.draw, siteIndia, tutorialjaeger, tutorialvue) %>% 
    summarise(mean_response = sum(`p * response`)) %>% 
    mutate(site = ifelse(siteIndia == 0, "Europe", "India"),
           tutorial = as.factor(ifelse(tutorialjaeger == 1, "jaeger", ifelse(tutorialvue == 1, "vue", "genai")))) 
}

expected_value_T3 <- function(m, lower=0.025, upper=0.975) {
  draw_T3_posterior(m) |> group_by(tutorial, site) |> summarise(expected=mean(mean_response), lower=quantile(mean_response, lower), upper=quantile(mean_response, upper))
}

plot_T3_posterior_mean <- function(m, title, sample_mean, limits=c(1,7)) {
  sample_means <- m$data |> group_by(tutorial, site) |> sample_mean()
  m_genAI_Europe <- sample_means |> filter(tutorial=="genai", site == "Europe") |> pull()
  m_genAI_India   <- sample_means |> filter(tutorial=="genai", site == "India") |> pull()
  m_jaeger_Europe <- sample_means |> filter(tutorial=="jaeger", site == "Europe") |> pull()
  m_jaeger_India   <- sample_means |> filter(tutorial=="jaeger", site == "India") |> pull()
  m_vue_Europe <- sample_means |> filter(tutorial=="vue", site == "Europe") |> pull()
  m_vue_India   <- sample_means |> filter(tutorial=="vue", site == "India") |> pull()

    # note, the magrittr pipe %>% is needed, because native |> pipe does not handle dot
  result <- draw_T3_posterior(m) 
  genAIdata <- result |> filter(tutorial=="genai")
  vuedata <- result |> filter(tutorial=="vue")
  jaegerdata <- result |> filter(tutorial=="jaeger")
  
  result |>
    group_by(tutorial) |>
    # the trick with and without fct_rev() helps order the axes, colors, and legend labels
    ggplot(aes(x = mean_response, y = fct_rev(site), fill = site)) +
    stat_halfeye(.width = .95) +
    facet_wrap(~ tutorial) +
    geom_vline(data = genAIdata, aes(xintercept = m_genAI_Europe), linetype = 2, color = "#e66101") +
    geom_vline(data = genAIdata, aes(xintercept = m_genAI_India), linetype = 2, color = "#5e3c99") +
    geom_vline(data = vuedata, aes(xintercept = m_vue_Europe), linetype = 2, color = "#e66101") +
    geom_vline(data = vuedata, aes(xintercept = m_vue_India), linetype = 2, color = "#5e3c99") +
    geom_vline(data = jaegerdata, aes(xintercept = m_jaeger_Europe), linetype = 2, color = "#e66101") +
    geom_vline(data = jaegerdata, aes(xintercept = m_jaeger_India), linetype = 2, color = "#5e3c99") +
    scale_fill_manual(NULL, values = c(alpha("#e66101", 0.5), alpha("#5e3c99", 0.5))) +
    labs(title = paste0("The posterior for the mean of the rating values for ", title, ", by tutorial and site"),
         subtitle = paste("Sample means per tutorial and site as dashed vertical lines\nFormula:", m$formula),
         x = expression(mu[response]),
         y = NULL) +
    theme_bw() +
    theme(axis.text.y = element_text(hjust = 0)) +
    scale_x_continuous(limits=limits,
                       breaks = c(1,2,3,4,5,6,7),
                       labels=c("XL", "QL", "SL", "N", "SUL", "QUL", "XUL"))
}

# total model, incorporating all tutorials (as a fixed effect)
draw_T4_posterior <- function(m) {
  as_draws_df(m) %>%
    select(.draw, starts_with("b_")) %>%
    set_names(".draw", str_c("tau[", 1:6, "]"), "b_site_India", "b_jaeger", "b_vue", "b_disc_India", "b_disc_jaeger", "b_disc_vue") %>%
    # replicate the data for each combination of predictors
    bind_rows(., ., ., ., ., ., ., .) %>%
    # we make it easy for us, and have 8 combinations, but we filter away the ones where Jaeger and Vue co-occur, as they are mutually exclusive
    mutate(siteIndia = rep(0:1, each = n() / 2),
           tutorialjaeger=rep(rep(0:1, each= n()/4), times=2),
           tutorialvue=rep(rep(0:1, each=n()/8), times=4)) %>%
    filter(!(tutorialjaeger == 1 & tutorialvue == 1)) %>%
    # compute the p_k values conditional on the dummies
    mutate(p1 = pnorm(`tau[1]`, 
                      mean = 0 + siteIndia * b_site_India + tutorialjaeger * b_jaeger + tutorialvue * b_vue, 
                      sd = 1/exp(0 + siteIndia * b_disc_India + tutorialjaeger * b_disc_jaeger + tutorialvue * b_disc_vue)),
           p2 = pnorm(`tau[2]`, 
                      mean = 0 + siteIndia * b_site_India + tutorialjaeger * b_jaeger + tutorialvue * b_vue,
                      sd = 1/exp(0 + siteIndia * b_disc_India + tutorialjaeger * b_disc_jaeger + tutorialvue * b_disc_vue)) - pnorm(`tau[1]`, 
                                mean = 0 + siteIndia * b_site_India + tutorialjaeger * b_jaeger + tutorialvue * b_vue,
                                sd = 1/exp(0 + siteIndia * b_disc_India + tutorialjaeger * b_disc_jaeger + tutorialvue * b_disc_vue)),
           p3 = pnorm(`tau[3]`, 
                      mean = 0 + siteIndia * b_site_India + tutorialjaeger * b_jaeger + tutorialvue * b_vue,
                      sd = 1/exp(0 + siteIndia * b_disc_India + tutorialjaeger * b_disc_jaeger + tutorialvue * b_disc_vue)) - pnorm(`tau[2]`,
                                mean = 0 + siteIndia * b_site_India + tutorialjaeger * b_jaeger + tutorialvue * b_vue,
                                sd = 1/exp(0 + siteIndia * b_disc_India + tutorialjaeger * b_disc_jaeger + tutorialvue * b_disc_vue)),
           p4 = pnorm(`tau[4]`,
                      mean = 0 + siteIndia * b_site_India + tutorialjaeger * b_jaeger + tutorialvue * b_vue,
                      sd = 1/exp(0 + siteIndia * b_disc_India + tutorialjaeger * b_disc_jaeger + tutorialvue * b_disc_vue)) - pnorm(`tau[3]`,
                                mean = 0 + siteIndia * b_site_India + tutorialjaeger * b_jaeger + tutorialvue * b_vue,
                                sd = 1/exp(0 + siteIndia * b_disc_India + tutorialjaeger * b_disc_jaeger + tutorialvue * b_disc_vue)),
           p5 = pnorm(`tau[5]`,
                      mean = 0 + siteIndia * b_site_India + tutorialjaeger * b_jaeger + tutorialvue * b_vue,
                      sd = 1/exp(0 + siteIndia * b_disc_India + tutorialjaeger * b_disc_jaeger + tutorialvue * b_disc_vue)) - pnorm(`tau[4]`,
                                mean = 0 + siteIndia * b_site_India + tutorialjaeger * b_jaeger + tutorialvue * b_vue,
                                sd = 1/exp(0 + siteIndia * b_disc_India + tutorialjaeger * b_disc_jaeger + tutorialvue * b_disc_vue)),
           p6 = pnorm(`tau[6]`,
                      mean = 0 + siteIndia * b_site_India + tutorialjaeger * b_jaeger + tutorialvue * b_vue,
                      sd = 1/exp(0 + siteIndia * b_disc_India + tutorialjaeger * b_disc_jaeger + tutorialvue * b_disc_vue)) - pnorm(`tau[5]`,
                                mean = 0 + siteIndia * b_site_India + tutorialjaeger * b_jaeger + tutorialvue * b_vue,
                                sd = 1/exp(0 + siteIndia * b_disc_India + tutorialjaeger * b_disc_jaeger + tutorialvue * b_disc_vue)),
           p7 = 1 - pnorm(`tau[6]`,
                          mean = 0 + siteIndia * b_site_India + tutorialjaeger * b_jaeger + tutorialvue * b_vue,
                          sd = 1/exp(0 + siteIndia * b_disc_India + tutorialjaeger * b_disc_jaeger + tutorialvue * b_disc_vue))) %>%
    # wrangle
    pivot_longer(starts_with("p"), values_to = "p") %>%
    mutate(response = str_extract(name, "\\d") %>% as.double()) %>%
    # compute p_k * k
    mutate(`p * response` = p * response) %>% 
    # sum those values within each posterior draw, by the dummies
    group_by(.draw, siteIndia, tutorialjaeger, tutorialvue) %>% 
    summarise(mean_response = sum(`p * response`)) %>% 
    mutate(site = ifelse(siteIndia == 0, "Europe", "India"),
           tutorial = as.factor(ifelse(tutorialjaeger == 1, "jaeger", ifelse(tutorialvue == 1, "vue", "genai")))) 
}

expected_value_T4 <- function(m, lower=0.025, upper=0.975) {
  draw_T4_posterior(m) |> group_by(tutorial, site) |> summarise(expected=mean(mean_response), lower=quantile(mean_response, lower), upper=quantile(mean_response, upper))
}

plot_T4_posterior_mean <- function(m, title, sample_mean, limits=c(1,7)) {
  sample_means <- m$data |> group_by(tutorial, site) |> sample_mean()
  m_genAI_Europe <- sample_means |> filter(tutorial=="genai", site == "Europe") |> pull()
  m_genAI_India   <- sample_means |> filter(tutorial=="genai", site == "India") |> pull()
  m_jaeger_Europe <- sample_means |> filter(tutorial=="jaeger", site == "Europe") |> pull()
  m_jaeger_India   <- sample_means |> filter(tutorial=="jaeger", site == "India") |> pull()
  m_vue_Europe <- sample_means |> filter(tutorial=="vue", site == "Europe") |> pull()
  m_vue_India   <- sample_means |> filter(tutorial=="vue", site == "India") |> pull()
  
  # note, the magrittr pipe %>% is needed, because native |> pipe does not handle dot
  result <- draw_T4_posterior(m) 
  genAIdata <- result |> filter(tutorial=="genai")
  vuedata <- result |> filter(tutorial=="vue")
  jaegerdata <- result |> filter(tutorial=="jaeger")
  
  result |>
    group_by(tutorial) |>
    # the trick with and without fct_rev() helps order the axes, colors, and legend labels
    ggplot(aes(x = mean_response, y = fct_rev(site), fill = site)) +
    stat_halfeye(.width = .95) +
    facet_wrap(~ tutorial) +
    geom_vline(data = genAIdata, aes(xintercept = m_genAI_Europe), linetype = 2, color = "#e66101") +
    geom_vline(data = genAIdata, aes(xintercept = m_genAI_India), linetype = 2, color = "#5e3c99") +
    geom_vline(data = vuedata, aes(xintercept = m_vue_Europe), linetype = 2, color = "#e66101") +
    geom_vline(data = vuedata, aes(xintercept = m_vue_India), linetype = 2, color = "#5e3c99") +
    geom_vline(data = jaegerdata, aes(xintercept = m_jaeger_Europe), linetype = 2, color = "#e66101") +
    geom_vline(data = jaegerdata, aes(xintercept = m_jaeger_India), linetype = 2, color = "#5e3c99") +
    scale_fill_manual(NULL, values = c(alpha("#e66101", 0.5), alpha("#5e3c99", 0.5))) +
    labs(title = paste0("The posterior for the mean of the rating values for ", title, ", by tutorial and site"),
         subtitle = paste("Sample means per tutorial and site as dashed vertical lines\nFormula:", m$formula),
         x = expression(mu[response]),
         y = NULL) +
    theme_bw() +
    theme(axis.text.y = element_text(hjust = 0)) +
    scale_x_continuous(limits=limits,
                       breaks = c(1,2,3,4,5,6,7),
                       labels=c("XL", "QL", "SL", "N", "SUL", "QUL", "XUL"))
}

plot_T4_latent_distribution <- function(m, title, lower=-4.5, upper=3.5) {
  # jaeger and vue never occur simultaneously. But they do occur independent of siteIndia
  tibble(siteIndia=c(rep(0, 4), rep(1,4)),
         tutorialjaeger=rep(c(0,0,1,1), 2),
         tutorialvue=rep(c(0,1), 4)) |> 
    # remove the two cases where jaeger and vue co-occur before sending the rest down to the pipeline
    filter(!(tutorialjaeger == 1 & tutorialvue == 1)) |>
    mutate(mu = siteIndia*fixef(m)["siteIndia", 1] + tutorialjaeger * fixef(m)["tutorialjaeger", 1] + tutorialvue * fixef(m)["tutorialvue", 1],
           sigma = 1/exp(siteIndia*fixef(m)["disc_siteIndia", 1] + tutorialjaeger * fixef(m)["disc_tutorialjaeger", 1] + tutorialvue * fixef(m)["disc_tutorialvue", 1])) |>
    expand(nesting(siteIndia, tutorialjaeger, tutorialvue, mu, sigma), x = seq(from = lower, to = upper, length.out = 200)) |>
    mutate(d   = dnorm(x, mean = mu, sd = sigma),
           site = ifelse(siteIndia == 0, "Europe", "India"),
           tutorial = as.factor(ifelse(tutorialjaeger == 1, "Jaeger", ifelse(tutorialvue == 1, "Vue", "GenAI")))) |> 
    group_by(tutorial) |>
    ggplot(aes(x = x, y = d, fill = site)) +
    geom_area(alpha = 1/2, position = "identity") +
    geom_vline(xintercept = fixef(m)[1:6, 1], linetype = 3) +
    scale_fill_manual(NULL, values = c("#e66101", "#5e3c99")) +
    scale_x_continuous(expression(phi), breaks = -4:3,
                       sec.axis = dup_axis(name = NULL,
                                           breaks = fixef(m)[1:6, 1] |> as.double(),
                                           labels = c("XL", "QL", "SL", "N", "SUL", "QUL"))) +
    scale_y_continuous(NULL, breaks = NULL) +
    coord_cartesian(xlim = c(lower, upper)) +
    theme_bw() +
    facet_wrap(~ tutorial) +
    labs(title = paste("Latent distribution for", title),
         subtitle = paste("Formula: ", m$formula))
}
