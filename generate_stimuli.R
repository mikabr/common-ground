library(tidyverse)
source("util.R")

conditions <- c("positive", "negative", "prior")
stimuli <- yaml::yaml.load_file("stimuli/run4/stimuli.yml")

generate_prompt <- function(condition, verb, property) {

  prompt <- sprintf("How likely is someone being %s to %s people?", property,
                    verb)

  if (condition == "prior") return(prompt)
  if (condition == "positive") {
    if (grepl("[^aeiou]y$", verb)) {
      # verb ends in consonant+y -> pluralize by replacing y with ies
      verb_phrase <- paste(substr(verb, 1, nchar(verb) - 1),
                           "ies", sep = "")
    } else if (grepl("ss$", verb)) {
      # verb ends in ss -> pluralize by adding es
      verb_phrase <- paste(verb, "es", sep = "")
    } else {
      # otherwise pluralize by adding s
      verb_phrase <- paste(verb, "s", sep = "")
    }
  } else if (condition == "negative") {
    verb_phrase <- paste("doesn't", verb)
  }
  names <- sample(stimuli$names, 2)
  return(sprintf('%s tells you: "It %s me that %s is %s". %s',
                 names[1], verb_phrase, names[2], property, prompt))
}

verbs <- stimuli$verbs %>%
  as_data_frame() %>%
  gather(verb_valence, verb, positive, negative)

properties <- stimuli$properties %>%
  as_data_frame() %>%
  gather(property_valence, property, positive, negative) #%>%
# mutate(ord = row_number() %% 2) %>%
# arrange(ord) %>%
# select(-ord)

# items <- cross_d(list(condition = conditions,
#                       verb = unlist(stimuli$verbs),
#                       property = unlist(stimuli$properties))) %>%
#   left_join(verbs) %>%
#   left_join(properties)


items <- bind_cols(verbs, properties)
# group_by(verb_valence, property_valence) %>%
# mutate(property = sample(property)) %>%
# ungroup()

# walk(conditions, function(condition) {
#   dots <- list(~map2_chr(verb, property,
#                          ~generate_prompt(condition, .x, .y)))
#   items <<- items %>%
#     mutate_(.dots = setNames(dots, condition))
# })

condition_items <- conditions %>%
  map_df(~items %>% mutate(condition = .x)) %>%
  mutate(prompt = pmap_chr(list(condition, verb, property), generate_prompt),
         #type = "item",
         code = pmap_chr(list(verb_valence, property_valence),
                         function(v, p) {
                           encode(lst("verb_valence" = v,
                                      "property_valence" = p))
                         })) %>%
  select(condition, code, prompt) #type,

  #group_by(condition) %>%
  #sample_frac() #%>%
  #gather_("condition", "prompt", conditions)

#num_items <- 12
#num_lists <- nrow(items) / num_items

# items_randomized <- items %>%
#   group_by(condition, verb_valence, property_valence) %>%
#   mutate(list = sample(1:num_lists)) %>%
#   group_by(list) %>%
#   sample_frac() %>%
#   mutate(trial = 1:n())
#
# cp_sep <- ";"
#
# items_wide <- items_randomized %>%
#   mutate(prompt = pmap_chr(list(condition, verb, property), generate_prompt),
#          code = pmap_chr(list(condition, verb_valence, property_valence),
#                          function(c, v, p) {
#                            encode(lst("condition" = c,
#                                       "verb_valence" = v,
#                                       "property_valence" = p))
#                          })) %>%
#   select(list, trial, code, prompt) %>%
#   unite(trial_info, code, prompt, sep = cp_sep) %>%
#   spread(trial, trial_info)

# walk(1:num_items, function(i) {
#   items_wide <<- items_wide %>%
#     separate_(as.character(i),
#               c(sprintf("code_%s", i), sprintf("prompt_%s", i)),
#               sep = cp_sep)
# })

#items_wide %>% write.csv("stimuli/combined.csv", row.names = FALSE)

# items %>%
#   mutate(code = sprintf("verb_%s;property_%s", verb_valence,
#                         property_valence)) %>%
#   select(condition, code, prompt) %>%
#   split(.$condition) %>%
#   walk(function(cond_data) {
#     cond <- unique(cond_data$condition)
#     map_df(1:num_lists, ~
#              cond_data %>%
#              select(-condition) %>%
#              sample_frac() %$%
#              set_names(c(code, prompt),
#                        c(paste("code", 1:num_items, sep = "_"),
#                          paste("prompt", 1:num_items, sep = "_"))) %>%
#              t() %>%
#              as_data_frame() %>%
#              mutate(list = .x)
#     ) %>%
#       write.csv(sprintf("stimuli/%s.csv", cond), row.names = FALSE)
#   })

fillers <- stimuli$fillers %>%
  transpose() %>%
  simplify_all() %>%
  as_data_frame() %>%
  gather(filler_value, attitude, high, low)

# fillers <- stimuli$fillers %>%
#   #as_data_frame() %>%
#   map(generate_filler_prompt)
#   gather(filler_value, filler, high, low) %>%
#   mutate(type = "filler",
#          code = map_chr(filler_value, ~encode(lst("filler_value" = .x))),
#          prompt = map_chr(filler, generate_filler_prompt))

generate_filler_prompt <- function(condition, event, trigger, attitude) {
  prompt <- sprintf('How likely is %s to make someone %s?', trigger, attitude)
  if (condition == "prior") return(prompt)
  name <- sample(stimuli$names, 1)
  return(sprintf('%s tells you: "I %s". %s', name, event, prompt))
}

condition_fillers <- conditions %>%
  map_df(~fillers %>% mutate(condition = .x)) %>%
  mutate(prompt = pmap_chr(list(condition, event, trigger, attitude),
                           generate_filler_prompt),
         #type = "filler",
         code = map_chr(filler_value, ~encode(lst("filler_value" = .x)))) %>%
  select(condition, code, prompt) #type,

num_lists <- 30
num_items <- nrow(items) + nrow(fillers)

condition_items %>%
  bind_rows(condition_fillers) %>%
  split(.$condition) %>%
  walk(function(cond_data) {
    cond <- unique(cond_data$condition)
    map_df(1:num_lists, ~
             cond_data %>%
             #group_by(type) %>%
             sample_frac() %>%
             #ungroup() %>%
             #arrange(desc(type)) %>%
             #select(-condition, -type) %$%
             select(-condition) %$%
             set_names(c(code, prompt),
                       c(paste("code", 1:num_items, sep = "_"),
                         paste("prompt", 1:num_items, sep = "_"))) %>%
             t() %>%
             as_data_frame() %>%
             mutate(list = .x)
    ) %>%
      write.csv(sprintf("stimuli/run4/%s.csv", cond), row.names = FALSE)
  })
