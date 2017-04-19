library(tidyverse)

conditions <- c("positive", "negative", "prior")
stimuli <- yaml::yaml.load_file("stimuli.yml")

generate_prompt <- function(condition, verb, property) {

  prompt <- sprintf("How likely is someone being %s to %s people?", property,
                    verb)

  if (condition == "prior") {
    return(prompt)
  } else {
    if (condition == "positive") {
      if (grepl("[^aeiou]y$", verb)) {
        # verb ends in consonant+y -> pluralize by replacing y with ies
        verb_phrase <- paste(substr(verb, 1, nchar(verb) - 1),
                             "ies", sep = "")
      } else {
        # otherwise pluralize by adding s
        verb_phrase <- paste(verb, "s", sep = "")
      }
    } else if (condition == "negative") {
      verb_phrase <- paste("doesn't", verb)
    }
    names <- sample(stimuli$names, 2)
    return(sprintf("%s tells you: 'It %s me that %s is %s'. %s",
                   names[1], verb_phrase, names[2], property, prompt))
  }
}

items <- bind_cols(
  stimuli$verbs %>% as_data_frame() %>%
    gather(verb_valence, verb, positive, negative),
  stimuli$properties %>% as_data_frame() %>%
    gather(property_valence, property, positive, negative) %>%
    mutate(ord = row_number() %% 2) %>%
    arrange(ord) %>%
    select(-ord)
) %>%
  group_by(verb_valence, property_valence) %>%
  mutate(property = sample(property)) %>%
  ungroup()

walk(conditions, function(condition) {
  dots <- list(~map2_chr(verb, property,
                         ~generate_prompt(condition, .x, .y)))
  items <<- items %>%
    mutate_(.dots = setNames(dots, condition))
})

items <- items %>%
  gather_("condition", "prompt", conditions)



num_lists <- 10
num_items <- nrow(items) / length(conditions)
items %>%
  mutate(code = sprintf("verb_%s;property_%s", verb_valence,
                        property_valence)) %>%
  select(condition, code, prompt) %>%
  split(.$condition) %>%
  walk(function(cond_data) {
    cond <- unique(cond_data$condition)
    map_df(1:num_lists, ~
             cond_data %>%
             select(-condition) %>%
             sample_frac() %$%
             set_names(c(code, prompt),
                       c(paste("code", 1:num_items, sep = "_"),
                         paste("prompt", 1:num_items, sep = "_"))) %>%
             t() %>%
             as_data_frame() %>%
             mutate(list = .x)
    ) %>%
      write.csv(sprintf("stimuli/%s.csv", cond), row.names = FALSE)
  })
