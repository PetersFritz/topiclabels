generate_standard_prompt = function(terms,
                                    context = "",
                                    sep_terms = "; ",
                                    max_length_label = 5,
                                    type = 1L){
  intro = "You are an expert in labeling outputs from topic models.\n\n"
  task = paste0("Could you please help me labeling the following topic ",
                "based on its top terms?")
  introduce_terms = "The top terms of the topic are listed below:\n\n"
  note = paste0("Your answer should only consist of the best label ",
                "for the topic and should not be longer than ",
                max_length_label, " words. ",
                "Only label topics if you can generate a meaningful label.")
  paste0(context, ifelse(context == "", "", "\n\n"),
         intro,
         task, " ", introduce_terms,
         paste(terms, collapse = sep_terms), "\n\n",
         note)
}
