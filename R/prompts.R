generate_standard_prompt = function(terms,
                                    context = "",
                                    sep_terms = "; ",
                                    max_length_label = 5,
                                    type = "json"){
  intro = "You are an expert in labeling outputs from topic models.\n\n"
  if (type == "json-roles") intro = paste0("<|user|> \n ", intro)
  task = paste0("Could you please help me labeling the following topic ",
                "based on its top terms?")
  introduce_terms = "The top terms of the topic are listed below:\n\n"
  if (type == "json"){
    note = paste0("Output your response in JSON format with a single field called ",
                  "'label', specifying the best label for the topic in no more than ",
                  max_length_label, " words. ")
  }else if (type == "plain"){
    note = paste0("Your answer should only consist of the best label ",
                  "for the topic and should not be longer than ",
                  max_length_label, " words.")
    #"Only label topics if you can generate a meaningful label.")
    # Letzter Satz hat bei mir zu Verschlechterung geführt - daher auskommentiert.
    # Das Modell hat dann zwei Label als Optionen gegeben, die aber beide sehr an den
    # Label orientiert waren statt an der übergeordneten Bedeutung.
  }else if (type == "json-roles"){
    note = paste0("Output your response in JSON format with a single field called ",
                  "'label', specifying the best label for the topic in no more than ",
                  max_length_label, " words. </s> \n <|assistant|> \n ")
  }
  paste0(context, ifelse(context == "", "", "\n\n"),
         intro,
         task, " ", introduce_terms,
         paste(terms, collapse = sep_terms), "\n\n",
         note)
}
