# Initial analysis 
# TODONE: inspo = gatherplotting(https://arxiv.o(rg/abs/1708.08033)
# combo of facets + wafflechart?
# TODONE: decide how to handle missingness
# TODO: decide whether it's ordinal or not...
# TODO: multinominal or ordinal logistic regression
# TODO: (bonus) sankey diagram


library(tidyverse)
# install.packages("waffle")
library(waffle)
library(janitor)
library(gridExtra)
library(grid)


data <- read_csv("../animacy_covariecy/computer_agency_data.csv") %>%
    remove_missing()


data %>%
  ggplot()

waffle_data <- waffle_iron(data, 
                           aes_d(group = llms_understand))

ggplot(waffle_data, aes(x, y, fill = group)) + 
  geom_waffle() + 
  coord_equal() + 
  scale_fill_waffle() + 
  theme_waffle()



# basic waffle plot
waffle(table(data$computers_know), rows = 21, title = "Yes or maybe to any meaning question",
       keep = TRUE, glyph_size = 12, legend_pos = "right")


table(data$assign_computers_agency_incl._jokes) %>% 
  waffle(rows = 21, keep = TRUE, glyph_size = 12, 
         legend_pos = "right")

table(data$llms_understand) %>% 
  waffle(rows = 21, keep = TRUE, glyph_size = 12, 
         legend_pos = "right")

table(data$spell_check_understands) %>% 
  waffle(rows = 21, keep = TRUE, glyph_size = 12, 
         legend_pos = "right")



#### ss training by workign in tech
ss_traing_X_tech <- data %>%
  tabyl(social_science_training, work_in_tech) 

# people who DON"T work in tech
not_tech <- setNames(ss_traing_X_tech$No, ss_traing_X_tech$social_science_training) %>%
  waffle(rows = 13, glyph_size = 1, pad = 0, xlab = "Not in tech") +
  theme(legend.position = "none")

# people who do work in tech
yes_tech <- setNames(ss_traing_X_tech$Yes, ss_traing_X_tech$social_science_training) %>%
  waffle(rows = 13, glyph_size = 14, pad = 0, xlab = "Work in tech")

grid.arrange(not_tech, yes_tech,
             top = "Do you have training (broadly defined)\nin human cognition, neuroscience\nor a social science (e.g. sociology, linguistics, \nanthropology, etc.)? ",
             layout_matrix = rbind(c(1,2,2,2,2,2,2,2)))


### is there a relationship between social science training & 
# if you think LLMs understand

ss_traing_X_llm_understand <- data %>%
  tabyl(llms_understand, social_science_training) 

sst <- setNames(ss_traing_X_llm_understand$No, 
                ss_traing_X_llm_understand$llms_understand) %>%
  waffle(rows = 13, glyph_size = 1, pad = 0, xlab = "Social science training") +
  theme(legend.position = "none")

no_sst <- setNames(ss_traing_X_llm_understand$Yes, 
                   ss_traing_X_llm_understand$llms_understand) %>%
  waffle(rows = 13, glyph_size = 14, pad = 0, xlab = "No social science\ntraining")

grid.arrange(sst, no_sst,
             top = "Do you believe that large language models\n(like BERT or GPT-3) are capable of\nunderstanding language\nin some meaningful way?",
             layout_matrix = rbind(c(1,2,2)))


### relationship between spell check & llm beliefs

llms_vs_spellchk <- data %>%
  tabyl(llms_understand, spell_check_understands) 

spell_check_yes <- setNames(llms_vs_spellchk$Yes, 
                            llms_vs_spellchk$llms_understand) %>%
  waffle(rows = 13, glyph_size = 1, pad = 0, xlab = "Spell check\ncaputures\nintention") +
  theme(legend.position = "none")

spell_check_maybe <- setNames(llms_vs_spellchk$Maybe, 
                              llms_vs_spellchk$llms_understand) %>%
  waffle(rows = 13, glyph_size = 14, pad = 0, xlab = "Spell check\nmight caputure\nintention")+
  theme(legend.position = "none")

spell_check_no <- setNames(llms_vs_spellchk$No, 
                           llms_vs_spellchk$llms_understand) %>%
  waffle(rows = 13, glyph_size = 14, pad = 0, xlab = "Spell check\ndoesn't caputure\nintention")


grid.arrange(spell_check_yes, spell_check_maybe, spell_check_no,
             top = "Do you believe that large language models (like BERT or GPT-3)\nare capable of understanding language in some meaningful way?",
             layout_matrix = rbind(c(1,2,3,3)))

### relationship between spell check & llm beliefs

llms_vs_comp_agency <- data %>%
  tabyl(llms_understand, assign_computers_agency_incl._jokes) 

comp_agency_yes <- setNames(llms_vs_comp_agency$Yes, 
                            llms_vs_comp_agency$llms_understand) %>%
  waffle(rows = 13, glyph_size = 1, pad = 0, xlab = "Refer to computers as if animate") +
  theme(legend.position = "none")

comp_agency_ironically <- setNames(llms_vs_comp_agency$`Only ironically`, 
                              llms_vs_comp_agency$llms_understand) %>%
  waffle(rows = 13, glyph_size = 14, pad = 0, xlab = "Ironically refer to computers as if animate")+
  theme(legend.position = "none")

comp_agency_no <- setNames(llms_vs_comp_agency$No, 
                           llms_vs_comp_agency$llms_understand) %>%
  waffle(rows = 13, glyph_size = 14, pad = 0, xlab = "Doesn't refer to computers\nas if animate")


grid.arrange(comp_agency_yes, comp_agency_ironically, comp_agency_no,
             top = "Do you believe that large language models (like BERT or GPT-3)\nare capable of understanding language in some meaningful way?",
             layout_matrix = rbind(c(1,1,2,2,3,3)))

### relationship between spell check, animacy & llm beliefs

llms_vs_spellchk_animacy_yes <- data %>%
  filter(data$assign_computers_agency_incl._jokes == "Yes") %>%
  tabyl(llms_understand, spell_check_understands) 

spell_check_yes_agency_Yes <- setNames(llms_vs_spellchk_animacy_yes$Yes, 
                                       llms_vs_spellchk_animacy_yes$llms_understand) %>%
  waffle(rows = 13, glyph_size = 1, pad = 0, xlab = "Spell check\ncaputures\nintention") +
  theme(legend.position = "none")

spell_check_maybe_agency_Yes <- setNames(llms_vs_spellchk_animacy_yes$Maybe, 
                                         llms_vs_spellchk_animacy_yes$llms_understand) %>%
  waffle(rows = 13, glyph_size = 14, pad = 0, xlab = "Spell check\nmight caputure\nintention")+
  theme(legend.position = "none")

spell_check_no_agency_Yes <- setNames(llms_vs_spellchk_animacy_yes$No, 
                                      llms_vs_spellchk_animacy_yes$llms_understand) %>%
  waffle(rows = 13, glyph_size = 14, pad = 0, xlab = "Spell check\ndoesn't caputure\nintention")

llms_vs_spellchk_animacy_no <- data %>%
  filter(data$assign_computers_agency_incl._jokes != "Yes") %>%
  tabyl(llms_understand, spell_check_understands) 


spell_check_yes_agency_no <- setNames(llms_vs_spellchk_animacy_no$Yes, 
                                      llms_vs_spellchk_animacy_no$llms_understand) %>%
  waffle(rows = 13, glyph_size = 1, pad = 0, xlab = "Spell check\ncaputures\nintention") +
  theme(legend.position = "none")

spell_check_maybe_agency_no <- setNames(llms_vs_spellchk_animacy_no$Maybe, 
                                        llms_vs_spellchk_animacy_no$llms_understand) %>%
  waffle(rows = 13, glyph_size = 14, pad = 0, xlab = "Spell check\nmight caputure\nintention")+
  theme(legend.position = "none")

spell_check_no_agency_no <- setNames(llms_vs_spellchk_animacy_no$No, 
                                     llms_vs_spellchk_animacy_no$llms_understand) %>%
  waffle(rows = 13, glyph_size = 14, pad = 0, xlab = "Spell check\ndoesn't caputure\nintention")

lables <- tableGrob(c("", "Refer to computers as animate\n(including ironically)", 
                      "Do not refer to computers\nas animate"), 
                    theme= ttheme_minimal())
graphs <- arrangeGrob(spell_check_yes_agency_Yes, spell_check_maybe_agency_Yes, spell_check_no_agency_Yes,
             spell_check_yes_agency_no, spell_check_maybe_agency_no, spell_check_no_agency_no,
             top = "Do you believe that large language models (like BERT or GPT-3)\nare capable of understanding language in some meaningful way?",
             layout_matrix = rbind(c(1,2,3,3),c(4,5,6,6)))

grid.newpage()
grid.draw(cbind(lables, graphs, size = "last"))

### relationship between spell check & llm beliefs

llms_vs_comp_agency <- data %>%
  tabyl(llms_understand, assign_computers_agency_incl._jokes) 

comp_agency_yes <- setNames(llms_vs_comp_agency$Yes, 
                            llms_vs_comp_agency$llms_understand) %>%
  waffle(rows = 13, glyph_size = 1, pad = 0, xlab = "Refer to computers as if animate") +
  theme(legend.position = "none")

comp_agency_ironically <- setNames(llms_vs_comp_agency$`Only ironically`, 
                                   llms_vs_comp_agency$llms_understand) %>%
  waffle(rows = 13, glyph_size = 14, pad = 0, xlab = "Ironically refer to computers as if animate")+
  theme(legend.position = "none")

comp_agency_no <- setNames(llms_vs_comp_agency$No, 
                           llms_vs_comp_agency$llms_understand) %>%
  waffle(rows = 13, glyph_size = 14, pad = 0, xlab = "Doesn't refer to computers\nas if animate")


grid.arrange(comp_agency_yes, comp_agency_ironically, comp_agency_no,
             top = "Do you believe that large language models (like BERT or GPT-3)\nare capable of understanding language in some meaningful way?",
             layout_matrix = rbind(c(1,1,2,2,3,3)))


#### ARE YOU KIDDING ME THERE"S A GGWAFFLE GOD DAMN
#devtools::install_github("liamgilbey/ggwaffle")
library(ggwaffle)
