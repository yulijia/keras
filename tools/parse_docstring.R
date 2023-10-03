

D <- keras$layers$Attention$`__doc__`

d <- str_split_1(D, "\n") %>% str_trim("right")

known_section_headings <- c("Args:", "Call arguments:", "Output:")

df <- tibble(line = d) %>%
  mutate(indentation_lvl = str_extract(line, "^\\s*") %>% str_width(),
         line0 = str_trim(line))

df %>%
  filter(str_trim(line) %in% known_section_headings)

re <- known_section_headings %>%
  str_sub(end = -2) %>%
  str_flatten("|") %>%
  # str_c("^\\s*(", ., "):$")
  str_c("\n\\s*(", ., "):\n")

d <- D %>%
  str_split_1(re)
# %>% length()

# split()




section <- rep("description", nrow(df))
section[1] <- "title"

for(section_heading in known_section_headings) {
  i <- which(str_trim(df$line) == section_heading)
  if(!length(i))
    next
  if(length(i) > 1) stop("too many ", section_heading)

  section[i] <- sh <- str_sub(section_heading, end=-2)
  ind_lvl0 <- df$indentation_lvl[i]
  for(i in seq(i+1, nrow(df))) {
    ind_lvl <- df$indentation_lvl[i]
    if(ind_lvl == 0 || ind_lvl > ind_lvl0)
      section[i] <- sh
    else
      break
  }
};

# in_code_block <- ""
# indentation_lvl <- 0
# for(i in seq_len(nrow(df))) {
#   if(in_code_block == "```") {
#
#     section[i] <- "code"
#     if(df$line0[[i]] == "```") {
#       in_code_block <- ""
#     }
#
#   } else if (in_code_block == ">>>") {
#     df$indentation_lvl[i] == 0
#     if(indentation_lvl)) {
#       indentation_lvlin_code_block <- TRUE
#       section[i] <- "code"
#     }
#
#   } else {
#     # not in a code bock
#   }
# }

in_code_block <- FALSE
for(i in seq_len(nrow(df))) {
  if(in_code_block) {

    section[i] <- "code"
    if(df$line0[[i]] == "```") {
      in_code_block <- FALSE
    }

  } else { # not in a code bock

    if(startsWith(df$line0[[i]], "```")) {
      section[i] <- "code"
      in_code_block <- TRUE
    }

  }
}
df$section <- section; df %>% select(section, indentation_lvl, line) %>% print(n=Inf)

in_section <- FALSE
last_indentation_lvl <- 0
for(i in 2:nrow(df)) {
  line <- str_trim(df$line[[i]])
  indentation_lvl <- df$indentation_lvl[[i]]

  if(indentation_lvl == 0) {
    # in_section <- NULL
    # section[i] <- ""
    next
  }

  if(line %in% known_section_headings) {
    section[i] <- in_section <- line %>% str_sub(end = -2) #drop colon ":"
    stopifnot(indentation_lvl == 4)
    last_indentation_lvl <- 4
    next
  }

  if(indentation_lvl > 4) {
    if(is.null(in_section))
      stop("unknown section")
    section[i] <- in_section
    next
  }

  if(indentation_lvl == 4) {
    section[i] <- "description"
  }
}; df$section <- section; df %>% select(section, indentation_lvl, line) %>% print(n=Inf)






section <- character(nrow(df))
section[1] <- "title"
in_section <- FALSE
last_indentation_lvl <- 0
for(i in 2:nrow(df)) {
  line <- str_trim(df$line[[i]])
  indentation_lvl <- df$indentation_lvl[[i]]

  if(indentation_lvl == 0) {
    # in_section <- NULL
    # section[i] <- ""
    next
  }

  if(line %in% known_section_headings) {
    section[i] <- in_section <- line %>% str_sub(end = -2) #drop colon ":"
    stopifnot(indentation_lvl == 4)
    last_indentation_lvl <- 4
    next
  }

  if(indentation_lvl > 4) {
    if(is.null(in_section))
      stop("unknown section")
    section[i] <- in_section
    next
  }

  if(indentation_lvl == 4) {
    section[i] <- "description"
  }
}; df$section <- section; df %>% select(section, indentation_lvl, line) %>% print(n=Inf)






  if



}

if_else(str_trim(d, "left") %in% c(), )
