library(tidyverse)
library(googleLanguageR)

budget_raw <- read.csv("(67) Thailand's Budget.csv")
prov_pop <- read.csv("stat_c65.csv")

json_file <- NA

translate_text <- function(text) {
        gl_translate(text, source = "th", target = "en")$translatedText
}

gl_auth(json_file)

# 1. Transforming the dataframe ----
budget_trans <- budget_raw
budget_trans$ITEM_ENG <- sapply(budget_raw$ITEM_DESCRIPTION, translate_text)

provinces <- c("amnat charoen", "ang thong", "ayutthaya", "bangkok", "bueng kan", "buriram", "chachoengsao", "chai nat", "chaiyaphum", "chanthaburi", "chiang mai", "chiang rai", "chonburi", "chumphon", "kalasin", "kamphaeng phet", "kanchanaburi", "khon kaen", "krabi", "lampang", "lamphun", "loei", "lopburi", "mae hong son", "maha sarakham", "mukdahan", "nakhon nayok", "nakhon pathom", "nakhon phanom", "nakhon ratchasima", "nakhon sawan", "nakhon si thammarat", "nan", "narathiwat", "nong bua lamphu", "nong khai", "nonthaburi", "pathum thani", "pattani", "phang nga", "phatthalung", "phayao", "phetchabun", "phetchaburi", "phichit", "phitsanulok", "phrae", "phuket", "prachin buri", "prachuap khiri khan", "ranong", "ratchaburi", "rayong", "roi et", "sa kaeo", "sakon nakhon", "samut prakan", "samut sakhon", "samut songkhram", "saraburi", "satun", "sing buri", "sisaket", "songkhla", "sukhothai", "suphan buri", "surat thani", "surin", "tak", "trang", "trat", "ubon ratchathani", "udon thani", "uthai thani", "uttaradit", "yala", "yasothon")
provinces2 <- c("chainat","prachinburi", "singburi", "suphanburi", "northeastern", "northern", "northeast","central","southern border", "central","eastern","ka phaeng phet","nakhon san","battani")

all_provinces <- unique(c(provinces,provinces2))

## Enhanced function to extract and concatenate words before each "Province"
extract_provinces <- function(item) {
        # This pattern looks for up to three words before "Province" and handles commas, dots, and conjunctions
        pattern <- "(\\b\\S+(?:\\s+\\S+){0,2}\\s+Province)"
        matches <- str_extract_all(item, pattern)[[1]]
        matches <- sapply(matches, function(x) {
                x <- gsub(" Province", "", x)
                x <- gsub("[.,;:]", "", x) # Remove punctuation
                x <- gsub("\\band\\b|\\bor\\b", "", x) # Remove conjunctions 'and', 'or'
                trimws(x) # Trim whitespace
        })
        paste(unique(matches), collapse=", ")
}

## Applying the function to the dataframe
budget_trans <- budget_trans |>
        mutate(ITEM_EXTRACTED = sapply(ITEM_ENG, extract_provinces)) |>
        mutate(ITEM_EXTRACTED = tolower(ITEM_EXTRACTED))

## Mapping from provinces2 to provinces
province_mapping <- c(
        "chainat" = "chai nat",
        "prachinburi" = "prachin buri",
        "singburi" = "sing buri",
        "suphanburi" = "suphan buri",
        "ka phaeng phet" = "kamphaeng phet",
        "nakhon san" = "nakhon sawan",
        "battani" = "pattani"
)

## Function to map variances to standard province names
map_province_names <- function(province) {
        if (province %in% names(province_mapping)) {
                return(province_mapping[province])
        } else {
                return(province)
        }
}

## Updated function to identify all matching provinces with word boundaries and return NA for no match
find_province <- function(extracted) {
        matched_provinces <- sapply(all_provinces, function(province) {
                pattern <- paste0("\\b", province, "\\b")  # Add word boundaries to the pattern
                if (grepl(pattern, extracted)) return(map_province_names(province))
                else return(NA)
        })
        matched_provinces <- na.omit(matched_provinces)
        if (length(matched_provinces) > 0) return(paste(matched_provinces, collapse=", "))
        else return(NA)
}

budget_trans$ITEM_PROVINCE <- sapply(budget_trans$ITEM_EXTRACTED, find_province)

## Assign NA to ITEM_EXTRACTED where 'Province' wasn't found
budget_trans$ITEM_EXTRACTED[budget_trans$ITEM_EXTRACTED == "NA"] <- NA

## Adding the ITEM_BANGKOK column
budget_trans <- budget_trans |>
        mutate(ITEM_BANGKOK = ifelse(grepl("bangkok", tolower(ITEM_ENG)), "bangkok", NA))

## Combining ITEM_PROVINCE and ITEM_BANGKOK
budget_trans <- budget_trans |>
        mutate(PROVINCE_COMBINED = case_when(!is.na(ITEM_BANGKOK) & !is.na(ITEM_PROVINCE) ~ paste(ITEM_PROVINCE, ITEM_BANGKOK, sep=", "),
                                             is.na(ITEM_BANGKOK) & !is.na(ITEM_PROVINCE) ~ ITEM_PROVINCE,
                                             !is.na(ITEM_BANGKOK) & is.na(ITEM_PROVINCE) ~ ITEM_BANGKOK))

## Updated counting of unique provinces
budget_trans$NO_PROVINCES <- sapply(strsplit(budget_trans$PROVINCE_COMBINED, ",\\s*"), function(x) {
        x <- x[x != "" & !is.na(x)] # Remove empty and NA values
        length(unique(x))
})

## Finding the maximum number of provinces in a single row
max_provinces <- max(budget_trans$NO_PROVINCES, na.rm = TRUE)

## Splitting provinces into separate columns
province_cols <- strsplit(budget_trans$PROVINCE_COMBINED, ", ")
budget_trans[paste0("PROVINCE", 1:max_provinces)] <- t(sapply(province_cols, `[`, 1:max_provinces))

# 2. Dividing the budget ----
# Remove non-numeric characters (like currency symbols, commas, etc.) and then convert to numeric
budget_trans_new <- budget_trans |>
        rownames_to_column(var = "ID") |>
        mutate(AMOUNT = as.numeric(gsub("[^0-9.-]", "", AMOUNT))) |>
        filter(AMOUNT > 0) |>
        filter(NO_PROVINCES > 0)

# Ensure PROVINCE is a character vector in each new row
budget_div_expanded <- do.call(rbind, lapply(1:nrow(budget_trans_new), function(i) {
        row <- budget_trans_new[i, ]
        if (!is.na(row$NO_PROVINCES) && row$NO_PROVINCES == 1) {
                row$PROVINCE <- as.character(row$PROVINCE1)
                return(data.frame(row))
        } else if (!is.na(row$NO_PROVINCES) && row$NO_PROVINCES > 1) {
                split_rows <- lapply(1:row$NO_PROVINCES, function(j) {
                        new_row <- row
                        new_row$PROVINCE <- as.character(row[paste0("PROVINCE", j)])
                        if (!is.na(new_row$AMOUNT)) {
                                new_row$AMOUNT <- as.numeric(new_row$AMOUNT) / as.numeric(row$NO_PROVINCES)
                        }
                        return(data.frame(new_row))
                })
                return(do.call(rbind, split_rows))
        }
}))

# Convert the result back to a dataframe
budget_div_expanded <- as.data.frame(budget_div_expanded)

budget_div <- budget_div_expanded |>
        dplyr::select(1:20,"PROVINCE") |>
        filter(FISCAL_YEAR == "2024")

table(budget_div$FISCAL_YEAR)

# 3. Prelim analysis ----
#prov_pop$PROVINCE <- sapply(prov_pop$PROVINCE_THAI, translate_text)
prov_pop_new <- prov_pop |>
        mutate(PROVINCE = gsub(" Province", "", PROVINCE)) |>
        mutate(PROVINCE = tolower(PROVINCE)) |>
        mutate(PROVINCE = case_when(PROVINCE == "chainat" ~ "chai nat",
                                    PROVINCE == "phra nakhon si ayutthaya" ~ "ayutthaya",
                                    PROVINCE == "prachinburi" ~ "prachin buri",
                                    PROVINCE == "singburi" ~ "sing buri",
                                    PROVINCE == "suphanburi" ~ "suphan buri",
                                    TRUE ~ PROVINCE)) |>
        mutate(POP = as.numeric(gsub("[^0-9.-]", "", POP))) 


budget_rank <- budget_div |> group_by(PROVINCE) |>
        summarise(TOT_AMOUNT = sum(AMOUNT)) |>
        arrange(desc(TOT_AMOUNT)) |>
        left_join(prov_pop_new, by = "PROVINCE") |>
        mutate(BUDGET_PERHEAD = TOT_AMOUNT/POP) |>
        filter(!is.na(CODE)) |>
        mutate(PROVINCE = reorder(PROVINCE, TOT_AMOUNT, FUN = max)) |>
        mutate(TOT_B = TOT_AMOUNT/1000000000)

options(scipen = 10)
ggplot(budget_rank, mapping = aes(y = TOT_B, x = PROVINCE)) +
        geom_col() +
        ylab("Budget 2024 by Province (Billion Baht)") +
        coord_flip() +
        theme_light()

ggsave("~/Desktop/budget_province.png", height = 8, width = 5.5, dpi = 300)

budget_perhead <- budget_div |> group_by(PROVINCE) |>
        summarise(TOT_AMOUNT = sum(AMOUNT)) |>
        arrange(desc(TOT_AMOUNT)) |>
        left_join(prov_pop_new, by = "PROVINCE") |>
        mutate(BUDGET_PERHEAD = TOT_AMOUNT/POP) |>
        filter(!is.na(CODE)) |>
        mutate(PROVINCE = reorder(PROVINCE, BUDGET_PERHEAD, FUN = max)) |>
        mutate(TOT_B = TOT_AMOUNT/1000000000)

ggplot(budget_perhead, mapping = aes(y = BUDGET_PERHEAD, x = PROVINCE)) +
        geom_col() +
        ylab("Budget 2024 per Head by Province (Baht)") +
        coord_flip() +
        theme_light()

ggsave("~/Desktop/budget_province_perhead.png", height = 8, width = 5.5, dpi = 300)

