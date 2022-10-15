#' Create the full quarto text for a phonics slidedeck in revealjs.
#'
#' @param let_1 A vector of letters to be included as possible first letters.
#' @param let_2 A vector of letters to be included as possible second letters.
#' @param let_3 A vector of letters to be included as possible third letters.
#' @param title The title to be displayed as well as the title of the file.
#' @param image_dir The path to a folder where the letter images are stored.
#' @param output_dir The path to the folder where the quarto file will be saved.
#'
#' @return A long string with the text necessary for a complete slide in
#' reveal_js
#' @export

create_deck <- function(let_1, let_2, let_3, title, image_dir, output_dir) {

    document <- paste0(
        "---\n",
        "title: \"", title, "\"\n",
        "format:\n",
        "  revealjs:\n",
        "    navigation-mode: grid\n",
        "    width: 1920\n",
        "    height: 1080\n",
        "---\n\n"
    )
    
    first <- let_1[!(let_1 %in% c("x"))] |>
        sample()
    second <- let_2
    third <- let_3[!(let_3 %in% c("y", "w"))] |>
        sample()

    words <- apply(expand.grid(first, second, third), 1, paste, collapse = "")

    words <- sample(words)

    bad_words <- c(
        "cum", "dic", "cig", "vaj", "fag", "vuk", "fuk", "fux",
        "coc", "cok", "koc", "kok"
    )

    clean_words <- words[!(words %in% bad_words)]

    a_words <- stringr::str_subset(clean_words, "a")
    e_words <- stringr::str_subset(clean_words, "e")
    i_words <- stringr::str_subset(clean_words, "i")
    o_words <- stringr::str_subset(clean_words, "o")
    u_words <- stringr::str_subset(clean_words, "u")

    

    if (length(a_words) > 0) {
        slides <- purrr::map(a_words, ~ create_slide(., image_dir)) |>
            unlist() |>
            paste(collapse = "")

        document <- paste0(
            document,
            "# A words\n\n",
            slides
        )
    }

    if (length(e_words) > 0) {
        slides <- purrr::map(e_words, ~ create_slide(., image_dir)) |>
            unlist() |>
            paste(collapse = "")

        document <- paste0(
            document,
            "# E words\n\n",
            slides
        )
    }

    if (length(i_words) > 0) {
        slides <- purrr::map(i_words, ~ create_slide(., image_dir)) |>
            unlist() |>
            paste(collapse = "")

        document <- paste0(
            document,
            "# I words\n\n",
            slides
        )
    }

    if (length(o_words) > 0) {
        slides <- purrr::map(o_words, ~ create_slide(., image_dir)) |>
            unlist() |>
            paste(collapse = "")

        document <- paste0(
            document,
            "# O words\n\n",
            slides
        )
    }

    if (length(u_words) > 0) {
        slides <- purrr::map(u_words, ~ create_slide(., image_dir)) |>
            unlist() |>
            paste(collapse = "")

        document <- paste0(
            document,
            "# U words\n\n",
            slides
        )
    }

    cat(document, file = paste0(output_dir, "/", title, ".qmd"))


    return(paste0(output_dir, "/", title, ".qmd"))
}