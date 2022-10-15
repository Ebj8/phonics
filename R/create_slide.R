#' Create the quarto text for a phonics slide.
#'
#' @param word A 3-letter nonsense word.
#' @param image_dir The path to a folder where the letter images are stored.
#' 
#' @return A long string with the text necessary for a complete slide in 
#' reveal_js
#' @export

create_slide <- function(word, image_dir) {

    first <- stringr::str_sub(word, 1, 1)
    second <- stringr::str_sub(word, 2, 2)
    third <- stringr::str_sub(word, 3, 3)

    text <- paste0(
        "##\n\n",
        "![](", image_dir, "/", first, ".png){.absolute top=175 left=384}\n",
        "![](", image_dir, "/", second, ".png){.absolute top=175 left=768}\n",
        "![](", image_dir, "/", third, ".png){.absolute top=175 left=1152}\n",
        "\n"
    )

    return(text)
}