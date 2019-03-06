#' Get video clip download url in news
#'
#' Get naver news video url
#'
#' @param tar_url like 'http://news.naver.com/main/read.nhn?mode=LSD&mid=shm&sid1=100&oid=056&aid=0010335895'.
#' @return Get character url.
#' @export
#' @importFrom rvest html_nodes html_attr html_text
#' @importFrom httr GET content add_headers user_agent
video_url_get <- function(tar_url) {
  . <- NULL
  uat <-
    httr::user_agent("N2H4 by chanyub.park <mrchypark@gmail.com>")
  turl <- "http://news.naver.com/"

  httr::GET(tar_url, uat) %>%
    httr::content() %>%
    rvest::html_nodes("iframe") %>%
    rvest::html_attr("_src") %>%
    .[!is.na(.)] %>%
    paste0("http://news.naver.com", .) %>%
    httr::GET(uat, httr::add_headers(Referer = turl)) %>%
    httr::content() %>%
    rvest::html_nodes("script") %>%
    rvest::html_text(src) %>%
    .[nchar(.) > 0] ->
    src

  cod <- strsplit(x = src, split = "'")
  bky <- cod[[1]][4]
  key <- cod[[1]][8]

  paste0("http://play.rmcnmv.naver.com/vod/play/v2.0/",
           bky,
           "?key=",
           key) %>%
    httr::GET(uat) %>%
    httr::content() %>%
    .$videos %>%
    .$list %>%
    .[[1]] %>%
    .$source %>%
    return()
}
