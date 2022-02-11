#install.packages(c("Rcpp","rvest", "stringr", "RSelenium", "pdftools", "dplyr", "LSAfun"))

rm(list = ls())

docs_folder = "docs"

library(openxlsx)
library(pdftools)
library(rvest)
library(stringr)
library(RSelenium)
library(LSAfun)

download_pdfs <- function(search_link, sleeptime = 0.5) {
  print("Initializing ...")
  # Initialize driver
  shell(
    "docker run -d --name myselenium -p 4445:4444 -v C:/Users/benjamin.garzon/Desktop/projects/ScrapeUN/docs:/home/seluser/Downloads selenium/standalone-firefox"
  )
  #shell("docker run -d -p 4445:4444 -p 5900:5900 -v C:/Users/benjamin.garzon/Desktop/projects/ScrapeUN/docs:/home/seluser/Downloads selenium/standalone-firefox")
  Sys.sleep(5)
  pdfprof <- makeFirefoxProfile(
    list(
      "pdfjs.disabled" = TRUE,
      "plugin.scan.plid.all" = FALSE,
      "plugin.scan.Acrobat" = "99.0",
      "browser.helperApps.neverAsk.saveToDisk" = "application/pdf"
    )
  )
  
  driver <-
    remoteDriver(
      browserName = "firefox",
      port = 4445L,
      extraCapabilities = pdfprof
    )
  
  print("Checking links...")
  driver$open()
  driver$navigate(search_link)
  doc_links = NULL
  page = driver$getPageSource()[[1]]
  # Find links
  while (1) {
    new_links = read_html(page) %>% html_nodes("a") %>% html_attr("href") %>% str_subset("Download")
    if (new_links[1] %in% doc_links)
      break
    doc_links <- c(doc_links, new_links)
    
    tryCatch({nextPage <-
      driver$findElement(using = "xpath", "//input[@class='rgPageNext']")
    nextPage$clickElement()}, 
    error <- function(cond){
      break
    }
    )
    
    page = driver$getPageSource()[[1]]
    Sys.sleep(sleeptime)
    
  }
  
  print(paste("Found", length(doc_links), "links."))

  print("Downloading pdfs...")
  # Download pdfs
  i = 0
  for (doc_link in doc_links)  {
    driver$navigate(doc_link)
    tryCatch({
      english_pdf <-
        driver$findElement(using = "xpath", "//a[@title='English pdf']")
      english_pdf$clickElement()
      i = i + 1
    }, error = function(cond) {
      print(paste("No english pdf found", doc_link, sep = "\n"))
    })
    Sys.sleep(sleeptime)
  }
  shell("docker stop myselenium; docker rm myselenium")
  print(paste("Downloaded", i, "pdfs."))
  
}

convertpdf2txt <- function(dirpath) {
  files <- list.files(dirpath, full.names = T)
  x <- sapply(files, function(x) {
    x <- pdftools::pdf_text(x) %>%
      paste(sep = " ") %>%
      stringr::str_replace_all(fixed("\n"), " ") %>%
      stringr::str_replace_all(fixed("\r"), " ") %>%
      stringr::str_replace_all(fixed("\t"), " ") %>%
      stringr::str_replace_all(fixed("\""), " ") %>%
      paste(sep = " ", collapse = " ") %>%
      stringr::str_squish() %>%
      stringr::str_replace_all("- ", "")
    return(x)
  })
}

summarize_pdfs <-
  function(dir_path,
           summary_file = file.path(output_folder, "summaries.txt"),
           sentences = 3) {
    files <- list.files(dir_path, full.names = T)
    
    summaries <-
      sapply(files, function(x)
        genericSummary(x, k = sentences))
    names(summaries) = files
    fileConn <- file(summary_file)
    
    for (filename in files) {
      writeLines(c(filename, summaries[filename], "\newpage"), fileConn)
    }
    close(fileConn)
    
  }

select_pdfs <- function(words, output_folder, remove = T) {
  txts <- convertpdf2txt(docs_folder)
  wordlist = sapply(str_split(words, ","), str_trim)
  
  dir.create(file.path(docs_folder, output_folder))
  for (txtname in names(txts)) {
    txt = txts[txtname]
    wordvec <-
      unname(unlist(sapply(txt, function(z)
        str_split(tolower(z), " "))))
    wordvec = wordvec[wordvec %in% tolower(wordlist)]
    
    mytable = table(wordvec)
    if (sum(mytable) > 0) {
      if (remove)
        file.move(txtname, file.path("docs", output_folder))
      else
        file.copy(txtname, file.path("docs", output_folder))
    } else
      unlink(txtname)
  }
}
