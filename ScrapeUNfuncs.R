#install.packages(c("Rcpp","rvest", "stringr", "RSelenium", "pdftools", "dplyr", "LSAfun"))

docs_folder = "docs"

library(openxlsx)
library(pdftools)
library(rvest)
library(stringr)
library(RSelenium)
library(LSAfun)

download_pdfs <- function(mylink, sleeptime = 0.5) {
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
  driver$navigate(mylink)
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
    error = function(cond){
      print("Element not found")
    }
    )
    
    page = driver$getPageSource()[[1]]
    print("Reading new page...")
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
      print(paste("No english pdf found: ", doc_link))
    })
    Sys.sleep(sleeptime)
  }
  shell("docker stop myselenium")
  shell("docker rm myselenium")
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
clean_txt <- function(x){
#  x = gsub("\\.{2}", ".", x)
#  x = gsub("-{2}", "-", x)
  return(x)
  
}
summarize_pdfs <-
  function(dir_path, summary_file = file.path(output_folder, "summaries.txt"),
           sentences = 3) {
    
    txts <- convertpdf2txt(file.path(docs_folder, dir_path))
    browser()
    summaries <-
      sapply(txts, function(x)
        genericSummary(clean_txt(x), k = sentences))
    browser()
    names(summaries) = names(txts)
    fileConn <- file(summary_file)
    
    for (filename in names(txts)) {
      writeLines(c(filename, summaries[filename],"\n", strrep("-", 100), "\n\n"), fileConn)
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
    if (remove) {
      if (sum(mytable) > 0) 
        file.move(txtname, file.path("docs", output_folder))
      else
        unlink(txtname)
    } else {
      if (sum(mytable) > 0) 
        file.copy(txtname, file.path("docs", output_folder))
    }
  }
}
