#install.packages(c("Rcpp","rvest", "stringr", "RSelenium", "pdftools", "dplyr", "LSAfun", "ggplot2", 
#"tm", "lda", "ldatuning", "topicmodels"))

docs_folder = "docs"

library(openxlsx)
library(pdftools)
library(rvest)
library(stringr)
library(RSelenium)
library(LSAfun)
library(dplyr)
library(reshape2)
library(ggplot2)
library(lexRankr)
library(lda)
library(ldatuning)
library(topicmodels)

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
    
    tryCatch({
      nextPage <-
        driver$findElement(using = "xpath", "//input[@class='rgPageNext']")
      nextPage$clickElement()
    },
    error = function(cond) {
      print("Element not found")
    })
    
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
      print(paste("No english pdf found:", doc_link))
    })
    Sys.sleep(sleeptime)
  }
  shell("docker stop myselenium")
  shell("docker rm myselenium")
  print(paste("Downloaded", i, "pdfs."))
  
}

convertpdf2txt <- function(dirpath) {
  files <- list.files(dirpath, full.names = T, pattern = "\\.pdf$")
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
clean_txt <- function(x) {
  x = gsub("\\.+", ".", x)
  x = gsub("\\. (?=\\d+)", " ", x, perl=T)
  
  return(x)
  
}

summarize_txt = function(txt, k=5) {

  top = lexRank(txt, n = k)
  order_of_appearance = order(as.integer(gsub("_", "", top$sentenceId)))
  ordered_top = top[order_of_appearance, "sentence"]
  return(paste(ordered_top))
}

summarize_pdfs <-
  function(dir_path,
           summary_file = "Summaries.txt",
           sentences = 3) {
    txts <- convertpdf2txt(file.path(docs_folder, dir_path))
    summaries <-
      sapply(txts, function(x)
        summarize_txt(clean_txt(x), k = sentences))

    names(summaries) = sapply(names(txts), basename)
    fileConn <- file(file.path(docs_folder, dir_path, summary_file), 'a')
    
    mytable = read.table(file = file.path(docs_folder, dir_path, "Word_counts.csv"),
      sep = ",",
      header = T
    )
    #browser()
    for (filename in lapply(mytable$Document, basename)) {
      writeLines(paste0(filename, ':\n'), fileConn)
      writeLines(summaries[[filename]], fileConn)
      writeLines(strrep("-", 120), fileConn)
      }
    close(fileConn)
    return(txts)
  }

select_pdfs <- function(words, output_folder, remove = F) {
  wordlist = sapply(str_split(words, ","), str_trim)
  
  mytables = NULL
  txts <- convertpdf2txt(docs_folder)
  output_path = file.path(docs_folder, output_folder)
  if (file.exists(output_path))
    unlink(output_path, recursive = TRUE)
  dir.create(output_path)
  for (txtname in names(txts)) {
    txt = txts[txtname]
    wordvec <-
      unname(unlist(sapply(txt, function(z)
        str_split(tolower(z), " "))))
    wordvec = wordvec[wordvec %in% tolower(wordlist)]
    
    mytable = table(factor(wordvec, levels = wordlist))
    mytables = rbind(mytables, c(txtname, mytable))
    
    if (remove) {
      if (sum(mytable) > 0)
        file.move(txtname, output_path)
      else
        unlink(txtname)
    } else {
      if (sum(mytable) > 0)
        file.copy(txtname, output_path)
    }
  }
  colnames(mytables)[1] = "Document"
  df = as.data.frame(mytables)
  df[,-1] = apply(df[,-1], c(1, 2), as.numeric)
  df$Total = rowSums(df[,-1])
  df = df %>% arrange(desc(Total)) %>% filter(Total > 0)
  #View(df)
  df.melt = melt(df, id.vars = "Document", variable.name = "Word")
  myplot = ggplot(df.melt, aes(value, col = Word)) + geom_freqpoly() + ggtitle(output_folder)
  print(myplot)
  ggsave(file.path(output_path, "Word_counts.png"), width = 10, height = 10)
  write.table(
    df,
    file = file.path(output_path, "Word_counts.csv"),
    row.names = F,
    col.names = T,
    sep = ","
  )
  print(paste("Finished selecting documents for:", words))

}
