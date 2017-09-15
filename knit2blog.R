knit2blog <- function(rmd) {
    outpath <- file.path('~/Dropbox/ajrominger.github.io/_posts', gsub('.*/', '', rmd))
    outpath <- gsub('.Rmd', '.md', outpath)
    temp <- tempfile()
    
    rmarkdown::render(rmd, output_file = temp)
    
    yamlHead <- readLines(rmd)
    yamlHead <- yamlHead[2:(which(yamlHead == '---')[2] - 1)]
    yamlHead <- yamlHead[1:(which(grepl('output: ', yamlHead)) - 1)]
    yamlHead[grepl('date: ', yamlHead)] <- sprintf('date: "%s"', 
                                                   format(Sys.time(), '%d %B %Y'))
    yamlHead <- c('---', 
                  'layout: post',
                  yamlHead, 
                  '---')
    
    writeLines(c(yamlHead, '', readLines(temp)), outpath)
}
