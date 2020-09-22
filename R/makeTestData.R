#' @importFrom stats rbinom
#' @importFrom stats rnorm
#' @importFrom stats lm
#' @importFrom utils write.table

makeTestData = function (n = 2000, p = 4) {

  x1 = rnorm(n, 10, 2)
  x2 = rnorm(n, 5, 3)
  x3 = rnorm(n, -2, 1)
  x4 = rnorm(n, 0, 5)
  
  y = 2.4 - 0.6 * x1 + 5.5 * x2 - 7.2 * x3 + 5.7 * x4 + rnorm(n)

  dat = cbind(x1, x2, x3, x4, y)

  rn = character(n)
  for (i in 1:n)
    rn[i] = paste(LETTERS[sample(1:26, 2)], collapse = '')

  setheader = c(TRUE, FALSE)
  setsep = c('\t', '_', '.', ',', ';', ' ')
  setdec = c('.', ',')
  setrownames = c(TRUE, FALSE)
  setskip = 0
  setnastring = c('NA')

  for (sh in setheader)
    for (ssep in setsep)
      for (sdec in setdec)
        for (srn in setrownames)
          for (sskip in setskip)
            for (sna in setnastring)
            {
              if (ssep == sdec) next
              dathelp = dat
              if (sh) colnames(dathelp) = c(LETTERS[1:4], 'y')
              if (srn) rownames(dathelp) = rn
              psep = ifelse(ssep == '\t', 't', ssep)
              filehelp = tempfile(pattern = paste0('dat^header', sh, '^sep', psep, '^dec', sdec, '^rownames', srn, '^skip', sskip, '^nastring', sna))
              if (sskip == 0)
                write.table(dathelp, file = paste0(filehelp, ".txt"), sep = ssep, dec = sdec,
                            col.names = sh, row.names = srn)
            }

}

