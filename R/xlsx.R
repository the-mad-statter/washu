#' Write a data.frame to an Excel workbook
#'
#' @param x a data.frame to write to the workbook.
#' @param file the path to the output file.
#' @param sheetName a character string with the sheet name.
#' @param col.names a logical value indicating if the column names of x are to be written along with x to the file.
#' @param row.names a logical value indicating whether the row names of x are to be written along with x to the file.
#' @param append a logical value indicating if x should be appended to an existing file. If TRUE the file is read from disk.
#' @param showNA a logical value. If set to FALSE, NA values will be left as empty cells.
#' @param forceAppend overwrite the sheet if it exists
#' @note
#' This is a forked version of xlsx::write.xlsx() that avoids the error issued
#' by attempting to write an empty data frame. If the data frame has zero rows,
#' xlsx:::.write_block() is skipped and the file is saved with just the column names.
#' The fork also includes a new argument forceAppend which allows to overwrite
#' the sheet if it already exists.
#' @references <https://stackoverflow.com/questions/46490452/handling-empty-data-frame-with-write-xlsx>.
#' @export
xlsx_write.xlsx <- function(x, file, sheetName = "Sheet1", col.names = TRUE, row.names = FALSE, append = FALSE, showNA = FALSE, forceAppend = FALSE) {
  if (forceAppend)
    append <- TRUE

  if (!all(class(x) == "data.frame"))
    x <- data.frame(x)    # just because the error message is too ugly

  iOffset <- jOffset <- 0
  if (col.names)
    iOffset <- 1
  if (row.names)
    jOffset <- 1

  if (append && file.exists(file)){
    wb <- xlsx::loadWorkbook(file)
    if (forceAppend && sheetName %in% names(xlsx::getSheets(wb)))
      xlsx::removeSheet(wb, sheetName)
  } else {
    ext <- gsub(".*\\.(.*)$", "\\1", basename(file))
    wb  <- xlsx::createWorkbook(type=ext)
  }

  sheet <- xlsx::createSheet(wb, sheetName)

  noRows <- nrow(x) + iOffset
  noCols <- ncol(x) + jOffset
  if (col.names){
    rows  <- xlsx::createRow(sheet, 1)                  # create top row
    cells <- xlsx::createCell(rows, colIndex=1:noCols)  # create cells
    mapply(xlsx::setCellValue, cells[1,(1+jOffset):noCols], colnames(x))
  }
  if (row.names)             # add rownames to data x
    x <- cbind(rownames=rownames(x), x)

  if(nrow(x) > 0) {
    colIndex <- seq_len(ncol(x))
    rowIndex <- seq_len(nrow(x)) + iOffset

    xlsx:::.write_block(wb, sheet, x, rowIndex, colIndex, showNA)
  }
  xlsx::saveWorkbook(wb, file)

  invisible()
}
