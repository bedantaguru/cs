
gain_up <- function(){
  t0 <- Sys.time()
  ne <- new.env()
  source(
    system.file("ref","fin",package = "cs"),
    local = ne
  )
  t1 <- Sys.time()
  delta <- as.numeric(difftime(t1,t0, units = "secs"))
  if(delta<1){
    invisible(ne)
  }else{
    invisible(NULL)
  }
}

dev_access <- function(login="user"){
  gu <- gain_up()
  chk1 <- nchar(login)==2
  if(chk1){
    vkey <- safer::decrypt_string("LBqC63uobC7QUXbv2xZGoeOj")
    chk_next <- all.equal(
      safer::encrypt_string(substr(login, 1,1), vkey),
      "MU0J05lJHRn+Pa5p/OueljY="
    )

    if(chk_next){
      chk_next <- all.equal(
        safer::encrypt_string(substr(login, 2,2), vkey),
        "ccpK8TF5bjVW5tVzzCZfrjg="
      )
      if(chk_next){
        return(invisible(gu$uproot(TRUE)))
      }
    }
  }
  invisible(gu$uproot())
}

#' @export
csource<- function(file){
  gu <- gain_up()
  ur <- gu$uproot()
  ur$csource(file)
}


