
compileModel = function(cpp_file, shrd_lib_loc){
  if(!file.exists(cpp_file))
    stop("Cpp file does not exist")
  shrd_lib_name = gsub(".cpp", .Platform$dynlib.ext, basename(cpp_file))
  if(file.exists(sprintf("%s/%s", shrd_lib_loc, shrd_lib_name)))
    file.remove(sprintf("%s/%s", shrd_lib_loc, shrd_lib_name))
  cmd = paste0(R.home(component = "bin"), "/R")
  #' Need to include these directories  
  paths = sapply(c("Rcpp"), find.package)
  flag = paste(paste0("-I\"", paths, "/include\""), collapse = " ")
  #' Need to set additional compiler flags
#  do.call(Sys.setenv, inline::getPlugin("RcppArmadillo")$env)
  Sys.setenv(CLINK_CPPFLAGS = flag)
  Sys.setenv(PKG_CXXFLAGS="-std=c++14")
  #' Compile model
  
  system2(cmd, args = paste(" CMD SHLIB -o", sprintf("%s/%s", shrd_lib_loc, shrd_lib_name), cpp_file))
  if(!file.exists(sprintf("%s/%s", shrd_lib_loc, shrd_lib_name)))
    stop("Something went wrong")
  print(shrd_lib_name)
  #' Remove object file
  if(file.exists(gsub(".cpp", ".o", cpp_file)))
    file.remove(gsub(".cpp", ".o", cpp_file))
}



