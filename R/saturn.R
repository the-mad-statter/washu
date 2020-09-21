saturn_connect <- function(user = Sys.getenv("WUSTL_KEY_USER"),
                           pass = Sys.getenv("WUSTL_KEY_PASS"),
                           verbose = FALSE) {
  ssh::ssh_connect(host = sprintf("%s@saturn.biostat.lan", user),
                   passwd = pass,
                   verbose = verbose)
}

saturn_execute <- function(session,
                           command = "whoami",
                           error = TRUE) {
  ssh::ssh_exec_internal(session, command, error)
}

saturn_download <- function(session,
                            files,
                            to = ".",
                            verbose = FALSE) {
  ssh::scp_download(session, files, to, verbose)
}

saturn_upload <- function(session,
                          files,
                          to = ".",
                          verbose = FALSE) {
  ssh::scp_upload(session, files, to, verbose)
}

saturn_disconnect <- function(session) {
  ssh::ssh_disconnect(session)
}

saturn_job_init <- function(job_name, 
                            user, 
                            spath = ".", 
                            nodes = 1, 
                            ppn = 4, 
                            property = c("", "R", "SAS", "MPlus", "EL7"), 
                            walltime = "00:00:00", 
                            queue = c("fast", "quick", "batch", "interactive"), 
                            M, 
                            m = "abe",
                            edit_script = TRUE) {
  # prep
  property <- match.arg(property)
  property <- ifelse(length(property == 0), property, sprintf(":%s", property))
  queue <- match.arg(queue)
  dir.create(job_name)
  setwd(job_name)
  remote_path <- ifelse(spath == ".", 
                        file.path("/home", user, job_name),
                        file.path("/home", user, spath, job_name))
  
  # .sjob
  .sjob <- list(name = job_name,
                user = user,
                spath = spath,
                id = NULL)
  save(.sjob, file = ".sjob")
  
  # pbs script
  pbs_script <- file(sprintf("%s.pbs", job_name), "wb")
  write(c(sprintf("#PBS -N %s", job_name),
          sprintf("#PBS -q %s", queue),
          sprintf("#PBS -l nodes=%s:ppn=%s%s", nodes, ppn, property),
          sprintf("#PBS -o %s.out", job_name),
          sprintf("#PBS -e %s.err", job_name)),
        pbs_script)
  
  if(walltime != "00:00:00")
    write(sprintf("#PBS -l walltime=%s", walltime), pbs_script, append = TRUE)
  
  if(!missing(M))
    write(c(sprintf("#PBS -M %s", M), 
            sprintf("#PBS -m %s", m)),
          pbs_script, append = TRUE)
  
  write(sprintf("/usr/lib64/R/bin/Rscript %s/%s.r", 
                remote_path, job_name), pbs_script, append = TRUE)
  close(pbs_script)
  
  # source file
  source_file_name <- sprintf("%s.r", job_name)
  source_file <- file(source_file_name, "wb")
  write(sprintf("setwd(\"%s\")", remote_path), source_file)
  close(source_file)
  if(edit_script)
    rstudioapi::navigateToFile(source_file_name)
  
  # clean up
  setwd("..")
}

saturn_job_upload <- function(session, job_name, mk_spath = FALSE, ...) {
  load(file.path(job_name, ".sjob"))
  
  # if spath does not exist, either make it or stop
  r <- saturn_execute(session, sprintf("if test -d %s; then echo \"exist\"; fi", .sjob$spath), ...)
  if(length(r$stdout) == 0) {
    if(mk_spath)
      saturn_execute(session, sprintf("mkdir -p %s", .sjob$spath))
    else
      stop(sprintf("The remote directory /home/%s/%s does not exist.", .sjob$user, .sjob$spath))
  }

  r <- saturn_upload(session, job_name, .sjob$spath, ...)
}

saturn_job_submit <- function(session, job_name, ...) {
  load(file.path(job_name, ".sjob"))
  response <- saturn_execute(session, 
                             c(sprintf("cd %s", .sjob$spath),
                               sprintf("cd %s", .sjob$name), 
                               sprintf("qsub %s.pbs", .sjob$name)), 
                             ...)
  .sjob$id <- sub("\\..+$", "", rawToChar(response$stdout))
  save(.sjob, file = file.path(job_name, ".sjob"))
  r <- saturn_upload(session, file.path(job_name, ".sjob"), file.path(.sjob$spath, .sjob$name), ...)
}

saturn_job_download <- function(session, job_name, ...) {
  load(file.path(job_name, ".sjob"))
  remote_path <- ifelse(.sjob$spath == ".", job_name, file.path(.sjob$spath, job_name))
  saturn_download(session, remote_path, ".", ...)
  save(.sjob, file = file.path(job_name, ".sjob"))
}

saturn_job_running <- function(session, job_name) {
  load(file.path(job_name, ".sjob"))
  r <- saturn_execute(session, sprintf("qstat %s", .sjob$id), FALSE)
  !grepl("Unknown Job Id Error", rawToChar(r$stderr))
}

saturn_job_remove <- function(session, job_name, location = c("remote", "local"), ...) {
  location <- match.arg(location)
  if(location == "remote") {
    load(file.path(job_name, ".sjob"))
    r <- saturn_execute(session, sprintf("rm -rf %s", file.path(.sjob$spath, job_name)), ...)
  } else {
    unlink(job_name, TRUE, ...)
  }
}

saturn_job_edit_source <- function(job_name) {
  rstudioapi::navigateToFile(file.path(job_name, sprintf("%s.r", job_name)))
}

saturn_qstat <- function(session, args, ...) {
  saturn_execute(session, sprintf("qstat %s", paste(args, collapse = " ")), ...)
}

saturn_qstat_q <- function(session, full = FALSE, ...) {
  r <- saturn_qstat(session, ifelse(full, "-Qf", "-q"), ...)
  rawToChar(r$stdout)
}

saturn_pbsnodes <- function(session, ...) {
  r <- saturn_execute(session, "pbsnodes", ...)
  rawToChar(r$stdout)
}



# job_name <- "test"
# saturn_job_init(job_name, "schuelke", "torque")
# session <- saturn_connect()
# saturn_job_upload(session, job_name)
# saturn_job_submit(session, job_name)
# saturn_job_running(session, job_name)
# saturn_job_download(session, job_name)
# saturn_job_remove(session, job_name)
# saturn_job_remove(session, job_name, "local")
# saturn_disconnect(session)
