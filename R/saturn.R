#' Saturn Connect
#'
#' @param user saturn username as string
#' @inheritParams ssh::ssh_connect
#'
#' @return ssh session
#' @export
#'
#' @examples
#' \dontrun{
#' # retrive user and pass from .Renviron file in user home directory
#' session <- saturn_connect()
#' }
#'
#' @seealso \code{\link[ssh]{ssh}}
saturn_connect <- function(user = Sys.getenv("WUSTL_KEY_USER"),
                           passwd = Sys.getenv("WUSTL_KEY_PASS"),
                           verbose = FALSE) {
  ssh::ssh_connect(host = sprintf("%s@saturn.biostat.lan", user),
                   passwd = passwd,
                   verbose = verbose)
}

#' Saturn Execute
#'
#' @param session ssh connection created with \code{\link{saturn_connect}}
#' @inheritParams ssh::ssh_exec_internal
#'
#' @return list containing exit status, buffered raw stdout, and buffered raw stderr
#' @export
#'
#' @examples
#' \dontrun{
#' session <- saturn_connect()
#' out <- saturn_execute(session)
#' rawToChar(out$stdout)
#' }
#'
#' @seealso \code{\link[ssh]{ssh_exec}}
saturn_execute <- function(session,
                           command = "whoami",
                           error = TRUE) {
  ssh::ssh_exec_internal(session, command, error)
}

#' Saturn Download
#'
#' @param session ssh connection created with \code{\link{saturn_connect}}
#' @inheritParams ssh::scp_download
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # recursively download files and directories
#' session <- saturn_connect()
#' saturn_download(session, "~/target/*", tempdir())
#' }
#'
#' @seealso \code{\link[ssh]{scp}}
saturn_download <- function(session,
                            files,
                            to = ".",
                            verbose = FALSE) {
  ssh::scp_download(session, files, to, verbose)
}

#' Saturn Upload
#'
#' @param session ssh connection created with \code{\link{saturn_connect}}
#' @inheritParams ssh::scp_upload
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # recursively upload files and directories
#' session <- saturn_connect()
#' files <- c(R.home("doc"), R.home("COPYING"))
#' saturn_upload(session, files, "~/target")
#' }
#'
#' @seealso \code{\link[ssh]{scp}}
saturn_upload <- function(session,
                          files,
                          to = ".",
                          verbose = FALSE) {
  ssh::scp_upload(session, files, to, verbose)
}

#' Saturn Disconnect
#'
#' @param session ssh connection created with \code{\link{saturn_connect}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' session <- saturn_connect()
#' saturn_disconnect(session)
#' }
#'
#' @seealso \code{\link[ssh]{ssh}}
saturn_disconnect <- function(session) {
  ssh::ssh_disconnect(session)
}


#' Saturn Job Initialize
#'
#' @param job_name job name
#' @param spath directory on the destination where files will be copied into
#' @param nodes number of nodes
#' @param ppn processors per node
#' @param user user account
#' @param property requested node property
#' @param walltime total time requested for the job
#' @param queue queue name (note, this will be auto redirected depending on resource request)
#' @param M email address to send notifications
#' @param m email notifications: (a) abort, (b) begin, (e) end
#' @param edit_script open script for editing
#' @export
#'
#' @examples
#' \dontrun{
#' job_name <- "my_job"
#' saturn_job_init(job_name)
#' }
saturn_job_init <- function(job_name,
                            spath = ".",
                            nodes = 1,
                            ppn = 4,
                            user = Sys.getenv("WUSTL_KEY_USER"),
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

#' Saturn Job Upload
#'
#' @param session ssh connection created with \code{\link{saturn_connect}}
#' @param job_name job name
#' @param mk_spath make the spath if it does not exist
#' @param ... additional arguments passed to \code{\link{saturn_execute}} and \code{\link{saturn_upload}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' session <- saturn_connect()
#' job_name <- "my_job"
#' saturn_job_init(job_name, "pooh")
#' saturn_job_upload(session, job_name)
#' }
saturn_job_upload <- function(session, job_name, mk_spath = FALSE, ...) {
  .sjob <- NULL
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

#' Saturn Job Submit
#'
#' @param session ssh connection created with \code{\link{saturn_connect}}
#' @param job_name job name
#' @param ... additional arguments passed to \code{\link{saturn_execute}}, \code{\link{saturn_upload}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' session <- saturn_connect()
#' job_name <- "my_job"
#' saturn_job_init(job_name, "pooh")
#' saturn_job_upload(session, job_name)
#' saturn_job_submit(session, job_name)
#' }
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

#' Saturn Job Running
#'
#' @param session ssh connection created with \code{\link{saturn_connect}}
#' @param job_name job name
#'
#' @return logical indicating if job is running
#' @export
#'
#' @examples
#' \dontrun{
#' session <- saturn_connect()
#' job_name <- "my_job"
#' saturn_job_init(job_name, "pooh")
#' saturn_job_upload(session, job_name)
#' saturn_job_submit(session, job_name)
#' saturn_job_running(session, job_name)
#' }
saturn_job_running <- function(session, job_name) {
  .sjob <- NULL
  load(file.path(job_name, ".sjob"))
  r <- saturn_execute(session, sprintf("qstat %s", .sjob$id), FALSE)
  !grepl("Unknown Job Id Error", rawToChar(r$stderr))
}

#' Saturn Job Download
#'
#' @param session ssh connection created with \code{\link{saturn_connect}}
#' @param job_name job name
#' @param ... additional arguments passed to \code{\link{saturn_upload}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' session <- saturn_connect()
#' job_name <- "my_job"
#' saturn_job_init(job_name, "pooh")
#' saturn_job_upload(session, job_name)
#' saturn_job_submit(session, job_name)
#' saturn_job_running(session, job_name)
#' saturn_job_download(session, job_name)
#' }
saturn_job_download <- function(session, job_name, ...) {
  .sjob <- NULL
  load(file.path(job_name, ".sjob"))
  remote_path <- ifelse(.sjob$spath == ".", job_name, file.path(.sjob$spath, job_name))
  saturn_download(session, remote_path, ".", ...)
  save(.sjob, file = file.path(job_name, ".sjob"))
}

#' Saturn Job Remove
#'
#' @param session ssh connection created with \code{\link{saturn_connect}}
#' @param job_name job name
#' @param location remove the job directory on the cluster or locally
#' @param ... additional arguments passed to \code{\link{saturn_execute}}, \code{\link[base]{unlink}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' session <- saturn_connect()
#' job_name <- "my_job"
#' saturn_job_init(job_name, "pooh")
#' saturn_job_upload(session, job_name)
#' saturn_job_submit(session, job_name)
#' saturn_job_running(session, job_name)
#' saturn_job_download(session, job_name)
#' saturn_job_remove(session, job_name)
#' saturn_job_remove(session, job_name, "local")
#' }
saturn_job_remove <- function(session, job_name, location = c("remote", "local"), ...) {
  .sjob <- NULL
  location <- match.arg(location)
  if(location == "remote") {
    load(file.path(job_name, ".sjob"))
    r <- saturn_execute(session, sprintf("rm -rf %s", file.path(.sjob$spath, job_name)), ...)
  } else {
    unlink(job_name, TRUE, ...)
  }
}

#' Saturn Job Edit Source
#'
#' @param job_name job name
#'
#' @export
#'
#' @examples
#' \dontrun{
#' job_name <- "my_job"
#' saturn_job_init(job_name, "pooh", edit_script = FALSE)
#' saturn_job_edit_source(job_name)
#' }
saturn_job_edit_source <- function(job_name) {
  rstudioapi::navigateToFile(file.path(job_name, sprintf("%s.r", job_name)))
}

#' Saturn qstat Command
#'
#' @param session ssh connection created with \code{\link{saturn_connect}}
#' @param args arguments to pass to qstat
#' @param ... additional arguments passed to \code{\link{saturn_execute}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' session <- saturn_connect()
#' r <- saturn_qstat(session, "-a")
#' cat(rawToChar(r$stdout))
#' }
#'
#' @details \tabular{ll}{
#' \strong{Argument} \tab \strong{Details}           \cr
#' -q        \tab list all queues                    \cr
#' -a        \tab list all jobs                      \cr
#' -u userid \tab list jobs for userid               \cr
#' -r        \tab list running jobs                  \cr
#' -f job_id \tab list full information about job_id \cr
#' -Qf queue \tab list full information about queue  \cr
#' -B        \tab list summary status of the job server
#' }
saturn_qstat <- function(session, args, ...) {
  saturn_execute(session, sprintf("qstat %s", paste(args, collapse = " ")), ...)
}

#' Saturn pbsnodes
#'
#' @param session ssh connection created with \code{\link{saturn_connect}}
#' @param ... additional arguments passed to \code{\link{saturn_qstat}}
#'
#' @return status of all compute nodes
#' @export
#'
#' @examples
#' \dontrun{
#' session <- saturn_connect()
#' r <- saturn_pbsnodes(session)
#' cat(rawToChar(r$stdout))
#' }
saturn_pbsnodes <- function(session, ...) {
  r <- saturn_execute(session, "pbsnodes", ...)
  rawToChar(r$stdout)
}
