#' WU VPN Connect
#'
#' Connect to the WUSTL VPN using the Cisco AnyConnect VPN CLI client.
#'
#' @param user wustl key username
#' @param pass wustl key password
#' @param vpn_cli path to Cisco AnyConnect VPNCLI.exe
#' @param vpn_endpoint vpn endpoint
#'
#' @export
wu_vpn_connect <- function(
  user = Sys.getenv('WUSTL_KEY_USER'),
  pass = Sys.getenv('WUSTL_KEY_PASS'),
  vpn_cli = Sys.getenv('WU_VPN_CLI_PATH'),
  vpn_endpoint = Sys.getenv('WU_VPN_ENDPOINT')
) {
  system2(
    command = vpn_cli,
    args = sprintf('-s connect %s', vpn_endpoint),
    input = c(user, pass, 'push', 'y')
  )
}

#' WU VPN Disconnect
#'
#' Disconnect from the WUSTL VPN using the Cisco AnyConnect VPN CLI client.
#'
#' @inheritParams wu_vpn_connect
#'
#' @export
wu_vpn_disconnect <- function(
  vpn_cli = Sys.getenv('WU_VPN_CLI_PATH')
) {
  system2(
    command = vpn_cli,
    args = '-s disconnect',
    input = 'quit'
  )
}

#' WU VPN Status
#'
#' Check the status of the WUSTL VPN connection using the Cisco AnyConnect VPN CLI client.
#'
#' @inheritParams wu_vpn_connect
#'
#' @export
wu_vpn_status <- function(
  vpn_cli = Sys.getenv('WU_VPN_CLI_PATH')
) {
  system2(
    command = vpn_cli,
    args = '-s',
    input = 'quit'
  )
}
