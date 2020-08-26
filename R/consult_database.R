# db_write(data.frame(a = 1), "user", db)
db_write <- function(x, sheetName, db) {
  tbl_user <- xlsx::read.xlsx(db, sheetName = "user")
  tbl_consult <- xlsx::read.xlsx(db, sheetName = "consult")
  tbl_user_consult <- xlsx::read.xlsx(db, sheetName = "user_consult")
  switch(sheetName,
         "user" =         x -> tbl_user,
         "consult" =      x -> tbl_consult,
         "user_consult" = x -> tbl_user_consult)
  xlsx::write.xlsx(tbl_user,         db, "user",         row.names = FALSE,                showNA = FALSE)
  xlsx::write.xlsx(tbl_consult,      db, "consult",      row.names = FALSE, append = TRUE, showNA = FALSE)
  xlsx::write.xlsx(tbl_user_consult, db, "user_consult", row.names = FALSE, append = TRUE, showNA = FALSE)
}

# returns numeric(0) if not found
#db_get_user_id("schuelke@wustl.edu", db)
db_get_user_id <- function(email, db) {
  tbl_user <- xlsx::read.xlsx(db, sheetName = "user")
  row <- which(tbl_user$mail == email)
  tbl_user$user_id[row]
}

#db_max_user_id(db)
db_max_user_id <- function(db) {
  tbl_user <- readxl::read_xlsx(db, sheet = "user")
  row <- which.max(tbl_user$user_id)
  tbl_user$user_id[row]
}

#db_add_user("schuelke@wustl.edu", db)
db_add_user <- function(email, db) {
  q <- washu::wu_ldap_query("mail", email)
  v <- c("user_id",
         "cn",
         "sn",
         "title",
         "physicalDeliveryOfficeName",
         "telephoneNumber",
         "givenName",
         "displayName",
         "department",
         "streetAddress",
         "personalTitle",
         "name",
         "sAMAccountName",
         "userPrincipalName",
         "mail",
         "eduPersonNickname",
         "eduPersonPrimaryAffiliation",
         "app_role")
  for(m in setdiff(v, names(q))) {
    q[m] <- switch(m,
                   "user_id" = db_max_user_id(db) + 1,
                   "app_role" = "user",
                   NA)
  }
  q <- q[v]

  tbl_user <- rbind(xlsx::read.xlsx(db, sheetName = "user"), q)
  db_write(tbl_user, "user", db)
}

# db_add_consult("new consult id", title = "a new title", db = db)
db_add_consult <- function(consult_id,
                           due = "",
                           referal = "",
                           berd_record_id = "",
                           berd_study_type = "",
                           title = "",
                           award_pre_work = "",
                           award_post_work = "",
                           publication_work = "",
                           award_funding_agency = "",
                           award_funding_vehicle = "",
                           publication_outlet = "",
                           award_funded = "",
                           award_total = "",
                           publication_published = "",
                           db) {
  tbl_consult <-
    rbind(xlsx::read.xlsx(db, sheetName = "consult"),
          data.frame(consult_id = consult_id,
                     due = due,
                     referal = referal,
                     berd_record_id = berd_record_id,
                     berd_study_type = berd_study_type,
                     title = title,
                     award_pre_work = award_pre_work,
                     award_post_work = award_post_work,
                     publication_work = publication_work,
                     award_funding_agency = award_funding_agency,
                     award_funding_vehicle = award_funding_vehicle,
                     publication_outlet = publication_outlet,
                     award_funded = award_funded,
                     award_total = award_total,
                     publication_published = publication_published))
  db_write(tbl_consult, "consult", db)
}

#db_add_user_consult("new_cid", 1000, "superstar", db)
db_add_user_consult <- function(consult_id, user_id, role, db) {
  tbl_user_consult <-
    rbind(xlsx::read.xlsx(db, sheetName = "user_consult"),
          data.frame(consult_id = consult_id,
                     user_id = user_id,
                     role = role))
  db_write(tbl_user_consult, "user_consult", db)
}
