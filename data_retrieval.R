require(kaiaulu)
require(data.table)
require(jsonlite)
require(knitr)
require(magrittr)
require(gt)
require(readxl)
require(JirAgileR)
source('Kaiaulu_edits.R')

extract_timezone = function(data){
  if (length(data) >5){
    return(data[[6]])
  } else {
    return(data[[5]])
  }
}

base_dir = '/home/wouter'
delay = 5

token = scan("~/Documents/github pat.txt",what="character",quiet=TRUE)

information = read_xlsx(paste0(base_dir, 'info.xlsx'))

row.names(information) =  information$Name

perceval_path = '~/.local/share/pipx/venvs/perceval/bin/perceval'

#project_git <- parse_gitlog('~/.local/share/pipx/venvs/perceval/bin/perceval','~/gits/geronimo_gits/geronimo/.git')
#project_git2 = parse_gitlog('~/.local/share/pipx/venvs/perceval/bin/perceval','~/gits/geronimo_gits/geronimo-health/.git')

#project_git3 = rbind(project_git, project_git2)

results = c()
for (file in list.files('~/gits/geronimo_gits')){
  print(paste0('Parsing ',file, '...'))
  temp_proj = parse_gitlog('~/.local/share/pipx/venvs/perceval/bin/perceval',paste0('~/gits/geronimo_gits/',file,'/.git'))
  results = rbind(results, temp_proj)
  print(paste0(file, ' Completed!'))
}


results$author_tz <- sapply(stringi::stri_split(results$author_datetimetz,
                                                    regex=" "),"[[",6)
results$author_datetimetz <- as.POSIXct(results$author_datetimetz,
                                            format = "%a %b %d %H:%M:%S %Y %z", tz = "UTC")


results$committer_tz <- sapply(stringi::stri_split(results$committer_datetimetz,
                                                       regex=" "),"[[",6)
results$committer_datetimetz <- as.POSIXct(results$committer_datetimetz,
                                               format = "%a %b %d %H:%M:%S %Y %z", tz = "UTC")

fwrite(results, '~/gits/test/data/geronimo_githubs.csv')
save_jira_credentials(domain = 'https://issues.apache.org/jira')

mbox_paths <- c('geronimo-dev', 'geronimo-xbean-dev')
jira_paths <- c('GERONIMO', 'GERONIMODEVTOOLS', 'XBEAN')
github_paths <- list.files('~/gits/geronimo_gits')
delay = 10

mbox_result=c()
github_replies_result = c()
jira_result = c()

if(!is.null(mbox_paths)){
  for (mailing_list in mbox_paths){
    
    if (!dir.exists(paste0('~/mbox/geronimo/', mailing_list, '/'))){
      dir.create(paste0('~/mbox/geronimo/', mailing_list, '/'))
      
      print(paste0('Obtaining ',mailing_list,' Mail data...'))
    
    
    mbox = download_mod_mbox_per_month(base_url = 'http://mail-archives.apache.org/mod_mbox',
                             mailing_list = mailing_list,
                             save_folder_path = paste0('~/mbox/geronimo/', mailing_list, '/'),
                             from_year = 2003,
                             to_year = 2007, verbose = T)
    }
    
    print(paste0('Saving ',mailing_list,' Mail data...'))
    project_mbox <- parse_mbox(perceval_path,paste0('~/mbox/geronimo/', mailing_list, '/'))  
    print('0')
    project_mbox$reply_tz <- sapply(stringi::stri_split(project_mbox$reply_datetimetz,
                                                        regex=" "),extract_timezone)
    print('1')
    project_mbox$reply_datetimetz <- as.POSIXct(project_mbox$reply_datetimetz,
                                                format = "%a, %d %b %Y %H:%M:%S %z", tz = "UTC")
    print('2')
    mbox_result = rbind(mbox_result, project_mbox)
    
  }

}
fwrite(mbox_result, '~/gits/test/data/geronimo_mbox.csv')
if(!is.null(jira_paths)){
  
  for (jira in jira_paths){
    
    if (!dir.exists(paste0('~/jira_issues/geronimo/', jira, '/'))){
      dir.create(paste0('~/jira_issues/geronimo/', jira, '/'))
      print(paste0('Obtaining ',jira,' Jira data...'))
      json_issues = get_jira_issues(jql_query = paste0("project='",jira,"'"),
                                    fields = c('summary',
                                               'description',
                                               'creator',
                                               'assignee',
                                               'reporter',
                                               'issuetype',
                                               'status',
                                               'resolution',
                                               'components',
                                               'created',
                                               'updated',
                                               'resolutiondate',
                                               'comment'),
                                    verbose = T,
                                    as.data.frame = F)
      jsonlite::write_json(json_issues, paste0('~/jira_issues/geronimo/', jira, '/issues.json'))
    }
      
  print(paste0('Saving ',jira,' Jira data...'))
  project_jira <- parse_jira_replies(parse_jira(paste0('~/jira_issues/geronimo/', jira, '/'))) 
  
  # Timezone is embedded on separated field. All times shown in UTC.
  project_jira$reply_tz <- "0000"
  
  project_jira$reply_datetimetz <- as.POSIXct(project_jira$reply_datetimetz,
                                              format = "%Y-%m-%dT%H:%M:%S.000+0000", tz = "UTC")
  jira_result=rbind(jira_result, project_jira)
  Sys.sleep(delay)
  }
}
fwrite(jira_result, '~/gits/test/data/geronimo_jira.csv')

owner = 'apache'
if(!is.null(github_paths)){
  for (github in github_paths){
    repo=github
    
    if (!dir.exists(paste0('~/git_communication/geronimo/', github, '/'))){
      dir.create(paste0('~/git_communication/geronimo/', github, '/'))
      print(paste0('Obtaining ',github,' Issue data...'))
      
      dir.create(paste0('~/git_communication/geronimo/', github,'/issue/'))
      github_api_iterate_pages(token, 
                               github_api_project_issue('apache', github, token),
                               paste0('~/git_communication/geronimo/', github,'/issue/'),
                               prefix='issue')
      print('first')
      Sys.sleep(5)
      print(paste0('Obtaining ',github,' PR data...'))
      dir.create(paste0('~/git_communication/geronimo/', github,'/pull_request/'))
      github_api_iterate_pages(token, 
                               github_api_project_pull_request('apache', github, token),
                               paste0('~/git_communication/geronimo/', github,'/pull_request/'),
                               prefix='pull_request')
      print('3')
      Sys.sleep(5)
      print(paste0('Obtaining ',github,' comment data...'))
      dir.create(paste0('~/git_communication/geronimo/', github,'/issue_or_pr_comment/'))
      github_api_iterate_pages(token, 
                               github_api_project_issue_or_pr_comments('apache', github, token),
                               paste0('~/git_communication/geronimo/', github,'/issue_or_pr_comment/'),
                               prefix='issue_or_pr_comment')
      Sys.sleep(5)
      print(paste0('Obtaining ',github,' commit data...'))
      dir.create(paste0('~/git_communication/geronimo/', github,'/commit/'))
      github_api_iterate_pages(token, 
                               github_api_project_commits('apache', github, token),
                               paste0('~/git_communication/geronimo/', github,'/commit/'),
                               prefix='commit')
    }
  print(paste0('Saving ',github,' GitHub data...'))
  project_github_replies <- parse_github_replies(paste0('~/git_communication/geronimo/', github))  
  
  
  # Timezone is not available on GitHub timestamp, all in UTC
  project_github_replies$reply_tz <- "0000"
  
  project_github_replies$reply_datetimetz <- as.POSIXct(project_github_replies$reply_datetimetz,
                                                        format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  
  Sys.sleep(delay)
  github_replies_result = rbind(github_replies_result, project_github_replies)
  }
}
github_replies_result$reply_body[lengths(github_replies_result$reply_body) == 0] = NA
github_replies_result$reply_body = unlist(github_replies_result$reply_body)
fwrite(github_replies_result, '~/gits/test/data/geronimo_github_replies.csv')
