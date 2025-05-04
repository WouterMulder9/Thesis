require(kaiaulu)
require(data.table)
require(jsonlite)
require(knitr)
require(magrittr)
require(gt)
require(readxl)
require(JirAgileR)
require(stringi)
require(visNetwork)
source('Kaiaulu_edits.R')

seed = 02052025

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


github_replies_result = read.csv('~/gits/test/data/geronimo_github_replies.csv')
jira_result = read.csv('~/gits/test/data/geronimo_jira.csv')
mbox_result = read.csv('~/gits/test/data/geronimo_mbox.csv')
project_reply = rbind(github_replies_result, jira_result, mbox_result)
project_reply$reply_datetimetz <- as.POSIXct(project_reply$reply_datetimetz,
                                             format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

results = read.csv('~/gits/test/data/geronimo_githubs.csv')
results$author_datetimetz <- as.POSIXct(results$author_datetimetz,
                                                    format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
results$committer_datetimetz <- as.POSIXct(results$committer_datetimetz,
                                        format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")



project_reply= project_reply[project_reply$reply_datetimetz >as.POSIXct('2002-12-31') & project_reply$reply_datetimetz<as.POSIXct('2008-01-01'),]
results = results[results$author_datetimetz>as.POSIXct('2002-12-31') & results$author_datetimetz<as.POSIXct('2008-01-01'),]

window_size = 30

project_log = list(project_git = results, project_reply = project_reply)
project_log = identity_match(project_log, name_column = c('author_name_email','reply_from'),
                                         assign_exact_identity,
                                         label='raw_name')

project_git = project_log$project_git
project_reply = project_log$project_reply[!grepl('(JIRA)', project_log$project_reply$reply_from),]

project_git = as.data.table(project_git)
project_reply = as.data.table(project_reply)

# Define all timestamp in number of days since the very first commit of the repo 
# Note here the start_date and end_date are in respect to the git log.

# Transform commit hashes into datetime so window_size can be used
start_date <- project_reply[15954,"reply_datetimetz"][[1]]
end_date <- project_reply[13627,"reply_datetimetz"][[1]]
datetimes <- project_git$author_datetimetz
reply_datetimes <- project_reply$reply_datetimetz

# Format time window for posixT
window_size_f <- stringi::stri_c(window_size," day")

# Note if end_date is not (and will likely not be) a multiple of window_size, 
# then the ending incomplete window is discarded so the metrics are not calculated 
# in a smaller interval
time_window <- seq.POSIXt(from=start_date,to=end_date,by=window_size_f)


oslom_dir_path = '~/Downloads/OSLOM2/OSLOM2/oslom_dir'
oslom_undir_path = '~/Downloads/OSLOM2/OSLOM2/oslom_undir'
# Create a list where each element is the social smells calculated for a given commit hash
smells <- list()
size_time_window <- length(time_window)
for(j in 2:size_time_window){
  
  # Initialize
  commit_interval <- NA
  start_day <- NA
  end_day <- NA
  org_silo <- NA
  missing_links <- NA
  radio_silence <- NA
  primma_donna <- NA
  st_congruence <- NA
  communicability <- NA
  num_tz <- NA
  code_only_devs <- NA
  code_files <- NA
  ml_only_devs <- NA
  ml_threads <- NA
  code_ml_both_devs <- NA
  
  i <- j - 1
  
  # If the time window is of size 1, then there has been less than "window_size_f"
  # days from the start date.
  if(length(time_window)  == 1){
    # Below 3 month size
    start_day <- start_date
    end_day <- end_date 
  }else{
    start_day <- time_window[i]
    end_day <- time_window[j] 
  }
  
  
  # Note: The start and end commits in your project config file should be set so 
  # that the dates cover overlapping date ranges in bothproject_git_slice and project_reply_slice dates.
  # Double-check your project_git and project_reply to ensure this is the case if an error arises.
  
  # Obtain all commits from the gitlog which are within a particular window_size
  project_git_slice <- project_git[(author_datetimetz >= start_day) & 
                                     (author_datetimetz < end_day)]
  
  # Obtain all email posts from the reply which are within a particular window_size
  project_reply_slice <- project_reply[(reply_datetimetz >= start_day) & 
                                         (reply_datetimetz < end_day)]
  
  # Check if slices contain data
  gitlog_exist <- (nrow(project_git_slice) != 0)
  ml_exist <- (nrow(project_reply_slice) != 0)
  
  # Create Networks 
  if(gitlog_exist){
    i_commit_hash <- data.table::first(project_git_slice[project_git_slice$author_datetimetz == min(project_git_slice$author_datetimetz,na.rm=TRUE)])$commit_hash
    
    j_commit_hash <- data.table::first(project_git_slice[project_git_slice$author_datetimetz == max(project_git_slice$author_datetimetz,na.rm=TRUE)])$commit_hash
    
    # Parse networks edgelist from extracted data
    network_git_slice <- transform_gitlog_to_bipartite_network(project_git_slice,
                                                               mode="author-file")
    # Community Smells functions are defined base of the projection networks of 
    # dev-thread => dev-dev, and dev-file => dev-dev. This creates both dev-dev via graph projections
    
    git_network_authors <- bipartite_graph_projection(network_git_slice,
                                                      mode = TRUE,
                                                      weight_scheme_function = weight_scheme_sum_edges)
    print(9)
    code_clusters <- community_oslom(oslom_undir_path,
                                     git_network_authors,
                                     seed=seed,
                                     n_runs = 1000,
                                     is_weighted = TRUE)
    print(1)
    
  }
  if(ml_exist){
    network_reply_slice <- transform_reply_to_bipartite_network(project_reply_slice)
    
    
    reply_network_authors <- bipartite_graph_projection(network_reply_slice,
                                                        mode = TRUE,
                                                        weight_scheme_function = weight_scheme_sum_edges)    
    
    # Community Detection
    
    mail_clusters <- community_oslom(oslom_undir_path,
                                     reply_network_authors,
                                     seed=seed,
                                     n_runs = 1000,
                                     is_weighted = TRUE)
    print(2)
  }
  # Metrics #
  
  if(gitlog_exist){
    commit_interval <- stri_c(i_commit_hash,"-",j_commit_hash)
    # Social Network Metrics 
    code_only_devs <- length(unique(project_git_slice$identity_id))
    code_files <- length(unique(project_git_slice$file_pathname))
    print(3)
    
  }  
  if(ml_exist){
    # Smell
    
    radio_silence <- length(smell_radio_silence(mail.graph=reply_network_authors, 
                                                clusters=mail_clusters))
    
    # Social Technical Metrics
    ml_only_devs <- length(unique(project_reply_slice$identity_id))
    ml_threads <- length(unique(project_reply_slice$reply_subject))
    print(4)
  }
  if (ml_exist & gitlog_exist){
    # Smells 
    org_silo <- length(smell_organizational_silo(mail.graph=reply_network_authors,
                                                 code.graph=git_network_authors))
    
    missing_links <- length(smell_missing_links(mail.graph=reply_network_authors,
                                                code.graph=git_network_authors))
    # Social Technical Metrics
    st_congruence <- smell_sociotechnical_congruence(mail.graph=reply_network_authors,
                                                     code.graph=git_network_authors)
    #    communicability <- community_metric_mean_communicability(reply_network_authors,git_network_authors)
    num_tz <- length(unique(c(project_git_slice$author_tz,
                              project_git_slice$committer_tz,
                              project_reply_slice$reply_tz)))
    code_ml_both_devs <- length(intersect(unique(project_git_slice$identity_id),
                                          unique(project_reply_slice$identity_id)))
    
  }
  
  # Aggregate Metrics
  smells[[stringi::stri_c(start_day,"|",end_day)]] <- data.table(commit_interval,
                                                                 start_datetime = start_day,
                                                                 end_datetime = end_day,
                                                                 org_silo,
                                                                 missing_links,
                                                                 radio_silence,
                                                                 #primma_donna,
                                                                 st_congruence,
                                                                 #communicability,
                                                                 num_tz,
                                                                 code_only_devs,
                                                                 code_files,
                                                                 ml_only_devs,
                                                                 ml_threads,
                                                                 code_ml_both_devs)
}
smells_interval <- rbindlist(smells)

# --------------------------------------------------------------------------------------------------
# Good variables


# Transform commit hashes into datetime so window_size can be used
start_date <- project_reply[5702,"reply_datetimetz"][[1]]
end_date <- project_reply[45096,"reply_datetimetz"][[1]]
datetimes <- project_git$author_datetimetz
reply_datetimes <- project_reply$reply_datetimetz

# Format time window for posixT
window_size_f <- stringi::stri_c(window_size," day")

# Note if end_date is not (and will likely not be) a multiple of window_size, 
# then the ending incomplete window is discarded so the metrics are not calculated 
# in a smaller interval
time_window <- seq.POSIXt(from=start_date,to=end_date,by=window_size_f)


oslom_dir_path = '~/Downloads/OSLOM2/OSLOM2/oslom_dir'
oslom_undir_path = '~/Downloads/OSLOM2/OSLOM2/oslom_undir'
# Create a list where each element is the social smells calculated for a given commit hash
smells <- list()
size_time_window <- length(time_window)

# Create the wanted metrics per developer
for (j in 2:length(time_window)){
  i <- j - 1
  result = NA
  
  # If the time window is of size 1, then there has been less than "window_size_f"
  # days from the start date.
  if(length(time_window)  == 1){
    # Below 3 month size
    start_day <- start_date
    end_day <- end_date 
  }else{
    start_day <- time_window[i]
    end_day <- time_window[j] 
  }
  
  # Obtain all commits from the gitlog which are within a particular window_size
  project_git_slice <- project_git[(author_datetimetz >= start_day) & 
                                     (author_datetimetz < end_day)]
  
  # Obtain all email posts from the reply which are within a particular window_size
  project_reply_slice <- project_reply[(reply_datetimetz >= start_day) & 
                                         (reply_datetimetz < end_day)]
  
  # Check if slices contain data
  gitlog_exist <- (nrow(project_git_slice) != 0)
  ml_exist <- (nrow(project_reply_slice) != 0)
  
  # Create Networks 
  if(gitlog_exist){
    i_commit_hash <- data.table::first(project_git_slice[project_git_slice$author_datetimetz == min(project_git_slice$author_datetimetz,na.rm=TRUE)])$commit_hash
    
    j_commit_hash <- data.table::first(project_git_slice[project_git_slice$author_datetimetz == max(project_git_slice$author_datetimetz,na.rm=TRUE)])$commit_hash
    
    # Parse networks edgelist from extracted data
    network_git_slice <- transform_gitlog_to_bipartite_network(project_git_slice,
                                                               mode="author-file")
    # Community Smells functions are defined base of the projection networks of 
    # dev-thread => dev-dev, and dev-file => dev-dev. This creates both dev-dev via graph projections
    
    git_network_authors <- bipartite_graph_projection(network_git_slice,
                                                      mode = TRUE,
                                                      weight_scheme_function = weight_scheme_sum_edges)
    print(9)
    code_clusters <- community_oslom(oslom_undir_path,
                                     git_network_authors,
                                     seed=seed,
                                     n_runs = 1000,
                                     is_weighted = TRUE)
    print(1)
    
  }
  if(ml_exist){
    network_reply_slice <- transform_reply_to_bipartite_network(project_reply_slice)
    
    
    reply_network_authors <- bipartite_graph_projection(network_reply_slice,
                                                        mode = TRUE,
                                                        weight_scheme_function = weight_scheme_sum_edges)    
    
    # Community Detection
    
    mail_clusters <- community_oslom(oslom_undir_path,
                                     reply_network_authors,
                                     seed=seed,
                                     n_runs = 1000,
                                     is_weighted = TRUE)
    print(2)
    # Collect
    radio_silence_brokers = smell_radio_silence(mail.graph = reply_network_authors,
                                                clusters = mail_clusters)
    
    result = data.table('Radio Silence' = git_network_authors$nodes$name %in% radio_silence_brokers)
    rownames(result) = git_network_authors$nodes$name
    
  }
  if (ml_exist & gitlog_exist){
    # Smells 
    org_silo <- unique(unlist(smell_organizational_silo(mail.graph=reply_network_authors,
                                                 code.graph=git_network_authors)))
    
    result = cbind('Organisational Silo' = git_network_authors$nodes$name %in% org_silo, result)
    
    missing_links <- unique(unlist(smell_missing_links(mail.graph=reply_network_authors,
                                                code.graph=git_network_authors)))
    
    result = cbind('Lone Wolf' = git_network_authors$nodes$name %in% missing_links, result)
    # Social Technical Metrics
    st_congruence <- smell_sociotechnical_congruence(mail.graph=reply_network_authors,
                                                     code.graph=git_network_authors)
    
    git_igraph = igraph::graph_from_data_frame(git_network_authors[['edgelist']], 
                                               directed = FALSE,
                                               vertices = git_network_authors[["nodes"]])
    
    reply_igraph = igraph::graph_from_data_frame(reply_network_authors[['edgelist']], 
                                                 directed = FALSE,
                                                 vertices = reply_network_authors[["nodes"]])
    
    igraph::E(reply_igraph)$weight = 1
    git_diam = igraph::diameter(reply_igraph, directed = F)
    
    neighbor_distances = c()
    
    for (dev in git_network_authors$nodes$name){
      # Calculate the distances to the neighbours in the collaboration network
      neighbors = igraph::neighbors(git_igraph, dev)$name
      neighbors_in_communication = intersect(neighbors, igraph::V(reply_igraph))
      missing_neighbors = length(neighbors) - length(neighbors_in_communication)
      
      total_dist = tryCatch(
        {
          neighbor_dist = sum(igraph::distances(reply_igraph, v = dev, to = neighbors_in_communication))
          neighbor_dist + (git_diam+1)*missing_neighbors
        }, error = function(w){
          print(paste0(dev, ' not found in communication network'))
          NA
        }
      )
      
      neighbor_distances = c(neighbor_dist, total_dist)
    }
    
    result = cbind('Avg. distance' = neighbor_distances, result)
    
    result = cbind('Messages Sent' = )
  }
  
  
}


project_collaboration_network <- recolor_network_by_community(git_network_authors,code_clusters)

gcid <- igraph::graph_from_data_frame(d=project_collaboration_network[["edgelist"]], 
                                      directed = FALSE,
                                      vertices = project_collaboration_network[["nodes"]])

visIgraph(gcid,randomSeed = 1)


fwrite(project_reply, '~/gits/test/data/geronimo_communication_2003-2007.csv')
