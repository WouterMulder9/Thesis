parse_jira <- function(json_folder_path){
  
  file_list <- list.files(json_folder_path)
  
  if (identical(file_list, character(0))){
    stop(stringi::stri_c("cannot open the connection"))
  }
  
  # Comments list parser. Comments may occur on any json issue.
  jira_parse_comment <- function(comment){
    parsed_comment <- list()
    parsed_comment[["comment_id"]] <- comment[["id"]][[1]]
    
    parsed_comment[["comment_created_datetimetz"]] <- comment[["created"]][[1]]
    parsed_comment[["comment_updated_datetimetz"]] <- comment[["updated"]][[1]]
    
    parsed_comment[["comment_author_id"]] <- comment[["author"]][["name"]][[1]]
    parsed_comment[["comment_author_name"]] <- comment[["author"]][["displayName"]][[1]]
    parsed_comment[["comment_author_timezone"]] <- comment[["author"]][["timeZone"]][[1]]
    
    parsed_comment[["comment_author_update_id"]] <- comment[["updateAuthor"]][["name"]][[1]]
    parsed_comment[["comment_author_update_name"]] <- comment[["updateAuthor"]][["displayName"]][[1]]
    parsed_comment[["comment_author_update_timezone"]] <- comment[["updateAuthor"]][["timeZone"]][[1]]
    
    parsed_comment[["comment_body"]] <- comment[["body"]][[1]]
    
    return(parsed_comment)
  }
  
  # Issues parser
  jira_parse_issues <- function(jira_file){
    
    json_issue_comments <- jsonlite::read_json(jira_file)
    
    n_issues <- length(json_issue_comments[["base_info"]])
    
    # Prepare two lists which will contain data.tables for all issues and all comments
    # Both tables can share the issue_key, so they can be joined if desired.
    all_issues <- list()
    all_issues_comments <- list()
    
    for(i in 1:n_issues){
      
      # This is the issue key
      issue_key <- json_issue_comments[["base_info"]][[i]][["key"]]
      
      # All other information is contained in "fields"
      issue_comment <- json_issue_comments[["ext_info"]][[i]]
      
      print(issue_key)
      
      # Parse all relevant *issue* fields
      all_issues[[i]] <- data.table(
        issue_key = issue_key,
        
        issue_summary = issue_comment[["summary"]][[1]],
        issue_parent = issue_comment[["parent"]][["name"]][[1]],
        issue_type = issue_comment[["issuetype"]][["name"]][[1]],
        issue_status = issue_comment[["status"]][["statusCategory"]][["name"]][[1]],
        issue_resolution = issue_comment[["resolution"]][["name"]][[1]],
        issue_components = stringi::stri_c(unlist(sapply(issue_comment[["components"]],"[[","name")),collapse = ";"),
        issue_description = if(length(issue_comment[["description"]])>0) issue_comment[["description"]][[1]] else NULL,
        issue_priority = issue_comment[["priority"]][["name"]][[1]],
        issue_affects_versions = stringi::stri_c(unlist(sapply(issue_comment[["versions"]],"[[","name")),collapse = ";"),
        issue_fix_versions = stringi::stri_c(unlist(sapply(issue_comment[["fixVersions"]],"[[","name")),collapse = ";"),
        issue_labels = stringi::stri_c(unlist(sapply(issue_comment[["labels"]],"[[",1)),collapse = ";"),
        issue_votes = issue_comment[["votes"]][["votes"]][[1]],
        issue_watchers = issue_comment[["watches"]][["watchCount"]][[1]],
        
        issue_created_datetimetz = issue_comment[["created"]][[1]],
        issue_updated_datetimetz = issue_comment[["updated"]][[1]],
        issue_resolution_datetimetz = if(length(issue_comment[["resolutiondate"]])>0) issue_comment[["resolutiondate"]][[1]] else NULL,
        
        issue_creator_id = issue_comment[["creator"]][["name"]][[1]],
        issue_creator_name = issue_comment[["creator"]][["displayName"]][[1]],
        issue_creator_timezone = issue_comment[["creator"]][["timeZone"]][[1]],
        
        issue_assignee_id = issue_comment[["assignee"]][["name"]][[1]],
        issue_assignee_name = issue_comment[["assignee"]][["displayName"]][[1]],
        issue_assignee_timezone = issue_comment[["assignee"]][["timeZone"]][[1]],
        
        issue_reporter_id = issue_comment[["reporter"]][["name"]][[1]],
        issue_reporter_name = issue_comment[["reporter"]][["displayName"]][[1]],
        issue_reporter_timezone = issue_comment[["reporter"]][["timeZone"]][[1]]
      )
      # Comments
      # For each issue, comment/comments contain 1 or more comments. Parse them
      # in a separate table.
      root_of_comments_list <- json_issue_comments[["ext_info"]][[i]][["comment"]]
      # If root_of_comments_list does not exist, then this is an issue only json, skip parsing
      if(length(root_of_comments_list) > 0){
        comments_list <- json_issue_comments[["ext_info"]][[i]][["comment"]][["comments"]]
        # Even on a json with comments, some issues may not have comments, check if comments exist:
        if(length(comments_list) > 0){
          # Parse all comments into issue_comments
          issue_comments <- rbindlist(lapply(comments_list,
                                             jira_parse_comment))
          # Add issue_key column to the start of the table
          issue_comments <- cbind(data.table(issue_key=issue_key),issue_comments)
          all_issues_comments[[i]] <- issue_comments
        }
      }
    }
    
    
    all_issues <- rbindlist(all_issues,fill=TRUE)
    all_issues_comments <- rbindlist(all_issues_comments,fill=TRUE)
    
    parsed_issues_comments <- list()
    parsed_issues_comments[["issues"]] <- all_issues
    parsed_issues_comments[["comments"]] <- all_issues_comments
    
    return(parsed_issues_comments)
  }
  
  issues_holder <- list()
  comments_holder <- list()
  
  for(filename in file_list){
    current_json <- paste0(json_folder_path, "/", filename)
    parsed_data <- jira_parse_issues(current_json)
    issues_holder <- append(issues_holder, list(parsed_data[["issues"]]))
    comments_holder <- append(comments_holder, list(parsed_data[["comments"]]))
  }
  
  issues_holder <- rbindlist(issues_holder, fill=TRUE)
  comments_holder <- rbindlist(comments_holder, fill=TRUE)
  
  return_info <- list()
  return_info[["issues"]] <- issues_holder
  return_info[["comments"]] <- comments_holder
  
  return(return_info)
}


parse_jira_replies <- function(parsed_jira){
  
  
  project_jira_issues <- parsed_jira[["issues"]]
  project_jira_issues <- project_jira_issues[,.(reply_id=issue_key,
                                                in_reply_to_id=NA_character_,
                                                reply_datetimetz=issue_created_datetimetz,
                                                reply_from=issue_creator_name,
                                                reply_to=NA_character_,
                                                reply_cc=NA_character_,
                                                reply_subject=issue_key,
                                                reply_body=issue_description)]
  
  
  project_jira_comments <- parsed_jira[["comments"]]
  project_jira_comments <- project_jira_comments[,.(reply_id=comment_id,
                                                    in_reply_to_id=NA_character_,
                                                    reply_datetimetz=comment_created_datetimetz,
                                                    reply_from=comment_author_name,
                                                    reply_to=NA_character_,
                                                    reply_cc=NA_character_,
                                                    reply_subject=issue_key,
                                                    reply_body=comment_body)]
  
  project_jira <- rbind(project_jira_issues,
                        project_jira_comments)
  
  return(project_jira)
}


parse_github_replies <- function(github_replies_folder_path){
  
  issues_json_folder_path <- paste0(github_replies_folder_path,"/issue/")
  pull_requests_json_folder_path <- paste0(github_replies_folder_path,"/pull_request/")
  comments_json_folder_path <- paste0(github_replies_folder_path,"/issue_or_pr_comment/")
  commit_json_folder_path <- paste0(github_replies_folder_path,"/commit/")
  
  issues = T
  prs = T
  comments = T
  
  # Tabulate Issues
  all_issue <- lapply(list.files(issues_json_folder_path,
                                 full.names = TRUE),jsonlite::read_json)
  all_issue <- lapply(all_issue,
                      kaiaulu::github_parse_project_issue)
  all_issue <- rbindlist(all_issue,fill=TRUE)
  
  # Tabulate PRs
  all_pr <- lapply(list.files(pull_requests_json_folder_path,
                              full.names = TRUE),jsonlite::read_json)
  all_pr <- lapply(all_pr,
                   github_parse_project_pull_request)
  all_pr <- rbindlist(all_pr,fill=TRUE)
  
  # Tabulate Comments
  all_issue_or_pr_comments <- lapply(list.files(comments_json_folder_path,
                                                full.names = TRUE),jsonlite::read_json)
  all_issue_or_pr_comments <- lapply(all_issue_or_pr_comments,
                                     github_parse_project_issue_or_pr_comments)
  all_issue_or_pr_comments <- rbindlist(all_issue_or_pr_comments,fill=TRUE)
  
  if (length(all_issue)>0){
  all_issue <- all_issue[,.(reply_id=issue_id,
                            in_reply_to_id=NA_character_,
                            reply_datetimetz=created_at,
                            reply_from=issue_user_login,
                            reply_to=NA_character_,
                            reply_cc=NA_character_,
                            reply_subject=issue_number,
                            reply_body=body)]
  } else {
    issues = F
  }
  # Note because GitHub API treats PRs as Issues, then pr_number <=> issue_number
  if (length(all_pr)>0){
  all_pr <- all_pr[,.(reply_id=pr_id,
                      in_reply_to_id=NA_character_,
                      reply_datetimetz=created_at,
                      reply_from=pr_user_login,
                      reply_to=NA_character_,
                      reply_cc=NA_character_,
                      reply_subject=pr_number,
                      reply_body=body)]
  } else {
    prs = F
  }
  if (length(all_issue_or_pr_comments)>0){
  all_issue_or_pr_comments <- all_issue_or_pr_comments[,.(reply_id=comment_id,
                                                          in_reply_to_id=NA_character_,
                                                          reply_datetimetz=created_at,
                                                          reply_from=comment_user_login,
                                                          reply_to=NA_character_,
                                                          reply_cc=NA_character_,
                                                          reply_subject=issue_url,
                                                          reply_body=body)]
  issue_or_pr_comments_reply_subject <- stringi::stri_split_regex(all_issue_or_pr_comments$reply_subject,
                                                                  "/")
  all_issue_or_pr_comments$reply_subject <- sapply(issue_or_pr_comments_reply_subject,"[[",8)
  } else {
    comments = F
  }
  print(c(issues,prs,comments))
  if (issues){
    if(prs){
      if(comments){
        replies <- rbind(all_issue,
                         all_pr,
                         all_issue_or_pr_comments)
      } else {
        replies <- rbind(all_issue,
                         all_pr)
      }
    } else {
      if(comments){
        replies <- rbind(all_issue,
                         all_issue_or_pr_comments)
      } else {
        replies <- all_issue
    }
    }
  } else {
    if(prs){
      if(comments){
        replies <- rbind(all_pr,
                         all_issue_or_pr_comments)
      } else {
        replies <- all_pr
      }
    } else {
      if(comments){
        replies <- all_issue_or_pr_comments
      } else {
        print('here')
        return()
      }
    }
  }
  # We can then parse the commit messages, and format so we have a look-up table of authors
  # and committers name, e-mail, and github ID:
  
  all_commits <- lapply(list.files(commit_json_folder_path,
                                   full.names = TRUE),jsonlite::read_json)
  all_commits <- lapply(all_commits,
                        github_parse_project_commits)
  all_commits <- rbindlist(all_commits,fill=TRUE)
  all_github_authors <- all_commits[,.(github_login=author_login,
                                       name_email = stringi::stri_c(commit_author_name,
                                                                    " ",
                                                                    commit_author_email))]
  
  all_github_committers <- all_commits[,.(github_login=committer_login,
                                          name_email = stringi::stri_c(commit_committer_name,
                                                                       " ",
                                                                       commit_committer_email))]
  
  all_github_developers <- rbind(all_github_authors,all_github_committers)
  
  # For simplicity here, when the same GitHub id contains
  # multiple e-mails, we choose one. In the future, we will
  # consider including all e-mails.
  all_github_developers <- all_github_developers[,.(name_email=name_email[1]),by="github_login"]
  # Replace `reply_from` by name<space>email when information is available (i.e.)
  # the github id modified as author or commiter at least one file.
  replies <- merge(replies,all_github_developers,
                   all.x=TRUE,
                   by.x="reply_from",
                   by.y="github_login")
  
  replies[!is.na(name_email)]$reply_from <-  replies[!is.na(name_email)]$name_email
  replies[,name_email:=NULL]
  
  return(replies)
}






community_oslom <- function(oslom_bin_dir_undir_path,graph,seed,n_runs,is_weighted){
  
  edgelist <- graph[["edgelist"]]
  
  oslom_bin_dir_undir_path <- path.expand(oslom_bin_dir_undir_path)
  mapping_names <- unique(c(as.character(edgelist$from),edgelist$to))
  mapping_ids <- 1:length(mapping_names)
  names(mapping_ids) <- mapping_names
  weight_flag <- ""
  
  if(is_weighted){
    oslom_edgelist <- data.table(mapping_ids[edgelist$from],
                                 mapping_ids[edgelist$to],
                                 edgelist$weight)
    weight_flag <- "-w"
  }else{
    oslom_edgelist <- data.table(mapping_ids[edgelist$from],
                                 mapping_ids[edgelist$to])
  }
  
  
  fwrite(oslom_edgelist,"/tmp/oslom_edgelist.txt",
         sep = "\t",
         row.names = FALSE,
         col.names = FALSE)

  
  print(c(oslom_bin_dir_undir_path, weight_flag, seed, n_runs))
  system2(
    oslom_bin_dir_undir_path,
    args = c('-f', '/tmp/oslom_edgelist.txt',weight_flag,'-seed',seed, '-r',n_runs),
    stdout = F,
    stderr = F
  )
  f_con <- file("/tmp/oslom_edgelist.txt_oslo_files/tp")
  tp_raw <- readLines(f_con)
  close(f_con)
  cluster_metadata <- stri_match(tp_raw,regex="#module (.+) size: (.+) bs: (.+)")
  
  is_node_line <- which(is.na(cluster_metadata[,1]))
  is_cluster_line <- which(!is.na(cluster_metadata[,1]))
  
  cluster_id <- cluster_metadata[is_cluster_line,2]
  cluster_size <- as.integer(cluster_metadata[is_cluster_line,3])
  cluster_pvalue <- cluster_metadata[is_cluster_line,4]
  cluster_nodes <- tp_raw[is_node_line]
  
  cluster_nodes_list <- stri_split(cluster_nodes,regex=" ")
  names(cluster_nodes_list) <- cluster_id
  
  # Remove "" strings added as ending element, create data.table and assign ids
  prepare_data_table <- function(name,x){
    dt <- data.table(node_id=x[[name]][x[[name]] != ""],cluster_id=name)
    return(dt)
  }
  cluster_assignment <- rbindlist(lapply(names(cluster_nodes_list),
                                         prepare_data_table,
                                         cluster_nodes_list))
  
  # Replace temporary id by original values
  cluster_assignment$node_id <- mapping_names[as.numeric(cluster_assignment$node_id)]
  cluster <- list()
  cluster[["assignment"]] <- cluster_assignment
  cluster[["info"]] <- data.table(cluster_id,cluster_size,cluster_pvalue)
  
  # assign a unique and different from existing cluster id to all nodes which have no neighbors
  isolated_node_ids <- setdiff(graph[["nodes"]]$name,cluster[["assignment"]]$node_id)
  
  if(length(isolated_node_ids) > 0){
    isolated_nodes_cluster_ids <- max(as.numeric(cluster[["assignment"]]$cluster_id)) + 1
    isolated_nodes_cluster_ids <- as.character(seq.int(from=isolated_nodes_cluster_ids,
                                                       length.out = length(isolated_node_ids)))
    isolated_cluster_assignments <- data.table(node_id=isolated_node_ids,
                                               cluster_id=isolated_nodes_cluster_ids)
    
    cluster[["assignment"]] <- rbind(cluster[["assignment"]],
                                     isolated_cluster_assignments)
    
    isolated_cluster_infos <- data.table(cluster_id=isolated_nodes_cluster_ids,
                                         cluster_size=1,
                                         cluster_pvalue=NA)
    
    cluster[["info"]] <- rbind(cluster[["info"]],
                               isolated_cluster_infos)
  }
  
  # Indexes starting cluster id to 1 instead of 0 to align with R indexes
  cluster[["assignment"]]$cluster_id <- as.character(as.numeric(cluster[["assignment"]]$cluster_id) + 1)
  cluster[["info"]]$cluster_id <- as.character(as.numeric(cluster[["info"]]$cluster_id) + 1)
  
  return(cluster)
}
