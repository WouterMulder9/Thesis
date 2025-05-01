parse_gitlog <- function(perceval_path,git_repo_path,save_path=NA,perl_regex=NA){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  perceval_path <- path.expand(perceval_path)
  git_repo_path <- path.expand(git_repo_path)
  git_uri <-  git_repo_path
  save_path <- ifelse(!is.na(save_path),path.expand(save_path),NA)

  # Use percerval to parse .git --json line is required to be parsed by jsonlite::fromJSON.
  # The log will be saved to the /tmp/ folder
  gitlog_path <- "/tmp/gitlog.log"

  # Perceval suggested flags
  perceval_flags <-
    c(
      '--raw',
      '--numstat',
      '--pretty=fuller',
      '--decorate=full',
      '--parents',
      '--reverse',
      '--topo-order',
      '-M',
      '-C',
      '-c'
    )
  # Execute shell command to extract gitlog using Percerval recommended format (See it's README.md).
  if(!is.na(perl_regex)){
    flags <- c('--no-merges',
               'master',
               stringi::stri_c('--grep=', '"', perl_regex, '"'),
               '--perl-regexp',
               perceval_flags)
    gitlog_call_message <- git_log(git_repo_path,flags,gitlog_path)
    if(is.null(gitlog_call_message) || length(gitlong_call_message > 0)){
      stop(stringi::stri_c("Unable to generate git log from this repository.",
                           " Perhaps the path specified was incorrect or the repository has no commits?"))
    }
  }else{
    flags <- perceval_flags
    gitlog_call_message <- git_log(git_repo_path,flags,gitlog_path)
    print('Cleared')
    print(gitlog_call_message)
    tryCatch(
      {
        if(is.null(gitlog_call_message) || length(gitlong_call_message > 0)){
        stop(stringi::stri_c("Unable to generate git log from this repository.",
                             " Perhaps the path specified was incorrect or the repository has no commits?"))
      }
        },
      error = function(cond){
        print('Call finished!')
      }
    )
  }

  # Parsed JSON output.
  perceval_output <- system2(perceval_path,
                             args = c('git', '--git-log',gitlog_path,git_uri,'--json-line'),
                             stdout = TRUE,
                             stderr = FALSE)

  perceval_parsed <- data.table(jsonlite::stream_in(textConnection(perceval_output),verbose = FALSE))

  if(nrow(perceval_parsed) == 0){
    stop("The repository specified has no commits.")
  }

  # APR very first commit is a weird single case of commit without files. We filter them here.
  is_commit_with_files <- !!sapply(perceval_parsed$data.files,length)
  perceval_parsed <- perceval_parsed[is_commit_with_files]
  # Column data.files is a data.table. Unlist, so perceval_parsed is a table instead of a table of tables.

  # Only when a file is renamed, Perceval will add a field "newfile". Normalize the list so every
  # element contain newfile, so the subsequent step can correctly tabulate "newfile" field.
  add_new_files_to_table <- function(data_files_row){
    commit_change_table <- data.table(data_files_row)

    # In some cases for APR (e.g. 873ca8616235529ceb222c8dd428c2d0e23824b6 and
    # b43bbf82e946864de970bf792c5d0907ae142dba, Perceval can only parse added, file  and removed.
    # If a file is modified, Perceval will include a #newfiles field, leading to a total of 7 fields.
    # To be safe, fill with NA any column that is missing.
    if(!("action" %in% colnames(commit_change_table))) commit_change_table$action <- NA_character_
    if(!("added" %in% colnames(commit_change_table))) commit_change_table$added <- NA_character_
    if(!("indexes" %in% colnames(commit_change_table))) commit_change_table$indexes <- NA_character_
    if(!("modes" %in% colnames(commit_change_table))) commit_change_table$modes <- NA_character_
    if(!("newfile" %in% colnames(commit_change_table))) commit_change_table$newfile <- NA_character_
    if(!("removed" %in% colnames(commit_change_table))) commit_change_table$removed <- NA_character_

    commit_change_table <- commit_change_table[,.(action,
                                                  added,
                                                  file,
                                                  indexes,
                                                  modes,
                                                  newfile,
                                                  removed)]
    return(commit_change_table)
  }
  perceval_parsed$data.files <- lapply(perceval_parsed$data.files,add_new_files_to_table)

  perceval_parsed <- perceval_parsed[, .(file=unlist(data.files[[1]]$file),
                                         added=unlist(data.files[[1]]$added),
                                         removed=unlist(data.files[[1]]$removed),
                                         newfile=unlist(data.files[[1]]$newfile)),, by = list(data.Author,
                                                                                              data.AuthorDate,
                                                                                              data.commit,
                                                                                              data.Commit,
                                                                                              data.CommitDate,
                                                                                              data.message)]


  setnames(perceval_parsed,
           c("data.Author","data.AuthorDate","data.commit","data.Commit","data.CommitDate","data.message",
             "file","added","removed","newfile"),
           c("author_name_email","author_datetimetz","commit_hash","committer_name_email","committer_datetimetz",
             "commit_message","file_pathname","lines_added","lines_removed","file_pathname_renamed"))

  # When newfile is provided, replace "file" with "newfile"
  # This avoids situations where a file is renamed, and never again modified, to not be included
  # in the list of files
  perceval_parsed[!is.na(file_pathname_renamed)]$file_pathname <- perceval_parsed[!is.na(file_pathname_renamed)]$file_pathname_renamed

  # Parsing gitlog can take awhile, save if a path is provided
  if(!is.na(save_path)){
    saveRDS(perceval_parsed,save_path)
  }
  return(perceval_parsed)
}

git_log <- function(git_repo_path,flags,save_path){
  out <- tryCatch(
    {
      # Main Execution
      system2(
        "git",
        args = c(
          '--git-dir',
          git_repo_path,
          'log',
          flags
        ),
        stdout = save_path,
        stderr = T
      )
    },
    error = function(cond){
      #message(stringi::stri_c("An error ocurred when generating the git log from this repository.",
      #                        " Perhaps the path specified was incorrect or the repository has no commits?"))
      #message(cond)
      return(NULL)
    },
    warning = function(cond){
      #message(stringi::stri_c("A warning ocurred when generating the git log from this repository.",
      #" Perhaps the path specified was incorrect or the repository has no commits?"))
      #message(cond)
      return(NULL)
    }
  )
  print(out)
  return(out)
}
