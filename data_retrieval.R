require(kaiaulu)
require(data.table)
require(jsonlite)
require(knitr)
require(magrittr)
require(gt)
require(readxl)
source('Kaiaulu_edits.R')

base_dir = 'C:/Users/woute/OneDrive/Documenten/JADS/Thesis/'
delay = 5

token = scan("~/.ssh/github_token.txt",what="character",quiet=TRUE)

information = read_xlsx(paste0(base_dir, 'info.xlsx'))

row.names(information) =  information$Name

# Iteratively crawl data
for (project in information$Name){
  print(paste0('Crawling ', project, '...'))

  # Check if the data directory already exists
  if (dir.exists(paste0(base_dir, 'data/', project))){
    # Skip to the next iteration
    print(paste0(project, ' data directory already exists, skipping iteration...'))
    next
  } else{
    # Create data directory
    dir.create(paste0(base_dir, 'data/', project))
  }

  # Retrieve the information for all things we need to crawl
  gits = strsplit(information[project,'GitHub'][[1]], ', ', fixed = T)[[1]]
  mboxes = strsplit(information[project,'Mbox_accs'][[1]], ', ', fixed = T)[[1]]
  jiras = strsplit(information[project,'Jira'][[1]], ', ', fixed = T)[[1]]

  for (git in gits){
    git_info = tail(git, n=2)

  }
}


project_git <- parse_gitlog('C:/Users/woute/AppData/Local/Programs/Python/Python310/Scripts/perceval','~/JADS/Thesis/gits/geronimo/.git')
project_git <- project_git  %>%
  filter_by_file_extension(file_extensions,"file_pathname")  %>%
  filter_by_filepath_substring(substring_filepath,"file_pathname")



flags = c(
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
perceval_path = path.expand('/wsl.localhost/Ubuntu/home/wouter/grimoirelab-perceval/.venv/bin/perceval')
git_repo_path = path.expand('~/JADS/Thesis/gits/geronimo/.git')
save_path = '/tmp/gitlog.log'

system2(perceval_path,
        args = c('git', '--git-log',save_path,git_repo_path,'--json-line'),
        stdout = TRUE,
        stderr = T)

system2(perceval_path, stdout = T, stderr = T)
