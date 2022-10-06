### Cleaning script 1
library('fastverse')
Data <- readxl::read_xls('dataset1.xls')
# Task 1: Clean column names
Data <- Data |> qDT() |>
  janitor::clean_names() |>
  frename(
    city_5      = 'office_city',
    state_6     = 'office_state',
    zip_code_7  = 'office_zip_code',
    city_13     = 'candidate_city',
    state_14    = 'candidate_state',
    zip_code_15 = 'candidate_zip_code' 
  )

# Task 2: Check Date columns for non correct dates
Data |>
  fgroup_by(expiration_date) |>
  fsummarise(count = GRPN()) |>
  # The Date format is "mm/dd/yyyy"
  ftransform(expiration_date = as.IDate(expiration_date, format = '%m/%d/%Y')) |>
  # no additional NA values (all Dates correct)
  View()

Data |>
  fgroup_by(commissioned_date) |>
  fsummarise(count = GRPN()) |>
  ftransform(commissioned_date = as.IDate(commissioned_date, format = '%m/%d/%Y'))
  # 17 additional NAs, there has to be at least one incorrect Date

is_no_Date <- function(Date, format){
  # TRUE if it is not a Date (because as.IDate() returns NA)
  # FALSE otherwise
  is.na(as.IDate(Date,format = format))
}

Data |>
  # some Dates were NA before, those should not be effected
  fmutate(
    is_no_Date = is_no_Date(
      commissioned_date, format = '%m/%d/%Y'
      ) & !is.na(commissioned_date)) |>
  # find the incorrect Dates
  fsubset(is_no_Date != isTRUE(is_no_Date)) |>
  with(commissioned_date) |>
  funique()
# There is only one incorrect Date, apply a transforming function
Data <- Data |> 
  fmutate(
    is_no_Date = is_no_Date(
      commissioned_date, format = '%m/%d/%Y'
    ) & !is.na(commissioned_date)) |>
  ftransform(
    commissioned_date = fifelse(
      is_no_Date, as.IDate(commissioned_date, format = '%d/%m/%Y'), as.IDate(commissioned_date, format = '%m/%d/%Y')),
    expiration_date = as.IDate(expiration_date, format = '%d/%m/%Y')
  ) |>
  fselect(-is_no_Date)
# Check, if it worked
Data |>
  fmutate(
    is_no_Date = is_no_Date(
      commissioned_date, format = '%m/%d/%Y'
    ) & !is.na(commissioned_date)) |>
  fsubset(is_no_Date != isTRUE(is_no_Date)) |>
  with(commissioned_date) |>
  funique()

# Task 3: Combine office titles, if they are the same with different writing (e.g. white spaces or abbreviations)

# Get an overview of near matching strings
Data <- Data |> ftransformv(office_title, trimws)
expand.grid(funique(Data$office_title),funique(Data$office_title)) |>
  qDT() |>
  fmutate(distance = stringdist::stringdist(as.character(Var1),as.character(Var2))) |>
  fsubset(distance < 5 & distance != 0) |>
  View()
# Write a Dictionary for Joining
Dictionary <- data.table(
  corrupted = c(
    'Aldermen',
    'Member','Councilmen','Council Member(s)','Council Member I','Council Member II','Council Member III','Councilmember',
    'Councilmember at Large',
    'Constable(s)',
    'Justice of the Peace(s)'
  ),
  correct = c(
    'Alderman',
    rep('Council Member',7),
    'Council Member at Large',
    'Constable',
    'Justice of the Peace'
  )
)

Data <- Dictionary[Data, on = c('corrupted' = 'office_title')] |>
  frename(corrupted = 'office_title') |>
  ftransform(
    office_title = fifelse(
      is.na(correct), office_title, correct
    )
  ) |>
  fselect(-correct)
rm(Dictionary)
# Task 4: Remove duplicates in candidate names

# unique and non NA names
Names <- Data |>
  fsubset(!is.na(candidate_name)) |>
  ftransform(candidate_name = stringr::str_remove_all(candidate_name, '"')) |>
  with(candidate_name) |>
  funique()
# overview of similar names
expand.grid(Names,Names) |>
  qDT() |>
  fmutate(distance = stringdist::stringdist(as.character(Var1),as.character(Var2))) |>
  fsubset(distance < 2 & distance != 0) |>
  View()
# Dictionary
Dictionary <- data.table(
  corrupted = c(
    'Brenda Hensley Smith',
    'J.P. Morrell',
    'Sidney H. Cates, V',
    'Kenneth  O. Stinson',
    'Russell  P. Pavich'
  ),
  correct = c(
    'Brenda Hensley-Smith',
    'J. P. Morrell',
    'Sidney H. Cates, IV',
    'Kenneth O. Stinson',
    'Russell P. Pavich'
  )
)
Data <- Dictionary[Data, on = c('corrupted' = 'candidate_name')] |>
  frename(corrupted = 'candidate_name') |>
  ftransform(
    office_title = fifelse(
      is.na(correct), candidate_name, correct
    )
  ) |>
  fselect(-correct)
rm(list = c('Dictionary','Names'))
fwrite(Data, 'dataset1_clean.csv')
gc()
