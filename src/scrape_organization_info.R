# -----------------------------------------------------------------------------
#
# Scrape organization service information from websites
#
# Jason Jiang - Created: 2022/Nov/30
#               Last edited: 2022/Dec/30
#
# UTM - CRAWL
#
# -----------------------------------------------------------------------------

library(tidyverse)
library(rvest)  # for web scraping

###############################################################################

## PART 1: GET LINKS TO ORGANIZATION SERVICE PAGES

###############################################################################

# Define websites to mine service links from

SERVICE_WEBSITES <- 
  c('https://www.centralwesthealthline.ca/listservices.aspx?id=10566',
    'https://www.torontocentralhealthline.ca/listServices.aspx?id=10566')

###############################################################################

# Helper functions

get_base_url <- function(site) {
  # ---------------------------------------------------------------------------
  # Extracts the base domain for a website.
  # 
  # ex:
  # get_base_url('https://www.google.com/foo/bar') -> 'https://www.google.com'
  # get_base_url('https://www.google.com') -> 'https://www.google.com'
  # ---------------------------------------------------------------------------
  return(str_extract(site, '^https://www\\.[[A-Za-z]]+\\.[[A-Za-z]]{2,3}'))
}

###############################################################################

# Extract organization names + links from each website in SERVICE_WEBSITES

# Initialize dataframe for holding organization links
# Also initialize columns for holding web-scraped information about these
# organizations
# The columns for scraped info will all be NA until Part 2 of the code.
organization_info_df <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(organization_info_df) <- c('organization', 'link', 'org_link', 'address',
                                     'description', 'fees', 'eligibility',
                                     'languages', 'areas_served', 'last_updated')

for (site in SERVICE_WEBSITES) {
  
  # get base domain for the website
  base_url <- get_base_url(site)
  
  # load in HTML for the website
  site <- read_html(site)
  
  # HTML nodes in website that *may* contain links to organizations
  candidate_nodes <- site %>%
    html_nodes('td') %>%
    html_nodes('a')
  
  # id attributes for such HTML nodes
  candidate_nodes_ids <- candidate_nodes %>%
    html_attr('id')
  
  # indices for such HTML nodes having id attributes corresponding to
  # organizations
  links_idx <-
    which(!is.na(candidate_nodes_ids) & str_detect(candidate_nodes_ids, '.+_lnkService$'))
  
  # get full links to organizations from the HTML nodes corresponding to
  # organizations
  links <- unname(sapply((candidate_nodes %>% html_attr('href'))[links_idx],
                         function(s) {str_c(base_url, '/', s)}))
  
  # get names for the organizations each link redirects to
  organization_names <- candidate_nodes[links_idx] %>% html_text()
  
  # add the extracted service names and links for this website to
  # service_links_df
  organization_info_df <- rbind(organization_info_df,
                              # create "mini-dataframe" holding extracted
                              # organization names + links to append to
                              # organization_info_df
                                 data.frame(organization = organization_names,
                                            link = links,
                                            org_link = NA,
                                            address = NA,
                                            description = NA,
                                            fees = NA,
                                            eligibility = NA,
                                            languages = NA,
                                            areas_served = NA,
                                            last_updated = NA))
  
}

# Remove unnecessary variables from environment
rm(candidate_nodes, site, base_url, candidate_nodes_ids, links, links_idx,
   organization_names)

###############################################################################

## PART 2: SCRAPE SERVICE INFORMATION FOR EACH ORGANIZATION

###############################################################################

# Define attributes we'd like to scrape information about from websites, and the
# ids of their HTML nodes
IDS_OF_INTEREST <- list('address' = 'ctl00_ContentPlaceHolder1_lblAddress',
                        'description' = 'ctl00_ContentPlaceHolder1_lblDescription',
                        'fees' = 'ctl00_ContentPlaceHolder1_lblFees',
                        'eligibility' = 'ctl00_ContentPlaceHolder1_lblEligibility',
                        'languages' = 'ctl00_ContentPlaceHolder1_lblLanguages',
                        'areas_served' = 'ctl00_ContentPlaceHolder1_lblAreasServed',
                        'last_updated' = 'ctl00_ContentPlaceHolder1_lblLastUpdated')

###############################################################################

# Replace NAs in organization_info_df with scraped information, if possible
# Otherwise, keep as NA
for (i in 1 : nrow(organization_info_df)) {
  org_site <- read_html(organization_info_df[['link']][i])
  
  # Get HTML nodes for spans, which contain all the attributes we want to scrape
  spans <- org_site %>%
    html_nodes('span')
  
  # Get the ids for all these nodes, which we can use to find nodes containing
  # the attributes we want
  spans_ids <- spans %>%
    html_attr('id')
  
  # Scrape information for organization link separately, due to its different
  # HTML formatting from all other information
  hyperlinks <- org_site %>% html_nodes('a')
  hyperlinks_ids <- hyperlinks %>% html_attr('id')
  org_link <-
    hyperlinks[which(hyperlinks_ids == 'ctl00_ContentPlaceHolder1_lnkUrl')] %>%
    html_attr('href')
  
  if (length(org_link) == 0) {
    organization_info_df[['org_link']][i] <- NA 
  } else {
    organization_info_df[['org_link']][i] <- org_link
  }
  
  # Scrape information for other attributes of interest in this row's site, and
  # add the scraped information to their respective columns in
  # organization_info_df
  for (attr in names(IDS_OF_INTEREST)) {
    scraped_attr <- spans[which(spans_ids == IDS_OF_INTEREST[attr])] %>%
      html_text2()
    
    # quick and dirty fix for some missing service description links
    # ex 1: https://www.centralwesthealthline.ca/displayService.aspx?id=170809
    # ex 2: https://www.centralwesthealthline.ca/displayService.aspx?id=132679
    if (attr == 'description') {
      # I would've combined these 2 into a single '&' statement above, but that
      # leads to an error
      if (str_length(scraped_attr) < 200) {
        scraped_attr_2 <- spans[which(spans_ids == IDS_OF_INTEREST[attr]) + 1] %>%
          html_text2() 
        
        if (!is.na(scraped_attr_2) & length(scraped_attr_2) > 0) {
          scraped_attr <- str_c(scraped_attr, ' ', scraped_attr_2)
        }
      }
    }
    
    # If scraping failed to retrieve any information (i.e: got an empty string
    # or empty vector), then replace scraped information with NA
    if (length(scraped_attr) == 0 || scraped_attr == '') {
      scraped_attr <- NA
      
      # Otherwise, clean-up the scraped information, replacing newlines with
      # single spaces so the scraped information is more readable
    } else {
      scraped_attr <- trimws(str_replace_all(
        str_replace_all(scraped_attr, '\n|\r', ' '),
        ' {2,}',
        ' '
      ))
    }
    
    organization_info_df[[attr]][i] <- scraped_attr
  }
}

# Find rows where any column has missing values
# This will be useful for troubleshooting any web-scraping errors
missing_vals <- organization_info_df[rowSums(is.na(organization_info_df)) > 0, ] 

################################################################################

# Save our scraped information + rows with missing values as csv files
write_csv(organization_info_df, '../results/organization_service_info.csv')
write_csv(missing_vals, '../results/missing_values.csv')

################################################################################

# Save links of unique organization links as a text file
# (to help with coding the websites in Nvivo)
write_lines(
  unique(sapply(na.omit(organization_info_df$org_link), function(s) {str_remove(s, '\\/$')})),
  '../results/organization_websites.txt'
)