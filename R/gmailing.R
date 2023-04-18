# Load libraries
library(gmailr)
library(lubridate)

# Set up authentication
gmail_auth()

check_gmail <- function(sender = "Lejla.Fajic@gov.si") {

  # Specify the sender and attachment file type
  sender <- "example@gmail.com"
  filetype <- "application/vnd.ms-excel"

  # Get the time the script last ran (assuming the script has saved the time to a file)
  last_run <- as_datetime(readLines("last_run_time.txt"))

  # Search for messages from the sender that have been modified since the last run
  msg <- messages_history(query = paste0("from:", sender),
                          max_results = 1, # Get up to 50 most recent messages
                          start_time = last_run)

# Extract the message IDs from the search results
msg_ids <- pluck(msg, "id")

# Loop through the message IDs and download the attachments for each message
for (id in msg_ids) {
  attachments <- message_attachments(id)

  for (attachment in attachments) {
    if (attachment$type == filetype) {
      writeBin(attachment$content, file.path("path/to/your/folder", attachment$filename))
    }
  }
}

# Save the current time to a file for the next run
writeLines(as.character(Sys.time()), "last_run_time.txt")
}
