# Description:
#   Utility functions for data processing.


sanitize_whitespace = function(string)
  # Remove unusual or extra whitespace.
{
  # UTF-8 codepoint 0xA0 is "non-breaking space".
  string = gsub("\uA0", " ", string)
  string = gsub(" {2,}", " ", string)

  return(string)
}


remove_repeats = function(string)
  # Remove repeated tokens from a string.
{
  string = gsub("(.{3,}) ?\\1", "\\1", string, perl = TRUE)

  return(string)
}


extract_email_username = function(string)
  # Extract the username from an email address.
{
  sub("@.*", "", string)
}
