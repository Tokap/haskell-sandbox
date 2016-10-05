##########################################################################
############ ORIGINAL SUGGESTED WORK FLOW ###############################
########################################################################
# Consider Moment library (may not be needed with updated query)
# Slowly increment the CLI Tool function
# Hello world
# Print x - Y months
# Print just one month
# Build merge structure

##########################################################################
############ BASED ON MY QUICK CHECK OF DATA ############################
########################################################################
# Took template and attempted to mock raw query in syntax provided
# (??) What kind of object does this return
# (??) How do I manipulate this kind of object to combine monthly totals
# (??) How can I test these programs/results in a non-destructive fashion

# Reviewed support libraries and proprietary functions
# (??) Need to understand what tools are being actively used in queries
# (??) Need to interact with those tools and test output

##########################################################################
############ WHAT MY APPLICATION IS GOING TO DO #########################
########################################################################

# Run query that gives data broken down by month created, resembling:
# Count	| Name	    | Month	Year
# --------------------------------
# 196	  | Twitter	  | 6	2015
# 169	  | Instagram |	6	2015
# 147	  | Vine	    | 6	2015
# 478	  | Twitter	  | 7	2015
# 302	  | Instagram	| 7	2015
# 167	  | Vine	    | 7	2015

# Each line from the original query represents just that month, so the data
# from month to month needs to be combined for each network. So:
# Starting on May 15, 2015 it would be like:
# May Twitter = May Twitter
# June Twitter = June Twitter + Previous Month TOTAL
# July Twitter = July Twitter _ Previous Month TOTAL
# Etc...
