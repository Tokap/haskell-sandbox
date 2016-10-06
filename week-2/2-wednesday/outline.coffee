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

####### BASICALLY:
# Run query
# Add months together as they progress
# Return object representing data. Possibly:

# June.2015 = { twitter: 196, instagram: 169 }
# twitter = { June.2015: 196, July.2015: 478 }
# 2015 = { june: { twitter: 196, instagram: 169 }, july: {...} }
# june = { 2015: { twitter: 196, instagram: 169, ... }, 2016: { twitter: 297, instagram: 436, ... } }
