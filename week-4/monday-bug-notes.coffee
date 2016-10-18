# Response when triggering broken FB verification for Bobdog

# POST https://app.dev.influential.co/api/campaign/539/offer/4662/verify 400 (Bad Request)

# The above falls in line with what we would expect, but the front end goes dead
# To me, this means the process is working on the backend but failing to trigger
# The image on the front end denoting "Not a valid Facebook URL"
# Errors are:
  # [ //ww.not.co ]            -> Not a valid URL
  # [ https://www.google.com ] -> Not a valid Facebook URL
  # [ BLANK ]                  -> This feild is required
