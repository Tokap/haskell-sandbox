findByUrl             = (post_id, post_url, network_id) ->

  status_id = Facebook.parseStatusIdFromUrl(post_url)
  Facebook.getStatusById("#{network_id}_#{status_id}")
  .then (response) ->
    _updatePostWithResponse(response, post_id, post_url)
  .catch (e) ->
    _updatePostWithError(e, post_id)
    .then -> throw e

_updatePostWithResponse = (response, post_id, post_url) ->
  rethinkdb.run(rethinkdb.r.table('post').get(post_id).update({
      post_id         : response.id
      posted_date     : rethinkdb.r.epochTime(moment(response.created_time).unix())
      network_url     : post_url
      attempts        : rethinkdb.r.row('attempts').add(1)
      api_calls       : rethinkdb.r.row('api_calls').append(response)
      status          : 'live'
      updated         : new Date()
    })
  )

_updatePostWithError = (e, post_id) ->
  rethinkdb.run(rethinkdb.r.table('post').get(post_id).update({
      attempts        : rethinkdb.r.row('attempts').add(1)
      api_calls       : rethinkdb.r.row('api_calls').append( e.tin or e.toString() )
      updated         : new Date()
    })
  )

# FYI, from facebook.coffee
  Facebook.getStatusById = (post_id) ->
    url = "#{BASE_URL}#{post_id}?debug=all"

# @ 12:15PM on Tuesday
# ------------------------------------------------------
# ------------- OK, I think I got it -------------------
# ------------------------------------------------------
# Error is returning with:
# BadRequest: Could Not Get Facebook Status
# Front end has absolutely no reference to this error.
# Review backend to determine why

# ERROR OUTPUT:
# 0|influent | ----- LOOK MA! AN ERROR! BadRequest: Could Not Get Facebook Status
# 0|influent | POST /campaign/537/offer/4658/verify 400 3413.315 ms - 180

# ------------------------------------------------------
# ---------- PR Notes from Michal  ---------------------
# ------------------------------------------------------

# mszymulanski 16 minutes ago
# Pass in more information to the ValidationError. Look at node-api/lib/error.coffee.
# The constructor takes 3 arguments: messages, method, code. You can definitely
# add method. It's our convention to list the file and method where an error
# is being thrown:
Err.ValidationError(
   {post_url: 'invalid_facebook_post_url'}
   ' lib/facebook.coffee::getStatusById'
 )

# If there is anything wrong with this error, future developers will know
# where to look.

# .catch (e) -> catches all errors. If there is anything other than validation
# wrong with this method it would still throw a ValidationError.
# You should check.
.catch (e) ->
   if e.name is 'A Mark Zuckerberg error'
      throw Err.ValidationError(...)
   else throw e
