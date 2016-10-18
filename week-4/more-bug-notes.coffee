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
