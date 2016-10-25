router.get '/brand-manager/:brandManagerId/campaign',
  Campaign.searchBrand

# ------- Campaign.searchBrand --------
searchBrand          = (req, res, next) ->
  deps  =
    db      : req.mysql

  Bluebird.props({
    network_type      : req.param 'network_type', null
    is_blessed        : req.param 'is_blessed', null
    starts_with       : req.param 'starts_with', null
    brand_user        : req.paramUser
    open              : req.param 'open', null
    page              : req.param 'page', 1
    limit             : req.param 'limit', 10
    current_user_id   : req.user.id
  })

  .then Campaign.search(deps)

  .then _filterCampaignSearchForBrand
  # _filterCampaignSearchForBrand = (results) ->
    # _filterCampaignForBrand campaign for campaign in results.rows
    # return results

    #LOOK-HERE-PATRICK #Add another mapOnRows with our function
  .then mapOnRows(_addChangeRequestCount(req.mysql))
  # from lib/domain/helpers.coffee
  # mapOnRows = (fn) -> (results) ->
    # Bluebird.resolve(results.rows)
    # .map fn
    # .then (rows) -> R.merge results, rows :rows

  .then res.json.bind(res)

  .catch next

# ------------ _addChangeRequestCount -------------
_addChangeRequestCount = (db) -> (campaign) ->
  #each row being passed in is a campaign
  Campaign.getChangeRequestCount(db)(campaign.id)

  .then R.compose(
    renameKeys({'count':'pending_change_request_count'}),
    R.pickAll([
      'pending_brand_approval',
      'pending_campaign_manager_approval',
      'count'
    ])
  )

  .then R.merge(campaign)
  # Then merge these updates into the original campaign object

# ------------ Campaign.getChangeRequestCount -------------
# Query in separate file
