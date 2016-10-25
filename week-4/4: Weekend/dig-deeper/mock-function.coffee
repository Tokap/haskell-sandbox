# ---------------------- Model Function ----------------------
# ------------ _addChangeRequestCount ------------------------
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

# ---- Calling countExternalPending ---------
JwIdea.countExternalPending (_makeReq campaign_id: campaign_id)
_makeReq = (data) -> R.assoc('data', data, req)

# ---------------------- MY FUNCTION ---------------------
_addPendingConceptCount = (db) -> (campaign) ->
  JwIdea.countExternalPending (_makeReq campaign_id: campaign.id)
