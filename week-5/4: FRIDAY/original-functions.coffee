#Original Two functions used to count pending concepts

_addPendingConceptCount = (req) -> (campaign) ->
  JwIdea.countInternalApprovedAndExternalPending(
    R.assoc('data', campaign_id: campaign.id, req)
  )
  .then (count) ->
    R.assoc('concept_external_pending_count', count, campaign)

_addPastConceptCount = (req) -> (campaign) ->
  JwIdea.countPastConcepts(
    R.assoc('data', campaign_id: campaign.id, req)
  )
  .then (count) ->
    R.assoc('past_concept_count', count, campaign)
