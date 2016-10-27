#LOOK-HERE-PATRICK
# jw.offer-concept-idea.js
# ---------------- NEW-ADDITION-FOR-FEATURE #LOOK-HERE-PATRICK--------
const countExternalPendingConcepts = {
  transform: (req, data) => {
    return R.assoc('count', _countExternalPending(data.campaign_offers), data);
  }
}
# --------------------------------------------------------------------

# --------------------------------------------------------------------
# --------------------- Unused Custom Route --------------------------
# --------------------------------------------------------------------
router.get '/campaign-concept/:campaign_id/offer/count', JW(
  getParameters('params', ['campaign_id'])
  # From jw.offer-concept-idea
  JwIdea.getOffers
  # JwIdea.filterClientReady
  JwIdea.countExternalPendingConcepts
)

# Our final product produces a count via:
concept_external_pending_count

checkForConceptExistence

# ------------- EXISTING JW ROUTE ---------------
router.get '/campaign-concept/:campaign_id/element', JW(
  getParameters('params', ['campaign_id'])
  JwConceptElement.getByCampaignId
)

# JS SYNTAX
const getByCampaignId = {
  awesomize: (v) => ({
    campaign_id: {
      read    : R.path([ 'data', 'campaign_id' ])
    , validate: [ v.required, v.isInt ]
    }
  })

, io: (req, data) => ({
    concept_elements: req.DO.ConceptElement.getByCampaignId(req.mysql2, data.campaign_id)
  })
}

# ------------------------------------------------------------------------------

const getUsersForComments = {
  awesomize: (v) => ({
    comments: {
      read: R.path(['data', 'comments'])
    , validate: [ v.isArray() ]
    }
  })

, io: (req, data) => ({
    actors: req.DO.Actor.getByIdList(req.mysql2)(
      R.pluck('actor_id', data.comments)
    )
  , authors: req.DO.User.getByIdList(req.mysql2)(
      R.pluck('author_id', data.comments)
    )
  , roles: req.DO.Role.getByUserIdList(req.mysql2)(
      R.pluck('author_id', data.comments)
    )
  })

, transform: (req, data) => ({
    comments: R.map( mergeUsersForComment(data), data.comments)
  })
}

# ------------- SCRAP PAPER ---------------
router.get '/brand-management/:brandManagerId/campaign', JW(
  getParameters('params', ['brandManagerId'])
)
