# client/app/components/cards/brand-campaign-card.react.coffee
      div
        className: 'campaign-status-wrapper'

# client/app/components/brand-management/campaign/search/searchCampaigns.react.coffee

# client/app/components/brand-management/campaign/details/campaignActions.react.coffee
# HOW THE EXISTING COUNTER WORKS:
        "Content for Approval (#{_getChangeRequestCount(@props)})"

# Lines 53 - 57
_getChangeRequestCount = _.pathOr 0, [
  'campaign',
  'pending_brand_approval'
]

# _                   = require 'ramda'
# ------------------------------------------------------------------------------
# -------------------- Doc Containing Concepts----------------------------------
# ------------------------------------------------------------------------------
# client/app/dashboard/brand-management/campaign/details/searchCampaigns.react.coffee

# HOW THE EXISTING COUNTER WORKS AND PULSES:
      div
        className: 'button-div'
        Link
          className: [
            'btn'
            'btn-primary'
            'pulse-yellow' if row.pending_brand_approval > 0
            'btn-disabled' unless CurrentUserStore.isBrandViewerForCampaign(row)
          ].join(' ')
          to: 'brand-management-campaign-change-requests'
          params:
            campaign_id: row.id
          if row.pending_brand_approval > 0
            "Content for Approval (#{row.pending_brand_approval})"
          else 'Content for Approval'

# HOW THE SECTION REQUIRING UPDATE LOOKS:
      div
        className: 'button-div'
        Link
          className: 'btn btn-primary'
          to: 'brand-management-offer-concepts'
          params:
            campaign_id: row.id
          'Concepts for Approval'


# ----------------------
# query and find out how many are pending: (#{row.pending_brand_approval})"
  componentWillReceiveProps: (props) ->
    props.query['limit'] = @state.page_limit
    @search(props)

# ---------------------------------------------------------------------
# -----------------------Let's find a query: --------------------------
# ---------------------------------------------------------------------
# pending_campaign_manager_approval is what gets used in front & backend

###### NODE CHAIN OF USE: ##########
#  ---- THE DATA IS AN OBJECT -----
{ campaign_offers:
  [
    {
      id: '4659',
      parent_id: null,
      is_scheduled: 0,
      is_interested: 1,
      terms_accepted: 1,
      post_attempted: 0,
      post_id: null,
      post_confirmed: 0,
      scheduled_time: null,
      is_schedule_reserved: 0,
      is_schedule_time_locked: 0,
      character_count: '0',
      sent: 0,
      campaign_offer_status_id: '1',
      created: 1476831123,
      updated: 1476831145,
      deleted: null,
      network_slug: 'facebook',
      network: 'Facebook',
      campaign: [Object],
      bitly: null,
      twitter_card: null,
      terms: null,
      creator: [Object],
      affiliate: [Object],
      social_account: [Object],
      price: [Object],
      header: null,
      copy: null,
      media: [Object],
      type: [Object],
      forward_url: null,
      offer_concepts: [Object],
      internal_status: 'approved',
      external_status: 'pending'
    }
  ]
}

# ---- offer_concepts Object from above: -----
[
  {
    id: '12',
    theme: 'Nevada Christmas',
    material: 'Pictures of festive desert scenes',
    created: 1476905039,
    campaign_id: '538',
    ideas: [ [Object]]
  },
  {
    id: '13',
    theme: '2016 Presidential Debate',
    material: '2 people yelling nonsense for about an hour.',
    created: 1476909649,
    campaign_id: '538',
    ideas: [ [Object] ]
  }
]
# ------------------------------------------------------------------
const countCampaignConcepts = {
  transform: (req, data) => {
    if (R.is(Object, data)) {
      console.log('---- THE DATA IS AN OBJECT -----', data);
      console.log('---- Offer_concepts -----', data.campaign_offers[0].offer_concepts);
      return R.length(data.campaign_offers[0].offer_concepts);
    }
    return 0;
  }
}

# I'm copying the functionality of: pending_brand_approval
# ------------------------------------------------------------------
# [API] - jw.offer-concept.idea.js (lines 245 - 252)
const countCampaignConcepts = {
  transform: (req, data) => {
    if (R.length(data.campaign_offers) > 0) {
      return R.length(data.campaign_offers[0].offer_concepts);
    }
    return 0;
  }
}
# ------------------------------------------------------------------
# [API] - campaign-concept.coffee
# NEW-ADDITION-FOR-FEATURE (lines 46 - 52)
  router.get '/campaign-concept/:campaign_id/offer/count', JW(
    getParameters('params', ['campaign_id'])
    JwIdea.getOffers
    JwIdea.filterClientReady
    JwIdea.countCampaignConcepts
  )

# ------------------------------------------------------------------
# ------ This is how pending_brand_approval is stored --------------
# ------------------------------------------------------------------
# lines 873 - 896 in domain/campaign.coffee
getById = (db, no_cache) ->
  _getById               = _domain.getById(db, no_cache)
  _getChangeRequestCount = getChangeRequestCount(db)
  _getCampaignStatsCount = getCampaignStatsCount(db)
  _getCampaignLegalCount = getCampaignLegalCount(db)

  (campaign_id) ->
    Bluebird. all [
      _getById(campaign_id)
      _getChangeRequestCount(campaign_id)
      _getCampaignStatsCount(campaign_id)
      _getCampaignLegalCount(campaign_id)
    ]
    .spread (campaign, request_count, stats_count, legal_count) ->
      R.merge(campaign,
        pending_change_request_count: parseInt request_count.count, 10
        invalid_change_request_count: parseInt request_count.invalid, 10
        pending_brand_approval:
          parseInt request_count.pending_brand_approval, 10
        pending_campaign_manager_approval:
          parseInt request_count.pending_campaign_manager_approval, 10
        stats_count                 : parseInt stats_count, 10
        legal_count                 : parseInt legal_count, 10
      )

# ------------------------------------------------------------------
# ---- In reviewing the database structure, our count is: ----------
# ------------------------------------------------------------------

# campaign-concept-element

# The issue becomes checking the status of this particular item.
# Is it pending? Approved? etc.
# Current query tied to route will return the number for ALL, but just pending

# ------------------------------------------------------------------
# ---- In reviewing the database structure, our count is: ----------
# ------------------------------------------------------------------
# Old function: pending_brand_approval
# --------- countExternalPending --------
const countExternalPending      = _offersStatusCount(_countExternalPending);

const _countExternalPending     = countIf(R.propEq('external_status', PENDING))

const _offersStatusCount = (fn) => (data) => {
  return JW.promisify(
    getOffers
  , { transform: (req, data) => {
        return R.assoc('count', fn(data.campaign_offers), data);
      }
    }
  )(data).get('count');
}

# Tags I've added to find code:

#Newly-added-code
# ADDED-FOR-TEST

# ----- Use Example ------
# email-send.coffee
_makeReq = (data) -> R.assoc('data', data, req)
  # external pending count
  when NOTIFICATION_TYPE.CONCEPT_CLIENT_APPROVED
    campaign_id = _campaignIdFromNotificationContext context
    JwIdea.countExternalPending _makeReq campaign_id: campaign_id

# ------------------------------------------------------------------
# ---- Below Works: ----------
# ------------------------------------------------------------------
  router.get '/campaign-concept/:campaign_id/offer/count', JW(
    getParameters('params', ['campaign_id'])
    JwIdea.getOffers
    JwIdea.filterClientReady
    JwIdea.countCampaignConcepts
  )

# NEW-ADDITION-FOR-FEATURE - I doubt the veracity of this function
const countCampaignConcepts = {
  transform: (req, data) => {
    let count = 0;
    if (R.length(data.campaign_offers) > 0) {
      count = R.length(data.campaign_offers[0].offer_concepts);
    }
    return {count: count};
  }
}
# --------------------------------------------------------------------

# ------------------------------------------------------------------
# ---- Below Doesnt Work: ----------
# ------------------------------------------------------------------
  router.get '/campaign-concept/:campaign_id/offer/count', JW(
    getParameters('params', ['campaign_id'])
    JwIdea.countExternalPending
  )

# ------------------------------------------------------------------
# ---- campaign.coffee returns pending_concept_approval ----------
# ------------------------------------------------------------------

  getById = (db, no_cache) ->
  _getById               = _domain.getById(db, no_cache)
  _getChangeRequestCount = getChangeRequestCount(db)
  _getCampaignStatsCount = getCampaignStatsCount(db)
  _getCampaignLegalCount = getCampaignLegalCount(db)

  (campaign_id) ->
    Bluebird. all [
      _getById(campaign_id)
      _getChangeRequestCount(campaign_id)
      _getCampaignStatsCount(campaign_id)
      _getCampaignLegalCount(campaign_id)
    ]
    .spread (campaign, request_count, stats_count, legal_count) ->
      R.merge(campaign,
        pending_change_request_count: parseInt request_count.count, 10
        invalid_change_request_count: parseInt request_count.invalid, 10
        pending_brand_approval:
          parseInt request_count.pending_brand_approval, 10
        pending_concept_approval:
          JwIdea.countExternalPending()
        pending_campaign_manager_approval:
          parseInt request_count.pending_campaign_manager_approval, 10
        stats_count                 : parseInt stats_count, 10
        legal_count                 : parseInt legal_count, 10
      )

#LOOK-HERE-PATRICK

# ------------------------------------------------------------------
# ---- After speaking with Jason, he advised updating to:  ----------
# ------------------------------------------------------------------
const _offersStatusCount = (fn) => (data) => {
  return JW.pipe(
    getOffers
  , { transform: (req, data) => {
        return R.assoc('count', fn(data.campaign_offers), data);
      }
    }
  )(data).get('count');
}

# &&

const countInternalPending      = _offersStatusCount(_countInternalPending);
const countExternalPending      = _offersStatusCount(_countExternalPending);
const countHasNoIdeas           = _offersStatusCount(_countHasNoIdeas);
const countNotInternalApproved  = _offersStatusCount(_countNotInternalApproved);

const countInternalPendingPromise      = JW.promisify(_offersStatusCount(_countInternalPending));
const countExternalPendingPromise      = JW.promisify(_offersStatusCount(_countExternalPending));
const countHasNoIdeasPromise           = JW.promisify(_offersStatusCount(_countHasNoIdeas));
const countNotInternalApprovedPromise  = JW.promisify(_offersStatusCount(_countNotInternalApproved));


# ------- crudMap.coffee from the node-web-client ---------
# Campaign Concept External Pending

CampaignConceptExternalPending:
  read : '/campaign-concept/:campaign_id/offer/count'

# ------- searchCampaigns.react.coffee from the node-web-client ---------
# Campaign Concept External Pending

BrandCampaignSearch = SearchHandlerTemplate
  displayName: 'BrandCampaignSearch'
  mixins: [
    CrudReact('BrandCampaignSearch')
    Router.Navigation
    FormMixin(ErrorMsg)
  ]


# To fix outter box containing buttons
# .brand-campaign-card .additional
element.style {
  height: 188px;
}
