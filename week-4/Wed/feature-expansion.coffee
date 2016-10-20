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

# ------------------------------------------------------------------
const countCampaignConcepts = {
  transform: (req, data) => {
    if (R.length(data.campaign_offers) > 0) {
      return R.length(data.campaign_offers[0].offer_concepts);
    }
    return 0;
  }
}
