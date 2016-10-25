# ----- This is the chain of the main structure that feeds the thing that
# ----- filters out pending

const _getOffers = JW.pipe(
  [
    JwCampaign.getOffers
  , JwConceptElement.getByCampaignId
  , getByCampaignId
  , getStatesByCampaignId
  ]
, combineOffers
);

# --------------------- JwCampaign.getOffers ----------------------------

const getOffers = {
  awesomize: (v) => ({
    campaign_id: {
      read    : R.path([ 'data', 'campaign_id' ])
    , validate: [ v.required, v.isInt ]
    }
  })

, io: (req, data) => ({
    campaign_offers: CampaignOffer.getByCampaign(req.mysql)(data.campaign_id)
  })
}

# ---------------   JwConceptElement.getByCampaignId ---------------------

const getByCampaignId = {
  awesomize: (v) => ({
    campaign_id: {
      read    : R.path([ 'params', 'campaign_id' ])
    , validate: [ v.required, v.isInt ]
    }
  })

, io: (req, data) => ({
    concept_elements: req.DO.ConceptElement.getByCampaignId(req.mysql2, data.campaign_id)
  })
}

# --------- same-file getByCampaignId ------------
const getByCampaignId = {
  awesomize: (v) => ({
    campaign_id: {
      read: R.path(['data', 'campaign_id'])
    , validate: [ v.required ]
    }
  })
, io: (req, data) => ({
    offer_concept_ideas: req.DO.OfferConceptIdea.getByCampaignId(
      req.mysql2
    , data.campaign_id
    )
  })
}
