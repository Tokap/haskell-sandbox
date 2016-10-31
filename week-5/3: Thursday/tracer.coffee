JW                  = require 'jigawatt'
JwCampaign          = require '../middleware/jw.campaign.js'
JwConcept           = require '../middleware/campaign/jw.concept.js'
JwConceptElement    = require '../middleware/campaign/jw.concept-element.js'
JwIdea              = require '../middleware/jw.offer-concept-idea.js'
JwState             = require '../middleware/jw.state-of-things.js'
{ getParameters }   = require '../middleware/helpers.js'

# JS FILE
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

# @ 10:50 I am attempting to trace back where the DO is created for this example
# This is being done by searching ConceptElement
# memory-objects/campaign/concept-element.js
const getByCampaignId = R.curry((store, id) => {
  return store.getWhere('concept-element', { campaign_id: R.equals(id) })
})


#mysql-objects/campaign/concept-element.js
# This is what is being called by the DO object
const [req.DO.ConceptElement.]getByCampaignId = R.curry((mysql, id) => Query.select(
    mysql
  , sql_factory({ where: ' AND `campaign_concept_element`.`campaign_id` = ? ' })
  , [id]
  )
);

const sql_factory = SqlFactory('campaign_concept_element', {
  select: 'SELECT '
      + '`campaign_concept_element`.`id` AS `id`,'
      + '`campaign_concept_element`.`theme`,'
      + '`campaign_concept_element`.`material`,'
      + '`campaign_concept_element`.`created`,'
      + '`campaign_concept_element`.`campaign_id`'

, where: ' WHERE `campaign_concept_element`.`deleted` IS NULL '

});

const { Query, SqlFactory } = require('pimp-my-sql')
