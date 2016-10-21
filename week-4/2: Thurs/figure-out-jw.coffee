#NEW-ADDITION-FOR-FEATURE #LOOK-HERE-PATRICK:
# Originally from campaign-concept.coffee
  router.get '/campaign-concept/:campaign_id/offer/count', JW(
    getParameters('params', ['campaign_id'])
    JwIdea.getOffers
    JwIdea.filterClientReady
    JwIdea.countExternalPendingConcepts
  )

# ------ JwIdea.getOffers ------
const getOffers = _useDepOr('campaign_offers', _getOffers)

# ---- Function Being Used:
const _useDepOr = R.curry((dep_name, other) => JW.branch(
  (req, data) => R.pathSatisfies(notNilOrEmpty, ['deps', dep_name], data)
, { transform: (req, data) => {
      return R.set(R.lensProp(dep_name), R.path(['deps', dep_name], data), data)
    }
  }
, other
))

const _getOffers = JW.pipe(
  [
    JwCampaign.getOffers
  , JwConceptElement.getByCampaignId
  , getByCampaignId
  , getStatesByCampaignId
  ]
, combineOffers
);
##### ---------- JwCampaign.getOffers ------
      const JwCampaign.getOffers = {
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

      # const CampaignOffer = require('../domain/campaign/offer.coffee')
      CampaignOffer.getByCampaign   = (db) ->
        query   = _domain.getWhereNoCache(db, ' AND `campaign`.`id` = ? ')

        return (campaign_id) ->
          query [ campaign_id ]

          # _domain = DomainFactory
          # DomainFactory = require '../../lib/domain'
            DomainFactory.getWhereNoCache = (db, where, method = 'Domain.getWhereNoCache') ->
              sql   = MySql.noCacheQuery _sqlFactory(null, null, where)
              return MySql.select(db, sql, method)

            # Uses:
            _sqlFactory     = options.sqlFactory

              # MySql = require './promise-mysql'
              noCacheQuery    = (sql) ->
                random        = randomBytes(8).toString('hex') + (new Date()).getTime()
                return "#{sql} -- #{random}"

              # &&
              select = (db, sql, method) -> (params, no_cache) ->
                sql         = noCacheQuery sql if no_cache
                run         = query(db, sql, method)

                _transform  = (row) ->
                  NullifyEmpty.transform(Nestify.transform(JSONParse.transform(row)))

                run(params).then (results) -> (_transform row for row in results)

                # uses:
                # NullifyEmpty    = require './promise-mysql/nullify-empty.coffee'
                NullifyEmpty.transform = (row) ->
                  row[key] = _nullify item for key, item of row when isPlainObject item
                  return row

                  #uses:
                  { isPlainObject }   = require "lodash"
                  # &&
                  _nullify  = (item) ->
                    if item.hasOwnProperty "id"
                      return null unless item.id > 0

                    is_nullifiable = true
                    for key, value of item when value isnt null
                      if isPlainObject value
                        item[key] = _nullify value
                      if item[key] isnt null
                        is_nullifiable = false

                    if is_nullifiable
                      return null
                    else
                      return item
                 # &&
                  Nestify.transform = (obj) ->
                    _splitKey obj[key], obj, key for key in Object.keys(obj) when key.indexOf(".") > 0
                    return obj
                # &&
                # JSONParse = require './promise-mysql/json-parse.coffee'
                JSONParse.transform = (row) ->
                  for key, value of row when key.indexOf("JSON:") is 0
                    try
                      row[key.substr 5] = JSON.parse value
                      delete row[key]
                    catch err
                      console.log err, value
                      throw err

                  return row

##### ---------- JwConceptElement.getByCampaignId ------
      const JwConceptElement.getByCampaignId = {
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
##### ---------- getByCampaignId ------
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

##### ---------- getStatesByCampaignId ------
      const getStatesByCampaignId = {
        awesomize: (v) => ({
          campaign_id: {
            read: R.path(['data', 'campaign_id'])
          , validate: [ v.required ]
          }
        })

      , io: (req, data) => ({
          offer_concept_idea_states: req.DO.StateOfThings.getOfferConceptIdeaStatesForCampaign(
            req.mysql2
          , data.campaign_id
          )
        })
      }

# ------ JwIdea.filterClientReady ---------
# Checks to see if client ready. This may not even be needed. Results are same
# FINAL STEP:
const filterClientReady = {
  transform: (req, data) => R.over(
    R.lensProp('campaign_offers')
  , R.filter(R.propEq('internal_status', APPROVED))
  )(data)
}

# --- I wrote this, and it simply counts the items that meet our requirement
# FINAL STEP:
# ------ JwIdea.countExternalPendingConcepts ------
const countExternalPendingConcepts = {
  transform: (req, data) => {
    return {count: _countExternalPending(data.campaign_offers)}
  }
}

const _countExternalPending = countIf(R.propEq('external_status', PENDING))
