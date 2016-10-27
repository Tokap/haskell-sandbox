router.get '/:campaignId', Campaign.get


Campaign = require '../middleware/campaign'

Campign.get   = (req, res, next) ->
  deps      =
    db  : req.mysql

  Bluebird.resolve(req.campaign)

  .then CampaignDetail.get(deps)

  .then (detail) -> res.json detail

  .catch next
