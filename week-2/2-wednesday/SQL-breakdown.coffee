Bluebird    = require 'bluebird'
{ mysql }   = require '../../../lib/container.coffee'

module.exports  = (sa) -> new Bluebird (resolve, reject) ->

  sql       = '''
    SELECT
      COUNT `sa`.`id`                      AS `total`,
      `sat`.`name`                         AS `network`,
      EXTRACT (MONTH FROM `sa`.`created`)  AS `mon`,
      EXTRACT (YEAR FROM `sa`.`created`)   AS `yr`
    FROM `social_account` AS 'sa'
    JOIN `social_account_type` AS `sat`
      ON `sa`.`social_account_type_id` = `sat`.`id`
   WHERE `sa`.`active` = 1
     AND `sa`.`indicator` IS NULL
   GROUP BY EXTRACT(YEAR FROM `sa`.`created`),
            EXTRACT (MONTH FROM `sa`.`created`),
            `sa`.`social_account_type_id`
  '''

  mysql.query sql, [ sa ], (err, results) ->
    if err then return reject err

    if not results[0] then return reject new Error('NotFound')

    return resolve results[0]

###### EXPORT THE CONTENT? #######
# module.exports  =
#
#   createForCampaign   : (campaign_id) -> _run createForCampaign(campaign_id)
#   createForAffiliate  : (user_id) -> _run createForAffiliate(user_id)
#   createForBlessedCampaigns : -> _run createForBlessedCampaigns()
