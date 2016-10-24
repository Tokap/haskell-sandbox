router.get '/brand-manager/:brandManagerId/campaign', Campaign.searchBrand
 # Search Brand result out of middleware/campaign.coffee

searchBrand          = (req, res, next) ->
  deps  =
    db      : req.mysql

  Bluebird.props({
    network_type      : req.param 'network_type', null
    is_blessed        : req.param 'is_blessed', null
    starts_with       : req.param 'starts_with', null
    brand_user        : req.paramUser
    open              : req.param 'open', null
    page              : req.param 'page', 1
    limit             : req.param 'limit', 10
    current_user_id   : req.user.id
  })

  .then Campaign.search(deps)
  # Tremendous function, shown below this function.
  # I think I'm largely unconcerned with this.
  # I care about what it does with the results of this.

  .then _filterCampaignSearchForBrand
  # _filterCampaignSearchForBrand = (results) ->
    # _filterCampaignForBrand campaign for campaign in results.rows
    # return results

# ADD MY ADDITION THROUGH THE FUNCTION BELOW OR A MOCK ONE TO ADD ATTRIBUTE
  .then mapOnRows(_addChangeRequestCount(req.mysql))
  # from lib/domain/helpers.coffee
  # mapOnRows = (fn) -> (results) ->
    # Bluebird.resolve(results.rows)
    # .map fn
    # .then (rows) -> R.merge results, rows :rows

  .then res.json.bind(res)
  # Then ship it

  .catch next


# -------------- SIDE-NOTES REGARDING DESTINATION:  -----------------
# -------------- How to call our existing promise function  -----------------
# JwIdea            = require '../middleware/jw.offer-concept-idea.js'
JwIdea.countExternalPending (_makeReq campaign_id: campaign_id)

_makeReq = (data) -> R.assoc('data', data, req)





# -------------- BACK TO THE SEARCH FUNCTION:  -----------------
# -------------- I Think this is the important part  -----------------
# -------------- _addChangeRequestCount  -----------------
_addChangeRequestCount = (db) -> (campaign) ->
  Campaign.getChangeRequestCount(db)(campaign.id) # I get access to ID #LOOK-HERE-PATRICK
  # getChangeRequestCount is query show in named file

  # Below grabs the attributes listed out of the query.
  .then R.compose(
    renameKeys({'count':'pending_change_request_count'}),
    R.pickAll([
      'pending_brand_approval',
      'pending_campaign_manager_approval',
      'count'
    ])
  )
  .then R.merge(campaign)

# -------------- _filterCampaignForBrand -----------------
_filterCampaignForBrand = (campaign) ->
  blacklist = [
    'budget'
    'budget_pledged'
    'budget_spent'
    ]
  delete campaign[key] for key in blacklist
  return campaign





# -------------- Campaign.search: Large Query -----------------
# Campaign is in /domain/campaign.coffee
search  = _domain.search {
  searchParamParser       : (deps) -> (request) ->

    if request.brand_user?.id
      request.brand_user_id = request.brand_user.id
      delete request.brand_user

    request
  searchFilterBuilder     : (sql_factory) -> (params) ->
    select  = ''
    where   = ''
    order   = ''
    join    = ''

    notNil  = R.compose(R.not, R.isNil)

    if notNil params.network_type
      where  += ' AND `social_account_type`.`slug` = :network_type '

    if notNil params.is_blessed
      filter  = if parseInt(params.is_blessed, 10) is 1 then 1 else 0
      where += " AND `campaign`.`is_blessed` = #{( filter )} AND `campaign`.`closed` = 0 "

    if notNil params.closed
      filter  = if parseInt(params.closed, 10) is 1 then 1 else 0
      where += " AND `campaign`.`closed` = #{( filter )} "

    if notNil params.starts_with
      params.starts_with = params.starts_with + '%'

      where += ' AND `campaign`.`title` LIKE :starts_with '

    if notNil params.current_user_id

      select += '''
        MAX(`permission_level`.`level`) AS `permission_level`
      '''

      join += '''
       JOIN (
          SELECT
          	`permission`.`id` AS `id`,
          	`permission`.`user_id` AS `user_id`,
          	`permission`.`campaign_id` AS `campaign_id`,
          	`permission`.`permission_level` AS `permission_level`,
          	`permission`.`deleted` AS `deleted`,
          	`permission`.`deleted_timestamp` AS `deleted_timestamp`
          FROM `campaign_user_permission_level` AS `permission`
          WHERE
            `permission`.`user_id` = :current_user_id
           	AND `permission`.`deleted` = 0
        ) AS `permission`
          ON (`permission`.`campaign_id`  = `campaign`.`id` OR `permission`.`campaign_id`  = 0 )

        JOIN `permission_level`
          ON `permission_level`.`id` = `permission`.`permission_level`
          AND `permission_level`.`level` > 0
      '''

    if notNil params.user_id
      if select then select += ', '
      select += '''
          MAX(`permission_level`.`level`)  AS `permission_level`
        '''

      join += '''
          LEFT JOIN `user_role` AS `campaign_user_role`
            ON `campaign_user_role`.`user_id` = :user_id
            AND `campaign_user_role`.`deleted` IS NULL
          LEFT JOIN `role_object_max_permission_level` AS `max_perm`
            ON `campaign_user_role`.`role_id` = `max_perm`.`role_id`
            AND `max_perm`.`object_name` = 'campaign'
          LEFT JOIN `permission_level` AS `max_level`
            ON `max_perm`.`permission_level_id` = `max_level`.`id`
          LEFT JOIN `campaign_user_permission_level` AS `permission`
            ON `permission`.`user_id` = :user_id
            AND `permission`.`campaign_id` = `campaign`.`id`
            AND `permission`.`deleted` <> 1

          LEFT JOIN `permission_level`
            ON `permission`.`permission_level` = `permission_level`.`id`
            AND (
              `permission_level`.`level` IS NULL
              OR
              (
                `permission`.`campaign_id` = `campaign`.`id`
                AND
                `permission_level`.`level` <= `max_level`.`level`
              )
            )
        '''

    if notNil params.open
      open = parseInt(params.open, 10)
      if open is 1
        where += ' AND (
          (
            `campaign`.`is_scheduled` = 0
            AND
            `campaign`.`offer_end_date` > NOW()
          )
          OR (
            `campaign`.`is_scheduled` = 1
            AND
            `campaign`.`run_end_date` > NOW()
          )
        ) AND `campaign`.`closed` = 0

      '
      if open is 0
        where += ' AND (
          (
            `campaign`.`is_scheduled` = 0
            AND
            `campaign`.`offer_end_date` < NOW()
          )
          OR (
            `campaign`.`is_scheduled` = 1
            AND
            `campaign`.`run_end_date` < NOW()
          )
        ) OR `campaign`.`closed` = 1
      '
    if params.order is 'offer_start'
      order = 'ORDER BY `campaign`.`offer_start_date` DESC'

    return sql_factory(select, join, where,order)
}
