# THE EXAMPLES AND EXPLANATONS BELOW TAKEN FROM:
# -----------------------------------------------------------------------------#
# https://github.com/influentialpublishers/node-api/blob/dev/lib/promise-mysql.coffee
# -----------------------------------------------------------------------------#
# Run a raw query.
# ----------------
# @param {MySql} db
# @param {string} sql
# @param {string} method
# @param {Object | Arary} params
# @return {Promise::*}
query = (db, sql, method) -> (params) -> new Bluebird (res, rej) ->

  debug("Running Query (#{method}): #{sql}")
  debug("Params (#{method}): #{JSON.stringify(params)}")

  db.query sql, params, (err, results) ->
    if err then return rej Err.MySqlError(err, method or 'MySql.query')
    return res results

# Run a select query against the databsae.
# ----------------------------------------
# @param {MySql} db
# @param {string} sql
# @param {string} method
# @param {Object | Array} params
# @return {Promise::Array}
select = (db, sql, method) -> (params, no_cache) ->
  sql         = noCacheQuery sql if no_cache
  run         = query(db, sql, method)

  _transform  = (row) ->
    NullifyEmpty.transform(Nestify.transform(JSONParse.transform(row)))

  run(params).then (results) -> (_transform row for row in results)

# Retrieve records using a simple where statment constructed from params.
# -----------------------------------------------------------------------
# @param {MySql} db
# @param {string} table
# @param {string} select
# @param {string} method
# @param {Object} params
# @return {Promise::Object}
getWhereParams  = (db, table, project = '*', method = 'MySql::getWhere') ->
  sql = " SELECT #{project} FROM #{table} WHERE 1 "

  (params, no_cache) ->
    sql += (" AND #{key} = :#{key}" for key in Object.keys params).join ''
    sql = interpolate(db)(sql)(params)

    select(db, sql, method)(params, no_cache)


# Retrieve a record by identifier.
# --------------------------------
# @param {MySql} db
# @param {string} sql
# @param {string} method
# @param {int} id
# @return {Promise::Object}
getOneById = (db, sql, method) ->
  run    = getOne(db, sql, method)

  return (id) -> run(id).then (result) ->
    return result unless result?.id + '' isnt id + ''
    throw Err.NotFoundError method, id

# MySQL Transaction Start.
# ------------------------
# @param {MySQL} db
# @param {http.IncomingMessage} req
# @return {Promise::<http.IncomingMessage>}
transactionStart  = (db, req) -> new Bluebird (res, rej) ->
  db.beginTransaction (err) ->
    if err then return rej Err.MySqlError err, 'MySql.transactionStart'

    req.inMySqlTransaction  = true

    return res req

# -----------------------------------------------------------------------------#
# -----------------------------------------------------------------------------#
# -----------------------------------------------------------------------------#
DomainFactory   = require '../../lib/domain.coffee'

_domain             = DomainFactory
  baseTable : 'campaign_collateral'

  fieldFactory : (v, db) ->
    'campaign_id':
      immutable: true
      validation: [ v.isInt, v.required ]
    'content_type':
      validation: [ v.required ]
    'link_type':
      validation: [ v.required ]
    'title':
      validation: []
    'url':
      validation: [ v.required ]


  sqlFactory        : (select, join, where, order) -> "
    SELECT
      `campaign_collateral`.`id`              AS `id`,
      `campaign_collateral`.`campaign_id`     AS `campaign_id`,
      `campaign_collateral`.`content_type`    AS `content_type`,
      `campaign_collateral`.`link_type`       AS `link_type`,
      `campaign_collateral`.`title`           AS `title`,
      `campaign_collateral`.`url`             AS `url`,
      `campaign_collateral`.`created`         AS `created`
    FROM `campaign_collateral`
      #{( if join then ' ' + join else '' )}
    WHERE
      `campaign_collateral`.`deleted` IS NULL
      AND `campaign_collateral`.`link_type` = 'collateral'
      #{( if where then ' ' + where else '' )}
    "

getByCampaignId = (db) -> (campaign_id) ->
  join  = 'JOIN `campaign` ON `campaign`.`id` = `campaign_collateral`.`campaign_id` '
  where = ' AND `campaign`.`id` = ? '
  query   = _domain.get(db, null, join, where, null, 'CampaignWebsite::getByCampaignId')
  return query [ campaign_id ]

module.exports =
  save : _domain.save({})
  del  : _domain.softDelete({})

  getByCampaignId : getByCampaignId
