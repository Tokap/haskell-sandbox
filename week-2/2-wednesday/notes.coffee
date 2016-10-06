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
