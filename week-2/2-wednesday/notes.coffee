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

# -----------------------------------------------------------------------------#
