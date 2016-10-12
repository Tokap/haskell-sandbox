R                 = require 'ramda'
Bluebird          = require 'bluebird'
moment            = require 'moment'
fs                = require 'fs'
MySql             = require '../../lib/promise-mysql.coffee'
{ mysql, config } = require '../../lib/container.coffee'

queryString       = (date) -> """
  SELECT
    COUNT(`sa`.`id`) AS `total`,
    `sat`.`name` AS `name`
  FROM `social_account` AS `sa`
  JOIN `social_account_type` AS `sat`
    ON `sa`.`social_account_type_id`= `sat`.`id`
  WHERE
    (`sa`.`created` < "#{date + '-01 00:00:00'}")
    AND (
            (`sa`.`active` = 1 AND `sa`.`indicator` IS NULL)
        OR (`sa`.`active` = 0 AND `sa`.`updated` >= "#{date + '-01 00:00:00'}")
    )
  GROUP BY `sa`.`social_account_type_id`;
"""

startDateQuery     = """
  SELECT MIN(created) as startDate
  FROM social_account
"""
# Date Manipulation
_advanceMonth = (momentObject) ->
  return moment(momentObject).add('1', 'months').format('YYYY-MM')

_reduceMonth = (momentObject) ->
  return moment(momentObject).subtract('1', 'months').format('MMM-YYYY')

_formatUnixDate = (dateObject) ->
  moment.unix(dateObject).format("YYYY-MM")

_loopToNow = (startingDate) ->
  startDate = moment().format(startingDate)
  now = moment().format("YYYY-MM")
  startDate = _advanceMonth(startDate) until startDate > now
# Queries & Promises
_getData  = (sql) ->
  MySql.query(mysql, sql)()
  .then (result) ->
    return result

_getStartDate  = (sql) ->
  MySql.query(mysql, sql)()
  .then (result) ->
    return _formatUnixDate result[0].startDate

_getAllPromises = (queryList) ->
  queryList.map (query) ->
    return _getData(query)

_getUserHome = ->
  process.env.HOME or process.env.USERPROFILE

_finalQueryProcess = ->
  startDate = _getStartDate(startDateQuery)
  datesForOutput = startDate.then(->
    startDate
      .then (results) -> _loopToNow(results).map (x) -> _reduceMonth(x)
    )
  datesForOutput.then(->
    startDate
      .then (results) -> _loopToNow results
      .then (results) -> [].concat.apply([], [queryString(date) for date in results])
      .then (results) -> Promise.all _getAllPromises(results)
      .then (results) ->
        results.map (x, i) ->
          x.map (y) -> fs.appendFileSync _getUserHome() + "/Desktop/social-network-data.csv", datesForOutput.value()[i] + " , " + y.name + " , " + y.total + "\n", "utf8"
        console.log "Output has been saved to Desktop as 'social-network-data.csv'"
        process.exit(23)
    )
# Run Process
_run      = ->
  console.log '---- Querying DB for Social Network Data -----'
  fs.writeFileSync _getUserHome() + "/Desktop/social-network-data.csv", "Date, Name, Total" + "\n", "utf8"
  _finalQueryProcess()

module.exports  = _run
