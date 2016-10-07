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
  SELECT MIN(created) as StartDate
  FROM social_account
"""
################ DATE MANIPULATION FUNCTIONS ###################
iterateDate = (momentObject) ->
  return moment(momentObject).add('1', 'months').format('YYYY-MM')

reduceMonth = (momentObject) ->
  return moment(momentObject).subtract('1', 'months').format('MMM-YYYY')

loopToNow = (startingDate) ->
  start_date = moment().format(startingDate) #06/2015 is the beginning of creation
  now = moment().format("YYYY-MM")
  start_date = iterateDate(start_date) until start_date > now

datesForQuery = loopToNow("2015-06")
# datesForOutput = datesForQuery.map (x) -> reduceMonth(x)
################# QUERY & PROMISE FUNCTIONS ###################
_getData  = (sql) ->
  MySql.query(mysql, sql)()
  .then (result) ->
    return result
    process.exit(23)

_getStartDate  = (sql) ->
  MySql.query(mysql, sql)()
  .then (result) ->
    return moment.unix(result[0].StartDate).format("YYYY-MM")
    process.exit(23)

allPromises = (queryList) ->
  queryList.map (query) ->
    return _getData(query)

getUserHome = ->
  process.env.HOME or process.env.USERPROFILE
# "2015-06"

# d = c.then(->
#     promiseD
#   )
runProcess = ->
  startDate = _getStartDate(startDateQuery)
  datesForOutput = startDate.then(->
                 loopToNow(startDate).map (x) -> reduceMonth(x)
  )
  startDate
    .then (results) -> loopToNow(results)
    .then (results) -> [].concat.apply([], [queryString(date) for date in results])
    .then (results) -> Promise.all allPromises(results)
    .then (results) ->
      results.map (x, i) ->
        x.map (y) -> fs.appendFileSync getUserHome() + "/Desktop/social-network-data.csv", datesForOutput[i] + " , " + y.name + " , " + y.total + "\n", "utf8"
      console.log "Output has been saved to Desktop as 'social-network-data.csv'"
      process.exit(23)
######################## RUN PROCESS ##########################
_run      = ->
  console.log '---- Querying DB for Social Network Data -----'
  fs.writeFileSync getUserHome() + "/Desktop/social-network-data.csv", "Date, Name, Total" + "\n", "utf8"
  runProcess()



  loopToNow(results).map (x) -> reduceMonth(x)
