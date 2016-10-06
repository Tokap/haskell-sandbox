R                 = require 'ramda'
Bluebird          = require 'bluebird'
moment            = require 'moment'

MySql             = require '../../lib/promise-mysql.coffee'
{ mysql, config } = require '../../lib/container.coffee'
# (??) What was the issue with Indicator Null pulling bad #s? Remove OR clause
# in WHERE?
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
        OR ((`sa`.`active` = 0 OR `sa`.`indicator` IS NOT NULL) AND `sa`.`updated` >= '2015-09-01 00:00:00')
    )
  GROUP BY `sa`.`social_account_type_id`;
"""
#### DATE MANIPULATION FUNCTIONS
printDateAndIterate = (momentObject) ->
  return moment(momentObject).add('1', 'months').format('YYYY-MM')

loopToNow = () ->
  start_date = moment().format("2015-06")
  now = moment().format("YYYY-MM")
  start_date = printDateAndIterate(start_date) until start_date == now

dateArray = loopToNow()

# sql = queryString('2016-09')
_getData  = (sql) ->
  MySql.query(mysql, sql)()
  .then (result) ->
    # Result is an array of objects, so result[1].total is valid
    console.log result
    process.exit(23)

# Syntax forEach loop in coffeescript
# myFunction(item) for item in array

queryList = [queryString(date) for date in dateArray]
mergedQueryList = [].concat.apply([], queryList)
_run      = ->
  console.log('Hello World!')
  sql = queryString('2016-09')
  console.log(dateArray)
  _getData(sql)
  # return
  # process.exit(23)
  ########## STILL FUNCTIONS WITH BELOW COMMENTED OUT - ASK #################
  MySql.query(mysql, sql)()

  .map (x) -> { labels : R.keys(x), values : R.values(x) }

  .reduce (cur, x) ->
    labels  : cur?.labels or x.labels
    values  : R.concat cur?.values or [], [ x.values ]
  , {}

  .then (x) -> if R.is(Array, x.values)
    R.concat [ x.labels ], x.values
  else []

  .map (x) -> R.join(',', x) + '\n'

  .then R.join('')

  .then (output) -> process.stdout.write(output)

  .then -> process.exit(0)

  .catch (e) ->

    process.stderr.write(JSON.stringify e + '\n')
    process.stderr.write(e.message + '\n')
    process.stderr.write(e.stack + '\n')
    process.exit(1)

module.exports  = _run
