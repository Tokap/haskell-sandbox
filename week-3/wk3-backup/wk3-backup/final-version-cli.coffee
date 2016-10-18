R                 = require 'ramda'
Bluebird          = require 'bluebird'
moment            = require 'moment'
fs                = require 'fs'
os                = require 'os'
stringify         = require 'csv-stringify'
MySql             = require '../../lib/promise-mysql.coffee'
{ mysql, config } = require '../../lib/container.coffee'

# To run this report - ./bin/cli report:account-gain <Output File Name>.csv

# Output File Details
csvFileName = process.argv[3]
csvSavePath = "#{os.tmpdir()}/#{csvFileName}"

_confirmOrCreateDirectory = ->
  try
    fs.statSync os.tmpdir()
    console.log "Destination directory exists. Moving to query process..."
  catch e
    fs.mkdirSync os.tmpdir()
    console.log "Destination directory did not exist. Creating now..."
  return

# Variables & Query Support
QUERY_FORMAT = 'YYYY-MM-01 00:00:00'
OUTPUT_FORMAT = 'MMM-YYYY'

_createQueryString       = (date) -> """
  SELECT
    COUNT(`sa`.`id`) AS `total`,
    `sat`.`name` AS `name`
  FROM `social_account` AS `sa`
  JOIN `social_account_type` AS `sat`
    ON `sa`.`social_account_type_id`= `sat`.`id`
  WHERE
    (`sa`.`created` < "#{date}")
    AND (
            (`sa`.`active` = 1 AND `sa`.`indicator` IS NULL)
        OR (`sa`.`active` = 0 AND `sa`.`updated` >= "#{date}")
    )
  GROUP BY `sa`.`social_account_type_id`;
"""

startDateQuery     = """
  SELECT MIN(created) as startDate
  FROM social_account
"""

# Date Manipulation Functions
_advanceMonth = (momentObject) ->
  return moment(momentObject).add('1', 'months').format(QUERY_FORMAT)

_reduceMonth = (momentObject) ->
  return moment(momentObject).subtract('1', 'months').format(OUTPUT_FORMAT)

_formatUnixDate = (dateObject) ->
  moment.unix(dateObject).format(QUERY_FORMAT)

_loopToNow = (startingDate) ->
  startDate = moment().format(startingDate)
  now = moment().format(QUERY_FORMAT)
  startDate = _advanceMonth(startDate) until startDate > now

# Query & Promise Functions
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

_formatString = (text) ->
  text.replace(/["]/g, '')

_generateCSVFile = ->
  console.log "Querying DB for Social Network Data..."
  fs.writeFileSync csvSavePath,
  "Date, Name, Total" + "\n", "utf8"

_printQueryResultsToCSV = ->
  startDate = _getStartDate(startDateQuery)
  datesForOutput = startDate.then(->
    startDate
      .then (earliestCreate) ->
        _loopToNow(earliestCreate).map (x) -> _reduceMonth(x)
    )
  datesForOutput.then(->
    startDate
      .then (earliestCreate) -> _loopToNow earliestCreate
      .then (allActiveMonths) -> R.chain _createQueryString, allActiveMonths
      .then (queryByMonth) -> Promise.all _getAllPromises(queryByMonth)
      .then (queryResults) ->
        queryResults.map (monthlyData, i) ->
          monthlyData.map (socialAccount) ->
            fs.appendFileSync csvSavePath,
            "#{_formatString(datesForOutput.value()[i])}, " +
            "#{_formatString(socialAccount.name)}, " +
            "#{_formatString(socialAccount.total) + '\n'}", "utf8"
        console.log "Output has been saved to #{csvSavePath}"
        process.exit(23)
    )

# Run Process
_run      = ->
  _confirmOrCreateDirectory()
  _generateCSVFile()
  _printQueryResultsToCSV()

module.exports  = _run
