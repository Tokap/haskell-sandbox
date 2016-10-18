R                 = require 'ramda'
Bluebird          = require 'bluebird'
moment            = require 'moment'
fs                = require 'fs'
os                = require 'os'
stringify         = require 'csv-stringify'
MySql             = require '../../lib/promise-mysql.coffee'
{ mysql, config } = require '../../lib/container.coffee'

# To run this report - ./bin/cli report:account-gain <Output File Name>.csv

# Output File & Console Details
csvFileName = process.argv[3]
csvSavePath = "#{os.tmpdir()}/#{csvFileName}"

_confirmOrCreateDirectory = ->
  try
    fs.statSync os.tmpdir()
    _log "Destination directory exists. Moving to query process..."
  catch e
    fs.mkdirSync os.tmpdir()
    _log "Destination directory did not exist. Creating now..."

_log = (outputText) ->
  process.stdout.write(outputText + '\n')

# Variables & Query Support
QUERY_FORMAT = 'YYYY-MM-01 00:00:00'
OUTPUT_FORMAT = 'MMM-YYYY'

_mainQueryString       = """
  SELECT
    COUNT(`sa`.`id`) AS `total`,
    `sat`.`name` AS `name`
  FROM `social_account` AS `sa`
  JOIN `social_account_type` AS `sat`
    ON `sa`.`social_account_type_id`= `sat`.`id`
  WHERE
    (`sa`.`created` < ?)
    AND (
            (`sa`.`active` = 1 AND `sa`.`indicator` IS NULL)
        OR (`sa`.`active` = 0 AND `sa`.`updated` >= '2016-11-01 00:00:00')
    )
  GROUP BY `sa`.`social_account_type_id`;
"""

startDateQuery     = """
  SELECT MIN(created) as startDate
  FROM social_account
"""

# Date Manipulation Functions
_advanceMonth = (momentObject) ->
  moment(momentObject).add('1', 'months').format(QUERY_FORMAT)

_reduceMonth = (momentObject) ->
  moment(momentObject).subtract('1', 'months').format(OUTPUT_FORMAT)

_formatUnixDate = (dateObject) ->
  moment.unix(dateObject).format(QUERY_FORMAT)

_loopToNow = (startingDate) ->
  startDate = moment().format(startingDate)
  now = moment().format(QUERY_FORMAT)
  startDate = _advanceMonth(startDate) until startDate > now

# Query & Promise Functions
_socialAccountQuery  = (date) ->
  params = [date, date]
  MySql.query(mysql, _mainQueryString)() [ params ]
  .then (results) -> console.log results

_getStartDate  = (sql) ->
  MySql.query(mysql, sql)()
  .then (result) -> _formatUnixDate result[0].startDate

_formatString = (text) ->
  text.replace(/["]/g, '')

_generateCSVFile = ->
  _log "Querying DB for Social Network Data..."
  fs.writeFileSync csvSavePath,
  "Date, Name, Total" + "\n", "utf8"

_appendOutput = (dates, i) -> (socialAccount) ->
  accountData =    "#{_formatString(dates[i])}, " +
                   "#{_formatString(socialAccount.name)}, " +
                   "#{_formatString(socialAccount.total) + '\n'}"
  fs.appendFileSync csvSavePath, accountData, 'utf8'

_getQueryData = (allActiveMonths) ->
  datesForOutput = R.map _reduceMonth, allActiveMonths
  # Bluebird.map allActiveMonths, _socialAccountQuery
  _socialAccountQuery

  _getStartDate startDateQuery
  .then console.log
  # allActiveMonths.map (month) ->
  #   console.log month
  #   _socialAccountQuery month
  #   .then (result) -> console.log "Some RESULTS!!!! -----", results

  # .map (monthlyData, i) ->
  #   R.map _appendOutput(datesForOutput, i), monthlyData

_printQueryResultsToCSV = ->
  _getStartDate startDateQuery
  .then _loopToNow
  .then _getQueryData
  .finally ->
    _log "Output has been saved to #{csvSavePath}"
    process.exit(23)

# Run Process
_run      = ->
  _confirmOrCreateDirectory()
  _generateCSVFile()
  _printQueryResultsToCSV()

module.exports  = _run
