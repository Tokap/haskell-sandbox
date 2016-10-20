R                 = require 'ramda'
Bluebird          = require 'bluebird'
moment            = require 'moment'
fs                = require 'fs'
os                = require 'os'
MySql             = require '../../lib/promise-mysql.coffee'
{ mysql, config } = require '../../lib/container.coffee'

# type alias MomentDate = String

# To run report: ./bin/cli report:account-gain <Output File Name>.csv

# Output File & Console Details
# ---------------------------------------------------------------------
csv_file_name = process.argv[3]
csv_save_path = "#{os.tmpdir()}/#{csv_file_name}"

# _confirmOrCreateDirectory :: I/O
_confirmOrCreateDirectory = ->
  try
    fs.statSync os.tmpdir()
    _log "Destination directory exists. Moving to query process..."
  catch e
    fs.mkdirSync os.tmpdir()
    _log "Destination directory did not exist. Creating now..."

# _log :: String -> I/O
_log = (output_text) ->
  process.stdout.write(output_text + '\n')

# Date Manipulation Functions & Formatting Constants
# ---------------------------------------------------------------------
QUERY_FORMAT = 'YYYY-MM-01 00:00:00'
OUTPUT_FORMAT = 'MMM-YYYY'

# _advanceMonth :: MomentDate -> MomentDate
_advanceMonth = (moment_string) ->
  moment(moment_string).add('1', 'months').format(QUERY_FORMAT)

# _reduceMonth :: MomentDate -> MomentDate
_reduceMonth = (moment_string) ->
  moment(moment_string).subtract('1', 'months').format(OUTPUT_FORMAT)

# _formatUnixDate :: Number -> MomentDate
_formatUnixDate = (unix_date) ->
  moment.unix(unix_date).format(QUERY_FORMAT)

# _loopToNow :: MomentDate -> Array MomentDate
_loopToNow = (first_entry_date) ->
  start_date = moment().format(first_entry_date)
  now = moment().format(QUERY_FORMAT)
  start_date = _advanceMonth(start_date) until start_date > now

# Query & Promise Functions
# ---------------------------------------------------------------------
# _socialAccountQuery :: MomentDate -> Promise MysqlResponse
_socialAccountQuery  = (date) ->
  sql       = '''
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
          OR (`sa`.`active` = 0 AND `sa`.`updated` >= ?)
      )
    GROUP BY `sa`.`social_account_type_id`;
  '''

  MySql.query(mysql, sql) [ date, date ]

# _startDateQuery :: MomentDate
_startDateQuery  = ->
  sql       = '''
    SELECT MIN(created) as startDate
    FROM social_account
  '''

  MySql.query(mysql, sql)()
  .then (result) -> _formatUnixDate result[0].startDate

# _formatString :: String -> String
_formatString = (text) ->
  text.replace(/["]/g, '')

# _generateCSVFile :: I/O
_generateCSVFile = ->
  _log "Querying DB for Social Network Data..."
  fs.writeFileSync csv_save_path,
  "Date, Name, Total" + "\n", "utf8"

# _appendOutput :: (String -> Integer) -> Promise MysqlResponse -> I/O
_appendOutput = (dates, i) -> (social_account) ->
  account_data =    "#{_formatString(dates[i])}, " +
                   "#{_formatString(social_account.name)}, " +
                   "#{_formatString(social_account.total) + '\n'}"
  fs.appendFileSync csv_save_path, account_data, 'utf8'

# _getQueryData :: Array MomentDate -> I/O
_getQueryData = (all_active_months) ->
  dates_for_output = R.map _reduceMonth, all_active_months
  Bluebird.map all_active_months, _socialAccountQuery
  .map (monthly_data, i) ->
    R.map _appendOutput(dates_for_output, i), monthly_data

# _printQueryResultsToCSV :: I/O
_printQueryResultsToCSV = ->
  _startDateQuery()
  .then _loopToNow
  .then _getQueryData
  .finally ->
    _log "Output has been saved to #{csv_save_path}"
    process.exit(23)

# Run Process
# ---------------------------------------------------------------------
_run      = ->
  _confirmOrCreateDirectory()
  _generateCSVFile()
  _printQueryResultsToCSV()

module.exports  = _run
