R                 = require 'ramda'
Bluebird          = require 'bluebird'
moment            = require 'moment'
fs                = require 'fs'
MySql             = require '../../lib/promise-mysql.coffee'
{ mysql, config } = require '../../lib/container.coffee'

_createQueryString       = (date) -> """
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

_generateCSVFile = ->
  console.log '---- Querying DB for Social Network Data -----'
  fs.writeFileSync _getUserHome() + "/Desktop/social-network-data.csv", "Date, Name, Total" + "\n", "utf8"

_printQueryResultsToCSV = ->
  startDate = _getStartDate(startDateQuery)
  datesForOutput = startDate.then(->
    startDate
      .then (earliestCreate) -> _loopToNow(earliestCreate).map (x) -> _reduceMonth(x)
    )
  datesForOutput.then(->
    startDate
      .then (earliestCreate) -> _loopToNow earliestCreate
      .then (allActiveMonths) -> [].concat.apply([], [_createQueryString(month) for month in allActiveMonths])
      .then (queryByMonth) -> Promise.all _getAllPromises(queryByMonth)
      .then (queryResults) ->
        queryResults.map (monthlyData, i) ->
          monthlyData.map (socialAccount) -> fs.appendFileSync _getUserHome() + "/Desktop/social-network-data.csv", datesForOutput.value()[i] + " , " + socialAccount.name + " , " + socialAccount.total + "\n", "utf8"
        console.log "Output has been saved to Desktop as 'social-network-data.csv'"
        process.exit(23)
    )
# Run Process - ./bin/cli report:account-gain
_run      = ->
  _generateCSVFile()
  _printQueryResultsToCSV()

module.exports  = _run
#################################################
############## ORIGINAL APP.JS FILE #############
#################################################
require('coffee-script/register');

var express         = require('express');
var morgan          = require('morgan');
var cookieParser    = require('cookie-parser');
var bodyParser      = require('body-parser');
var mysql           = require('./plugins/mysql');
var Bitly           = require('./lib/bitly.coffee');
var Notifications   = require('./plugins/notifications');
var influential     = require('./plugins/influential-headers.coffee');

//Plugins & Config
var config          = require('node-nconf-config')({

  // jscs:disable requireCamelCaseOrUpperCaseIdentifiers
  base_path: __dirname + '/config'

  // jscs:enable requireCamelCaseOrUpperCaseIdentifiers
});

var errors          = require('./plugins/error_handler');
var validation      = require('./plugins/validation.coffee');

var container       = require('./lib/container.coffee');
var bunyanLogger    = require('bunyan-middleware');

var app             = express();

var bitly           = Bitly(config.TIN.BITLY);
var CrudPush        = require('./lib/crudPush.coffee');

//Let's make sure we have the proper config.
if (!config.TIN) {
  throw new Error('Configuration not loaded!');
}

if (!config.TIN.CRYPTO) {
  throw new Error('Cryptography configuration is not loaded.');
}

var StorePlugin = (config.NODE_ENV === 'UNIT_TEST') ?
  require('./plugins/memory-store.js') :
  require('./plugins/store.js');

app.logger          = container.logger;

// If we're not in production and we don't hav a current configuration
// then let's set up the environment for the dev.
if (config.NODE_ENV !== 'production') {
  app.use(morgan('dev'));

} else {
  app.use(bunyanLogger({
    headerName:         'X-Request-Id',
    propertyName:       'reqId',
    logName:            'req_id',
    obscureHeadings:    [],
    logger:             container.logger
  }));
}

var apiCrypto      = require('./plugins/api_crypto')(config);

CrudPush.init(config.TIN);

app.set('trust proxy', true);
app.use(bodyParser.json({limit: '1mb'}));
app.use(bodyParser.urlencoded({limit: '1mb', extended: true }));

//TODO: Probably should bring this into an environment variable or config
app.use(cookieParser('signed'));
app.use(influential);

app.use(mysql(config.TIN.DATABASE.MYSQL));
app.use(StorePlugin);
app.use(Notifications(config.TIN));
app.use(validation);

app.use(function(req, res, next) {
  res.setHeader('Cache-Control', 'no-cache');
  next();
});

config.logger   = console;

var routeConfigFull   = {
  crypto: apiCrypto,
  logger: config.logger,
  bitly: bitly,
  config: config
};

app.use(require('./routes/tracking.coffee')(routeConfigFull));
app.use(require('./plugins/user_action_logger.coffee'));
app.use(require('./plugins/optionalParam.coffee'));
app.use(require('./routes/sys.coffee')(routeConfigFull));

app.use('/', require('./routes')(config));
app.use(require('./routes/redirector')(routeConfigFull));
app.use('/signup', require('./routes/signup')(routeConfigFull));
app.use('/authenticate', require('./routes/authenticate')(routeConfigFull));
app.use(require('./routes/site-mode.coffee')(routeConfigFull));
app.use(require('./routes/user.coffee')(routeConfigFull));
app.use(require('./routes/social.coffee')(routeConfigFull));
app.use('/campaign', require('./routes/campaign.coffee')(routeConfigFull));
app.use(require('./routes/email.coffee')(routeConfigFull));
app.use(require('./routes/media.coffee')(routeConfigFull));
app.use('/bitly', require('./routes/bitly')(routeConfigFull));
app.use(require('./routes/accounting')(routeConfigFull));
app.use(require('./routes/affiliate_manager')(routeConfigFull));
app.use(require('./routes/brand_manager')(routeConfigFull));
app.use(require('./routes/comment')(routeConfigFull));
app.use(require('./routes/change_request')(routeConfigFull));
app.use(require('./routes/message')(routeConfigFull));
app.use(require('./routes/notification')(routeConfigFull));
app.use(require('./routes/tax')(routeConfigFull));
app.use(require('./routes/invoice')(routeConfigFull));
app.use(require('./routes/my-affiliates.coffee')(routeConfigFull));
app.use(require('./routes/social_account.coffee')(routeConfigFull));
app.use(require('./routes/ledger.coffee')(routeConfigFull));
app.use(require('./routes/campaign_group.coffee')(routeConfigFull));
app.use(require('./routes/company.coffee')(routeConfigFull));
app.use('/rfp', require('./routes/rfp.coffee')(routeConfigFull));
app.use('/keyhole', require('./routes/keyhole.coffee')(routeConfigFull));
app.use(require('./routes/watson.coffee')(routeConfigFull));
app.use(require('./routes/log.coffee')(routeConfigFull));
app.use(require('./routes/event.coffee')(routeConfigFull));
app.use('/career', require('./routes/career.coffee')(routeConfigFull));
app.use(require('./routes/location.coffee')(routeConfigFull));
app.use(require('./routes/suggestion.coffee')(routeConfigFull));
app.use(require('./routes/group.js')(routeConfigFull));
app.use(require('./routes/company_settings.coffee')(routeConfigFull));
app.use(require('./routes/holiday.coffee')(routeConfigFull));
app.use(require('./routes/address_office.coffee')(routeConfigFull));
app.use(require('./routes/employee.coffee')(routeConfigFull));
app.use(require('./routes/stub.coffee')(routeConfigFull));
app.use(require('./routes/vetting.coffee')(routeConfigFull));
app.use(require('./routes/affinity-tag.coffee')(routeConfigFull));
app.use(require('./routes/brand-tag.coffee')(routeConfigFull));
app.use(require('./routes/custom-tag.coffee')(routeConfigFull));
app.use(require('./routes/pricing-history-note.coffee')(routeConfigFull))
app.use(require('./routes/post-tag.coffee')(routeConfigFull));
app.use(require('./routes/layout.coffee')(routeConfigFull));
app.use(require('./routes/post')(routeConfigFull));
app.use(require('./routes/tag.js')(routeConfigFull));
app.use(require('./routes/comment-v2.coffee')(routeConfigFull));
app.use(require('./routes/campaign-concept.coffee')(routeConfigFull));
app.use(require('./routes/campaign-post.js')(routeConfigFull));

// catch 404 and forward to error handler
app.use(function(req, res, next) {
  var err = new Error('NotFound <route>');
  err.status = 404;
  next(err);
});

// error handlers
app.use(errors);

module.exports = app;
############## CATCH ERROR FOR QUERY ################
    .catch (e) ->

      process.stderr.write(JSON.stringify e + '\n')
      process.stderr.write(e.message + '\n')
      process.stderr.write(e.stack + '\n')
      process.exit(1)
