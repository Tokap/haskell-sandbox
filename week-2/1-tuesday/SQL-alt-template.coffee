R                 = require 'ramda'
Bluebird          = require 'bluebird'
MySql             = require '../../lib/promise-mysql.coffee'
{ mysql, config } = require '../../lib/container.coffee'

sql       = '''
  SELECT
    COUNT `sa`.`id`                      AS `total`,
    `sat`.`name`                         AS `network`,
    EXTRACT MONTH FROM `sa`.`created`    AS `mon`,
    EXTRACT YEAR FROM `sa`.`created`     AS `yr`,
    `sa`.`username`                      AS `handle`,
    `user`.`name_first`                  AS `first_name`,
    `user`.`name_last`                   AS `name_last`,
    `email`.`email`                      AS `email`
  FROM `social_account` AS `sa`
  JOIN `user`
    ON `sa`.`user_id` = `user`.`id`
  JOIN `user_email`
    ON `user`.`id` = `user_email`.`user_id`
    AND `user_email`.`deleted` = 0
  JOIN `email`
    ON `user_email`.`email_id` = `email`.`id`
  JOIN `social_account_type` AS `network`
    ON `sa`.`social_account_type_id` = `network`.`id`
'''

makeUrl   = (id) -> "#{config.TIN.HOSTS.dashboard}/#{id}/signup"

_run      = ->

  MySql.query(mysql, sql)()

  .map (x) -> { labels : R.keys(x), values : R.values(x) }

  .map (x) ->
    labels  : R.append 'referral_url', x.labels
    values  : R.append makeUrl(x.values[0]), x.values

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
