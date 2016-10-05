Bluebird    = require 'bluebird'
{ mysql }   = require '../../../lib/container.coffee'

module.exports  = (email) -> new Bluebird (resolve, reject) ->

  sql       = '''
    SELECT
      `email_verify`.`random` AS `token`,
      `email`.`email`         AS `email`,
      `user_email`.`verified` AS `is_verified`,
      `user_email`.`user_id`  AS `user_id`,
      `user_email`.`email_id` AS `email_id`
    FROM `email_verify`
    JOIN `user_email`
      ON `email_verify`.`user_email_id` = `user_email`.`id`
    JOIN `email`
      ON `user_email`.`email_id` = `email`.`id`
    WHERE `email_verify`.`deleted` IS NULL
    AND `email_verify`.`success` IS NULL
    AND `email_verify`.`fail` IS NULL
    AND `user_email`.`deleted` = 0
    AND `email`.`email` = ?
  '''

  mysql.query sql, [ email ], (err, results) ->
    if err then return reject err

    if not results[0] then return reject new Error('NotFound')

    return resolve results[0]
