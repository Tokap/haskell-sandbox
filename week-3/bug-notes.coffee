####### FILTERING ISSUE WHERE ALL IS TAKING PRIORITY ##########
# Second filter that is causing the problems:
<SearchHandler key="key-" query={role: "admin", status: "verified"}>

# The functioning filter changes the following section:
<UserManagementSearchHandler query={role: "root"(or "All" or w/e),
status: "verified", limit: 16} setSearchParams=bound setSearchParams()>

# The below two elements are the one's updated during filter. "All" will default
# to a null value.
<UserManagementSearchHandler>
<SearchHandler>

# Filter 1 - status (Ver, unver, banned)
# Filter 2 - role
# "All" leaves either object blank
# When role is set to all (and hence absent) is when the error occurs
# Role seems to work fine otherwise
# Nothing goes wrong when Role is used for any other filter

# Review lines 246-250
renderRoleOptions: (label) ->
  data = {All: ''}
  if @props.roles
    data[Formatter.prettify role.role] = role.role for role in @props.roles
  @renderOptions(data, label)


# Query route hits:
user/search/advanced?params=values


# authorize/privilege/pathMap.coffee
# routes/user.coffee -> Leads to:
  router.get '/user/search/advanced', JW(
    JWUser.search,
    JWUser.getRelated,
    JWUser.match
  )
# Review content from this path:
JWUser            = require('../middleware/jw.user-archetype')

# Continue to review data paths. Problem could be something like a 'SELECT *'
# query when All is chosen for the problem filter box.

# Alternatively, this could be an issue of conditional logic - "if all, then *"

# jw.user-archetype.js
const search = {
  awesomize  : (v) => ({
    page                   : {
      read      : R.path(['query', 'page'])
      , validate: [v.isInt]
    }
    , role                 : {
      read      : R.path(['query', 'role'])
      , validate: []
    }

  , io: (req, data) => ({
    response: req.DO.User.search(req.mysql2)(
      data
    )
  })

const getRelated = ({
  awesomize: (v) => ({
    response: {
      read: R.path(['data', 'response'])
      , validate: [v.required]
    }
  })


# mysql-objects/index.js
# mysql-objects/user.js
    verified             : () => {
      return {
        where:' AND `user`.`verified` = :verified'
      }
    },

    banned               : () => {
      return {
        where:' AND IF(:banned = \'0\', `user`.`banned` = 0, `user`.`banned` != 0)'
      }

    role                 : () => {
      return {
        join : 'JOIN `user_role` ON `user_role`.`user_id` = `user`.`id`'
        + ' AND `user_role`.`deleted` IS NULL'
        + ' JOIN `role` ON `user_role`.`role_id` = `role`.`id`',
        where:' AND `role`.`role` = :role'
      }
# Bug ultimately reduced to issues with pimp-my-sql
