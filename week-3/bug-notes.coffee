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

# authorize/privilege/pathMap.coffee
# routes/user.coffee -> Leads to:
  router.get '/user/search/advanced', JW(
    JWUser.search,
    JWUser.getRelated,
    JWUser.match
  )

# Object returns from query results:
# Return from good query for verified status and role: root
{params: {verified: "1", role: "root", offset: 0, limit: 16, page: 1}, count: "9", rows: [{,…},…]}
  count:"9"
  params:{verified: "1", role: "root", offset: 0, limit: 16, page: 1}
    limit:16
    offset:0
    page:1
    role:"root"
    verified:"1"
  rows:[{,…},…]
    0:{,…}
    1:{result: {id: "3886", name_first: "Jarin", name_last: "Ratanapeanchai", username: "jarinlv",…},…}
    2:{,…}
    3:{,…}
    4:{,…}
    5:{,…}
    6:{,…}
    7:{,…}
    8:{,…}

# return query from status: verified, role: all - FAIL
{params: {verified: "1", offset: 0, limit: 16, page: 1}, count: "8742", rows: [{,…},…]}
count:"8742"
params:{verified: "1", offset: 0, limit: 16, page: 1}
limit:16
offset:0
page:1
verified:"1"
rows:[{,…},…]
0:{,…}
1:{result: {id: "2682", name_first: "A Continuous", name_last: "Lean", username: "acontinuouslean",…},…}
2:{,…}
3:{result: {id: "363", name_first: "A Test", name_last: "User", username: "piotrcreatedtest1",…},…}
4:{,…}
5:{,…}
6:{,…}
7:{result: {id: "41", name_first: "Aaron", name_last: "Carpenter", username: "aaroncarpenter",…},…}
8:{,…}
9:{,…}
10:{,…}
11:{,…}
12:{,…}
13:{,…}
14:{,…}
15:{,…}

# Unverified & role: campaign_manager
{params: {verified: "0", role: "campaign_manager", offset: 0, limit: 16, page: 1}, count: "0",…}
count:"0"
params:{verified: "0", role: "campaign_manager", offset: 0, limit: 16, page: 1}
limit:16
offset:0
page:1
role:"campaign_manager"
verified:"0"
rows:[]
