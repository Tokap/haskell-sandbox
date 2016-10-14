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
