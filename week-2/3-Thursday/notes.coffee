############## ORIGINALLY PARTS OF SCRAP IDEAS FOR:
# RUN statement in report-account-gain.cofee

# sql = queryString('2016-08')
# sql2 = queryString('2016-09')
# sql3 = queryString('2016-10')
# Bluebird
#   .join _getData(sql), _getData(sql2), _getData(sql3), (test, test2, test3) ->
#     # console.log "test1 length : " + test[1].total + " test2 length : " + test2[1].total + " test3 length : " + test3[1].total
#     console.log test + test2 + test3
#     process.exit(23)
#     return
#   return
# console.log allPromises(mergedQueryList)

############ HOW SHOULD THE QUERY WORK? ##################
# Get data for each month
# Sum data leading into each month
# Output somewhere - maybe command line first then excel sheet?

####### Promises:
# Return an object that is the eventual execution of the function (error or resolve)
# can keep chaining .then()
# .promisify -> Converts a single callback taking function into a promises
# returning function. It does NOT alter the original function.

##### EXAMPLE:
Promise.all([User.findOne(), Product.findOne()])
 .then(function (results){
    # results = [user, product]
    return Review.create({user:results[0], product: results[1]})
})

### Map over the results of x in your results.map (x,i) functions. X is an
### array of object
