# DEFINITIONS:
# (!!!!!!!!!) = Likely the source of the error based on current review

# API response when triggering broken FB verification for Bobdog

# POST https://app.dev.influential.co/api/campaign/539/offer/4662/verify 400 (Bad Request)

# The above falls in line with what we would expect, but the front end goes dead
# To me, this means the process is working on the backend but failing to trigger
# the error notification on the front end denoting "Not a valid Facebook URL"
# Errors are:
  # [ //ww.not.co ]            -> Not a valid URL
  # [ https://www.google.com ] -> Not a valid Facebook URL
  # [ BLANK ]                  -> This feild is required

# Error messages are stored in:
client/app/lib/errorMsg.coffee

# Modal Templates are viewable in:
ux/dashboard/modal.html
client/app/dashboard/modal/templates/overlay.coffee

# Content of THIS Modal is available in:
client/app/dashboard/campaign/offers/details/actions.react.coffee

# "Verify Offer Post" modal header found in:
client/app/dashboard/offers/details/actions.react.coffee
# Search this file for 'onVerifyOffer'
# Lines 169-179
  onVerifyOffer: ->
    ModalStore.showForm
      title: 'Verify Offer Post'
      icon: 'bts bt-5x bt-link'
      description: 'Enter the URL for the post.'
      form: VerifyForm
      submit: @verifyOffer
      options:
        values:
          post_url: @props.offer?.post?.network_url or ''
        network: @props.offer?.network_slug

# Lines 236 - 248
  getOptions: ->
    callbacks =
      onChangeHeader: @onChangeHeader
      onChangeCopy: @onChangeCopy
      onChangeMedia: @onChangeMedia
      onChangeUrl: @onChangeUrl
      onChangePrice: @onChangePrice
      onVerifyOffer: @onVerifyOffer
      onChangeForwardUrl: @onChangeForwardUrl
      onReverseAcceptance: @onReverseAcceptance
      onOfferAccept: @onOfferAccept

    _.merge(@props, callbacks)

# ------------------------------------------------------------
ActionsOriginate = Reactify.create
  displayName: 'ActionsOriginate'

  render: ->
    div {},
# Lines 298 - 303
      ActionButton
        hidden: not @props.verify_post_network
        disabled: not @props.verify_post
        onClick: @props.onVerifyOffer
        icon: 'bts bt-link'
        'Verify Post'

# ------------------------------------------------------------
ActionsForward = Reactify.create
  displayName: 'ActionsForward'

  render: ->
    div {},
    # Lines 339 - 344
      ActionButton
        hidden: @props.verify_post_network
        disabled: not @props.verify_post
        onClick: @props.onVerifyOffer
        icon: 'bts bt-link'
        'Verify Post'


# ------------------------------------------------------------
# Lines 51 - 62, regarding verification of posts
  render: ->
    ActionWidget {},
      ActionContent
        offer: @props.offer
        offer_type: @props.offer.type.code
        immutable: not OfferHelpers.canChangeOffer(@props)
        change_forward_url: @canChangeForwardUrl()
        view_change_request: @canViewChangeRequest()
        verify_post_network: @isVerifyPostNetwork()
        verify_post: OfferHelpers.canVerifyPost(@props)
        reverse_acceptance: OfferHelpers.canReverseAcceptance(@props)
        force_acceptance: OfferHelpers.canAccept(@props.offer)

# ------------------------------------------------------------
# Lines 97 - 102, regarding verification of posts
  verifyOffer: (values) -> # (!!!!!!!!!)
    @createCampaignOfferVerify
      keys:
        campaign_id: @props.offer.campaign.id
        offer_id: @props.offer.id
      values: values
#########################################
  CampaignOfferVerify:
    create: '/campaign/:campaign_id/offer/:offer_id/verify'
#########################################
#In the related handler:
  onCreateOfferVerify: (request) ->
    @setState offer: request.response
#########################################
# ------------------------------------------------------------
# client/app/dashboard/campaign/offers/details/handler.react.coffee
# Lines 24 - 37
CampaignOfferDetailsHandler = React.createClass
  displayName: 'CampaignOfferDetailsHandler'
  mixins: [
    Router.State
    Router.Navigation
    CrudReactPromise('CampaignOffer')
    CrudReactPromise('OfferInquiry')
    CrudReactPromise('ChangeRequestAsUser')
    CrudReactPromise('CampaignOfferReverseAcceptance')
    CrudReactPromise('CampaignOfferVerify')
    CrudReactPromise('CampaignVisualAids')
    CrudReactPromise('UserOfferAccept')
    CrudReactPromise('CampaignInstruction')
  ]
  # The above derived from:
    {
      CrudReact
      CrudReactPromise
    }  = require '../../../../lib/crudFactory.coffee'

# ------------------------------------------------------------
# client/app/dashboard/campaign/offers/details/handler.react.coffee
# Lines 24 - 37
CrudReactPromise = (entityType) ->
  new CrudReactMixin(CrudActionsPromise(entityType), CrudStore(entityType))
# CrudReactMixin          = require './react-mixins/crud.coffee'

# ------------------------------------------------------------


# ------------------------------------------------------------
# Route in CRUD Mapping:
# client/app/lib/crudMap.coffee (this matches our expected route)
  CampaignOfferVerify:
    create: '/campaign/:campaign_id/offer/:offer_id/verify'

# ------------------------------------------------------------
# CoffeScript Note: 'in' for CS checks for inclusion in arrays, not objects
# Lines 48 - 49, regarding verification of posts
# The below appears to simply check the network.
isVerifyPostNetwork: ->
  @props.offer.network_slug in NetworkLib.POST_VERIFICATION_NETWORKS

# NetworkLib = require '../../../../lib/validate/network.coffee'
POST_VERIFICATION_NETWORKS = [
    'instagram'
    'vine'
    'facebook'
  ]

# OfferHelpers = require '../../../../lib/offerHelpers.coffee'
# canVerifyPost :: PropsContainer -> Bool
canVerifyPost = _.allPass([
  _.path [ 'offer', 'terms_accepted' ]
  _.compose _.not, _.path [ 'offer', 'post', 'network_url' ]
])

# ------------------------------------------------------------
# ----------------- ERROR HANDLING ---------------------------
# ------------------------------------------------------------
ErrorReporter = require '../../../lib/errorReporter.coffee'
ModalStore.catchError request
ModalStore          = require '../../../modal/store.coffee'
ErrorMsg            = require '../../../../lib/errorMsg.coffee'



# ------------------------------------------------------------
# ----------------- FROM NODE API ----------------------------
# ------------------------------------------------------------
# domain/campaign/offer/verify/facebook.coffee
{ rethinkdb }         = require '../../../../lib/container.coffee'
Facebook              = require('../../../../lib/facebook.coffee')()

moment                = require 'moment'
findByUrl             = (post_id, post_url, network_id) ->

  status_id = Facebook.parseStatusIdFromUrl(post_url)
  Facebook.getStatusById("#{network_id}_#{status_id}")
  .then (response) ->
    _updatePostWithResponse(response, post_id, post_url)
  .catch (e) ->
    _updatePostWithError(e, post_id)
    .then -> throw e

_updatePostWithResponse = (response, post_id, post_url) ->
  rethinkdb.run(rethinkdb.r.table('post').get(post_id).update({
      post_id         : response.id
      posted_date     : rethinkdb.r.epochTime(moment(response.created_time).unix())
      network_url     : post_url
      attempts        : rethinkdb.r.row('attempts').add(1)
      api_calls       : rethinkdb.r.row('api_calls').append(response)
      status          : 'live'
      updated         : new Date()
    })
  )

_updatePostWithError = (e, post_id) ->
  rethinkdb.run(rethinkdb.r.table('post').get(post_id).update({
      attempts        : rethinkdb.r.row('attempts').add(1)
      api_calls       : rethinkdb.r.row('api_calls').append( e.tin or e.toString() )
      updated         : new Date()
    })
  )

module.exports  =
  findByUrl     : findByUrl

#Export for unit tests
  _updatePostWithResponse : _updatePostWithResponse
  _updatePostWithError : _updatePostWithError


############# URL VALIDATION - only checks network, not owner ################
# Runs through each REGEX & determines if the url matches. SOME match - No error
  FacebookLib().validateStatusUrl = (url) ->
    Bluebird.all([
      REGEX_URL_POST.test(url)
      REGEX_URL_POST_PHP.test(url)
      REGEX_URL_POST_PHOTO.test(url)
      REGEX_URL_POST_PHOTO_PHP.test(url)
      REGEX_URL_POST_VIDEO_VB.test(url)
      REGEX_URL_POST_VIDEO.test(url)
      REGEX_URL_POST_STORY.test(url)
    ])
    .then (results) ->
      valid = results.some (r) -> r is true
      if valid then null else 'facebook_url_invalid'
      # Front end then updates this to "Not a valid Facebook URL"

REGEX_URL_POST = /^https:\/\/www\.facebook\.com\/.+\/posts\/([0-9]+)/i
REGEX_URL_POST_PHP = /^https:\/\/www\.facebook\.com\/permalink.php.+story_fbid=([0-9]+)/i
REGEX_URL_POST_PHOTO = /^https:\/\/www\.facebook\.com\/.+\/photos\/.+\/([0-9]+)\//i
REGEX_URL_POST_PHOTO_PHP = /^http:\/\/www\.facebook\.com\/photo.php.+pid=([0-9]+)/i
REGEX_URL_POST_VIDEO = /^https:\/\/www\.facebook\.com\/.+\/videos\/([0-9]+)/i
REGEX_URL_POST_VIDEO_VB = /^https:\/\/www\.facebook\.com\/.+\/videos\/.+\/([0-9]+)/i
REGEX_URL_POST_STORY = /^https:\/\/www\.facebook\.com\/story.php.+story_fbid=([0-9]+)/i
REGEX_FACEBOOK_ID = /([0-9]{15})$/

# domain/campaign/offer/verify.coffee

# _findPost = Offer, String -> Promise InstagramResponse (Lines 57 - 63)
_findPost = (offer, url) ->
  _strategy = network_search[offer.network_slug] # This will return facebook
  _post     = offer.post
# After this line, it will throw an error if _strategy doesn't return a slug

#Facebook.findByUrl...
_strategy.findByUrl(_post.id, url, offer.social_account.network_id) #(line 65)
