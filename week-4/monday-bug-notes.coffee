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
