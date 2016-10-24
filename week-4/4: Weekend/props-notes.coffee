render: ->
  #LOOK-HERE-PATRICK PENDING FORMULA VIA PROPS
  div {},
    Widget
      heading: "Pending (#{@props.pending.length})"
      width: 'change-request-pending-panel'
      div {},
        if @props.pending.length
          @renderPendingCard(offer) for offer in @props.pending
        else
          p {}, 'No Offer Concepts Pending Approval'

    span
      className: 'clearfix'



renderPendingCard: (offer) ->
  PendingCard
    key: "key-#{offer.id}"
    offer: offer





PendingCard = Reactify.create
  displayName: 'PendingConcept'

  offer.campaign.id
  offer.campaign.cover_image.image_url
  offer.social_account.avatar
  offer.social_account.username
  offer.external_status
  offer.campaign.title
  offer.campaign.slogan
