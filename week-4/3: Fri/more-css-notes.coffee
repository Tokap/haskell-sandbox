# ------------- FULL SIZE --------------
div.account-card.brand-campaign-card.clearfix {
  height: 564px;
}

# Container for buttons
div.additional {
  position: absolute;
  width: 95%;
  bottom: 10px;
}

# ------------- MEDIA QUERY: 1120px - 801px --------------
div.account-card.brand-campaign-card.clearfix {
  height: 604px;
}

div.additional { UNCHANGED }

# ------------- MEDIA QUERY: 800px - 683px --------------
div.account-card.brand-campaign-card.clearfix {
  height: 645px; #43.5em
}

div.additional { UNCHANGED }

# ------------- MEDIA QUERY: Less than 682 --------------
div.account-card.brand-campaign-card.clearfix { UNCHANGED }
div.additional { UNCHANGED }

# ------------- Finally: --------------
# If the result of the massive query we're constructing returns 0, the
# 'Concepts For Approval' button is set to display: none
