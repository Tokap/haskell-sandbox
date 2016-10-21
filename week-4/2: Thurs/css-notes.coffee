# ------- Full Width Sizing Styles ------------
#div.account-card.brand-campaign-card.clearfix
element.style {
  height: 564px !important;
}

# div.inner-container
element.style {
  position: absolute;
  width: 95%;
  bottom: 10px;
}

# div.campaign-info-container
element.style {
  position: relative;
  margin-bottom: 14%;
}

# Above width fails at media break below 1120px

# ------- Media Query at Lower Width (1120 - 801) ------------

#div.account-card.brand-campaign-card.clearfix
element.style {
  height: 601px !important;
}

# div.campaign-info-container
element.style {
  position: relative;
  margin-bottom: 9%;
}

# ------- Media Query at Lower Width (800 - 683) ------------
#div.account-card.brand-campaign-card.clearfix
element.style {
  height: 653px;
}

# div.inner-container
element.style {
  position: absolute;
  width: 97%;
  bottom: 10px;
}

# div.campaign-info-container
element.style {
  margin-bottom: 6%;
}

# ------- Media Query at Lower Width Less than 683 ------------
#div.account-card.brand-campaign-card.clearfix
element.style {
  height: 660px;
}

# div.inner-container
element.style {
  position: absolute;
  width: 97%;
  bottom: 10px;
}

# div.campaign-info-container
element.style {
  margin-bottom: 3%;
}
# ----------------------------------------------------------------
# ------------------------ NEW APPROACH --------------------------
# ----------------------------------------------------------------
# Link below provides good alternatives:
# http://stackoverflow.com/questions/585945/how-to-align-content-of-a-div-to-the-bottom-with-css
# div.additional
element.style {
    display: table-cell;
    vertical-align: bottom;
    width: 84%;
}

# This is all manageable, but I'm starting to think it doesn't make sense.
# Consider suggesting the button simply grey-out like the others when nothing pending



# The parent element needs its display type set to flex

div.parent {
  display: flex;
  height: 100%;
}
# Then you set the child element's align-self to flex-end.

span.child {
  display: inline-block;
  align-self: flex-end;
}
