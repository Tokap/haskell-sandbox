#LOOK-HERE-PATRICK
# jw.offer-concept-idea.js
# ---------------- NEW-ADDITION-FOR-FEATURE #LOOK-HERE-PATRICK--------
const countExternalPendingConcepts = {
  transform: (req, data) => {
    return R.assoc('count', _countExternalPending(data.campaign_offers), data);
  }
}
# --------------------------------------------------------------------

# --------------------------------------------------------------------
# --------------------- Unused Custom Route --------------------------
# --------------------------------------------------------------------
router.get '/campaign-concept/:campaign_id/offer/count', JW(
  getParameters('params', ['campaign_id'])
  JwIdea.getOffers
  # JwIdea.filterClientReady
  JwIdea.countExternalPendingConcepts
)
