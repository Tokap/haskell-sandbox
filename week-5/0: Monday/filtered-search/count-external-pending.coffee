# JS File
const countExternalPending      = _offersStatusCount(_countExternalPending);
const _countExternalPending     = countIf(R.propEq('external_status', PENDING))

const _offersStatusCount = (fn) => (data) => {
  return JW.promisify(
    getOffers
  , { transform: (req, data) => {
        return R.assoc('count', fn(data.campaign_offers), data);
      }
    }
  )(data).get('count');
}

# When called elsewhere, looks like:
JwIdea.countExternalPending _makeReq campaign_id: campaign_id

#R.assoc makes an object {data: data}

# ----- Example Test Object for this function -------
# Function counts where external_status: pending. This is set by middleware
const data = {
  campaign_id: 1
, deps: { campaign_offers }
};

  const campaign_offers = [
    { id: 1
    , external_status: 'pending'
    }
  , { id: 2
    , external_status: 'approved'
    }
  ];


countExternalPending({ data })
.then((count) => {
  expect(count).to.eql(1);
  done();
# -----------------------------------------------------------------------------
# -------- Promise Handling Example -------------------------------------------
# -----------------------------------------------------------------------------
_printQueryResultsToCSV = ->
  _startDateQuery() # Returns a promise with date
  .then _loopToNow
  .then _getQueryData
  .finally ->
    _log "Output has been saved to #{csv_save_path}"
    process.exit(23)
