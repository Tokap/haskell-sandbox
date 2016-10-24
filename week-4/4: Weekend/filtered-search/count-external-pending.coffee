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

_makeReq = (data) -> R.assoc('data', data, req)
#R.assoc makes an object {data: data} then req (campaign_id: X)
