describe('::countExternalPending', () => {
  const { countExternalPending } = JwOfferConceptIdea;

  it('counts a campaigns externally pending offer concepts.', (done) => {

    const campaign_offers = [
      { id: 1
      , external_status: 'pending'
      }
    , { id: 2
      , external_status: 'approved'
      }
    ];

    const data = {
      campaign_id: 1
    , deps: { campaign_offers }
    };

    countExternalPending({ data })
    .then((count) => {
      expect(count).to.eql(1);
      done();
    })
    .catch((err) => {
      throw err;
    })
  });
});
