{campaign_id: "539",…}
  campaign_id:"539"
  concept_elements:[{id: "20", theme: "Test Element", material: "Test phrase goes in this box", created: 1477372626,…}]
    0: {id: "20", theme: "Test Element", material: "Test phrase goes in this box", created: 1477372626,…}
      campaign_id:"539"
      created:1477372626
      id:"20"
      material:"Test phrase goes in this box"
      theme:"Test Element"

#DO Object

DO:
 { Campaign: { update: [Function], getById: [Function] },
   ConceptElement:
    { insert: [Function],
      getById: [Function],
      getByCampaignId: [Function],
      deleteById: [Function] },
   Group:
    { search: [Function],
      save: [Function],
      insert: [Function],
      update: [Function],
      getById: [Function],
      getAll: [Function: getAll],
      getAllGroupsByTypeId: [Function],
      getGroupByMemberId: [Function],
      getMembersByListOfMemberIds: [Function],
      deleteById: [Function],
      nonPseudo: [Function],
      groupExists: [Function],
      memberExists: [Function],
      getAllMembers: [Function: getAllMembers],
      getMemberByMemberId: [Function],
      getMembersByGroupId: [Function],
      getMembersByGroupTypeId: [Function],
      getMembersByGroupTypeIdAndReferenceId: [Function],
      addMember: [Function],
      removeMember: [Function],
      removeGroupMembersByGroupId: [Function] },
   GroupTag:
    { insert: [Function],
      update: [Function],
      getById: [Function],
      getAll: [Function: getAll],
      deleteById: [Function],
      getAllByGroupId: [Function],
      deleteByGroupId: [Function],
      addTagForGroupId: [Function],
      getTagIdsForGroupId: [Function] },
   OfferConceptIdea:
    { insert: [Function],
      getByCampaignId: [Function],
      getByOfferId: [Function] },
   User: { search: [Function], getByIdList: [Function] },
   Comment:
    { TYPE: [Object],
      insert: [Function],
      update: [Function],
      getById: [Function],
      getByReferenceTypeAndId: [Function],
      getVisibleByReferenceTypeAndId: [Function] },
   MasterList: { getById: [Function], getByIdsWithPermissionLevel: [Function] },
   StateOfThings:
    { insert: [Function],
      getById: [Function],
      getOfferConceptIdeaStates: [Function],
      getOfferConceptIdeaStatesForCampaign: [Function],
      notRejected: [Function] },
   Actor:
    { insert: [Function],
      getById: [Function],
      getByIdList: [Function] },
   Role: { getByUserIdList: [Function] },
   SocialAccount:
    { update: [Function],
      getById: [Function],
      getByTypes: [Function: getByTypes] },
   ResearchCategory: { insert: [Function], getById: [Function] } }