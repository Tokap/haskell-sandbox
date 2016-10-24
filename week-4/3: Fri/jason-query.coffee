user_id of brand_manager

JOIN campaign # with campaign_user_permission_level i think

JOIN campaign_offer

JOIN offer_concept_idea

JOIN state AS internal_approved_state
ON reference_type == 'idea'
   reference_id == offer_concept_idea.id
   actor_type == 'internal'
   status == 'approved'

LEFT JOIN state AS external_resolved_state
ON reference_type == 'idea'
   reference_id == offer_concept_idea.id
   actor_type == 'external'
   status == 'approved'

LEFT JOIN state AS external_rejected_state
ON reference_type == 'idea'
   reference_id == offer_concept_idea.id
   actor_type == 'external'
   status == 'rejected'

WHERE
  user_id = :user_id
  external_rejected_state.id IS NULL
