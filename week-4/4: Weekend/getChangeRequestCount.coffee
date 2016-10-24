getChangeRequestCount = (db) -> (campaign_id) ->
  sql = """
    SELECT
      COUNT(*) AS `count`,
      COALESCE(SUM(
        CASE
          WHEN
            `role`.`role` = 'client'
          THEN 1
          ELSE 0
        END
      ), 0) AS `pending_brand_approval`,
      COALESCE(SUM(
        CASE
          WHEN
            `role`.`role` = 'campaign_manager'
          THEN 1
          ELSE 0
        END
      ), 0) AS `pending_campaign_manager_approval`,
      COALESCE(SUM(
        CASE
          WHEN
            `change_request`.`character_count` >
            `social_account_type`.`character_limit`
          THEN 1
          ELSE 0
        END
      ), 0) AS `invalid`

    FROM `change_request`
    JOIN `change_request_status_type`
      ON `change_request`.`change_request_status_type_id` =
          `change_request_status_type`.`id`
    JOIN `campaign_offer`
      ON `change_request`.`campaign_offer_id` = `campaign_offer`.`id`
    JOIN `social_account`
      ON `social_account`.`id` = `campaign_offer`.`social_account_id`
    JOIN `social_account_type`
      ON `social_account_type`.`id` = `social_account`.`social_account_type_id`
    JOIN `change_request_review`
      ON `change_request_review`.`change_request_id` = `change_request`.`id`
    JOIN `role`
      ON `change_request_review`.`reviewer_required_role_id` = `role`.`id`
    WHERE
      `campaign_offer`.`campaign_id` = ?
      AND `change_request_status_type`.`code` = 'pending'
      AND `change_request_review`.`review_date` IS NULL
      AND `change_request_review`.`deleted` IS NULL
      AND `change_request`.`deleted` IS NULL
      AND `campaign_offer`.`deleted` IS NULL
  """
  MySql.query(db, sql, 'ChangeRequest::GetCounterByOffer')([campaign_id])
  .then (results) -> results[0]
