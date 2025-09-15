CREATE MATERIALIZED VIEW views.mat_kumulative AS
WITH parsed_periods AS (
 SELECT
   ldp.series_code,
   ldp.table_code,
   ldp.name_long,
   ldp.period_id,
   ldp.value,
   ldp.year,
   ldp.month,
   ldp.date,
   SPLIT_PART(ldp.series_code, '--', 3) as konto
 FROM views.mat_latest_data_points ldp
 WHERE ldp.year IS NOT NULL
   AND ldp.interval = 'M'
   AND ldp.table_code IN ('ZPIZ', 'ZZZS', 'OB', 'KBJF', 'DP')
),
cumulative_data AS (
 SELECT
   pp.*,
   jkl.group_code,
   SUM(pp.value) OVER (
     PARTITION BY pp.series_code, pp.year
     ORDER BY pp.month
     ROWS UNBOUNDED PRECEDING
   ) as cumulative_value
 FROM parsed_periods pp
 LEFT JOIN views."JF_konto_lookup" jkl ON pp.konto = jkl.konto
),
with_lag_calculations AS (
 SELECT
   c.*,
   LAG(c.cumulative_value, 1) OVER (
     PARTITION BY c.series_code, c.month
     ORDER BY c.year
   ) as prev_year_cumulative
 FROM cumulative_data c
),
with_yoy_calculations AS (
 SELECT
   wlc.*,
   wlc.cumulative_value - wlc.prev_year_cumulative as yoy_change,
   ROUND(
     (wlc.cumulative_value / NULLIF(wlc.prev_year_cumulative, 0) - 1) * 100, 2
   ) as yoy_pct_change
 FROM with_lag_calculations wlc
),
group_totals AS (
 SELECT
   table_code,
   year,
   month,
   konto as group_code,
   prev_year_cumulative as group_prev_cumulative
 FROM with_yoy_calculations
 WHERE konto IN ('4', '7')
),
current_max_period AS (
 SELECT
   series_code,
   MAX(month) as max_month
 FROM parsed_periods
 WHERE year = EXTRACT(YEAR FROM CURRENT_DATE)
 GROUP BY series_code
)
SELECT
 wyc.series_code,
 wyc.table_code,
 wyc.konto,
 wyc.group_code,
 wyc.name_long,
 wyc.period_id,
 wyc.date,
 wyc.year,
 wyc.month,
 wyc.value as monthly_value,
 wyc.cumulative_value,
 wyc.prev_year_cumulative,
 wyc.yoy_change,
 wyc.yoy_pct_change,
 CASE
   WHEN wyc.group_code IS NOT NULL THEN
     ROUND(
       wyc.yoy_pct_change *
       (wyc.prev_year_cumulative / NULLIF(gt.group_prev_cumulative, 0)), 2
     )
   ELSE NULL
 END as contribution,
 wyc.month = MAX(wyc.month) OVER () as max_month
FROM with_yoy_calculations wyc
LEFT JOIN group_totals gt ON wyc.table_code = gt.table_code
 AND wyc.year = gt.year
 AND wyc.month = gt.month
 AND wyc.group_code = gt.group_code
JOIN current_max_period cmp ON wyc.series_code = cmp.series_code
WHERE wyc.month <= cmp.max_month
ORDER BY wyc.series_code, wyc.year, wyc.month
WITH DATA;

-- Add performance indexes
CREATE INDEX idx_mat_kumulative_series_year_month ON views.mat_kumulative (series_code, year, month);
CREATE INDEX idx_mat_kumulative_table_code ON views.mat_kumulative (table_code);
CREATE INDEX idx_mat_kumulative_konto ON views.mat_kumulative (konto);
CREATE INDEX idx_mat_kumulative_date ON views.mat_kumulative (date);
CREATE INDEX idx_mat_kumulative_group_code ON views.mat_kumulative (group_code) WHERE group_code IS NOT NULL;

-- Power Query compatible wrapper
CREATE OR REPLACE VIEW views.jf_kumulative AS
SELECT * FROM views.mat_kumulative;


-- -----------------------------------------------------------------------------
-- QUARTERLY MATERIALISED VIEW WITH INDICES AND VIEW ---------------------------
-- -----------------------------------------------------------------------------
CREATE  MATERIALIZED VIEW views.mat_quarterly_yoy AS
WITH quarterly_aggregated AS (
  SELECT
    ldp.series_code,
    ldp.table_code,
    ldp.name_long,
    ldp.year,
    CASE
      WHEN ldp.month IN (1,2,3) THEN 1
      WHEN ldp.month IN (4,5,6) THEN 2
      WHEN ldp.month IN (7,8,9) THEN 3
      WHEN ldp.month IN (10,11,12) THEN 4
    END as quarter,
    SPLIT_PART(ldp.series_code, '--', 3) as konto,
    SUM(ldp.value) as quarterly_value,
    COUNT(*) as month_count
  FROM views.mat_latest_data_points ldp
  WHERE ldp.year IS NOT NULL
    AND ldp.interval = 'M'
    AND ldp.table_code IN ('ZPIZ', 'ZZZS', 'OB', 'KBJF', 'DP')
    -- Filter out current incomplete quarter upfront
    AND NOT (ldp.year = EXTRACT(YEAR FROM CURRENT_DATE)
             AND CASE
               WHEN ldp.month IN (1,2,3) THEN 1
               WHEN ldp.month IN (4,5,6) THEN 2
               WHEN ldp.month IN (7,8,9) THEN 3
               WHEN ldp.month IN (10,11,12) THEN 4
             END >= EXTRACT(QUARTER FROM CURRENT_DATE))
  GROUP BY ldp.series_code, ldp.table_code, ldp.name_long, ldp.year,
           CASE
             WHEN ldp.month IN (1,2,3) THEN 1
             WHEN ldp.month IN (4,5,6) THEN 2
             WHEN ldp.month IN (7,8,9) THEN 3
             WHEN ldp.month IN (10,11,12) THEN 4
           END,
           SPLIT_PART(ldp.series_code, '--', 3)
  HAVING COUNT(*) = 3  -- Only complete quarters
),
quarterly_with_metadata AS (
  SELECT
    qa.*,
    qa.year::text || 'Q' || qa.quarter::text as period_id,
    MAKE_DATE(qa.year, (qa.quarter - 1) * 3 + 1, 1) as date
  FROM quarterly_aggregated qa
),
with_yoy_calculations AS (
  SELECT
    qwm.*,
    jkl.group_code,
    LAG(qwm.quarterly_value, 1) OVER (
      PARTITION BY qwm.series_code, qwm.quarter
      ORDER BY qwm.year
    ) as prev_year_value
  FROM quarterly_with_metadata qwm
  LEFT JOIN views."JF_konto_lookup" jkl ON qwm.konto = jkl.konto
),
final_calculations AS (
  SELECT
    wyc.*,
    wyc.quarterly_value - wyc.prev_year_value as yoy_change,
    (wyc.quarterly_value / NULLIF(wyc.prev_year_value, 0) - 1) * 100 as yoy_pct_change
  FROM with_yoy_calculations wyc
),
group_totals AS (
  SELECT
    table_code, year, quarter, konto as group_code,
    prev_year_value as group_prev_year_value
  FROM final_calculations
  WHERE konto IN ('4', '7')
)
SELECT
  fc.series_code, fc.table_code, fc.konto, fc.group_code, fc.name_long,
  fc.period_id, fc.date, fc.year, fc.quarter, fc.quarterly_value,
  fc.prev_year_value, fc.yoy_change, fc.yoy_pct_change,
  CASE
    WHEN fc.group_code IS NOT NULL THEN
      fc.yoy_pct_change * (fc.prev_year_value / NULLIF(gt.group_prev_year_value, 0))
    ELSE NULL
  END as contribution
FROM final_calculations fc
LEFT JOIN group_totals gt ON fc.table_code = gt.table_code
  AND fc.year = gt.year AND fc.quarter = gt.quarter AND fc.group_code = gt.group_code
ORDER BY fc.series_code, fc.year, fc.quarter
WITH DATA;

-- Indexes
CREATE INDEX idx_mat_quarterly_yoy_series_year_quarter ON views.mat_quarterly_yoy (series_code, year, quarter);
CREATE INDEX idx_mat_quarterly_yoy_table_code ON views.mat_quarterly_yoy (table_code);
CREATE INDEX idx_mat_quarterly_yoy_konto ON views.mat_quarterly_yoy (konto);

-- Wrapper view
CREATE OR REPLACE VIEW views.jf_cetrtletni AS
SELECT * FROM views.mat_quarterly_yoy;

-- regular view for all KBJF stuff
CREATE VIEW views.filtered_latest_data_points AS
SELECT
    series_code,
    table_code,
    name_long,
    period_id,
    value,
    "interval",
    year,
    month,
    date,
    -- Extract penultimate segment between hyphens
    CASE
        WHEN array_length(string_to_array(series_code, '--'), 1) >= 2
        THEN (string_to_array(series_code, '--'))[array_length(string_to_array(series_code, '--'), 1) - 1]
        ELSE NULL
    END AS konto
FROM views.mat_latest_data_points
WHERE table_code IN ('KBJF', 'DP', 'OB', 'ZPIZ', 'ZZZS')
ORDER BY series_code, period_id;
